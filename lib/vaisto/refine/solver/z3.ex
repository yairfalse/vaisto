defmodule Vaisto.Refine.Solver.Z3 do
  @moduledoc """
  The production solver: Z3 over an Erlang port (RFC §11.1, §11.2).

  A `check/2` call runs one `z3 -in -smt2` process. Each VC gets a fresh
  context: `(reset)`, then fixed options (a deterministic resource limit,
  zero seeds, models on), then the query from `Vaisto.Refine.SMTLIB.lower/1`.
  It never uses `push`/`pop`, whose history could change answers. Each
  response is framed by an `(echo ...)` marker.

  `unsat` is `:valid`; `sat` is `{:invalid, model}`, with the model read by
  `(get-value ...)` and keyed by IR variable names. Anything else is
  `{:unknown, reason}`, never `:valid`:

    * `:solver_missing`: no `z3` executable;
    * `:resource_limit`: the `:rlimit` ran out;
    * `{:incomplete, text}`: Z3 answered `unknown` for another reason;
    * `:timeout`: the wall-clock backstop fired;
    * `{:solver_exit, status}`: the process died;
    * `{:solver_error, text}`: Z3 reported an error;
    * `{:unparsable, text}`: output that fits no expected shape.

  After a timeout or an exit the process is gone, so the remaining VCs of
  the call run on a new one.

  Options:

    * `:z3`: the executable (default: `z3` on PATH);
    * `:rlimit`: Z3's resource limit per VC (default 5_000_000);
    * `:timeout`: wall-clock milliseconds per VC (default 60_000).
  """

  @behaviour Vaisto.Refine.Solver

  alias Vaisto.Refine.SMTLIB

  @default_rlimit 5_000_000
  @default_timeout 60_000

  @doc "Whether the `z3` executable can be found."
  @spec available?(keyword()) :: boolean()
  def available?(opts \\ []), do: executable(opts) != nil

  @impl true
  def identity, do: identity([])

  @doc "`{\"z3\", version}` from `z3 --version`; the version is `\"unavailable\"` without a solver."
  @spec identity(keyword()) :: {String.t(), String.t()}
  def identity(opts) do
    with path when is_binary(path) <- executable(opts),
         {out, 0} <- System.cmd(path, ["--version"], stderr_to_stdout: true),
         [_, version] <- Regex.run(~r/Z3 version (\S+)/, out) do
      {"z3", version}
    else
      _ -> {"z3", "unavailable"}
    end
  end

  @doc """
  Answer each VC, in input order. Every VC is lowered first, so an
  ill-formed one raises `ArgumentError` whether or not Z3 is present.
  """
  @impl true
  @spec check([Vaisto.Refine.VC.t()], keyword()) :: [{term(), Vaisto.Refine.Solver.result()}]
  def check(vcs, opts) do
    queries = for vc <- vcs, do: {vc.id, SMTLIB.lower(vc)}

    settings = %{
      rlimit: Keyword.get(opts, :rlimit, @default_rlimit),
      timeout: Keyword.get(opts, :timeout, @default_timeout)
    }

    case executable(opts) do
      nil -> for {id, _query} <- queries, do: {id, {:unknown, :solver_missing}}
      path -> run(queries, path, settings, nil, [])
    end
  end

  defp executable(opts), do: System.find_executable(Keyword.get(opts, :z3, "z3"))

  # --- The session loop -------------------------------------------------------

  defp run([], _path, _settings, session, acc) do
    close(session)
    Enum.reverse(acc)
  end

  defp run(queries, path, settings, nil, acc) do
    case open(path) do
      {:ok, session} -> run(queries, path, settings, session, acc)
      {:error, reason} -> Enum.reverse(acc, for({id, _query} <- queries, do: {id, {:unknown, reason}}))
    end
  end

  defp run([{id, query} | rest], path, settings, session, acc) do
    case solve(session, query, settings) do
      {:ok, result, session} -> run(rest, path, settings, session, [{id, result} | acc])
      {:lost, result} -> run(rest, path, settings, nil, [{id, result} | acc])
    end
  end

  defp solve(session, query, settings) do
    deadline = System.monotonic_time(:millisecond) + settings.timeout

    script = [
      "(reset)\n",
      "(set-option :rlimit ", Integer.to_string(settings.rlimit), ")\n",
      "(set-option :random-seed 0)\n",
      "(set-option :smt.random_seed 0)\n",
      "(set-option :sat.random_seed 0)\n",
      "(set-option :produce-models true)\n",
      query.text
    ]

    with {:ok, lines, session} <- exchange(session, script, deadline) do
      case Enum.reject(lines, &(&1 == "")) do
        ["unsat"] -> {:ok, :valid, session}
        ["sat"] -> counterexample(session, query.names, deadline)
        ["unknown"] -> reason_unknown(session, deadline)
        _ -> {:ok, {:unknown, garbage(lines)}, session}
      end
    end
  end

  defp counterexample(session, names, _deadline) when map_size(names) == 0, do: {:ok, {:invalid, %{}}, session}

  defp counterexample(session, names, deadline) do
    smt_names = names |> Map.keys() |> Enum.sort_by(&{byte_size(&1), &1})

    with {:ok, lines, session} <- exchange(session, ["(get-value (", Enum.intersperse(smt_names, " "), "))\n"], deadline) do
      case parse_model(Enum.join(lines, "\n"), names) do
        {:ok, model} -> {:ok, {:invalid, model}, session}
        :error -> {:ok, {:unknown, garbage(lines)}, session}
      end
    end
  end

  defp reason_unknown(session, deadline) do
    with {:ok, lines, session} <- exchange(session, "(get-info :reason-unknown)\n", deadline) do
      reason =
        case read_sexps(Enum.join(lines, "\n")) do
          {:ok, [[":reason-unknown", "\"" <> quoted]]} -> classify_unknown(String.trim_trailing(quoted, "\""))
          _ -> garbage(lines)
        end

      {:ok, {:unknown, reason}, session}
    end
  end

  # No wall-clock limit is set inside Z3, so a cancellation means the rlimit ran out.
  defp classify_unknown(text) do
    if text == "canceled" or text =~ "resource limit", do: :resource_limit, else: {:incomplete, text}
  end

  defp garbage(lines) do
    text = Enum.join(lines, "\n")
    if Enum.any?(lines, &String.starts_with?(&1, "(error")), do: {:solver_error, text}, else: {:unparsable, text}
  end

  # --- The port -----------------------------------------------------------------

  defp open(path) do
    port = Port.open({:spawn_executable, path}, [:binary, :exit_status, :stderr_to_stdout, args: ["-in", "-smt2"]])
    {:ok, %{port: port, buffer: ""}}
  rescue
    e in ErlangError ->
      {:error, if(e.original == :enoent, do: :solver_missing, else: {:solver_spawn_failed, e.original})}
  end

  # Send commands, then wait for the output up to a fresh echo marker.
  defp exchange(%{port: port} = session, commands, deadline) do
    marker = "vaisto-refine-" <> Integer.to_string(System.unique_integer([:positive]))

    if command(port, [commands, "(echo \"", marker, "\")\n"]) do
      await(session, marker, deadline)
    else
      close(session)
      {:lost, {:unknown, {:solver_exit, :closed}}}
    end
  end

  defp command(port, iodata) do
    Port.command(port, iodata)
  rescue
    ArgumentError -> false
  end

  defp await(%{port: port, buffer: buffer} = session, marker, deadline) do
    case take_until(buffer, marker) do
      {:ok, lines, rest} ->
        {:ok, lines, %{session | buffer: rest}}

      :more ->
        receive do
          {^port, {:data, data}} ->
            await(%{session | buffer: buffer <> data}, marker, deadline)

          {^port, {:exit_status, status}} ->
            close(session)
            {:lost, {:unknown, {:solver_exit, status}}}
        after
          max(deadline - System.monotonic_time(:millisecond), 0) ->
            kill(session)
            {:lost, {:unknown, :timeout}}
        end
    end
  end

  defp take_until(buffer, marker) do
    {complete, [partial]} = buffer |> String.split("\n") |> Enum.split(-1)
    complete = Enum.map(complete, &String.trim_trailing(&1, "\r"))

    case Enum.split_while(complete, &(&1 != marker)) do
      {_lines, []} -> :more
      {lines, [_marker | after_marker]} -> {:ok, lines, Enum.map_join(after_marker, &(&1 <> "\n")) <> partial}
    end
  end

  # Closing the port does not stop a Z3 that is busy solving, so kill it first.
  defp kill(%{port: port} = session) do
    with {:os_pid, pid} <- Port.info(port, :os_pid),
         kill when is_binary(kill) <- System.find_executable("kill") do
      System.cmd(kill, ["-KILL", Integer.to_string(pid)], stderr_to_stdout: true)
    end

    close(session)
  end

  defp close(nil), do: :ok

  defp close(%{port: port}) do
    try do
      Port.close(port)
    rescue
      ArgumentError -> :ok
    end

    flush(port)
  end

  defp flush(port) do
    receive do
      {^port, _message} -> flush(port)
      {:EXIT, ^port, _reason} -> flush(port)
    after
      0 -> :ok
    end
  end

  # --- Responses ----------------------------------------------------------------

  defp parse_model(text, names) do
    with {:ok, [pairs]} when is_list(pairs) <- read_sexps(text) do
      Enum.reduce_while(pairs, {:ok, %{}}, fn
        [smt_name, value], {:ok, model} when is_map_key(names, smt_name) ->
          {:cont, {:ok, Map.put(model, Map.fetch!(names, smt_name), model_value(value))}}

        _pair, _acc ->
          {:halt, :error}
      end)
    else
      _ -> :error
    end
  end

  defp model_value("true"), do: true
  defp model_value("false"), do: false
  defp model_value(["-", digits] = value) when is_binary(digits), do: if(numeral?(digits), do: -String.to_integer(digits), else: render(value))
  defp model_value(value) when is_binary(value), do: if(numeral?(value), do: String.to_integer(value), else: value)
  defp model_value(value), do: render(value)

  defp numeral?(text), do: text =~ ~r/\A[0-9]+\z/

  defp render(leaf) when is_binary(leaf), do: leaf
  defp render(list), do: "(" <> Enum.map_join(list, " ", &render/1) <> ")"

  # A minimal reader for Z3's responses: lists, and leaves kept as their source text.
  defp read_sexps(text) do
    {:ok, read_all(skip_space(text), [])}
  catch
    :unparsable -> :error
  end

  defp read_all("", acc), do: Enum.reverse(acc)

  defp read_all(text, acc) do
    {form, rest} = read(text)
    read_all(skip_space(rest), [form | acc])
  end

  defp read("(" <> rest), do: read_list(skip_space(rest), [])
  defp read(")" <> _rest), do: throw(:unparsable)
  defp read("\"" <> rest), do: read_string(rest, "\"")

  defp read("|" <> rest) do
    case :binary.split(rest, "|") do
      [symbol, rest] -> {"|" <> symbol <> "|", rest}
      [_] -> throw(:unparsable)
    end
  end

  defp read(text) do
    case take_atom(text, []) do
      {"", _rest} -> throw(:unparsable)
      taken -> taken
    end
  end

  defp read_list(")" <> rest, acc), do: {Enum.reverse(acc), rest}
  defp read_list("", _acc), do: throw(:unparsable)

  defp read_list(text, acc) do
    {form, rest} = read(text)
    read_list(skip_space(rest), [form | acc])
  end

  # SMT-LIB escapes a quote inside a string by doubling it.
  defp read_string(text, acc) do
    case :binary.split(text, "\"") do
      [part, "\"" <> rest] -> read_string(rest, acc <> part <> "\"\"")
      [part, rest] -> {acc <> part <> "\"", rest}
      [_] -> throw(:unparsable)
    end
  end

  defp take_atom(<<c, _::binary>> = rest, acc) when c in ~c" \t\r\n()\"|",
    do: {acc |> Enum.reverse() |> IO.iodata_to_binary(), rest}

  defp take_atom(<<c, rest::binary>>, acc), do: take_atom(rest, [c | acc])
  defp take_atom("", acc), do: {acc |> Enum.reverse() |> IO.iodata_to_binary(), ""}

  defp skip_space(<<c, rest::binary>>) when c in ~c" \t\r\n", do: skip_space(rest)
  defp skip_space(text), do: text
end
