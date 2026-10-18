defmodule Vaisto.Liquid.Harness do
  @moduledoc """
  The differential harness (RFC §6.3, `docs/design/liquid-core.md` §9).

  `run/1` elaborates and verifies a signed program, evaluates its Core with
  the independent reference evaluator, and compares it with native BEAM:

    * `:agree` - evaluator and BEAM give the same outcome (§9), and the
      returned value inhabits `main`'s type (§5.1);
    * `{:disagree, backends}` - these backends give a different outcome from
      the evaluator, or return a value that is not of `main`'s type;
    * `{:lint_rejects, problems}` - the elaborator produced ill-typed Core;
    * `{:outside_fragment, reason}` - `main` uses something Core version 0
      does not cover yet;
    * `{:elab_rejects, error}` - the source fails checking;
    * `:no_main` - there is nothing to run.

  Every run happens in its own process, bounded in time and memory: a
  program may loop, and not ending is an outcome too.
  """

  alias Vaisto.Liquid.{Eval, Lint, Runtime}

  @timeout 2_000
  @max_heap_words 20_000_000

  @type outcome ::
          {:ok, term()} | {:crash, term()} | {:compile_error, String.t()} | :did_not_end | {:went_wrong, String.t()}

  @doc "The outcome of running `main` as native BEAM, bounded in time and memory."
  @spec beam_outcomes(String.t()) :: %{native: outcome()}
  def beam_outcomes(source), do: %{native: on_beam(source)}

  @doc "The outcome of evaluating a Core module's `main`, bounded in time and memory."
  @spec evaluate(Vaisto.Liquid.Canonical.tree()) :: outcome()
  def evaluate(core), do: bounded(fn -> Eval.run(core, :main, []) end)

  @doc "Whether two outcomes agree (liquid-core.md §9)."
  @spec agrees?(outcome(), outcome()) :: boolean()
  def agrees?(a, b), do: agree?(a, b)

  @doc "Run one program every way and compare (see the module doc)."
  @spec run(String.t()) :: map()
  def run(source) do
    with {:ok, core} <- Vaisto.Compilation.check_liquid(source, :Harness, format_errors: false) do
      if effectful?(core) do
        %{verdict: {:outside_fragment, "effects need a scripted handler"}, core: core}
      else
        compare(source, core)
      end
    else
      {:error, error} -> %{verdict: {:elab_rejects, error}}
    end
  end

  defp effectful?([:eff | labels]), do: Enum.any?(Enum.drop(labels, -1), &(&1 != :crash))
  defp effectful?(l) when is_list(l), do: Enum.any?(l, &effectful?/1)
  defp effectful?(_), do: false

  defp compare(source, core) do
    defs = for [:def, f, type, _] <- core, into: %{}, do: {f, type}
    decls = for [:type | _] = d <- core, do: d

    cond do
      not Map.has_key?(defs, :main) ->
        %{verdict: :no_main, core: core}

      true ->
        case Lint.check_module(core) do
          {:error, problems} ->
            %{verdict: {:lint_rejects, problems}, core: core}

          :ok ->
            result_type = result_type(Map.fetch!(defs, :main))
            eval = bounded(fn -> Eval.run(core, :main, []) end)
            beam = beam_outcomes(source)

            differing =
              for {backend, outcome} <- beam,
                  not agree?(eval, outcome) or not typed_value?(outcome, result_type, decls),
                  do: backend

            verdict = if differing == [], do: :agree, else: {:disagree, differing}
            %{verdict: verdict, core: core, eval: eval, beam: beam}
        end
    end
  end

  defp result_type([:forall, _binders, type]), do: result_type(type)
  defp result_type([:->, [], _eff, result]), do: result

  # --- Running on BEAM (§9) ----------------------------------------------------------

  defp on_beam(source) do
    module = :"VaistoHarness_#{:erlang.unique_integer([:positive])}"

    # Generated-code warnings are not the harness's to print.
    {compiled, _diagnostics} =
      Code.with_diagnostics(fn -> Vaisto.Compilation.compile(source, module, format_errors: false) end)

    case compiled do
      {:ok, ^module, _bytecode} ->
        try do
          bounded(fn -> Runtime.run(module, :main, [], handler: :pure) end)
        after
          :code.purge(module)
          :code.delete(module)
        end

      {:error, error} ->
        {:compile_error, error |> inspect(limit: 8) |> String.slice(0, 200)}
    end
  rescue
    e -> {:compile_error, Exception.message(e) |> String.slice(0, 200)}
  end

  # Two outcomes agree when their values are equal with functions left out, or
  # their crash reasons are equal (§9). Not ending agrees only with itself.
  defp agree?({:ok, a}, {:ok, b}), do: erase_functions(a) === erase_functions(b)
  defp agree?({:effect, op, a}, {:effect, op, b}), do: erase_functions(a) === erase_functions(b)
  defp agree?({:crash, a}, {:crash, b}), do: a === b
  defp agree?(a, b), do: a == b

  defp erase_functions(%Eval.Closure{}), do: :function
  defp erase_functions(f) when is_function(f), do: :function
  defp erase_functions(t) when is_tuple(t), do: t |> Tuple.to_list() |> erase_functions() |> List.to_tuple()
  defp erase_functions([h | t]), do: [erase_functions(h) | erase_functions(t)]
  defp erase_functions(m) when is_map(m), do: Map.new(m, fn {k, v} -> {erase_functions(k), erase_functions(v)} end)
  defp erase_functions(x), do: x

  defp typed_value?({:ok, v}, type, decls), do: Eval.value_of?(v, type, types: decls)
  defp typed_value?(_outcome, _type, _decls), do: true

  # Run `fun` in its own process, bounded in time and memory.
  defp bounded(fun) do
    parent = self()

    {pid, ref} =
      spawn_monitor(fn ->
        Process.flag(:max_heap_size, %{size: @max_heap_words, kill: true, error_logger: false})

        result =
          try do
            fun.()
          rescue
            e in Eval.Stuck -> {:went_wrong, Exception.message(e)}
          end

        send(parent, {self(), result})
      end)

    receive do
      {^pid, result} ->
        Process.demonitor(ref, [:flush])
        result

      {:DOWN, ^ref, _, _, _} ->
        :did_not_end
    after
      @timeout ->
        Process.exit(pid, :kill)
        Process.demonitor(ref, [:flush])
        :did_not_end
    end
  end

  # --- The corpus ----------------------------------------------------------------------

  @doc """
  Every Vaisto program with a `main` that appears as a string literal in the
  given Elixir test files (RFC §6.3: "the source of every existing test").
  """
  @spec corpus([Path.t()]) :: [{Path.t(), String.t()}]
  def corpus(files) do
    for file <- files,
        {:ok, quoted} = Code.string_to_quoted(File.read!(file)),
        program <- programs(quoted) |> Enum.uniq(),
        do: {file, program}
  end

  defp programs(quoted) do
    {_, found} =
      Macro.prewalk(quoted, [], fn
        s, acc when is_binary(s) -> {s, if(String.contains?(s, "(defn main"), do: [s | acc], else: acc)}
        node, acc -> {node, acc}
      end)

    Enum.reverse(found)
  end
end
