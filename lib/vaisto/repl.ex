defmodule Vaisto.REPL do
  @moduledoc """
  Interactive REPL for Vaisto with accumulating context.

  The REPL maintains state across inputs:
  - Display environment: Verified source-level signatures
  - Code buffer: Previously defined functions re-compiled with each expression

  This allows you to define functions incrementally:

      λ (defn double [x :int] :int (* x 2))
      => :ok (Fn Int Int)

      λ (double 21)
      => 42

      λ (defn quad [x :int] :int (double (double x)))
      => :ok (Fn Int Int)

      λ (quad 5)
      => 20

  ## Commands

  - `:help` - Show help
  - `:env` - Show defined functions and their types
  - `:clear` - Clear all definitions
  - `:save <file>` - Save definitions to file
  - `:load <file>` - Load definitions from file
  - `:quit` or `:q` - Exit REPL
  """

  alias Vaisto.{Compilation, Elab.Types}

  @version Mix.Project.config()[:version]

  defstruct [
    # Display environment: name -> verified signature text
    :env,
    # Source definitions (AST forms)
    :defs,
    # Original source strings for save
    :def_sources,
    # Module name counter
    :counter
  ]

  @doc """
  Starts the interactive REPL.
  """
  @spec start() :: :ok
  def start do
    IO.puts("Vaisto REPL v#{@version} (Liquid Core)")
    IO.puts("Type :help for commands, :quit to exit\n")

    state = %__MODULE__{
      env: %{},
      defs: [],
      def_sources: [],
      counter: 0
    }

    loop(state)
  end

  defp loop(state) do
    case IO.gets("λ ") do
      :eof ->
        IO.puts("\nGoodbye!")
        :ok

      {:error, _} ->
        IO.puts("\nGoodbye!")
        :ok

      input ->
        input = String.trim(input)

        if input == "" do
          loop(state)
        else
          case handle_input(input, state) do
            {:quit, _state} ->
              IO.puts("Goodbye!")
              :ok

            {:continue, new_state} ->
              loop(new_state)
          end
        end
    end
  end

  defp handle_input(":" <> command, state) do
    handle_command(command, state)
  end

  defp handle_input(input, state) do
    case process_input(input, state) do
      {:ok, result, new_state} ->
        IO.puts("=> #{result}")
        {:continue, new_state}

      {:error, msg, new_state} ->
        IO.puts("error: #{msg}")
        {:continue, new_state}
    end
  end

  # --- Commands ---

  defp handle_command("quit", state), do: {:quit, state}
  defp handle_command("q", state), do: {:quit, state}

  defp handle_command("help", state) do
    IO.puts("""
    Commands:
      :help        Show this help
      :env         Show defined functions and types
      :clear       Clear all definitions
      :save <file> Save definitions to file
      :load <file> Load definitions from file
      :quit, :q    Exit REPL

    Examples:
      (defn double [x :int] :int (* x 2))   Define a function
      (double 21)                  Call it
      (+ 1 2)                      Evaluate expression
    """)

    {:continue, state}
  end

  defp handle_command("env", state) do
    if map_size(state.env) == 0 do
      IO.puts("No user-defined functions yet.")
    else
      IO.puts("Defined functions:")

      state.env
      |> Enum.sort()
      |> Enum.each(fn {name, signature} ->
        IO.puts("  #{name} : #{signature}")
      end)
    end

    {:continue, state}
  end

  defp handle_command("clear", state) do
    IO.puts("Cleared all definitions.")
    {:continue, %{state | env: %{}, defs: [], def_sources: []}}
  end

  defp handle_command("save " <> filename, state) do
    filename = String.trim(filename)
    content = Enum.join(state.def_sources, "\n\n")

    case File.write(filename, content) do
      :ok ->
        IO.puts("Saved #{length(state.def_sources)} definitions to #{filename}")

      {:error, reason} ->
        IO.puts("Error saving: #{reason}")
    end

    {:continue, state}
  end

  defp handle_command("load " <> filename, state) do
    filename = String.trim(filename)

    case File.read(filename) do
      {:ok, content} ->
        case load_definitions(content, state) do
          {:ok, new_state} ->
            IO.puts("Loaded definitions from #{filename}")
            {:continue, new_state}

          {:error, message} ->
            IO.puts("Error loading: #{message}")
            {:continue, state}
        end

      {:error, reason} ->
        IO.puts("Error loading: #{reason}")
        {:continue, state}
    end
  end

  defp handle_command(unknown, state) do
    IO.puts("Unknown command: :#{unknown}")
    IO.puts("Type :help for available commands.")
    {:continue, state}
  end

  # --- Processing ---

  defp process_input(input, state) do
    case Compilation.parse(input, with_source: true) do
      {:ok, pairs} ->
        if Enum.all?(pairs, fn {form, _} -> definition?(form) end) do
          handle_definitions(pairs, state)
        else
          handle_expression(input, state)
        end

      {:error, error} ->
        {:error, format_error(error), state}
    end
  end

  defp definition?(form) when is_tuple(form) and tuple_size(form) > 1 do
    elem(form, 0) in [
      :defn,
      :defn_constrained,
      :defn_multi,
      :defval,
      :deftype,
      :deftype_deriving,
      :defclass,
      :instance,
      :instance_constrained,
      :extern,
      :process,
      :supervise,
      :ns,
      :import,
      :defprompt,
      :pipeline
    ]
  end

  defp definition?(_), do: false

  defp definition_key(form) do
    case elem(form, 0) do
      tag when tag in [:defn, :defn_constrained, :defn_multi, :defval] ->
        {:binding, elem(form, 1)}

      tag when tag in [:deftype, :deftype_deriving] ->
        {:type, elem(form, 1)}

      _ ->
        {:declaration, form}
    end
  end

  defp handle_definitions(incoming, state) do
    pairs =
      Enum.reduce(incoming, Enum.zip(state.defs, state.def_sources), fn {form, _} = pair, pairs ->
        Enum.reject(pairs, fn {old, _} -> definition_key(old) == definition_key(form) end) ++
          [pair]
      end)

    {defs, sources} = Enum.unzip(pairs)
    source = Enum.join(sources, "\n\n")

    case Compilation.check_liquid(source, :VaistoREPLCheck, format_errors: false) do
      {:ok, core} ->
        env = display_env(core, defs)

        message =
          case List.last(incoming) do
            {form, _} ->
              case Map.fetch(env, elem(form, 1)) do
                {:ok, signature} -> ":ok #{signature}"
                :error -> ":ok"
              end

            nil ->
              ":ok"
          end

        {:ok, message, %{state | env: env, defs: defs, def_sources: sources}}

      {:error, error} ->
        {:error, format_error(error), state}
    end
  end

  defp display_env(core, defs) do
    constraints =
      for [:meta | entries] <- core,
          [:"function-constraints" | cs] <- entries,
          [f | cs] <- cs,
          into: %{},
          do: {f, cs}

    bindings =
      for form <- defs,
          elem(form, 0) in [:defn, :defn_constrained, :defn_multi, :defval, :pipeline, :process],
          into: %{},
          do: {elem(form, 1), elem(form, 0)}

    for [:def, f, signature, _] <- core, Map.has_key?(bindings, f), into: %{} do
      {binders, type} =
        case signature do
          [:forall, binders, type] -> {binders, type}
          type -> {[], type}
        end

      cs = Map.get(constraints, f, [])
      type = if cs != [] or bindings[f] == :defval, do: List.last(type), else: type
      signature = if binders == [], do: type, else: [:forall, binders, type]

      suffix =
        if cs == [],
          do: "",
          else:
            " where " <> Enum.map_join(cs, ", ", fn [c, t, _] -> "#{c} #{Types.format(t)}" end)

      {f, Types.format(signature) <> suffix}
    end
  end

  defp handle_expression(input, state) do
    source = Enum.join(state.def_sources ++ [input], "\n\n")
    module_name = :"VaistoREPL_#{state.counter}"

    entry =
      Stream.iterate(0, &(&1 + 1))
      |> Stream.map(&:"__vaisto_repl_entry_#{&1}")
      |> Enum.find(&(not Map.has_key?(state.env, &1)))

    case compile_and_run(source, module_name, entry) do
      {:ok, value} -> {:ok, inspect(value), %{state | counter: state.counter + 1}}
      {:error, message} -> {:error, format_error(message), state}
    end
  end

  defp compile_and_run(source, module_name, entry) do
    case Compilation.compile_liquid(source, module_name,
           expression_entry: entry,
           format_errors: false
         ) do
      {:ok, ^module_name, _} ->
        try do
          case Vaisto.Liquid.Runtime.run(module_name, entry, [], handler: :native) do
            {:ok, value} -> {:ok, value}
            {:crash, reason} -> {:error, "runtime error: #{inspect(reason)}"}
          end
        rescue
          error -> {:error, Exception.message(error)}
        after
          :code.purge(module_name)
          :code.delete(module_name)
        end

      {:error, error} ->
        {:error, error}
    end
  end

  defp format_error(errors) when is_list(errors), do: Enum.map_join(errors, "; ", &format_error/1)
  defp format_error(%Vaisto.Error{message: message}), do: message
  defp format_error(message) when is_binary(message), do: message
  defp format_error(other), do: inspect(other)

  defp load_definitions(content, state) do
    with {:ok, pairs} <- Compilation.parse(content, with_source: true) do
      pairs = Enum.filter(pairs, fn {form, _} -> definition?(form) end)

      case handle_definitions(pairs, state) do
        {:ok, _, state} -> {:ok, state}
        {:error, error, _} -> {:error, error}
      end
    else
      {:error, error} -> {:error, format_error(error)}
    end
  end
end
