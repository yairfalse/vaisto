defmodule Vaisto.REPL do
  @moduledoc """
  Interactive REPL for Vaisto with accumulating context.

  The REPL maintains state across inputs:
  - Type environment: All defined function signatures for inference
  - Code buffer: Previously defined functions re-compiled with each expression

  This allows you to define functions incrementally:

      λ (defn double [x] (* x 2))
      => :ok (Int) -> Int

      λ (double 21)
      => 42

      λ (defn quad [x] (double (double x)))
      => :ok (Int) -> Int

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

  alias Vaisto.TypeSystem.Core

  @version Mix.Project.config()[:version]

  # Built-in primitives for the type environment
  @primitives %{
    :+ => {:fn, [:int, :int], :int},
    :- => {:fn, [:int, :int], :int},
    :* => {:fn, [:int, :int], :int},
    :/ => {:fn, [:int, :int], :int},
    :== => {:fn, [:any, :any], :bool},
    :< => {:fn, [:int, :int], :bool},
    :> => {:fn, [:int, :int], :bool},
    :<= => {:fn, [:int, :int], :bool},
    :>= => {:fn, [:int, :int], :bool},
    :!= => {:fn, [:any, :any], :bool}
  }

  defstruct [
    :env,           # Type environment: name -> type
    :defs,          # Source definitions (AST forms)
    :def_sources,   # Original source strings for save
    :counter        # Module name counter
  ]

  @doc """
  Starts the interactive REPL.
  """
  @spec start() :: :ok
  def start do
    IO.puts("Vaisto REPL v#{@version} (Algorithm W)")
    IO.puts("Type :help for commands, :quit to exit\n")

    state = %__MODULE__{
      env: @primitives,
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
      (defn double [x] (* x 2))   Define a function
      (double 21)                  Call it
      (+ 1 2)                      Evaluate expression
    """)
    {:continue, state}
  end

  defp handle_command("env", state) do
    if map_size(state.env) == map_size(@primitives) do
      IO.puts("No user-defined functions yet.")
    else
      IO.puts("Defined functions:")
      state.env
      |> Enum.reject(fn {name, _} -> Map.has_key?(@primitives, name) end)
      |> Enum.each(fn {name, type} ->
        IO.puts("  #{name} : #{Core.format_type(type)}")
      end)
    end
    {:continue, state}
  end

  defp handle_command("clear", state) do
    IO.puts("Cleared all definitions.")
    {:continue, %{state | env: @primitives, defs: [], def_sources: []}}
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
        # Parse and process each form
        new_state = load_definitions(content, state)
        IO.puts("Loaded definitions from #{filename}")
        {:continue, new_state}

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
    case parse(input) do
      {:ok, form} ->
        if definition?(form) do
          handle_definition(form, input, state)
        else
          handle_expression(form, state)
        end

      {:error, msg} ->
        {:error, msg, state}
    end
  end

  defp parse(input) do
    case Vaisto.Compilation.parse(input) do
      {:ok, ast} -> {:ok, ast}
      {:error, %Vaisto.Error{} = err} -> {:error, err.message}
      {:error, msg} -> {:error, msg}
    end
  end

  defp definition?({:defn, _, _, _, _}), do: true
  defp definition?({:defn, _, _, _}), do: true
  defp definition?({:deftype, _, _, _}), do: true
  defp definition?({:deftype, _, _}), do: true
  defp definition?({:defn_multi, _, _, _}), do: true
  defp definition?({:defn_multi, _, _}), do: true
  defp definition?(_), do: false

  defp handle_definition(form, source, state) do
    # Build a temporary module with existing defs + new one
    module_ast = build_module_ast(state.defs ++ [form])

    case typecheck(module_ast, state.env) do
      {:ok, _type, typed_ast} ->
        # Extract the new function's type from the typed AST
        {name, func_type} = extract_definition_type(typed_ast, form)

        new_env = Map.put(state.env, name, func_type)
        new_defs = state.defs ++ [form]
        new_sources = state.def_sources ++ [source]

        type_str = Core.format_type(func_type)
        {:ok, ":ok #{type_str}", %{state | env: new_env, defs: new_defs, def_sources: new_sources}}

      {:error, msg} ->
        {:error, msg, state}
    end
  end

  defp handle_expression(form, state) do
    # Build module with all defs + this expression as main
    module_ast = build_module_ast(state.defs ++ [form])
    module_name = :"VaistoREPL_#{state.counter}"

    case compile_and_run(module_ast, module_name) do
      {:ok, result} ->
        new_state = %{state | counter: state.counter + 1}
        {:ok, inspect(result), new_state}

      {:error, msg} ->
        {:error, msg, state}
    end
  end

  defp build_module_ast(forms) when is_list(forms), do: forms
  defp build_module_ast(form), do: [form]

  defp typecheck(ast, _env) do
    Vaisto.Compilation.typecheck(ast)
  end

  defp compile_and_run(ast, module_name) do
    with {:ok, _type, typed_ast} <- Vaisto.Compilation.typecheck(ast),
         {:ok, ^module_name, _binary} <- Vaisto.Compilation.emit(typed_ast, module_name, :core) do
      result = apply(module_name, :main, [])
      # Clean up
      :code.purge(module_name)
      :code.delete(module_name)
      {:ok, result}
    else
      {:error, msg} -> {:error, format_error(msg)}
      other -> {:error, "Unexpected: #{inspect(other)}"}
    end
  end

  defp format_error(errors) when is_list(errors) do
    errors
    |> Enum.map(&Vaisto.Error.normalize/1)
    |> Enum.map(& &1.message)
    |> Enum.join("; ")
  end

  defp format_error(%Vaisto.Error{message: msg}), do: msg
  defp format_error(msg) when is_binary(msg), do: msg
  defp format_error(other), do: inspect(other)

  defp extract_definition_type({:module, forms}, original_form) do
    # Find the typed form that matches our definition
    name = get_definition_name(original_form)

    typed_def = Enum.find(forms, fn
      {:defn, ^name, _, _, type} -> type
      {:defn_multi, ^name, _, _, type} -> type
      {:deftype, ^name, _, type} -> type
      _ -> false
    end)

    case typed_def do
      {:defn, ^name, _, _, type} -> {name, type}
      {:defn_multi, ^name, _, _, type} -> {name, type}
      {:deftype, ^name, fields, _type} ->
        # Constructor type
        field_types = Enum.map(fields, fn {_, t} -> t end)
        record_type = {:record, name, fields}
        {name, {:fn, field_types, record_type}}
      nil -> {name, :any}
    end
  end

  defp get_definition_name({:defn, name, _, _, _}), do: name
  defp get_definition_name({:defn, name, _, _}), do: name
  defp get_definition_name({:deftype, name, _, _}), do: name
  defp get_definition_name({:deftype, name, _}), do: name
  defp get_definition_name({:defn_multi, name, _, _}), do: name
  defp get_definition_name({:defn_multi, name, _}), do: name

  defp load_definitions(content, state) do
    # Split by blank lines or parse the whole thing
    case parse(content) do
      {:ok, forms} when is_list(forms) ->
        Enum.reduce(forms, state, fn form, acc ->
          if definition?(form) do
            source = "(defn ...)"  # Simplified - would need proper extraction
            case handle_definition(form, source, acc) do
              {:ok, _, new_state} -> new_state
              {:error, _, _} -> acc
            end
          else
            acc
          end
        end)

      {:ok, form} ->
        if definition?(form) do
          case handle_definition(form, content, state) do
            {:ok, _, new_state} -> new_state
            {:error, _, _} -> state
          end
        else
          state
        end

      {:error, _} ->
        state
    end
  end
end
