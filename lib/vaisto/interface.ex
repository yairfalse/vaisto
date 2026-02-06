defmodule Vaisto.Interface do
  @moduledoc """
  Handles .vsi (Vaisto Interface) files for cross-module type information.

  When a module is compiled, we export its type environment to a .vsi file.
  When another module imports it, we load the .vsi to get type signatures.

  Format: Erlang term file containing:
    %{
      module: :"Std.List",
      version: 1,
      exports: %{
        fold: {:fn, [:any, :any, :any], :any},
        map: {:fn, [:any, :any], :any},
        ...
      },
      types: %{
        Result: {:sum, :Result, [{:Ok, [:any]}, {:Err, [:any]}]},
        ...
      }
    }
  """

  @version 1

  @doc """
  Save a module's type environment to a .vsi file.

  Takes the module name, the type environment (after type checking),
  and the output directory.
  """
  @spec save(atom(), map(), map(), String.t()) :: {:ok, String.t()}
  def save(module_name, type_env, types \\ %{}, output_dir) do
    interface = %{
      module: module_name,
      version: @version,
      exports: extract_exports(type_env),
      types: types
    }

    path = interface_path(module_name, output_dir)
    File.mkdir_p!(Path.dirname(path))

    # Write as Erlang external term format (binary, efficient)
    binary = :erlang.term_to_binary(interface)
    File.write!(path, binary)

    {:ok, path}
  end

  @doc """
  Load a module's type interface from a .vsi file.

  Returns {:ok, interface_map} or {:error, reason}.
  """
  @spec load(atom(), [String.t()]) :: {:ok, map()} | {:error, String.t()}
  def load(module_name, search_paths) do
    case find_interface(module_name, search_paths) do
      {:ok, path} ->
        case File.read(path) do
          {:ok, binary} ->
            try do
              interface = :erlang.binary_to_term(binary)
              validate_interface(interface)
            rescue
              _ -> {:error, "Corrupt interface file: #{path}"}
            end

          {:error, reason} ->
            {:error, "Cannot read #{path}: #{reason}"}
        end

      :not_found ->
        {:error, "Interface not found for #{module_name}"}
    end
  end

  @doc """
  Build a type environment from loaded interfaces.

  Given a list of loaded interfaces, creates a map suitable for
  the TypeChecker with qualified names using ":" separator: "Math:square"
  """
  @spec build_env([map()], map()) :: map()
  def build_env(interfaces, aliases \\ %{}) do
    Enum.reduce(interfaces, %{}, fn interface, env ->
      module_name = interface.module
      alias_name = Map.get(aliases, module_name, module_name)

      # Add each export with qualified name (using : for TypeChecker compatibility)
      Enum.reduce(interface.exports, env, fn {func_name, type}, acc ->
        qualified = :"#{alias_name}:#{func_name}"
        Map.put(acc, qualified, type)
      end)
      |> then(fn env ->
        # Also add type constructors
        Enum.reduce(interface.types, env, fn {type_name, type_def}, acc ->
          case type_def do
            {:sum, _name, variants} ->
              # Register each variant constructor
              Enum.reduce(variants, acc, fn {ctor_name, field_types}, inner_acc ->
                qualified_ctor = :"#{alias_name}:#{ctor_name}"
                ctor_type = {:fn, field_types, type_def}
                Map.put(inner_acc, qualified_ctor, ctor_type)
              end)

            {:record, _name, _fields} = record_type ->
              # Register record constructor (using : for TypeChecker compatibility)
              qualified_ctor = :"#{alias_name}:#{type_name}"
              field_types = extract_field_types(record_type)
              ctor_type = {:fn, field_types, record_type}
              Map.put(acc, qualified_ctor, ctor_type)

            _ ->
              acc
          end
        end)
      end)
    end)
  end

  @doc """
  Extract module declarations from parsed AST.

  Returns {module_name, imports} or {:error, reason}
  """
  @spec extract_declarations(term()) :: {atom() | nil, [term()]}
  def extract_declarations(ast) when is_list(ast) do
    ns = find_ns(ast)
    imports = find_imports(ast)
    {ns, imports}
  end

  def extract_declarations(_ast), do: {nil, []}

  # Private helpers

  defp interface_path(module_name, dir) do
    # Strip Elixir. prefix for file paths: Elixir.Vaisto.Lexer → Vaisto/Lexer.vsi
    filename = module_name
      |> to_string()
      |> String.replace_prefix("Elixir.", "")
      |> String.replace(".", "/")
    Path.join(dir, "#{filename}.vsi")
  end

  defp find_interface(module_name, search_paths) do
    # Strip Elixir. prefix for file paths: Elixir.Vaisto.Lexer → Vaisto/Lexer.vsi
    filename = module_name
      |> to_string()
      |> String.replace_prefix("Elixir.", "")
      |> String.replace(".", "/")

    Enum.find_value(search_paths, :not_found, fn dir ->
      path = Path.join(dir, "#{filename}.vsi")
      if File.exists?(path), do: {:ok, path}
    end)
  end

  defp validate_interface(%{module: _, version: v, exports: _, types: _} = interface)
       when v == @version do
    {:ok, interface}
  end

  defp validate_interface(%{version: v}) do
    {:error, "Incompatible interface version: #{v} (expected #{@version})"}
  end

  defp validate_interface(_) do
    {:error, "Invalid interface format"}
  end

  defp extract_exports(type_env) do
    # Filter to only include function types (not primitives)
    type_env
    |> Enum.filter(fn
      {_name, {:fn, _, _}} -> true
      _ -> false
    end)
    |> Map.new()
  end

  defp extract_field_types({:record, _name, fields}) do
    Enum.map(fields, fn {_field_name, type} -> type end)
  end

  defp find_ns(ast) do
    Enum.find_value(ast, fn
      {:ns, name, _loc} -> name
      _ -> nil
    end)
  end

  defp find_imports(ast) do
    ast
    |> Enum.filter(&match?({:import, _, _, _}, &1))
    |> Enum.map(fn {:import, module, alias_name, _loc} ->
      {module, alias_name}
    end)
  end
end
