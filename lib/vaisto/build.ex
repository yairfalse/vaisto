defmodule Vaisto.Build do
  @moduledoc """
  Build system for multi-file Vaisto projects.

  Handles:
  - Scanning .va files for (ns) and (import) declarations
  - Building dependency graph
  - Topologically sorting for correct compilation order
  - Compiling modules and generating .vsi interface files
  """

  alias Vaisto.{Parser, TypeChecker, CoreEmitter, Emitter, Interface}

  @doc """
  Build all .va files in a directory.

  Options:
    - :output_dir - where to write .beam and .vsi files (default: same as source)
    - :backend - :core or :elixir (default: :core)
    - :prelude - prelude source to prepend (default: nil)

  Returns {:ok, results} or {:error, reason}
  """
  def build(source_dir, opts \\ []) do
    output_dir = Keyword.get(opts, :output_dir, source_dir)
    backend = Keyword.get(opts, :backend, :core)
    prelude = Keyword.get(opts, :prelude)

    with {:ok, files} <- scan_files(source_dir),
         {:ok, graph} <- build_dependency_graph(files),
         {:ok, order} <- topological_sort(graph) do
      compile_in_order(order, files, output_dir, backend, prelude)
    end
  end

  @doc """
  Compile a single file with its dependencies.

  Options:
    - :output_dir - where to write .beam and .vsi files (default: same as source)
    - :backend - :core or :elixir (default: :core)
    - :prelude - prelude source to prepend (default: nil)
    - :search_paths - directories to search for .vsi interfaces (default: [output_dir])
    - :auto_import - automatically load .vsi files for imports (default: true)

  The import_env should contain additional types from imported modules.
  """
  def compile_file(source_path, import_env, opts \\ []) do
    output_dir = Keyword.get(opts, :output_dir, Path.dirname(source_path))
    backend = Keyword.get(opts, :backend, :core)
    prelude = Keyword.get(opts, :prelude)
    search_paths = Keyword.get(opts, :search_paths, [output_dir])
    auto_import = Keyword.get(opts, :auto_import, true)

    case File.read(source_path) do
      {:ok, source} ->
        # Prepend prelude if provided
        full_source = if prelude, do: prelude <> "\n\n" <> source, else: source

        # Parse
        ast = Parser.parse(full_source, file: source_path)

        # Extract module name and imports
        {module_name, imports} = Interface.extract_declarations(ast)
        module_name = module_name || infer_module_name(source_path)

        # Load interfaces for imported modules (if auto_import is enabled)
        auto_import_env = if auto_import do
          load_import_env(imports, search_paths)
        else
          %{}
        end

        # Merge: primitives < auto-loaded imports < explicit import_env
        env = TypeChecker.primitives()
              |> Map.merge(auto_import_env)
              |> Map.merge(import_env)

        # Type check
        case TypeChecker.check(ast, env) do
          {:ok, _type, typed_ast} ->
            # Compile
            result = case backend do
              :core -> CoreEmitter.compile(typed_ast, module_name)
              :elixir -> Emitter.compile(typed_ast, module_name)
            end

            case result do
              {:ok, mod, bytecode} ->
                # Write .beam
                beam_path = Path.join(output_dir, "#{module_name}.beam")
                File.mkdir_p!(Path.dirname(beam_path))
                File.write!(beam_path, extract_bytecode(bytecode))

                # Write .vsi interface
                types = extract_types(typed_ast)
                exports = extract_exports(typed_ast)
                Interface.save(module_name, exports, types, output_dir)

                {:ok, %{module: module_name, beam: beam_path}}

              {:error, reason} ->
                {:error, "Compilation error: #{reason}"}
            end

          {:error, %Vaisto.Error{} = error} ->
            {:error, "Type error: #{inspect(error)}"}

          {:error, reason} ->
            {:error, "Type error: #{reason}"}
        end

      {:error, reason} ->
        {:error, "Cannot read #{source_path}: #{reason}"}
    end
  end

  # Private helpers

  defp scan_files(dir) do
    pattern = Path.join(dir, "**/*.va")
    files = Path.wildcard(pattern)
             # Exclude prelude.va since it's auto-prepended to all files
             |> Enum.reject(&String.ends_with?(&1, "/prelude.va"))

    if files == [] do
      {:error, "No .va files found in #{dir}"}
    else
      {:ok, files}
    end
  end

  defp build_dependency_graph(files) do
    # Parse each file to get (ns) and (import) declarations
    graph = Enum.reduce(files, %{}, fn file, acc ->
      case File.read(file) do
        {:ok, source} ->
          ast = Parser.parse(source, file: file)
          {module_name, imports} = Interface.extract_declarations(ast)
          module_name = module_name || infer_module_name(file)

          Map.put(acc, module_name, %{
            file: file,
            imports: imports
          })

        {:error, reason} ->
          IO.warn("Cannot read file #{file}: #{inspect(reason)}")
          acc
      end
    end)

    {:ok, graph}
  end

  defp topological_sort(graph) do
    # Kahn's algorithm for topological sorting
    # Build in-degree map and adjacency list
    modules = Map.keys(graph)

    # adjacency: module -> list of modules that depend on it
    adjacency = Enum.reduce(modules, %{}, fn mod, acc ->
      deps = graph[mod].imports |> Enum.map(fn {m, _alias} -> m end)
      Enum.reduce(deps, acc, fn dep, inner_acc ->
        Map.update(inner_acc, dep, [mod], &[mod | &1])
      end)
    end)

    # in_degree: module -> number of dependencies
    in_degree = Enum.reduce(modules, %{}, fn mod, acc ->
      deps = graph[mod].imports |> Enum.map(fn {m, _alias} -> m end)
      # Only count dependencies that are in our graph (internal modules)
      internal_deps = Enum.filter(deps, &Map.has_key?(graph, &1))
      Map.put(acc, mod, length(internal_deps))
    end)

    # Start with modules that have no dependencies
    queue = modules |> Enum.filter(&(Map.get(in_degree, &1, 0) == 0))

    do_topological_sort(queue, in_degree, adjacency, graph, [])
  end

  defp do_topological_sort([], _in_degree, _adjacency, _graph, result) do
    {:ok, Enum.reverse(result)}
  end

  defp do_topological_sort([mod | rest], in_degree, adjacency, graph, result) do
    # Add module to result
    new_result = [%{module: mod, file: graph[mod].file, imports: graph[mod].imports} | result]

    # Decrease in-degree of dependents
    dependents = Map.get(adjacency, mod, [])
    {new_in_degree, new_queue} = Enum.reduce(dependents, {in_degree, rest}, fn dep, {deg, q} ->
      new_deg = Map.update!(deg, dep, &(&1 - 1))
      if new_deg[dep] == 0 do
        {new_deg, q ++ [dep]}
      else
        {new_deg, q}
      end
    end)

    do_topological_sort(new_queue, new_in_degree, adjacency, graph, new_result)
  end

  defp compile_in_order(order, _files, output_dir, backend, prelude) do
    # Build up import environment as we compile
    {results, _final_env} = Enum.reduce(order, {[], %{}}, fn info, {acc, env} ->
      # Load interfaces for imports
      import_env = load_import_env(info.imports, output_dir)
      merged_env = Map.merge(env, import_env)

      case compile_file(info.file, merged_env, output_dir: output_dir, backend: backend, prelude: prelude) do
        {:ok, result} ->
          # Add this module's exports to env for next modules
          case Interface.load(info.module, [output_dir]) do
            {:ok, interface} ->
              module_env = Interface.build_env([interface])
              {[{:ok, result} | acc], Map.merge(env, module_env)}

            {:error, _} ->
              {[{:ok, result} | acc], env}
          end

        {:error, _} = err ->
          {[err | acc], env}
      end
    end)

    errors = Enum.filter(results, &match?({:error, _}, &1))
    if errors == [] do
      {:ok, Enum.reverse(results) |> Enum.map(fn {:ok, r} -> r end)}
    else
      {:error, errors}
    end
  end

  defp load_import_env(imports, search_paths) when is_list(search_paths) do
    Enum.reduce(imports, %{}, fn {module, alias_name}, acc ->
      case Interface.load(module, search_paths) do
        {:ok, interface} ->
          aliases = if alias_name, do: %{module => alias_name}, else: %{}
          Map.merge(acc, Interface.build_env([interface], aliases))

        {:error, _reason} ->
          # Interface not found - module may be external (erlang, etc.)
          # Will be resolved via extern declarations or at runtime
          acc
      end
    end)
  end

  # Handle single search dir for backwards compatibility
  defp load_import_env(imports, search_dir) when is_binary(search_dir) do
    load_import_env(imports, [search_dir])
  end

  defp infer_module_name(file_path) do
    file_path
    |> Path.basename(".va")
    |> String.split(~r/[_\-]/)
    |> Enum.map(&String.capitalize/1)
    |> Enum.join()
    |> String.to_atom()
  end

  defp extract_bytecode(bytecode) when is_binary(bytecode), do: bytecode
  defp extract_bytecode([{_mod, bytecode} | _]) when is_binary(bytecode), do: bytecode

  defp extract_types({:module, forms}) do
    forms
    |> Enum.filter(&match?({:deftype, _, _, _}, &1))
    |> Enum.reduce(%{}, fn {:deftype, name, _kind, type}, acc ->
      Map.put(acc, name, type)
    end)
  end
  defp extract_types(_), do: %{}

  defp extract_exports({:module, forms}) do
    forms
    |> Enum.filter(fn
      {:defn, _, _, _, _} -> true
      {:defn_multi, _, _, _, _} -> true
      _ -> false
    end)
    |> Enum.reduce(%{}, fn
      {:defn, name, _params, _body, type}, acc -> Map.put(acc, name, type)
      {:defn_multi, name, _arity, _clauses, type}, acc -> Map.put(acc, name, type)
    end)
  end
  defp extract_exports(_), do: %{}
end
