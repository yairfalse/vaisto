defmodule Vaisto.Build.DependencyResolver do
  @moduledoc """
  Dependency resolution for multi-file Vaisto builds.

  Responsible for:
  - Building a dependency graph from source files
  - Topologically sorting modules for correct compilation order
  - Detecting circular dependencies

  This is a stateless domain service operating on dependency graphs.

  ## Dependency Graph Structure

  A dependency graph is a map of module names to module info:

      %{
        :"Elixir.Foo" => %{file: "src/Foo.va", imports: [{:"Elixir.Bar", nil}]},
        :"Elixir.Bar" => %{file: "src/Bar.va", imports: []}
      }

  ## Compilation Order

  The compilation order is a list of module info maps in dependency order
  (dependencies come before dependents):

      [
        %{module: :"Elixir.Bar", file: "src/Bar.va", imports: []},
        %{module: :"Elixir.Foo", file: "src/Foo.va", imports: [...]}
      ]
  """

  alias Vaisto.{Parser, Interface}
  alias Vaisto.Build.ModuleNaming

  @type module_info :: %{
          file: String.t(),
          imports: [{atom(), atom() | nil}]
        }

  @type dependency_graph :: %{atom() => module_info()}

  @type compilation_unit :: %{
          module: atom(),
          file: String.t(),
          imports: [{atom(), atom() | nil}]
        }

  @doc """
  Build a dependency graph from a list of source files.

  Parses each file to extract `(ns)` and `(import)` declarations,
  using file paths to infer module names.

  ## Options

    * `:source_roots` - source root configuration for module naming

  ## Returns

    * `{:ok, graph}` - dependency graph mapping modules to their info
    * `{:error, reason}` - if files cannot be parsed
  """
  @spec build_graph([String.t()], keyword()) :: {:ok, dependency_graph()} | {:error, String.t()}
  def build_graph(files, opts \\ []) do
    source_roots = Keyword.get(opts, :source_roots, ModuleNaming.default_source_roots())

    graph =
      Enum.reduce(files, %{}, fn file, acc ->
        case File.read(file) do
          {:ok, source} ->
            ast = Parser.parse(source, file: file)
            {_declared_ns, imports} = Interface.extract_declarations(ast)
            module_name = ModuleNaming.infer(file, source_roots: source_roots)

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

  @doc """
  Topologically sort a dependency graph for compilation order.

  Uses Kahn's algorithm to produce a valid compilation order where
  dependencies are compiled before their dependents.

  ## Returns

    * `{:ok, order}` - list of compilation units in dependency order
    * `{:error, :circular_dependency}` - if a cycle is detected
  """
  @spec topological_sort(dependency_graph()) ::
          {:ok, [compilation_unit()]} | {:error, :circular_dependency}
  def topological_sort(graph) do
    # Sorted, so that modules with no ordering between them always come out
    # in the same order.
    modules = graph |> Map.keys() |> Enum.sort()

    # Build adjacency list: module -> modules that depend on it
    adjacency = build_adjacency(modules, graph)

    # Build in-degree map: module -> count of internal dependencies
    in_degree = build_in_degree(modules, graph)

    # Start with modules that have no dependencies
    queue = Enum.filter(modules, &(Map.get(in_degree, &1, 0) == 0))

    do_topological_sort(queue, in_degree, adjacency, graph, [])
  end

  @doc """
  Get the direct dependencies of a module from the graph.

  Returns only internal dependencies (modules that exist in the graph).
  """
  @spec dependencies(dependency_graph(), atom()) :: [atom()]
  def dependencies(graph, module) do
    case Map.get(graph, module) do
      nil ->
        []

      _info ->
        internal_deps(graph, module)
    end
  end

  @doc """
  Get the modules that depend on a given module.
  """
  @spec dependents(dependency_graph(), atom()) :: [atom()]
  def dependents(graph, module) do
    Enum.filter(Map.keys(graph), fn m ->
      module in dependencies(graph, m)
    end)
  end

  # Private helpers

  # Imports name a module as written (`(import A)` gives `:A`), while the graph
  # is keyed by the name inferred from the file path (`:"Elixir.A"`).
  defp internal_deps(graph, mod) do
    graph[mod].imports
    |> Enum.map(fn {m, _alias} -> module_key(m) end)
    |> Enum.filter(&Map.has_key?(graph, &1))
    |> Enum.uniq()
  end

  defp module_key(name) do
    case Atom.to_string(name) do
      "Elixir." <> _ -> name
      bare -> :"Elixir.#{bare}"
    end
  end

  defp build_adjacency(modules, graph) do
    Enum.reduce(modules, %{}, fn mod, acc ->
      Enum.reduce(internal_deps(graph, mod), acc, fn dep, inner_acc ->
        Map.update(inner_acc, dep, [mod], &[mod | &1])
      end)
    end)
  end

  defp build_in_degree(modules, graph) do
    Enum.reduce(modules, %{}, fn mod, acc ->
      Map.put(acc, mod, length(internal_deps(graph, mod)))
    end)
  end

  defp do_topological_sort([], in_degree, _adjacency, graph, result) do
    # Check for remaining modules with non-zero in-degree (cycle detection)
    remaining = Enum.filter(Map.keys(graph), &(Map.get(in_degree, &1, 0) > 0))

    if remaining == [] do
      {:ok, Enum.reverse(result)}
    else
      {:error, :circular_dependency}
    end
  end

  defp do_topological_sort([mod | rest], in_degree, adjacency, graph, result) do
    new_result = [
      %{
        module: mod,
        file: graph[mod].file,
        imports: graph[mod].imports
      }
      | result
    ]

    # Decrease in-degree of dependents
    dependents = adjacency |> Map.get(mod, []) |> Enum.sort()

    {new_in_degree, new_queue} =
      Enum.reduce(dependents, {in_degree, rest}, fn dep, {deg, q} ->
        new_deg = Map.update!(deg, dep, &(&1 - 1))

        if new_deg[dep] == 0 do
          {new_deg, q ++ [dep]}
        else
          {new_deg, q}
        end
      end)

    do_topological_sort(new_queue, new_in_degree, adjacency, graph, new_result)
  end
end
