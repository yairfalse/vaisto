defmodule Vaisto.Build do
  @moduledoc """
  Build system for multi-file Vaisto projects.

  This module is a facade that coordinates the build process using:

  - `Vaisto.Build.ModuleNaming` - path → module name inference
  - `Vaisto.Build.DependencyResolver` - dependency graph and topological sort
  - `Vaisto.Build.Compiler` - single-file compilation

  ## Usage

      # Build all .va files in a directory
      Vaisto.Build.build("src/", output_dir: "build/")

      # Compile a single file
      Vaisto.Build.compile_file("src/Foo.va", %{})

  ## Module Naming

  Module names are inferred from file paths using source roots:

      src/Vaisto/Lexer.va → Elixir.Vaisto.Lexer
      std/List.va → Elixir.Std.List
  """

  require Logger

  alias Vaisto.Build.{ModuleNaming, DependencyResolver, Compiler, Context}
  alias Vaisto.Package.{Manifest, Namespace}
  alias Vaisto.Interface

  @type build_opts :: keyword()
  @type build_result :: {:ok, [map()]} | {:error, term()}

  @source_glob "**/*.va"
  @prelude_filename "/prelude.va"

  @doc """
  Build all .va files in a directory.

  Scans for source files, resolves dependencies, and compiles in order.

  ## Options

    * `:output_dir` - where to write .beam and .vsi files (default: same as source)
    * `:backend` - `:core` or `:elixir` (default: `:core`)
    * `:prelude` - prelude source to prepend (default: nil)
    * `:source_roots` - list of `{path_prefix, module_prefix}` tuples

  ## Returns

    * `{:ok, results}` - list of compilation results
    * `{:error, reason}` - build error
  """
  @spec build(String.t(), build_opts()) :: build_result()
  def build(source_dir, opts \\ []) do
    ctx = Context.new()
    output_dir = Keyword.get(opts, :output_dir, source_dir)
    source_roots = Keyword.get(opts, :source_roots, ModuleNaming.default_source_roots())

    Logger.debug("build[#{ctx.build_id}]: starting in #{source_dir}")

    with {:ok, files} <- scan_files(source_dir),
         {:ok, graph} <- DependencyResolver.build_graph(files, source_roots: source_roots),
         {:ok, order} <- DependencyResolver.topological_sort(graph) do
      Logger.debug("build[#{ctx.build_id}]: #{length(files)} files, compile order: #{inspect(Enum.map(order, & &1.module))}")
      result = compile_in_order(order, output_dir, Keyword.put(opts, :build_context, ctx))
      Logger.debug("build[#{ctx.build_id}]: completed in #{Context.elapsed(ctx)}ms")
      result
    end
  end

  @doc """
  Build a project from its `vaisto.toml` manifest.

  Reads the manifest, resolves path dependencies, constructs source roots
  from the package namespace, and delegates to `build/2`.

  This is the entry point when `vaistoc build` is run in a directory with
  a `vaisto.toml`. Projects without a manifest use `build/2` directly.

  ## Options

    * `:backend` - `:core` or `:elixir` (default: `:core`)
    * `:prelude` - prelude source to prepend (default: nil)

  ## Returns

    * `{:ok, results}` - list of compilation results
    * `{:error, reason}` - build error
  """
  @spec build_project(String.t(), keyword()) :: build_result()
  def build_project(project_dir, opts \\ []) do
    case Manifest.load(project_dir) do
      {:ok, manifest} ->
        do_build_project(project_dir, manifest, opts)

      {:error, reason} ->
        {:error, reason}

      :none ->
        {:error, "no vaisto.toml found in #{project_dir}"}
    end
  end

  defp do_build_project(project_dir, manifest, opts) do
    with {:ok, dep_roots} <- resolve_path_deps(project_dir, manifest),
         {:ok, all_files, all_roots} <- collect_sources(project_dir, manifest, dep_roots) do
      output_dir = Path.join(project_dir, manifest.output_dir)
      File.mkdir_p!(output_dir)

      build_opts =
        Keyword.merge(opts,
          output_dir: output_dir,
          source_roots: all_roots,
          search_paths: [output_dir]
        )

      # Build dependency graph from all files (project + deps)
      ctx = Context.new()

      with {:ok, graph} <- DependencyResolver.build_graph(all_files, source_roots: all_roots),
           {:ok, order} <- DependencyResolver.topological_sort(graph) do
        Logger.debug("build_project[#{ctx.build_id}]: #{length(all_files)} files from #{1 + map_size(manifest.dependencies)} package(s)")
        result = compile_in_order(order, output_dir, Keyword.put(build_opts, :build_context, ctx))
        Logger.debug("build_project[#{ctx.build_id}]: completed in #{Context.elapsed(ctx)}ms")
        result
      end
    end
  end

  # Resolve path dependencies: read each dep's manifest, collect source roots
  defp resolve_path_deps(project_dir, manifest) do
    path_deps =
      manifest.dependencies
      |> Enum.filter(fn {_name, spec} -> is_map(spec) and Map.has_key?(spec, :path) end)

    results =
      Enum.map(path_deps, fn {name, %{path: rel_path}} ->
        dep_dir = Path.expand(rel_path, project_dir)

        case Manifest.load(dep_dir) do
          {:ok, dep_manifest} ->
            roots = Namespace.to_source_root(dep_dir, dep_manifest.name, dep_manifest.source_dirs)
            {:ok, {name, dep_dir, dep_manifest, roots}}

          :none ->
            # No manifest — use the dep name for namespace, assume src/
            roots = Namespace.to_source_root(dep_dir, name)
            {:ok, {name, dep_dir, nil, roots}}

          {:error, reason} ->
            {:error, "dependency `#{name}`: #{reason}"}
        end
      end)

    case Enum.find(results, &match?({:error, _}, &1)) do
      {:error, _} = err -> err
      nil -> {:ok, Enum.map(results, fn {:ok, info} -> info end)}
    end
  end

  # Collect all source files and source roots for the project + deps
  defp collect_sources(project_dir, manifest, dep_roots) do
    # Root project source roots (no namespace prefix — it's your project)
    project_roots =
      Enum.map(manifest.source_dirs, fn dir ->
        path = Path.join(project_dir, dir)
        path = if String.ends_with?(path, "/"), do: path, else: path <> "/"
        {path, ""}
      end)

    # Dependency source roots (as returned by Namespace.to_source_root/3)
    dep_source_roots = Enum.flat_map(dep_roots, fn {_name, _dir, _manifest, roots} -> roots end)

    # Standard library
    std_roots = [{"std/", "Std."}]

    all_roots = project_roots ++ dep_source_roots ++ std_roots

    # Scan all source directories for .va files
    project_files =
      Enum.flat_map(manifest.source_dirs, fn dir ->
        pattern = Path.join([project_dir, dir, @source_glob])
        Path.wildcard(pattern)
        |> Enum.reject(&String.ends_with?(&1, @prelude_filename))
      end)

    dep_files =
      Enum.flat_map(dep_roots, fn {_name, dep_dir, dep_manifest, _roots} ->
        source_dirs = if dep_manifest, do: dep_manifest.source_dirs, else: ["src"]

        Enum.flat_map(source_dirs, fn dir ->
          pattern = Path.join([dep_dir, dir, @source_glob])
          Path.wildcard(pattern)
          |> Enum.reject(&String.ends_with?(&1, @prelude_filename))
        end)
      end)

    all_files = project_files ++ dep_files

    if all_files == [] do
      {:error, "no .va files found in source directories"}
    else
      {:ok, all_files, all_roots}
    end
  end

  @doc """
  Compile a single file with its dependencies.

  ## Options

    * `:output_dir` - where to write .beam and .vsi files (default: same as source)
    * `:backend` - `:core` or `:elixir` (default: `:core`)
    * `:prelude` - prelude source to prepend (default: nil)
    * `:search_paths` - directories to search for .vsi interfaces
    * `:auto_import` - automatically load .vsi files for imports (default: true)
    * `:source_roots` - module naming configuration

  ## Returns

    * `{:ok, result}` - compilation result
    * `{:error, reason}` - compilation error
  """
  @spec compile_file(String.t(), map(), keyword()) :: {:ok, map()} | {:error, term()}
  def compile_file(source_path, import_env, opts \\ []) do
    Compiler.compile(source_path, import_env, opts)
  end

  @doc """
  Infer module name from file path.

  Delegates to `Vaisto.Build.ModuleNaming.infer/2`.

  ## Examples

      iex> Vaisto.Build.infer_module_name("src/Foo/Bar.va")
      :"Elixir.Foo.Bar"

      iex> Vaisto.Build.infer_module_name("std/List.va")
      :"Elixir.Std.List"
  """
  @spec infer_module_name(String.t(), [{String.t(), String.t()}]) :: atom()
  def infer_module_name(file_path, source_roots \\ ModuleNaming.default_source_roots()) do
    ModuleNaming.infer(file_path, source_roots: source_roots)
  end

  # Private helpers

  defp scan_files(dir) do
    pattern = Path.join(dir, @source_glob)

    files =
      Path.wildcard(pattern)
      |> Enum.reject(&String.ends_with?(&1, @prelude_filename))

    if files == [] do
      {:error, "No .va files found in #{dir}"}
    else
      {:ok, files}
    end
  end

  defp compile_in_order(order, output_dir, opts) do
    backend = Keyword.get(opts, :backend, :core)
    prelude = Keyword.get(opts, :prelude)
    source_roots = Keyword.get(opts, :source_roots, ModuleNaming.default_source_roots())

    {results, _final_env} =
      Enum.reduce(order, {[], %{}}, fn info, {acc, env} ->
        # Load interfaces for imports
        import_env = Compiler.load_import_env(info.imports, output_dir)
        merged_env = Map.merge(env, import_env)

        compile_opts = [
          output_dir: output_dir,
          backend: backend,
          prelude: prelude,
          source_roots: source_roots
        ]

        case Compiler.compile(info.file, merged_env, compile_opts) do
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
      {:ok, results |> Enum.reverse() |> Enum.map(fn {:ok, r} -> r end)}
    else
      error_msgs = Enum.map(errors, fn {:error, msg} -> msg end)
      {:error, Enum.join(error_msgs, "\n\n")}
    end
  end
end
