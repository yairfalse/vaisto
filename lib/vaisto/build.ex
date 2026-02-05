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

  alias Vaisto.Build.{ModuleNaming, DependencyResolver, Compiler}
  alias Vaisto.Interface

  @type build_opts :: keyword()
  @type build_result :: {:ok, [map()]} | {:error, term()}

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
    output_dir = Keyword.get(opts, :output_dir, source_dir)
    source_roots = Keyword.get(opts, :source_roots, ModuleNaming.default_source_roots())

    with {:ok, files} <- scan_files(source_dir),
         {:ok, graph} <- DependencyResolver.build_graph(files, source_roots: source_roots),
         {:ok, order} <- DependencyResolver.topological_sort(graph) do
      Logger.debug("build: #{length(files)} files, compile order: #{inspect(Enum.map(order, & &1.module))}")
      compile_in_order(order, output_dir, opts)
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
    pattern = Path.join(dir, "**/*.va")

    files =
      Path.wildcard(pattern)
      |> Enum.reject(&String.ends_with?(&1, "/prelude.va"))

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
