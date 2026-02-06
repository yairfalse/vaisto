defmodule Vaisto.Build.Compiler do
  @moduledoc """
  Single-file compilation orchestration for Vaisto.

  Responsible for compiling a single source file through the pipeline:
  parse → type check → emit → write outputs (.beam + .vsi)

  This is a domain service that coordinates the compilation phases
  without owning state.

  ## Compilation Result

  A successful compilation returns:

      {:ok, %{
        module: :"Elixir.Foo",
        beam: "/path/to/Foo.beam",
        interface: %{exports: %{...}, types: %{...}}
      }}
  """

  require Logger

  alias Vaisto.{Parser, TypeChecker, Interface, Compilation}
  alias Vaisto.Build.ModuleNaming

  @type compilation_result :: %{
          module: atom(),
          beam: String.t(),
          interface: map()
        }

  @type compile_opts :: [
          output_dir: String.t(),
          backend: :core | :elixir,
          prelude: String.t() | nil,
          source_roots: [ModuleNaming.source_root()],
          search_paths: [String.t()],
          auto_import: boolean()
        ]

  @doc """
  Compile a single Vaisto source file.

  ## Options

    * `:output_dir` - where to write .beam and .vsi files (default: source directory)
    * `:backend` - `:core` or `:elixir` (default: `:core`)
    * `:prelude` - prelude source to prepend (default: nil)
    * `:source_roots` - module naming configuration
    * `:search_paths` - directories to search for .vsi interfaces
    * `:auto_import` - automatically load .vsi files for imports (default: true)

  ## Returns

    * `{:ok, result}` - compilation result with module, beam path, interface
    * `{:error, reason}` - compilation error with formatted message
  """
  @spec compile(String.t(), map(), compile_opts()) ::
          {:ok, compilation_result()} | {:error, String.t()}
  def compile(source_path, import_env, opts \\ []) do
    output_dir = Keyword.get(opts, :output_dir, Path.dirname(source_path))
    backend = Keyword.get(opts, :backend, :core)
    prelude = Keyword.get(opts, :prelude)
    search_paths = Keyword.get(opts, :search_paths, [output_dir])
    auto_import = Keyword.get(opts, :auto_import, true)
    source_roots = Keyword.get(opts, :source_roots, ModuleNaming.default_source_roots())
    ctx = Keyword.get(opts, :build_context)

    if ctx, do: Logger.debug("build[#{ctx.build_id}]: compiling #{source_path}")

    with {:ok, source} <- read_source(source_path),
         full_source = prepend_prelude(source, prelude),
         ast = Parser.parse(full_source, file: source_path),
         {:ok, module_name} <- resolve_module_name(ast, source_path, source_roots),
         env = build_environment(ast, import_env, search_paths, auto_import),
         {:ok, typed_ast} <- typecheck(ast, env, full_source),
         {:ok, bytecode} <- emit(typed_ast, module_name, backend),
         :ok <- write_outputs(module_name, bytecode, typed_ast, output_dir) do
      {:ok,
       %{
         module: module_name,
         beam: Path.join(output_dir, "#{module_name}.beam"),
         interface: %{
           exports: extract_exports(typed_ast),
           types: extract_types(typed_ast)
         }
       }}
    end
  end

  @doc """
  Load type environment for a list of imports.

  Searches for .vsi interface files and builds the type environment.
  """
  @spec load_import_env([{atom(), atom() | nil}], [String.t()] | String.t()) :: map()
  def load_import_env(imports, search_paths) when is_list(search_paths) do
    Enum.reduce(imports, %{}, fn {module, alias_name}, acc ->
      case Interface.load(module, search_paths) do
        {:ok, interface} ->
          aliases = if alias_name, do: %{module => alias_name}, else: %{}
          Map.merge(acc, Interface.build_env([interface], aliases))

        {:error, _reason} ->
          # Interface not found - module may be external
          acc
      end
    end)
  end

  def load_import_env(imports, search_dir) when is_binary(search_dir) do
    load_import_env(imports, [search_dir])
  end

  # Private helpers

  defp read_source(path) do
    case File.read(path) do
      {:ok, source} -> {:ok, source}
      {:error, reason} -> {:error, "Cannot read #{path}: #{reason}"}
    end
  end

  defp prepend_prelude(source, nil), do: source
  defp prepend_prelude(source, prelude), do: prelude <> "\n\n" <> source

  defp resolve_module_name(ast, source_path, source_roots) do
    {declared_ns, _imports} = Interface.extract_declarations(ast)
    ModuleNaming.validate_namespace(declared_ns, source_path, source_roots: source_roots)
  end

  defp build_environment(ast, import_env, search_paths, auto_import) do
    {_ns, imports} = Interface.extract_declarations(ast)

    auto_import_env =
      if auto_import do
        load_import_env(imports, search_paths)
      else
        %{}
      end

    TypeChecker.primitives()
    |> Map.merge(auto_import_env)
    |> Map.merge(import_env)
  end

  defp typecheck(ast, env, source) do
    case Compilation.typecheck(ast, env) do
      {:ok, _type, typed_ast} ->
        {:ok, typed_ast}

      {:error, errors} when is_list(errors) ->
        formatted = Vaisto.ErrorFormatter.format_all(errors, source)
        {:error, formatted}

      {:error, %Vaisto.Error{} = error} ->
        formatted = Vaisto.ErrorFormatter.format(error, source)
        {:error, formatted}
    end
  end

  defp emit(typed_ast, module_name, backend) do
    case Compilation.emit(typed_ast, module_name, backend, load: false) do
      {:ok, _mod, bytecode} -> {:ok, bytecode}
      {:error, reason} -> {:error, "Compilation error: #{inspect(reason)}"}
    end
  end

  defp write_outputs(module_name, bytecode, typed_ast, output_dir) do
    # Write .beam
    beam_path = Path.join(output_dir, "#{module_name}.beam")
    File.mkdir_p!(Path.dirname(beam_path))
    File.write!(beam_path, extract_bytecode(bytecode))

    # Write .vsi interface
    types = extract_types(typed_ast)
    exports = extract_exports(typed_ast)
    Interface.save(module_name, exports, types, output_dir)

    :ok
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
