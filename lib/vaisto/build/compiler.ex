defmodule Vaisto.Build.Compiler do
  @moduledoc """
  Compile one source file to native BEAM and a verified canonical interface.
  Imported signatures are Core interfaces, never an HM environment.
  """

  require Logger
  alias Vaisto.{Compilation, Elab, Error, ErrorFormatter, Errors, Interface, Refine}
  alias Vaisto.Build.ModuleNaming
  alias Vaisto.Liquid.Lower

  @type compilation_result :: %{module: atom(), beam: String.t(), interface: map()}

  @doc """
  Compile a source file without loading it. The second argument maps source
  module names to canonical interface trees. With auto_import enabled,
  missing supplied interfaces are read from search_paths.

  Options include output_dir, prelude, source_roots, search_paths,
  auto_import and refinement solver options.
  """
  @spec compile(String.t(), map(), keyword()) :: {:ok, compilation_result()} | {:error, term()}
  def compile(source_path, interfaces, opts \\ []) do
    case File.read(source_path) do
      {:ok, source} -> compile_source(source, source_path, interfaces, opts)
      {:error, reason} -> {:error, Errors.source_read_error(source_path, reason)}
    end
  end

  defp compile_source(source, path, interfaces, opts) do
    if ctx = Keyword.get(opts, :build_context),
      do: Logger.debug("build[#{ctx.build_id}]: compiling #{path}")

    output_dir = Keyword.get(opts, :output_dir, Path.dirname(path))
    prelude = Keyword.get(opts, :prelude)
    full_source = if prelude, do: prelude <> "\n\n" <> source, else: source
    source_roots = Keyword.get(opts, :source_roots, ModuleNaming.default_source_roots())

    search_paths =
      if Keyword.get(opts, :auto_import, true),
        do: Keyword.get(opts, :search_paths, [output_dir]),
        else: []

    interfaces = Map.merge(interfaces, Keyword.get(opts, :interfaces, %{}))

    result =
      with :error <- Keyword.fetch(opts, :backend),
           {:ok, ast} <- Compilation.parse(full_source, file: path),
           {plain, sigs} = Refine.Surface.split(ast),
           {declared, _} = Interface.extract_declarations(plain),
           {:ok, module} <-
             ModuleNaming.validate_namespace(declared, path, source_roots: source_roots),
           {:ok, core} <-
             Elab.module(plain,
               name: module,
               interfaces: interfaces,
               search_paths: search_paths,
               atoms: Keyword.get(opts, :atoms, :existing)
             ),
           locs =
             for(
               form <- List.wrap(plain),
               is_tuple(form),
               elem(form, 0) == :defn,
               into: %{},
               do: {elem(form, 1), elem(form, tuple_size(form) - 1)}
             ),
           {:ok, tree} <-
             Interface.from_core(core, sigs, Keyword.merge(opts, source: full_source, locs: locs)),
           {:ok, ^module, binary} <- Lower.compile(core, module, load: false),
           {:ok, interface} <- Interface.validate_core(tree),
           :ok <- write_outputs(module, binary, tree, output_dir) do
        {:ok,
         %{module: module, beam: Path.join(output_dir, "#{module}.beam"), interface: interface}}
      else
        {:ok, _backend} -> {:error, Errors.backend_selection_removed()}
        other -> other
      end

    case result do
      {:error, errors} when is_list(errors) ->
        {:error, ErrorFormatter.format_all(Enum.map(errors, &Error.normalize/1), full_source)}

      {:error, %Error{} = error} ->
        {:error, ErrorFormatter.format(error, full_source)}

      {:outside, reason} ->
        {:error, "the compiler does not support #{reason}"}

      result ->
        result
    end
  end

  defp write_outputs(module, binary, tree, dir) do
    with :ok <- File.mkdir_p(dir),
         :ok <- File.write(Path.join(dir, "#{module}.beam"), binary),
         :ok <- Interface.save_core(tree, dir) do
      :ok
    end
  end
end
