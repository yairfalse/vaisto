defmodule Vaisto.Compilation do
  @moduledoc """
  Core compilation service for Vaisto.

  Provides the parse → elaborate → verify → native BEAM pipeline that all
  compilation entry points (CLI, REPL, Runner, Build) use.

  This module handles:
  - Parsing with error wrapping
  - Type checking with error normalization
  - Native BEAM lowering
  - Prelude handling

  ## Example

      # Simple compilation
      {:ok, module, bytecode} = Compilation.compile(source, MyModule)

      # With prelude
      {:ok, module, bytecode} = Compilation.compile(source, MyModule, prelude: prelude_source)

  """

  alias Vaisto.{Parser, Error, Errors, ErrorFormatter, Refine}

  @type compile_opts :: [
          prelude: String.t() | nil,
          load: boolean(),
          format_errors: boolean(),
          line_offset: non_neg_integer()
        ]

  @doc """
  Compile source through the bidirectional elaborator, verified Core and native BEAM lowering.

  ## Options

    * `:prelude` - Source to prepend (e.g., standard library definitions)
    * `:load` - Load module into VM (default: true)
    * `:format_errors` - Format errors with source context (default: true)
    * `:line_offset` - Line offset for error messages when prelude is prepended

  Returns `{:ok, module, binary}` or `{:error, reason}`. Definition signatures
  are required. There is no HM or old-emitter fallback.
  """
  @spec compile(String.t(), atom(), keyword()) :: {:ok, atom(), binary()} | {:error, term()}
  def compile(source, module_name, opts \\ []) do
    compile_liquid(source, module_name, opts)
  end

  @doc "Compile verified Liquid Core to native BEAM; also used by the public compiler."
  @spec compile_liquid(String.t(), atom(), keyword()) ::
          {:ok, atom(), binary()} | {:error, term()}
  def compile_liquid(source, module_name, opts \\ []) do
    result =
      with {:ok, core} <-
             check_liquid(source, module_name, Keyword.put(opts, :format_errors, false)) do
        Vaisto.Liquid.Lower.compile(core, module_name, Keyword.take(opts, [:load]))
      end

    full_source = prepend_prelude(source, Keyword.get(opts, :prelude))

    offset =
      Keyword.get(
        opts,
        :line_offset,
        if(prelude = Keyword.get(opts, :prelude), do: prelude_line_count(prelude), else: 0)
      )

    native_result(result, full_source, offset, opts)
  end

  @doc "Elaborate and verify source to Core, without invoking HM or emitting code."
  @spec check_liquid(String.t(), atom(), keyword()) :: {:ok, term()} | {:error, term()}
  def check_liquid(source, module_name \\ :Main, opts \\ []) do
    full_source = prepend_prelude(source, Keyword.get(opts, :prelude))

    offset =
      Keyword.get(
        opts,
        :line_offset,
        if(prelude = Keyword.get(opts, :prelude), do: prelude_line_count(prelude), else: 0)
      )

    result =
      with :error <- Keyword.fetch(opts, :backend),
           {:ok, ast} <- parse(full_source, Keyword.take(opts, [:file])) do
        {plain, sigs} = Refine.Surface.split(ast)
        refine_opts = Keyword.merge(opts, source: full_source, locs: definition_locs(plain))

        with {:ok, core} <-
               Vaisto.Elab.module(
                 plain,
                 Keyword.put(
                   Keyword.take(opts, [
                     :interfaces,
                     :search_paths,
                     :atoms,
                     :expression_entry,
                     :type_probe
                   ]),
                   :name,
                   module_name
                 )
               ),
             :ok <- Refine.check_core(core, sigs, refine_opts) do
          {:ok, core}
        end
      else
        {:ok, _backend} -> {:error, [Errors.backend_selection_removed()]}
        other -> other
      end

    native_result(result, full_source, offset, opts)
  end

  defp native_result(result, full_source, offset, opts) do
    case result do
      {:outside, reason} ->
        native_errors(
          [Error.new("the new compiler does not support #{reason} yet")],
          full_source,
          offset,
          opts
        )

      {:error, [%Error{} | _] = errors} ->
        native_errors(errors, full_source, offset, opts)

      {:error, %Error{} = error} ->
        native_errors([error], full_source, offset, opts)

      other ->
        other
    end
  end

  defp native_errors(errors, source, offset, opts) do
    errors = Enum.map(errors, &%{&1 | file: &1.file || Keyword.get(opts, :file)})

    if Keyword.get(opts, :format_errors, true),
      do: {:error, ErrorFormatter.format_all(errors, source, line_offset: offset)},
      else: {:error, errors}
  end

  @doc """
  Parses Vaisto source code into an AST.

  Returns `{:ok, ast}` on success or `{:error, %Error{}}` on failure.
  With `with_source: true`, return `{form, original_source}` pairs.
  """
  @spec parse(String.t(), keyword()) :: {:ok, term()} | {:error, Error.t()}
  def parse(source, opts \\ []) do
    try do
      ast =
        if Keyword.get(opts, :with_source, false),
          do: Parser.parse_with_source(source, opts),
          else: Parser.parse(source, opts)

      {:ok, ast}
    rescue
      e -> {:error, Errors.compilation_error(Exception.message(e))}
    end
  end

  @doc """
  Compiles and immediately executes Vaisto code, returning the result.

  Useful for REPL and eval scenarios.

  ## Options

  Same as `compile/3`.

  ## Returns

    * `{:ok, result}` on success
    * `{:error, reason}` on failure
  """
  @spec run(String.t(), keyword()) :: {:ok, term()} | {:error, term()}
  def run(source, opts \\ []) do
    module_name = :"VaistoRun_#{:erlang.unique_integer([:positive])}"

    with {:ok, ^module_name, _bytecode} <- compile(source, module_name, opts) do
      try do
        case Vaisto.Liquid.Runtime.run(module_name, :main, [], handler: :native) do
          {:ok, value} -> {:ok, value}
          {:crash, reason} -> {:error, Errors.runtime_error(inspect(reason))}
        end
      after
        :code.purge(module_name)
        :code.delete(module_name)
      end
    end
  end

  # Private helpers

  defp prepend_prelude(source, nil), do: source
  defp prepend_prelude(source, prelude), do: prelude <> "\n\n" <> source

  defp prelude_line_count(prelude) do
    # Count lines in prelude + 1 for the separator newlines
    length(String.split(prelude, "\n")) + 1
  end

  defp definition_locs(forms) when is_list(forms) do
    for form <- forms, is_tuple(form), elem(form, 0) == :defn, into: %{} do
      {elem(form, 1), elem(form, tuple_size(form) - 1)}
    end
  end

  defp definition_locs(form), do: definition_locs([form])
end
