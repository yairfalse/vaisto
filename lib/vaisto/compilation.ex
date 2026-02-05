defmodule Vaisto.Compilation do
  @moduledoc """
  Core compilation service for Vaisto.

  Provides the fundamental parse → type check → emit pipeline that all
  compilation entry points (CLI, REPL, Runner, Build) use.

  This module handles:
  - Parsing with error wrapping
  - Type checking with error normalization
  - Backend emission
  - Prelude handling

  ## Example

      # Simple compilation
      {:ok, module, bytecode} = Compilation.compile(source, MyModule)

      # With prelude
      {:ok, module, bytecode} = Compilation.compile(source, MyModule, prelude: prelude_source)

      # With custom backend
      {:ok, module, bytecode} = Compilation.compile(source, MyModule, backend: :elixir)
  """

  alias Vaisto.{Parser, TypeChecker, Backend, Error, ErrorFormatter}

  @type compile_opts :: [
          prelude: String.t() | nil,
          backend: :core | :elixir,
          env: map(),
          load: boolean(),
          format_errors: boolean(),
          line_offset: non_neg_integer()
        ]

  @doc """
  Compiles Vaisto source code to BEAM bytecode.

  ## Options

    * `:prelude` - Source to prepend (e.g., standard library definitions)
    * `:backend` - `:core` (default) or `:elixir`
    * `:env` - Initial type environment (default: primitives)
    * `:load` - Load module into VM (default: true)
    * `:format_errors` - Format errors with source context (default: true)
    * `:line_offset` - Line offset for error messages when prelude is prepended

  ## Returns

    * `{:ok, module_name, bytecode}` on success
    * `{:error, reason}` on failure (reason is formatted string if format_errors: true)
  """
  @spec compile(String.t(), atom(), compile_opts()) ::
          {:ok, atom(), binary() | [{atom(), binary()}]} | {:error, term()}
  def compile(source, module_name, opts \\ []) do
    prelude = Keyword.get(opts, :prelude)
    backend = Keyword.get(opts, :backend, :core)
    env = Keyword.get(opts, :env, TypeChecker.primitives())
    load = Keyword.get(opts, :load, true)
    format_errors = Keyword.get(opts, :format_errors, true)

    full_source = prepend_prelude(source, prelude)
    line_offset = if prelude, do: prelude_line_count(prelude), else: 0

    with {:ok, ast} <- parse(full_source),
         {:ok, _type, typed_ast} <- typecheck(ast, env, full_source, line_offset, format_errors) do
      emit(typed_ast, module_name, backend, load: load)
    end
  end

  @doc """
  Parses Vaisto source code into an AST.

  Returns `{:ok, ast}` on success or `{:error, %Error{}}` on failure.
  """
  @spec parse(String.t(), keyword()) :: {:ok, term()} | {:error, Error.t()}
  def parse(source, opts \\ []) do
    try do
      {:ok, Parser.parse(source, opts)}
    rescue
      e -> {:error, Error.new("parse error", note: Exception.message(e))}
    end
  end

  @doc """
  Type checks an AST.

  ## Options

    * `:env` - Type environment (default: primitives)
    * `:source` - Original source for error formatting
    * `:line_offset` - Line offset for prelude-adjusted errors
    * `:format_errors` - Whether to format errors with source context

  Returns `{:ok, type, typed_ast}` on success or `{:error, reason}` on failure.
  """
  @spec typecheck(term(), map()) ::
          {:ok, term(), term()} | {:error, term()}
  def typecheck(ast, env \\ TypeChecker.primitives()) do
    case TypeChecker.check(ast, env) do
      {:ok, _, _} = success -> success
      {:errors, errors} -> {:error, {:type_errors, errors}}
      {:error, err} -> {:error, err}
    end
  end

  @doc """
  Emits typed AST to BEAM bytecode using the specified backend.

  ## Options

    * `:load` - Whether to load the module into the VM (default: true)

  Returns `{:ok, module_name, bytecode}` on success.
  """
  @spec emit(term(), atom(), :core | :elixir, keyword()) ::
          {:ok, atom(), binary() | [{atom(), binary()}]} | {:error, term()}
  def emit(typed_ast, module_name, backend \\ :core, opts \\ []) do
    case backend do
      :core ->
        Backend.Core.compile(typed_ast, module_name, opts)

      :elixir ->
        Backend.Elixir.compile(typed_ast, module_name)

      other ->
        {:error,
         Error.new("unknown backend",
           note: "#{inspect(other)} is not valid",
           hint: "use :core or :elixir"
         )}
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
      result = apply(module_name, :main, [])
      # Clean up
      :code.purge(module_name)
      :code.delete(module_name)
      {:ok, result}
    end
  end

  # Private helpers

  defp prepend_prelude(source, nil), do: source
  defp prepend_prelude(source, prelude), do: prelude <> "\n\n" <> source

  defp prelude_line_count(prelude) do
    # Count lines in prelude + 1 for the separator newlines
    length(String.split(prelude, "\n")) + 1
  end

  # Type check with optional error formatting
  defp typecheck(ast, env, source, line_offset, format_errors) do
    case TypeChecker.check(ast, env) do
      {:ok, _, _} = success ->
        success

      {:errors, errors} when format_errors ->
        formatted = ErrorFormatter.format_all(errors, source, line_offset: line_offset)
        {:error, formatted}

      {:errors, errors} ->
        {:error, {:type_errors, errors}}

      {:error, %Error{} = error} when format_errors ->
        formatted = ErrorFormatter.format(error, source, line_offset: line_offset)
        {:error, formatted}

      {:error, err} ->
        {:error, Error.normalize(err)}
    end
  end
end
