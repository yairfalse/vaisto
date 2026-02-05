defmodule Vaisto.TestHelpers do
  @moduledoc """
  Shared test utilities for Vaisto compiler tests.

  Provides convenient functions for parsing, type checking, and running
  Vaisto code within test cases.
  """

  alias Vaisto.{Parser, TypeChecker, CoreEmitter}

  @doc """
  Parse Vaisto source code and return the AST.

  Raises an exception if parsing fails, making test failures more visible.

  ## Examples

      ast = parse!("(+ 1 2)")
      assert {:call, :+, _, _} = ast
  """
  @spec parse!(String.t(), keyword()) :: term()
  def parse!(code, opts \\ []) do
    Parser.parse(code, opts)
  end

  @doc """
  Parse and type check Vaisto code, returning the inferred type.

  Returns `{:ok, type}` on success, or `{:error, reason}` on failure.

  ## Examples

      assert {:ok, :int} = check_type("(+ 1 2)")
      assert {:ok, {:fn, [:int, :int], :int}} = check_type("(defn add [x :int y :int] :int (+ x y))")
  """
  @spec check_type(String.t()) :: {:ok, term()} | {:error, term()}
  def check_type(code) do
    ast = parse!(code)
    case TypeChecker.check(ast) do
      {:ok, type, _typed_ast} -> {:ok, type}
      {:error, _} = err -> err
      {:errors, errors} -> {:error, hd(errors)}
    end
  end

  @doc """
  Assert that Vaisto code type checks to the expected type.

  ## Examples

      assert_type "(+ 1 2)", :int
      assert_type "(list 1 2 3)", {:list, :int}
  """
  @spec assert_type(String.t(), term()) :: true
  def assert_type(code, expected_type) do
    case check_type(code) do
      {:ok, actual_type} ->
        if types_equal?(actual_type, expected_type) do
          true
        else
          raise ExUnit.AssertionError,
            message: "Expected type #{inspect(expected_type)}, got #{inspect(actual_type)}"
        end

      {:error, reason} ->
        raise ExUnit.AssertionError,
          message: "Type check failed: #{inspect(reason)}"
    end
  end

  @doc """
  Assert that Vaisto code fails type checking with a matching error.

  The pattern can be a string (checked with =~) or a regex.

  ## Examples

      assert_type_error "(+ 1 :foo)", "type mismatch"
      assert_type_error "(undefined_fn 1)", ~r/unknown function/
  """
  @spec assert_type_error(String.t(), String.t() | Regex.t()) :: true
  def assert_type_error(code, pattern) do
    case check_type(code) do
      {:ok, type} ->
        raise ExUnit.AssertionError,
          message: "Expected type error, but got type #{inspect(type)}"

      {:error, error} ->
        error_msg = error_to_string(error)
        matches = if is_binary(pattern) do
          String.contains?(error_msg, pattern)
        else
          Regex.match?(pattern, error_msg)
        end

        if matches do
          true
        else
          raise ExUnit.AssertionError,
            message: "Error message '#{error_msg}' does not match pattern #{inspect(pattern)}"
        end
    end
  end

  @doc """
  Compile and run Vaisto code, returning the result.

  Uses the Core Erlang backend by default.

  ## Examples

      assert {:ok, 3} = compile_and_run("(defn main [] :int (+ 1 2))")
      assert {:ok, [1, 2, 3]} = compile_and_run("(defn main [] (List :int) (list 1 2 3))")
  """
  @spec compile_and_run(String.t(), atom() | nil) :: {:ok, term()} | {:error, term()}
  def compile_and_run(code, module_name \\ nil) do
    module_name = module_name || generate_module_name()

    ast = parse!(code)
    case TypeChecker.check(ast) do
      {:ok, _type, typed_ast} ->
        case CoreEmitter.compile(typed_ast, module_name) do
          {:ok, _mod, binary} ->
            :code.load_binary(module_name, ~c"test", binary)
            result = apply(module_name, :main, [])
            {:ok, result}

          {:error, _} = err ->
            err
        end

      {:error, _} = err ->
        err

      {:errors, errors} ->
        {:error, hd(errors)}
    end
  end

  @doc """
  Compile and run Vaisto code, raising on failure.

  ## Examples

      assert 3 = compile_and_run!("(defn main [] :int (+ 1 2))")
  """
  @spec compile_and_run!(String.t(), atom() | nil) :: term()
  def compile_and_run!(code, module_name \\ nil) do
    case compile_and_run(code, module_name) do
      {:ok, result} -> result
      {:error, reason} -> raise "Compilation/execution failed: #{inspect(reason)}"
    end
  end

  @doc """
  Evaluate a Vaisto expression and return the result.

  This wraps the expression in a main function and runs it.

  ## Examples

      assert {:ok, 6} = eval("(+ 1 2 3)")
      assert {:ok, true} = eval("(> 5 3)")
  """
  @spec eval(String.t()) :: {:ok, term()} | {:error, term()}
  def eval(expr) do
    code = "(defn main [] :any #{expr})"
    compile_and_run(code)
  end

  @doc """
  Evaluate a Vaisto expression, raising on failure.
  """
  @spec eval!(String.t()) :: term()
  def eval!(expr) do
    case eval(expr) do
      {:ok, result} -> result
      {:error, reason} -> raise "Evaluation failed: #{inspect(reason)}"
    end
  end

  @doc """
  Parse source and return a clean AST (locations stripped).

  Useful for testing AST structure without location noise.
  """
  @spec parse_clean(String.t()) :: term()
  def parse_clean(code) do
    code
    |> parse!()
    |> strip_locations()
  end

  # ============================================================================
  # Private Helpers
  # ============================================================================

  defp generate_module_name do
    :"TestModule#{System.unique_integer([:positive])}"
  end

  defp types_equal?(actual, expected) when is_tuple(actual) and is_tuple(expected) do
    tuple_size(actual) == tuple_size(expected) and
      Enum.all?(Enum.zip(Tuple.to_list(actual), Tuple.to_list(expected)), fn {a, e} ->
        types_equal?(a, e)
      end)
  end

  defp types_equal?(actual, expected) when is_list(actual) and is_list(expected) do
    length(actual) == length(expected) and
      Enum.all?(Enum.zip(actual, expected), fn {a, e} -> types_equal?(a, e) end)
  end

  defp types_equal?(actual, expected), do: actual == expected

  defp error_to_string(%Vaisto.Error{message: msg}), do: msg
  defp error_to_string(error) when is_binary(error), do: error
  defp error_to_string(error), do: inspect(error)

  defp strip_locations(ast) when is_tuple(ast) do
    list = Tuple.to_list(ast)
    case List.last(list) do
      %Vaisto.Parser.Loc{} ->
        list
        |> Enum.take(length(list) - 1)
        |> Enum.map(&strip_locations/1)
        |> List.to_tuple()

      _ ->
        list
        |> Enum.map(&strip_locations/1)
        |> List.to_tuple()
    end
  end

  defp strip_locations(ast) when is_list(ast) do
    Enum.map(ast, &strip_locations/1)
  end

  defp strip_locations(ast), do: ast
end
