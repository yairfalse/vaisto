defmodule Vaisto.Errors do
  @moduledoc """
  Error constructors for all Vaisto compiler errors.
  """

  alias Vaisto.Error

  # ============================================================================
  # Type Errors
  # ============================================================================

  @doc "Type mismatch between expected and actual types"
  def type_mismatch(expected, actual, opts \\ []) do
    Error.new("type mismatch",
      Keyword.merge(opts, [expected: expected, actual: actual])
    )
  end

  @doc "List elements have inconsistent types"
  def list_type_mismatch(expected, actual, opts \\ []) do
    Error.new("list elements must have the same type",
      Keyword.merge(opts, [expected: expected, actual: actual])
    )
  end

  @doc "If/match branches have different types"
  def branch_type_mismatch(branch1_type, branch2_type, opts \\ []) do
    Error.new("branch types must match",
      Keyword.merge(opts, [
        expected: branch1_type,
        actual: branch2_type,
        note: "all branches of a conditional must return the same type"
      ])
    )
  end

  @doc "Return type doesn't match declared type"
  def return_type_mismatch(declared, inferred, opts \\ []) do
    Error.new("return type mismatch",
      Keyword.merge(opts, [
        expected: declared,
        actual: inferred,
        hint: "the function body returns a different type than declared"
      ])
    )
  end

  @doc "Wrong number of arguments to function"
  def arity_mismatch(func_name, expected, actual, opts \\ []) do
    Error.new("wrong number of arguments",
      Keyword.merge(opts, [
        note: "`#{func_name}` takes #{expected} argument(s), but #{actual} were provided"
      ])
    )
  end

  @doc "cons type mismatch between element and list"
  def cons_type_mismatch(elem_type, list_type, opts \\ []) do
    Error.new("cons type mismatch",
      Keyword.merge(opts, [
        expected: list_type,
        actual: elem_type,
        hint: "element type must match list element type"
      ])
    )
  end

  @doc "Operation expects a list"
  def not_a_list(operation, actual, opts \\ []) do
    Error.new("expected a list",
      Keyword.merge(opts, [
        expected: {:list, :any},
        actual: actual,
        note: "`#{operation}` operates on lists"
      ])
    )
  end

  @doc "Operation expects a function"
  def not_a_function(operation, actual, opts \\ []) do
    Error.new("expected a function",
      Keyword.merge(opts, [
        expected: {:fn, [:any], :any},
        actual: actual,
        note: "`#{operation}` requires a function argument"
      ])
    )
  end

  @doc "map/filter function has wrong arity"
  def mapper_arity(operation, expected, actual, opts \\ []) do
    Error.new("#{operation} function has wrong arity",
      Keyword.merge(opts, [
        note: "`#{operation}` function must take exactly #{expected} argument(s), got #{actual}"
      ])
    )
  end

  @doc "filter predicate must return bool"
  def predicate_not_bool(actual, opts \\ []) do
    Error.new("predicate must return Bool",
      Keyword.merge(opts, [
        expected: :bool,
        actual: actual,
        hint: "filter predicates must return true or false"
      ])
    )
  end

  # ============================================================================
  # Name Resolution Errors
  # ============================================================================

  @doc "Variable not defined in scope"
  def undefined_variable(name, opts \\ []) do
    Error.new("undefined variable",
      Keyword.merge(opts, [
        note: "`#{name}` is not defined in this scope"
      ])
    )
  end

  @doc "Function not found"
  def unknown_function(name, opts \\ []) do
    name_str = format_function_name(name)
    hint = case suggest_function(name) do
      nil -> nil
      suggestion -> "did you mean `#{suggestion}`?"
    end
    Error.new("unknown function",
      Keyword.merge(opts, [
        note: "`#{name_str}` is not defined",
        hint: hint
      ])
    )
  end

  @doc "Type not found"
  def unknown_type(name, opts \\ []) do
    Error.new("unknown type",
      Keyword.merge(opts, [
        note: "type `#{name}` is not defined"
      ])
    )
  end

  @doc "Process not defined"
  def unknown_process(name, opts \\ []) do
    Error.new("unknown process",
      Keyword.merge(opts, [
        note: "process `#{name}` is not defined in this module"
      ])
    )
  end

  # ============================================================================
  # Syntax Errors
  # ============================================================================

  @doc "Invalid defn syntax"
  def invalid_defn_syntax(opts \\ []) do
    Error.new("invalid function definition",
      Keyword.merge(opts, [
        hint: "expected (defn name [params] body) or (defn name [params] :type body)"
      ])
    )
  end

  @doc "Parse error"
  def parse_error(message, opts \\ []) do
    Error.new(message, opts)
  end

  @doc "Unknown expression type"
  def unknown_expression(expr, opts \\ []) do
    Error.new("unknown expression",
      Keyword.merge(opts, [
        note: "cannot type check: #{inspect(expr)}"
      ])
    )
  end

  @doc "Non-exhaustive pattern match on sum type"
  def non_exhaustive_sum(type_name, missing_variants, opts \\ []) do
    missing_list = missing_variants |> Enum.map(&to_string/1) |> Enum.join(", ")
    Error.new("non-exhaustive pattern match",
      Keyword.merge(opts, [
        note: "match on `#{type_name}` does not cover all variants",
        hint: "missing variants: #{missing_list}"
      ])
    )
  end

  @doc "Non-exhaustive pattern match on result tuples"
  def non_exhaustive_result(missing_tags, opts \\ []) do
    missing_str = missing_tags |> Enum.map(&":#{&1}") |> Enum.join(", ")
    Error.new("non-exhaustive pattern match",
      Keyword.merge(opts, [
        note: "result-like match is not exhaustive",
        hint: "missing patterns for: #{missing_str}"
      ])
    )
  end

  @doc "Unknown supervision strategy"
  def unknown_supervision_strategy(strategy, opts \\ []) do
    Error.new("unknown supervision strategy",
      Keyword.merge(opts, [
        note: "`#{inspect(strategy)}` is not a valid strategy",
        hint: "valid strategies are :one_for_one, :all_for_one, :rest_for_one"
      ])
    )
  end

  @doc "Extern function lookup failed"
  def extern_not_a_function(mod, func, actual, opts \\ []) do
    Error.new("extern is not a function",
      Keyword.merge(opts, [
        note: "`#{mod}:#{func}` resolved to #{inspect(actual)}, not a function type"
      ])
    )
  end

  # ============================================================================
  # Process/Concurrency Errors
  # ============================================================================

  @doc "Process doesn't accept this message type"
  def invalid_message(process_name, message, accepted, opts \\ []) do
    accepted_str = accepted |> Enum.map(&":#{&1}") |> Enum.join(", ")
    Error.new("invalid message type",
      Keyword.merge(opts, [
        note: "process `#{process_name}` does not accept `:#{message}`",
        hint: "accepted messages: #{accepted_str}"
      ])
    )
  end

  @doc "Can only send to PIDs"
  def send_to_non_pid(actual, opts \\ []) do
    Error.new("cannot send to non-pid",
      Keyword.merge(opts, [
        actual: actual,
        note: "the `!` operator requires a PID as the first argument"
      ])
    )
  end

  # ============================================================================
  # Helpers
  # ============================================================================

  defp suggest_function(name) when is_atom(name) do
    name_str = Atom.to_string(name)
    common = ~w(map filter fold head tail cons empty? length if let match defn deftype)

    Enum.find(common, fn func ->
      String.jaro_distance(name_str, func) > 0.75
    end)
  end
  defp suggest_function(_), do: nil

  defp format_function_name({:module_path, parts}) when is_list(parts) do
    Enum.join(parts, ".")
  end
  defp format_function_name(name) when is_atom(name), do: Atom.to_string(name)
  defp format_function_name(name) when is_binary(name), do: name
  defp format_function_name(name), do: inspect(name)
end
