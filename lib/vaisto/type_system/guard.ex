defmodule Vaisto.TypeSystem.Guard do
  @moduledoc """
  Which surface guards are guard-safe (liquid-core.md §6.6).

  A guard may use only variables, literals, field access, tuples, lists,
  constructors of sum types, `if`, and the primitives other than concatenation.
  Each surface form is judged by what the bridge lowers it to, so that the
  checkers, the elaborator and Core Lint agree on every guard.
  """

  alias Vaisto.LocationStripper
  alias Vaisto.TypeEnv

  @prims [:+, :-, :*, :/, :div, :rem, :<, :<=, :>, :>=, :==, :!=, :not, :and, :or, :head, :tail, :length, :empty?, :cons]

  @doc """
  The first form in `guard` that a guard may not contain, as `{description,
  node}`, or nil when the guard is guard-safe. `env` is the checker's: it tells
  a constructor from a function, and a primitive from a local that shadows it.
  """
  def unsafe(node, env) do
    case LocationStripper.strip_node(node) |> elem(0) do
      x when is_atom(x) -> name(x, node, env)
      x when is_number(x) -> nil
      {:atom, _} -> nil
      {:string, _} -> nil
      {:unit} -> nil
      {:tuple, es} when is_list(es) -> first(es, env)
      {:tuple_pattern, es} when is_list(es) -> first(es, env)
      {:list, es} when is_list(es) -> first(es, env)
      {:bracket, es} when is_list(es) -> first(es, env)
      {:bracket, {:cons, h, t}} -> first([h, t], env)
      {:field_access, e, _field} -> unsafe(e, env)
      {:if, c, a, b} -> first([c, a, b], env)
      {:do, [e]} -> unsafe(e, env)
      {:call, f, args} when f in @prims and is_list(args) -> if shadowed?(f, env), do: {"a call of `#{f}`", node}, else: first(args, env)
      {:call, :++, _} -> {"concatenation", node}
      {:call, f, args} when is_atom(f) and is_list(args) -> named_call(f, args, node, env)
      {:call, _, _} -> {"a function call", node}
      {:let, _, _} -> {"a let", node}
      {:match, _, _} -> {"a match", node}
      {:do, _} -> {"a do block", node}
      {:fn, _, _} -> {"a lambda", node}
      {:try, _, _, _} -> {"a try", node}
      {:receive, _} -> {"a receive", node}
      _ -> {"this form", node}
    end
  end

  # A variable or a literal atom; not a top-level function named as a value,
  # which a guard cannot build, nor a top-level value, which is computed by a call.
  defp name(x, node, env) do
    cond do
      local?(x, env) -> nil
      top_level_function?(x, env) -> {"the function `#{x}` as a value", node}
      Map.has_key?(env, {:__value__, x}) -> {"the top-level value `#{x}`", node}
      true -> nil
    end
  end

  # Only a constructor of a sum type is guard-safe among named calls: not a
  # function that returns a sum, a record's constructor, or a local.
  defp named_call(f, args, node, env) do
    case result_type(Map.get(env, f)) do
      {:sum, _, variants} -> if List.keymember?(variants, f, 0) and not local?(f, env), do: first(args, env), else: {"a call of `#{f}`", node}
      {:record, ^f, _} -> {"the construction of the record `#{f}`", node}
      _ -> {"a call of `#{f}`", node}
    end
  end

  defp result_type({:forall, _, type}), do: result_type(type)
  defp result_type({:fn, _, result}), do: result
  defp result_type(_), do: nil

  # A primitive's name bound in the program, by a let, a parameter or a
  # definition, names that binding, not the primitive.
  defp shadowed?(f, env), do: local?(f, env) or (Map.has_key?(env, f) and Map.get(env, f) != Map.get(TypeEnv.primitives(), f))

  defp top_level_function?(x, env), do: fn_type?(Map.get(env, x)) and not local?(x, env) and not Map.has_key?(TypeEnv.primitives(), x)

  defp fn_type?({:fn, _, _}), do: true
  defp fn_type?({:forall, _, type}), do: fn_type?(type)
  defp fn_type?(_), do: false

  defp local?(x, env), do: MapSet.member?(Map.get(env, :__local_vars__, MapSet.new()), x)

  defp first(nodes, env), do: Enum.find_value(nodes, &unsafe(&1, env))
end
