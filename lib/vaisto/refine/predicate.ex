defmodule Vaisto.Refine.Predicate do
  @moduledoc """
  The predicate translator (RFC §4.4 "faithful translation", M6): a refinement's
  predicate, written as a Vaisto expression, to the IR (`Vaisto.Refine.IR`).

  It is part of the trusted base, so it is small and admits only constructs
  whose meaning in the logic is their meaning in Vaisto. Predicates are pure
  and total by construction: no function calls, and no partial operators
  (`div`, `rem`, `head`, `tail`) until definedness is checked inside
  predicates. Anything else is rejected, never approximated (§11.2).

  The scope maps each name a predicate may mention to `{ir_term, sort}`.
  """

  alias Vaisto.Refine.IR
  alias Vaisto.Parser.Loc

  @arith %{:+ => :add, :- => :sub, :* => :mul}
  @compare [:<, :<=, :>, :>=]

  @doc "The sort of a base type written in a refinement, or an error message (C18)."
  @spec sort_of_base(term()) :: {:ok, IR.sort()} | {:error, String.t()}
  def sort_of_base(:int), do: {:ok, :int}
  def sort_of_base(:bool), do: {:ok, :bool}
  def sort_of_base({:call, :List, [_elem], _loc}), do: {:ok, :list}
  def sort_of_base(:float), do: {:error, "refinements on Float are not supported"}
  def sort_of_base(:string), do: {:error, "refinements on String are not supported"}
  def sort_of_base(type), do: {:error, "refinements on #{describe_type(type)} are not supported yet: only Int, Bool and List"}

  @doc """
  Split a predicate into its top-level conjuncts, as written, so that each is
  checked and reported on its own (§13.2).
  """
  @spec conjuncts(term()) :: [term()]
  def conjuncts({:call, :and, args, _loc}), do: Enum.flat_map(args, &conjuncts/1)
  def conjuncts(pred), do: [pred]

  @doc "Translate a predicate of sort Bool."
  @spec translate(term(), %{atom() => {IR.t(), IR.sort()}}) :: {:ok, IR.t()} | {:error, String.t()}
  def translate(pred, scope) do
    case term(pred, scope) do
      {:bool, p} -> {:ok, p}
      {sort, _} -> {:error, "a refinement must be a condition, but this is #{article(sort)}"}
    end
  catch
    {:predicate, message} -> {:error, message}
  end

  # term/2 returns {sort, ir}

  defp term(n, _scope) when is_integer(n), do: {:int, {:int, n}}
  defp term(b, _scope) when is_boolean(b), do: {:bool, {:bool, b}}
  defp term(f, _scope) when is_float(f), do: fail("refinements on Float are not supported")
  defp term({:string, _}, _scope), do: fail("refinements on String are not supported")

  defp term(name, scope) when is_atom(name) do
    case Map.fetch(scope, name) do
      {:ok, {ir, sort}} -> {sort, ir}
      :error -> fail("`#{name}` is not in scope in this refinement")
    end
  end

  defp term({:if, c, a, b, %Loc{}}, scope) do
    c = expect(:bool, c, scope)
    {sort, a} = term(a, scope)
    b = expect(sort, b, scope)
    {sort, {:ite, c, a, b}}
  end

  defp term({:call, op, [a], %Loc{}}, scope) when op == :- do
    {:int, {:arith, :neg, [expect(:int, a, scope)]}}
  end

  defp term({:call, op, [a, b], %Loc{}}, scope) when is_map_key(@arith, op) do
    {:int, {:arith, Map.fetch!(@arith, op), [expect(:int, a, scope), expect(:int, b, scope)]}}
  end

  defp term({:call, op, [a, b], %Loc{}}, scope) when op in @compare do
    a = expect(:int, a, scope)
    b = expect(:int, b, scope)

    case op do
      :< -> {:bool, {:lt, a, b}}
      :<= -> {:bool, {:le, a, b}}
      :> -> {:bool, {:lt, b, a}}
      :>= -> {:bool, {:le, b, a}}
    end
  end

  defp term({:call, op, [a, b], %Loc{}}, scope) when op in [:==, :!=] do
    {sort, a} = term(a, scope)
    b = expect(sort, b, scope)
    if op == :==, do: {:bool, {:eq, a, b}}, else: {:bool, IR.not_({:eq, a, b})}
  end

  defp term({:call, op, args, %Loc{}}, scope) when op in [:and, :or] and args != [] do
    {:bool, {op, Enum.map(args, &expect(:bool, &1, scope))}}
  end

  defp term({:call, :not, [a], %Loc{}}, scope), do: {:bool, IR.not_(expect(:bool, a, scope))}
  defp term({:call, :"=>", [a, b], %Loc{}}, scope), do: {:bool, {:implies, expect(:bool, a, scope), expect(:bool, b, scope)}}
  defp term({:call, :iff, [a, b], %Loc{}}, scope), do: {:bool, {:iff, expect(:bool, a, scope), expect(:bool, b, scope)}}

  defp term({:call, op, [xs], %Loc{}}, scope) when op in [:len, :length], do: {:int, {:len, expect(:list, xs, scope)}}

  defp term({:call, op, _args, %Loc{}}, _scope) when op in [:div, :rem, :head, :tail] do
    fail("`#{op}` cannot be used in a refinement yet: it is partial, and definedness inside predicates is not checked yet")
  end

  defp term({:call, op, _args, %Loc{}}, _scope) when is_atom(op) do
    fail("`#{op}` cannot be used in a refinement: only comparisons, arithmetic, `len`, `and`, `or`, `not`, `=>`, `iff` and `if` are allowed")
  end

  defp term(other, _scope), do: fail("this cannot be used in a refinement: #{inspect(other, limit: 4)}")

  defp expect(sort, expr, scope) do
    case term(expr, scope) do
      {^sort, ir} -> ir
      {other, _} -> fail("expected #{article(sort)} here, but this is #{article(other)}")
    end
  end

  defp fail(message), do: throw({:predicate, message})

  defp article(:int), do: "an Int"
  defp article(:bool), do: "a Bool"
  defp article(:list), do: "a List"

  defp describe_type({:call, name, _args, _loc}), do: "#{name}"
  defp describe_type(type) when is_atom(type), do: type |> Atom.to_string() |> String.capitalize()
  defp describe_type(type), do: inspect(type)
end
