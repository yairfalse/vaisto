defmodule Vaisto.Refine.IR do
  @moduledoc """
  The predicate IR (RFC §5.2), restricted to the sorts of Phase 2.1:
  `:int`, `:bool` and `:list` (an uninterpreted sort with the measure `len`).

  Terms are sorted Elixir terms; a predicate is a term of sort `:bool`.

      {:var, name, sort}  {:int, n}  {:bool, b}
      {:arith, :add | :sub | :mul | :tdiv | :trem, [a, b]}  {:arith, :neg, [a]}
      {:ite, c, a, b}  {:len, xs}
      {:eq, a, b}  {:lt, a, b}  {:le, a, b}
      {:not, p}  {:and, [p]}  {:or, [p]}  {:implies, p, q}  {:iff, p, q}

  `Vaisto.Refine.SMTLIB` lowers it; nothing else reaches the solver.
  """

  @type sort :: :int | :bool | :list
  @type t :: tuple()

  @true_ {:bool, true}
  @false_ {:bool, false}

  @doc "Conjunction, flattening nested conjunctions and dropping `true`."
  @spec and_([t()]) :: t()
  def and_(preds) do
    preds
    |> Enum.flat_map(fn
      {:and, ps} -> ps
      @true_ -> []
      p -> [p]
    end)
    |> case do
      [] -> @true_
      [p] -> p
      ps -> if @false_ in ps, do: @false_, else: {:and, ps}
    end
  end

  @spec not_(t()) :: t()
  def not_(@true_), do: @false_
  def not_(@false_), do: @true_
  def not_({:not, p}), do: p
  def not_(p), do: {:not, p}

  @spec implies(t(), t()) :: t()
  def implies(@true_, q), do: q
  def implies(_p, @true_), do: @true_
  def implies(p, q), do: {:implies, p, q}

  @doc "Replace variables by name. The map's values are terms."
  @spec subst(t(), %{term() => t()}) :: t()
  def subst({:var, name, _sort} = v, map), do: Map.get(map, name, v)
  def subst({:int, _} = t, _map), do: t
  def subst({:bool, _} = t, _map), do: t
  def subst({:arith, op, args}, map), do: {:arith, op, Enum.map(args, &subst(&1, map))}
  def subst({:ite, c, a, b}, map), do: {:ite, subst(c, map), subst(a, map), subst(b, map)}
  def subst({:len, xs}, map), do: {:len, subst(xs, map)}
  def subst({:not, p}, map), do: {:not, subst(p, map)}
  def subst({op, ps}, map) when op in [:and, :or], do: {op, Enum.map(ps, &subst(&1, map))}
  def subst({op, a, b}, map) when op in [:eq, :lt, :le, :implies, :iff], do: {op, subst(a, map), subst(b, map)}

  @doc "The variables of a term, as `{name, sort}`, in order of first occurrence."
  @spec vars(t() | [t()]) :: [{term(), sort()}]
  def vars(terms) when is_list(terms), do: terms |> Enum.flat_map(&collect/1) |> Enum.uniq()
  def vars(term), do: vars([term])

  defp collect({:var, name, sort}), do: [{name, sort}]
  defp collect({:int, _}), do: []
  defp collect({:bool, _}), do: []
  defp collect({:arith, _op, args}), do: Enum.flat_map(args, &collect/1)
  defp collect({:ite, c, a, b}), do: Enum.flat_map([c, a, b], &collect/1)
  defp collect({:len, xs}), do: collect(xs)
  defp collect({:not, p}), do: collect(p)
  defp collect({op, ps}) when op in [:and, :or], do: Enum.flat_map(ps, &collect/1)
  defp collect({_op, a, b}), do: collect(a) ++ collect(b)
end
