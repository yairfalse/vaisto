defmodule Vaisto.Liquid.Prelude do
  @moduledoc """
  Today's higher-order builtins, written in Core: library code, not
  primitives. Each mirrors the `lists` function both backends call: `map`,
  `filter` and `flatmap`, and `foldl` with the function taking (acc, elem).
  A program gets the ones it uses, through `defs/1`.

  Shared by the Phase 0 adapter and the Phase 1a elaborator.
  """

  @prelude Vaisto.Liquid.Canonical.parse!(
             """
             ((def prelude.map
                (forall ((a Type) (b Type)) (-> ((-> ((tvar a)) (eff closed) (tvar b)) (List (tvar a))) (eff closed) (List (tvar b))))
                (fn ((f (-> ((tvar a)) (eff closed) (tvar b))) (xs (List (tvar a))))
                  (match xs
                    ((inj (List (tvar a)) Nil) (inj (List (tvar b)) Nil))
                    ((inj (List (tvar a)) Cons h t)
                     (let ((y (tvar b) (app f h)))
                       (inj (List (tvar b)) Cons y (app (inst prelude.map (tvar a) (tvar b)) f t)))))))
              (def prelude.filter
                (forall ((a Type)) (-> ((-> ((tvar a)) (eff closed) (Bool)) (List (tvar a))) (eff closed) (List (tvar a))))
                (fn ((p (-> ((tvar a)) (eff closed) (Bool))) (xs (List (tvar a))))
                  (match xs
                    ((inj (List (tvar a)) Nil) (inj (List (tvar a)) Nil))
                    ((inj (List (tvar a)) Cons h t)
                     (if (app p h)
                         (inj (List (tvar a)) Cons h (app (inst prelude.filter (tvar a)) p t))
                         (app (inst prelude.filter (tvar a)) p t))))))
              (def prelude.fold
                (forall ((a Type) (b Type)) (-> ((-> ((tvar b) (tvar a)) (eff closed) (tvar b)) (tvar b) (List (tvar a))) (eff closed) (tvar b)))
                (fn ((f (-> ((tvar b) (tvar a)) (eff closed) (tvar b))) (acc (tvar b)) (xs (List (tvar a))))
                  (match xs
                    ((inj (List (tvar a)) Nil) acc)
                    ((inj (List (tvar a)) Cons h t) (app (inst prelude.fold (tvar a) (tvar b)) f (app f acc h) t)))))
              (def prelude.append
                (forall ((a Type)) (-> ((List (tvar a)) (List (tvar a))) (eff closed) (List (tvar a))))
                (fn ((xs (List (tvar a))) (ys (List (tvar a))))
                  (match xs
                    ((inj (List (tvar a)) Nil) ys)
                    ((inj (List (tvar a)) Cons h t) (inj (List (tvar a)) Cons h (app (inst prelude.append (tvar a)) t ys))))))
              (def prelude.flat_map
                (forall ((a Type) (b Type)) (-> ((-> ((tvar a)) (eff closed) (List (tvar b))) (List (tvar a))) (eff closed) (List (tvar b))))
                (fn ((f (-> ((tvar a)) (eff closed) (List (tvar b)))) (xs (List (tvar a))))
                  (match xs
                    ((inj (List (tvar a)) Nil) (inj (List (tvar b)) Nil))
                    ((inj (List (tvar a)) Cons h t)
                     (let ((ys (List (tvar b)) (app f h)))
                       (app (inst prelude.append (tvar b)) ys (app (inst prelude.flat_map (tvar a) (tvar b)) f t))))))))
             """,
             atoms: :create
           )
           |> Map.new(fn [:def, name | _] = def -> {name, def} end)

  # What each prelude definition needs besides itself.
  @needs %{"prelude.flat_map": [:"prelude.append"]}

  @doc "The names of every prelude definition."
  @spec names() :: [atom()]
  def names, do: Map.keys(@prelude)

  @doc "The Core type of a prelude definition, such as `:\"prelude.map\"`."
  @spec type(atom()) :: Vaisto.Liquid.Canonical.tree()
  def type(name) do
    [:def, ^name, type, _fn] = Map.fetch!(@prelude, name)
    type
  end

  @doc """
  The definitions a program that uses `names` needs, in a fixed order.

  A binder may not share a name with any definition of the module
  (liquid-core.md §10.12), so a prelude binder that would, such as `f` in a
  program that defines `f`, is renamed apart from `avoid`.
  """
  @spec defs(Enumerable.t(), Enumerable.t()) :: [Vaisto.Liquid.Canonical.tree()]
  def defs(names, avoid \\ []) do
    avoid = MapSet.new(avoid)

    names
    |> Enum.flat_map(&[&1 | Map.get(@needs, &1, [])])
    |> Enum.uniq()
    |> Enum.sort()
    |> Enum.map(&hygienic(Map.fetch!(@prelude, &1), avoid))
  end

  defp hygienic([:def, name, type, fun] = def, avoid) do
    case fun |> binders() |> Enum.filter(&MapSet.member?(avoid, &1)) do
      [] -> def
      clashes -> [:def, name, type, rename(fun, Map.new(clashes, &{&1, fresh(&1, avoid)}))]
    end
  end

  defp fresh(x, avoid), do: Stream.iterate(2, &(&1 + 1)) |> Stream.map(&:"#{x}'#{&1}") |> Enum.find(&(not MapSet.member?(avoid, &1)))

  # The names a prelude definition binds: parameters, let binders, and pattern
  # variables. None of them is also used as a symbol of the Core syntax.
  defp binders([:fn, params, body]), do: Enum.map(params, &hd/1) ++ binders(body)
  defp binders([:let, [[x, _type, e]], body]), do: [x | binders(e) ++ binders(body)]
  defp binders([:match, s | clauses]), do: binders(s) ++ Enum.flat_map(clauses, fn [p | rest] -> pattern_vars(p) ++ Enum.flat_map(rest, &binders/1) end)
  defp binders(list) when is_list(list), do: Enum.flat_map(list, &binders/1)
  defp binders(_), do: []

  defp pattern_vars(x) when is_atom(x) and x != :_, do: [x]
  defp pattern_vars([:inj, _type, _ctor | ps]), do: Enum.flat_map(ps, &pattern_vars/1)
  defp pattern_vars(_), do: []

  defp rename(x, map) when is_atom(x), do: Map.get(map, x, x)
  defp rename(list, map) when is_list(list), do: Enum.map(list, &rename(&1, map))
  defp rename(t, _map), do: t
end
