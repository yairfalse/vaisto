defmodule Vaisto.Liquid.Gen do
  @moduledoc """
  Random well-typed Liquid Core terms, and random corruptions of them, for the
  soundness tests of `docs/design/liquid-core.md` §10.15.

  `term/2` builds a closed term of a given type by following the rules of §10.4:
  every former it uses is one Lint accepts at that type, so a term Lint rejects
  is a disagreement between this generator and Lint. Binder names are fresh
  within a term (§10.12). The only effect generated is `crash`, so every term
  runs under the pure handler.
  """

  alias Vaisto.Liquid.Canonical

  @decls """
  ((type Point () (record (x Int) (y Int)))
   (type Box (a) (record (item (tvar a))))
   (type R () (sum (Ok Int) (Err String)))
   (type Shape () (sum (Circle Int) (Rect Int Int))))
  """

  @arrow [:->, [:Int], [:eff, :closed], :Int]
  @row_record [:Record, [:row, [:x, :Int], [:z, [:Bool]], :closed]]
  @tuple [:Tuple, :Int, [:Bool]]
  @list [:List, :Int]
  @data [:Int, :Float, :String, :Atom, :Unit, [:Bool], @list, @tuple, [:Point], [:R], [:Shape], [:Box, :Int], @row_record, [:Reason]]
  @types @data ++ [@arrow]

  @id_type [:forall, [[:a, :Type]], [:->, [[:tvar, :a]], [:eff, :closed], [:tvar, :a]]]
  @get_x_param [:Record, [:row, [:x, :Int], [:rvar, :r]]]
  @get_x_type [:forall, [[:r, :Row]], [:->, [@get_x_param], [:eff, :closed], :Int]]

  def decls, do: Canonical.parse!(@decls, atoms: :create)
  def types, do: @types

  @doc "A closed term of `type`, at most about `depth` formers deep."
  def term(type, depth, first_name \\ 0) do
    Process.put(:liquid_gen_names, first_name)
    gen(type, [], depth)
  end

  defp fresh do
    n = Process.get(:liquid_gen_names) + 1
    Process.put(:liquid_gen_names, n)
    :"v#{n}"
  end

  defp gen(type, env, depth) do
    vars = for {x, ^type} <- env, do: fn -> x end
    general = if depth > 0, do: general(type, env, depth - 1), else: []
    Enum.random(specific(type, env, max(depth - 1, 0), depth == 0) ++ general ++ vars).()
  end

  # Formers whose result type is `type` itself. At depth 0 (`leaf?`) only those
  # that end quickly: literals, constructors of leaves.
  defp specific(:Int, env, d, leaf?) do
    literal = [fn -> Enum.random([0, 1, -1, 2, 3, 7, -7, 2 ** 70, -(2 ** 70)]) end]

    if leaf? do
      literal
    else
      literal ++
        for(op <- [:add, :sub, :mul, :div, :rem], do: fn -> [:prim, op, gen(:Int, env, d), gen(:Int, env, d)] end) ++
        [
          fn -> [:prim, :neg, gen(:Int, env, d)] end,
          fn -> [:prim, :length, gen(@list, env, d)] end,
          fn -> [:select, gen([:Point], env, d), Enum.random([:x, :y])] end,
          fn -> [:select, gen(@row_record, env, d), :x] end,
          fn -> [:select, gen([:Box, :Int], env, d), :item] end,
          fn -> countdown(env, d) end,
          fn -> get_x(env, d) end
        ]
    end
  end

  defp specific(:Float, env, d, leaf?) do
    literal = [fn -> Enum.random([0.0, -0.0, 0.1, 1.5, -2.25, 1.0e308]) end]

    if leaf?,
      do: literal,
      else:
        literal ++
          for(op <- [:fadd, :fsub, :fmul, :fdiv], do: fn -> [:prim, op, gen(:Float, env, d), gen(:Float, env, d)] end) ++
          [fn -> [:prim, :fneg, gen(:Float, env, d)] end, fn -> [:prim, :"int-to-float", gen(:Int, env, d)] end]
  end

  defp specific(:String, env, d, leaf?) do
    literal = [fn -> Enum.random(["", "a", "hello"]) end]
    if leaf?, do: literal, else: literal ++ [fn -> [:prim, :concat, gen(:String, env, d), gen(:String, env, d)] end]
  end

  defp specific(:Atom, _env, _d, _leaf?), do: [fn -> [:atom, Enum.random([:a, :b, :ok])] end]
  defp specific(:Unit, _env, _d, _leaf?), do: [fn -> [:unit] end]

  defp specific([:Bool], env, d, leaf?) do
    literal = [fn -> [:inj, [:Bool], Enum.random([true, false])] end]

    if leaf? do
      literal
    else
      literal ++
        for(op <- [:lt, :le, :gt, :ge], do: fn -> [:prim, op, gen(:Int, env, d), gen(:Int, env, d)] end) ++
        for(op <- [:flt, :fge], do: fn -> [:prim, op, gen(:Float, env, d), gen(:Float, env, d)] end) ++
        [
          fn -> [:prim, :not, gen([:Bool], env, d)] end,
          fn -> [:prim, :empty?, gen(@list, env, d)] end,
          fn ->
            sigma = Enum.random(@data)
            [:prim, Enum.random([:eq, :ne]), gen(sigma, env, d), gen(sigma, env, d)]
          end
        ]
    end
  end

  defp specific(@list, env, d, leaf?) do
    empty = [fn -> [:inj, @list, :Nil] end]

    if leaf?,
      do: empty,
      else: empty ++ [fn -> [:inj, @list, :Cons, gen(:Int, env, d), gen(@list, env, d)] end, fn -> [:prim, :tail, gen(@list, env, d)] end]
  end

  defp specific(@tuple, env, d, _leaf?), do: [fn -> [:tuple, gen(:Int, env, d), gen([:Bool], env, d)] end]

  defp specific([:Point], env, d, _leaf?),
    do: [fn -> [:record, [:Point] | Enum.shuffle([[:x, gen(:Int, env, d)], [:y, gen(:Int, env, d)]])] end]

  defp specific([:Box, :Int], env, d, _leaf?), do: [fn -> [:record, [:Box, :Int], [:item, gen(:Int, env, d)]] end]

  defp specific(@row_record, env, d, _leaf?),
    do: [fn -> [:record, @row_record | Enum.shuffle([[:x, gen(:Int, env, d)], [:z, gen([:Bool], env, d)]])] end]

  defp specific([:R], env, d, _leaf?),
    do: [fn -> [:inj, [:R], :Ok, gen(:Int, env, d)] end, fn -> [:inj, [:R], :Err, gen(:String, env, d)] end]

  defp specific([:Shape], env, d, _leaf?),
    do: [fn -> [:inj, [:Shape], :Circle, gen(:Int, env, d)] end, fn -> [:inj, [:Shape], :Rect, gen(:Int, env, d), gen(:Int, env, d)] end]

  defp specific([:Reason], env, d, _leaf?) do
    [
      fn -> [:inj, [:Reason], Enum.random([:badarith, :badarg, :no_match, :bad_decode])] end,
      fn -> [:inj, [:Reason], :user, [:up, :Int, gen(:Int, env, d)]] end,
      fn -> [:inj, [:Reason], :raised, [:atom, :error], [:up, :String, gen(:String, env, d)]] end
    ]
  end

  defp specific(@arrow, env, d, _leaf?) do
    [
      fn ->
        x = fresh()
        [:fn, [[x, :Int]], gen(:Int, [{x, :Int} | env], d)]
      end
    ]
  end

  # Formers that make a term of any type.
  defp general(type, env, d) do
    [
      fn ->
        sigma = Enum.random(@types)
        x = fresh()
        [:let, [[x, sigma, gen(sigma, env, d)]], gen(type, [{x, sigma} | env], d)]
      end,
      fn -> [:if, gen([:Bool], env, d), gen(type, env, d), gen(type, env, d)] end,
      fn ->
        sigma = Enum.random(@types)
        x = fresh()
        [:app, [:fn, [[x, sigma]], gen(type, [{x, sigma} | env], d)], gen(sigma, env, d)]
      end,
      fn ->
        {n, s} = {fresh(), fresh()}

        [:match, gen([:R], env, d), [[:inj, [:R], :Ok, n], gen(type, [{n, :Int} | env], d)],
         [[:inj, [:R], :Err, s], gen(type, [{s, :String} | env], d)]]
      end,
      fn ->
        {h, t} = {fresh(), fresh()}

        [:match, gen(@list, env, d), [[:inj, @list, :Nil], gen(type, env, d)],
         [[:inj, @list, :Cons, h, t], gen(type, [{h, :Int}, {t, @list} | env], d)]]
      end,
      fn ->
        y = fresh()
        [:match, gen([:Point], env, d), [[:record, [:Point], [:y, y]], gen(type, [{y, :Int} | env], d)]]
      end,
      fn ->
        {w, a} = {fresh(), fresh()}

        [:match, gen(@tuple, env, d),
         [[:as, w, [:tuple, a, [:inj, [:Bool], true]]], gen(type, [{w, @tuple}, {a, :Int} | env], d)],
         [[:tuple, :_, [:inj, [:Bool], false]], gen(type, env, d)]]
      end,
      fn ->
        n = fresh()
        k = Enum.random([0, 1, 3, -2])

        guard =
          Enum.random([
            [:prim, :gt, n, k],
            [:prim, :gt, [:prim, :div, 10, n], 1],
            [:prim, :eq, [:prim, :rem, n, 2], 0]
          ])

        [:match, gen(:Int, env, d), [n, [:when, guard], gen(type, [{n, :Int} | env], d)], [:_, gen(type, env, d)]]
      end,
      fn ->
        r = fresh()
        [:"handle-crash", gen(type, env, d), [[r, [:Reason]], gen(type, [{r, [:Reason]} | env], d)]]
      end,
      fn ->
        x = fresh()
        [:let, [[x, type, [:perform, :crash, gen([:Reason], env, d)]]], x]
      end,
      fn ->
        {id, y} = {fresh(), fresh()}
        [:let, [[id, @id_type, [:fn, [[y, [:tvar, :a]]], y]]], [:app, [:inst, id, type], gen(type, env, d)]]
      end,
      fn -> [:select, [:tuple, gen(type, env, d), gen(:Int, env, d)], 0] end,
      fn -> [:select, [:record, [:Box, type], [:item, gen(type, env, d)]], :item] end
    ]
  end

  # A recursive function that always ends: it counts a small literal down to 0.
  defp countdown(env, d) do
    {f, n} = {fresh(), fresh()}

    [:letrec,
     [[f, @arrow, [:fn, [[n, :Int]], [:if, [:prim, :le, n, 0], gen(:Int, [{n, :Int} | env], d), [:app, f, [:prim, :sub, n, 1]]]]]],
     [:app, f, Enum.random(0..4)]]
  end

  # A row-polymorphic function applied to a named record (D24).
  defp get_x(env, d) do
    {g, p} = {fresh(), fresh()}

    [:let, [[g, @get_x_type, [:fn, [[p, @get_x_param]], [:select, p, :x]]]],
     [:app, [:inst, g, [:row, [:"#Point", :Unit], [:y, :Int], :closed]], gen([:Point], env, d)]]
  end

  # --- Mutation -------------------------------------------------------------------

  @doc """
  A random corruption of `term`: one node, anywhere in the tree (terms, types,
  patterns, labels), replaced, renamed, shortened, lengthened or reordered.
  """
  def mutate(term) do
    symbols = term |> symbols() |> Enum.uniq()
    path = term |> paths([]) |> Enum.random()
    update(term, path, &corrupt(&1, symbols))
  end

  # A type swapped for another well-formed type: the subtle mistake that only
  # checking every annotation catches.
  defp corrupt(type, _symbols) when type in @types do
    if :rand.uniform(2) == 1, do: Enum.random(@types -- [type]), else: corrupt_shape(type)
  end

  defp corrupt(node, symbols), do: corrupt_shape(node, symbols)

  defp corrupt_shape(type), do: corrupt_shape(type, [])

  defp corrupt_shape(n, _symbols) when is_integer(n), do: Enum.random([n + 1, -n, 0, 1.5, "s", :v1, [:atom, :a], [:unit]])
  defp corrupt_shape(r, _symbols) when is_float(r), do: Enum.random([1, -r, "s", [:unit]])
  defp corrupt_shape(s, _symbols) when is_binary(s), do: Enum.random([1, 2.5, s <> "x", [:atom, :a]])
  defp corrupt_shape(a, symbols) when is_atom(a), do: Enum.random(symbols ++ [:v999, :Int, :closed])
  defp corrupt_shape([], _symbols), do: [:unit]

  defp corrupt_shape(node, _symbols) when is_list(node) do
    n = length(node)

    case Enum.random([:drop, :duplicate, :swap, :replace]) do
      :drop when n > 1 -> List.delete_at(node, Enum.random(1..(n - 1)))
      :duplicate -> List.insert_at(node, Enum.random(1..n), Enum.at(node, Enum.random(0..(n - 1))))
      :swap when n > 2 -> swap(node, Enum.random(1..(n - 1)), Enum.random(1..(n - 1)))
      _ -> term(Enum.random(@types), 1, 900)
    end
  end

  defp swap(list, i, j), do: list |> List.replace_at(i, Enum.at(list, j)) |> List.replace_at(j, Enum.at(list, i))

  defp symbols(t) when is_atom(t), do: [t]
  defp symbols(t) when is_list(t), do: Enum.flat_map(t, &symbols/1)
  defp symbols(_t), do: []

  defp paths(t, here) when is_list(t),
    do: [Enum.reverse(here) | t |> Enum.with_index() |> Enum.flat_map(fn {c, i} -> paths(c, [i | here]) end)]

  defp paths(_t, here), do: [Enum.reverse(here)]

  defp update(t, [], f), do: f.(t)
  defp update(t, [i | rest], f), do: List.update_at(t, i, &update(&1, rest, f))
end
