defmodule Vaisto.Elab.Exhaustive do
  @moduledoc """
  Exhaustiveness for the elaborator (liquid-core.md §10.8): do the unguarded
  clauses of a match cover every value of the scrutinee's type? If not, a
  witness: a value no clause matches, in Vaisto syntax.

  Maranget's usefulness algorithm ("Warnings for pattern matching", 2007).
  Core Lint checks the same rule with its own implementation (§8.1: one
  table, two implementations), so a bug here is caught there.

  Patterns are Core patterns over zonked types; `decls` maps a type name to
  its declaration as the elaborator keeps it.
  """

  alias Vaisto.Elab.Types

  @doc "`nil` when `patterns` cover `type`, or a witness the first of them misses."
  @spec missing([term()], term(), map()) :: String.t() | nil
  def missing(patterns, type, decls) do
    rows = for p <- patterns, do: [normalize(p, type, decls)]

    case useful(rows, [type], decls) do
      nil -> nil
      [w] -> render(w)
    end
  end

  @doc """
  The position (from 1) of the first clause no value can reach, because the
  unguarded clauses before it match every value it does; `nil` if none.
  `clauses` is a list of `{pattern, guarded?}`.
  """
  @spec redundant([{term(), boolean()}], term(), map()) :: pos_integer() | nil
  def redundant(clauses, type, decls) do
    result =
      clauses
      |> Enum.with_index(1)
      |> Enum.reduce_while([], fn {{p, guarded?}, i}, earlier ->
        row = [normalize(p, type, decls)]

        cond do
          not useful?(earlier, row, [type], decls) -> {:halt, i}
          guarded? -> {:cont, earlier}
          true -> {:cont, earlier ++ [row]}
        end
      end)

    if is_integer(result), do: result
  end

  # Is the pattern vector q useful against `rows` (Maranget's U)?
  defp useful?(rows, [], [], _decls), do: rows == []

  defp useful?(rows, [{:con, k, args} | qs], [type | types], decls) do
    fts =
      case signature(type, decls) do
        {:finite, ctors} -> ctors |> List.keyfind(k, 0) |> elem(1)
        :infinite -> []
      end

    useful?(specialize(rows, k, length(args)), args ++ qs, fts ++ types, decls)
  end

  defp useful?(rows, [:wild | qs], [type | types], decls) do
    heads = rows |> Enum.flat_map(fn [p | _] -> if match?({:con, _, _}, p), do: [elem(p, 1)], else: [] end) |> Enum.uniq()

    with {:finite, ctors} <- signature(type, decls),
         true <- Enum.all?(ctors, fn {k, _} -> k in heads end) do
      Enum.any?(ctors, fn {k, fts} -> useful?(specialize(rows, k, length(fts)), List.duplicate(:wild, length(fts)) ++ qs, fts ++ types, decls) end)
    else
      _ -> useful?(default(rows), qs, types, decls)
    end
  end

  # A pattern as {:con, key, subpatterns} or :wild.
  defp normalize(x, _type, _decls) when is_atom(x), do: :wild
  defp normalize([:as, _x, p], type, decls), do: normalize(p, type, decls)
  defp normalize([:unit], _type, _decls), do: {:con, :unit, []}
  defp normalize([:tuple | ps], [:Tuple | ts], decls), do: {:con, :tuple, Enum.zip_with(ps, ts, &normalize(&1, &2, decls))}

  defp normalize([:inj, _type, c | ps], type, decls) do
    {:finite, ctors} = signature(type, decls)
    {_, fts} = List.keyfind(ctors, {:ctor, c}, 0)
    {:con, {:ctor, c}, Enum.zip_with(ps, fts, &normalize(&1, &2, decls))}
  end

  defp normalize([:record, _type | fields], [t | _] = type, decls) do
    {:finite, [{key, fts}]} = signature(type, decls)
    given = Map.new(fields, fn [l, p] -> {l, p} end)
    labels = for {l, _} <- Map.fetch!(decls, t).members, do: l
    {:con, key, Enum.zip_with(labels, fts, fn l, ft -> normalize(Map.get(given, l, :_), ft, decls) end)}
  end

  defp normalize(literal, _type, _decls), do: {:con, {:literal, literal}, []}

  # The constructors of a type: {:finite, [{key, field types}]} or :infinite.
  # A record's key carries its labelled field types.
  defp signature([:Bool], _decls), do: {:finite, [{{:ctor, true}, []}, {{:ctor, false}, []}]}
  defp signature([:List, a], _decls), do: {:finite, [{{:ctor, :Nil}, []}, {{:ctor, :Cons}, [a, [:List, a]]}]}
  defp signature([:Tuple | ts], _decls), do: {:finite, [{:tuple, ts}]}
  defp signature(:Unit, _decls), do: {:finite, [{:unit, []}]}

  defp signature([t | args], decls) when is_atom(t) and is_map_key(decls, t) do
    %{params: params, kind: kind, members: members} = Map.fetch!(decls, t)
    inst = Map.new(Enum.zip(params, args))

    case kind do
      :sum -> {:finite, for({c, fts} <- members, do: {{:ctor, c}, Enum.map(fts, &Types.subst_tvars(&1, inst))})}
      :record -> {:finite, [{{:record, t}, for({_l, ft} <- members, do: Types.subst_tvars(ft, inst))}]}
    end
  end

  defp signature(_type, _decls), do: :infinite

  # Is the all-wildcard row useful against `rows`? If so, a witness vector.
  defp useful([], types, _decls), do: Enum.map(types, fn _ -> :wild end)
  defp useful(_rows, [], _decls), do: nil

  defp useful(rows, [type | types], decls) do
    heads = rows |> Enum.flat_map(fn [p | _] -> if match?({:con, _, _}, p), do: [elem(p, 1)], else: [] end) |> Enum.uniq()

    case signature(type, decls) do
      {:finite, ctors} ->
        if Enum.all?(ctors, fn {k, _} -> k in heads end) do
          Enum.find_value(ctors, fn {k, fts} ->
            n = length(fts)

            case useful(specialize(rows, k, n), fts ++ types, decls) do
              nil -> nil
              w -> [{:con, k, Enum.take(w, n)} | Enum.drop(w, n)]
            end
          end)
        else
          with w when w != nil <- useful(default(rows), types, decls) do
            {k, fts} = Enum.find(ctors, fn {k, _} -> k not in heads end)
            [{:con, k, Enum.map(fts, fn _ -> :wild end)} | w]
          end
        end

      :infinite ->
        with w when w != nil <- useful(default(rows), types, decls), do: [:wild | w]
    end
  end

  defp specialize(rows, k, n) do
    Enum.flat_map(rows, fn
      [{:con, ^k, ps} | rest] -> [ps ++ rest]
      [{:con, _other, _} | _] -> []
      [:wild | rest] -> [List.duplicate(:wild, n) ++ rest]
    end)
  end

  defp default(rows), do: for([:wild | rest] <- rows, do: rest)

  defp render(:wild), do: "_"
  defp render({:con, {:ctor, b}, []}) when is_boolean(b), do: to_string(b)
  defp render({:con, {:ctor, :Nil}, []}), do: "[]"
  defp render({:con, {:ctor, :Cons}, [h, t]}), do: "[#{render(h)} | #{render(t)}]"
  defp render({:con, {:ctor, c}, []}), do: "(#{c})"
  defp render({:con, {:ctor, c}, ps}), do: "(#{c} " <> Enum.map_join(ps, " ", &render/1) <> ")"
  defp render({:con, :unit, []}), do: "()"
  defp render({:con, :tuple, ps}), do: "{" <> Enum.map_join(ps, " ", &render/1) <> "}"
  defp render({:con, {:record, t}, ps}), do: "(#{t} " <> Enum.map_join(ps, " ", &render/1) <> ")"
end
