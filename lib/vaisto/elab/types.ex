defmodule Vaisto.Elab.Types do
  @moduledoc """
  Types for the elaborator (liquid-types.md §3, §9): surface annotations to
  Core types, metavariables, and unification.

  A type is a Core type (liquid-core.md §3.2), possibly containing
  metavariables `{:meta, n}`, which stand for a type local inference has not
  chosen yet. Unification computes their most general solution, a
  coequalizer (§9). Metavariables never reach Core: the elaborator replaces
  them by their solution, and one that has none is an error, never `Dyn`.

  `[:tvar, a]` is a type variable of the signature being checked. Inside
  that definition it is rigid: it equals only itself.
  """

  @primitives %{int: :Int, float: :Float, string: :String, binary: :String, bool: [:Bool], atom: :Atom, unit: :Unit, ref: :Ref, dyn: :Dyn}

  @type t :: term()
  @type subst :: %{non_neg_integer() => t()}

  # --- Surface annotations ---------------------------------------------------------

  @doc """
  The Core type an annotation writes, given the declared types
  (`%{name => arity}`). Returns `{:ok, type}` or `{:error, message}`.

  `:a` is a type variable, `:Point` and `(Result :int :string)` are named
  types, and `(Fn :int :int :bool)` is a function of two Ints to a Bool.
  `:any` is no annotation: there is no `:any` type (§9).
  """
  @spec from_surface(term(), %{atom() => non_neg_integer()}) :: {:ok, t()} | {:error, String.t()}
  def from_surface(annotation, decls) do
    type = surface(annotation, decls)
    validate_kinds(type)
    {:ok, type}
  catch
    {:type_error, message} -> {:error, message}
  end

  defp surface({:atom, t}, decls), do: surface(t, decls)
  defp surface(:any, _decls), do: fail("no type is given")
  defp surface(:num, _decls), do: fail("`:num` is not a type: write `:int` or `:float`")
  defp surface(t, _decls) when is_map_key(@primitives, t), do: Map.fetch!(@primitives, t)
  defp surface(:Reason, _decls), do: [:Reason]

  defp surface(t, decls) when is_atom(t) do
    cond do
      Map.has_key?(decls, t) -> named(t, [], decls)
      lowercase?(t) -> [:tvar, t]
      true -> fail("the type `#{t}` is not declared")
    end
  end

  defp surface({:call, :List, [elem], _loc}, decls), do: [:List, surface(elem, decls)]
  defp surface({:call, :Map, [key, value], _loc}, decls), do: [:Map, surface(key, decls), surface(value, decls)]
  defp surface({:call, :Record, [{:bracket, fields}], _loc}, decls) do
    [:Record, record_row(fields, :closed, decls)]
  end
  defp surface({:call, :Record, [{:bracket, fields}, tail], _loc}, decls) do
    name = case tail do
      {:atom, name} when is_atom(name) -> name
      name when is_atom(name) -> name
      _ -> fail("a record row tail must be a variable")
    end
    unless lowercase?(name), do: fail("a record row variable must start with a lowercase letter")
    [:Record, record_row(fields, [:rvar, name], decls)]
  end

  defp surface({:call, :Pid, [protocol], _loc}, decls), do: [:Pid, surface(protocol, decls)]
  defp surface({:call, :Tuple, [], _loc}, _decls), do: :Unit
  defp surface({:call, :Tuple, ts, _loc}, decls), do: [:Tuple | Enum.map(ts, &surface(&1, decls))]

  defp surface({:call, :Fn, [_ | _] = ts, _loc}, decls) do
    {ts, eff} = case Enum.split(ts, -2) do
      {types, [{:atom, :effects}, row]} when types != [] -> {types, effect(row)}
      _ -> {ts, [:eff, :closed]}
    end
    {params, [result]} = Enum.split(ts, -1)
    [:->, Enum.map(params, &surface(&1, decls)), eff, surface(result, decls)]
  end

  defp surface({:call, {:module_path, ["Core", "Result"]}, args, loc}, decls),
    do: surface({:call, :"Core.Result", args, loc}, decls)

  defp surface({:call, {:module_path, path}, args, loc}, decls),
    do: surface({:call, path |> Enum.join(".") |> String.to_atom(), args, loc}, decls)
  defp surface({:module_path, path}, decls),
    do: surface(path |> Enum.join(".") |> String.to_atom(), decls)

  defp surface({:call, t, args, _loc}, decls) when is_atom(t) do
    if Map.has_key?(decls, t), do: named(t, Enum.map(args, &surface(&1, decls)), decls), else: fail("the type `#{t}` is not declared")
  end

  defp surface(other, _decls), do: fail("this is not a type: #{inspect(other, limit: 4)}")

  defp record_row(fields, tail, decls) do
    fields = fields |> Enum.chunk_every(2) |> Enum.map(fn
      [name, type] when is_atom(name) -> [name, surface(type, decls)]
      [{:atom, name}, type] -> [name, surface(type, decls)]
      _ -> fail("a record type contains field-name/type pairs")
    end)
    names = Enum.map(fields, &hd/1)
    if Enum.uniq(names) != names, do: fail("a record type declares a field twice")
    [:row | Enum.sort(fields) ++ [tail]]
  end

  def effect_from_surface(row) do
    {:ok, effect(row)}
  catch
    {:type_error, message} -> {:error, message}
  end

  defp effect({:atom, name}), do: effect(name)
  defp effect(name) when is_atom(name) do
    if lowercase?(name), do: [:eff, [:evar, name]], else: fail("an effect variable must start with a lowercase letter")
  end
  defp effect({:bracket, labels}) do
    labels = Enum.map(labels, fn
      {:atom, name} -> name
      name when is_atom(name) -> name
      _ -> fail("an effect label must be a name")
    end)
    if Enum.uniq(labels) != labels, do: fail("an effect label is declared twice")
    [:eff | Enum.sort(labels) ++ [:closed]]
  end
  defp effect({:call, :Effects, [{:bracket, _} = labels, tail], _}) do
    case {effect(labels), effect(tail)} do
      {[:eff | items], [:eff, [:evar, _] = tail]} -> splice_eff(Enum.drop(items, -1), tail)
      _ -> fail("Effects requires closed labels and an effect variable")
    end
  end
  defp effect(_), do: fail("effects require a variable or a list of labels")

  defp named(t, args, decls) do
    case Map.fetch!(decls, t) do
      {canonical, n} when n == length(args) -> [canonical | args]
      {_canonical, n} -> fail("`#{t}` takes #{n} type arguments, but #{length(args)} are given")
      n when n == length(args) -> [t | args]
      n -> fail("`#{t}` takes #{n} type argument#{if n == 1, do: "", else: "s"}, but #{length(args)} #{if length(args) == 1, do: "is", else: "are"} given")
    end
  end

  defp fail(message), do: throw({:type_error, message})

  defp lowercase?(t), do: String.match?(Atom.to_string(t), ~r/^[a-z]/)

  # --- Type variables and substitution -----------------------------------------------

  @doc "The type variables of a type, in order of first appearance."
  @spec tvars(t()) :: [atom()]
  def tvars(type), do: type |> collect_tvars() |> Enum.uniq()

  defp collect_tvars([:tvar, a]), do: [a]
  defp collect_tvars(list) when is_list(list), do: Enum.flat_map(list, &collect_tvars/1)
  defp collect_tvars(_), do: []

  @doc "Replace type variables by name."
  @spec subst_tvars(t(), %{atom() => t()}) :: t()
  def subst_tvars([:tvar, a] = t, map), do: Map.get(map, a, t)
  def subst_tvars([:eff | items], map) do
    {labels, [tail]} = Enum.split(items, -1)
    tail = case tail do
      [:evar, a] -> Map.get(map, a, tail)
      tail -> tail
    end
    splice_eff(labels, tail)
  end
  def subst_tvars([:row | items], map) do
    {fields, [tail]} = Enum.split(items, -1)
    fields = Enum.map(fields, fn [l, t] -> [l, subst_tvars(t, map)] end)
    tail = case tail do
      [:rvar, a] -> Map.get(map, a, tail)
      tail -> tail
    end
    splice_row(fields, tail)
  end
  def subst_tvars(list, map) when is_list(list), do: Enum.map(list, &subst_tvars(&1, map))
  def subst_tvars(t, _map), do: t

  @doc "Close a type over its type variables: `(forall ((a Type)...) τ)`."
  @spec generalize(t()) :: t()
  def generalize(type) do
    validate_kinds(type)
    case variables(type) do
      [] -> type
      binders -> [:forall, Enum.map(binders, fn {a, k} -> [a, k] end), type]
    end
  end

  def variables([:tvar, a]), do: [{a, :Type}]
  def variables([:evar, a]), do: [{a, :Eff}]
  def variables([:rvar, a]), do: [{a, :Row}]
  def variables(xs) when is_list(xs), do: Enum.flat_map(xs, &variables/1) |> Enum.uniq()
  def variables(_), do: []

  defp validate_kinds(type) do
    vars = variables(type)
    names = Enum.map(vars, &elem(&1, 0))
    if Enum.uniq(names) != names, do: fail("a variable name cannot have two different kinds")
  end

  # --- Metavariables and unification (§9) -----------------------------------------------

  @doc "A type with every solved metavariable replaced by its solution."
  @spec zonk(t(), subst()) :: t()
  def zonk({:meta, n} = m, s) do
    case Map.fetch(s, n) do
      {:ok, t} -> zonk(t, s)
      :error -> m
    end
  end

  def zonk([:row | items], s) do
    {fields, [tail]} = Enum.split(items, -1)
    fields = Enum.map(fields, fn [l, t] -> [l, zonk(t, s)] end)
    splice_row(fields, zonk(tail, s))
  end
  def zonk([:eff | items], s) do
    {labels, [tail]} = Enum.split(items, -1)
    splice_eff(labels, zonk(tail, s))
  end
  def zonk(list, s) when is_list(list), do: Enum.map(list, &zonk(&1, s))
  def zonk(t, _s), do: t

  defp splice_row(fields, [:row | items]) do
    {more, [tail]} = Enum.split(items, -1)
    splice_row(fields ++ more, tail)
  end
  defp splice_row(fields, tail) do
    names = Enum.map(fields, &hd/1)
    if Enum.uniq(names) != names, do: fail("row substitution gives a field twice")
    [:row | Enum.sort(fields) ++ [tail]]
  end

  defp splice_eff(labels, [:eff | items]) do
    {more, [tail]} = Enum.split(items, -1)
    splice_eff(labels ++ more, tail)
  end
  defp splice_eff(labels, tail) do
    if Enum.uniq(labels) != labels, do: fail("effect substitution gives a label twice")
    [:eff | Enum.sort(labels) ++ [tail]]
  end

  @doc "Whether a (zonked) type still has an unsolved metavariable."
  @spec unsolved?(t()) :: boolean()
  def unsolved?({:meta, _}), do: true
  def unsolved?(list) when is_list(list), do: Enum.any?(list, &unsolved?/1)
  def unsolved?(_), do: false

  @doc """
  The most general unifier of two types, extending substitution and the
  fresh-variable counter. Written type, row and effect variables are rigid.
  """
  @spec unify(t(), t(), subst(), non_neg_integer()) :: {:ok, subst(), non_neg_integer()} | :error
  def unify(a, b, s, next) do
    {s, next} = go(zonk(a, s), zonk(b, s), {s, next})
    {:ok, s, next}
  catch
    :mismatch -> :error
  end

  defp go({:meta, n}, {:meta, n}, s), do: s
  # The older metavariable stays the representative, so an undetermined type
  # is reported where it first arose.
  defp go({:meta, n}, {:meta, m}, s) when n < m, do: bind(m, {:meta, n}, s)
  defp go({:meta, n}, {:meta, m}, s), do: bind(n, {:meta, m}, s)
  defp go({:meta, n}, t, s), do: bind(n, t, s)
  defp go(t, {:meta, n}, s), do: bind(n, t, s)
  defp go(a, a, s) when is_atom(a), do: s

  defp go([tag | a], [tag | b], {s, next}) when tag in [:row, :eff] do
    {af, [at]} = Enum.split(a, -1)
    {bf, [bt]} = Enum.split(b, -1)
    field = fn f -> if tag == :row, do: List.to_tuple(f), else: {f, :Unit} end
    am = Map.new(af, field)
    bm = Map.new(bf, field)
    label = fn f -> if tag == :row, do: hd(f), else: f end
    splice = fn fields, tail -> if tag == :row, do: splice_row(fields, tail), else: splice_eff(fields, tail) end
    state = Enum.reduce(Enum.filter(Map.keys(am), &Map.has_key?(bm, &1)), {s, next}, fn k, {s, next} ->
      go(zonk(am[k], s), zonk(bm[k], s), {s, next})
    end)
    {s, next} = state
    only_a = Enum.reject(af, &Map.has_key?(bm, label.(&1)))
    only_b = Enum.reject(bf, &Map.has_key?(am, label.(&1)))
    at = zonk(at, s)
    bt = zonk(bt, s)
    case {only_a, only_b, at, bt} do
      {[], [], _, _} -> go(at, bt, state)
      {_, _, tail, tail} -> throw(:mismatch)
      {_, _, {:meta, _}, {:meta, _}} ->
        tail = {:meta, next}
        state = go(at, splice.(only_b, tail), {s, next + 1})
        {s, next} = state
        go(zonk(bt, s), zonk(splice.(only_a, tail), s), {s, next})
      {[], _, {:meta, _}, _} -> go(at, splice.(only_b, bt), state)
      {_, [], _, {:meta, _}} -> go(bt, splice.(only_a, at), state)
      _ -> throw(:mismatch)
    end
  end

  defp go(as, bs, state) when is_list(as) and is_list(bs) and length(as) == length(bs) do
    Enum.zip(as, bs) |> Enum.reduce(state, fn {a, b}, {s, next} -> go(zonk(a, s), zonk(b, s), {s, next}) end)
  end

  defp go(_a, _b, _s), do: throw(:mismatch)

  defp bind(n, t, {s, next}) do
    if occurs?(n, t), do: throw(:mismatch), else: {Map.put(s, n, t), next}
  end

  defp occurs?(n, {:meta, n}), do: true
  defp occurs?(n, list) when is_list(list), do: Enum.any?(list, &occurs?(n, &1))
  defp occurs?(_n, _t), do: false

  # --- Formatting ----------------------------------------------------------------------

  @doc "A type as a message shows it, in Vaisto's spelling."
  @spec format(t()) :: String.t()
  def format(:Int), do: "Int"
  def format(:Float), do: "Float"
  def format(:String), do: "String"
  def format(:Atom), do: "Atom"
  def format(:Unit), do: "Unit"
  def format(:Dyn), do: "Dyn"
  def format({:meta, _}), do: "_"
  def format([:tvar, a]), do: Atom.to_string(a)
  def format([:Bool]), do: "Bool"
  def format([:->, ps, eff, r]) do
    suffix = case eff do
      [:eff, :closed] -> ""
      [:eff, [:evar, e]] -> " :effects :#{e}"
      [:eff | labels] ->
        {labels, [tail]} = Enum.split(labels, -1)
        labels = "[" <> Enum.map_join(labels, " ", &Atom.to_string/1) <> "]"
        case tail do
          :closed -> " :effects " <> labels
          [:evar, e] -> " :effects (Effects " <> labels <> " :#{e})"
          {:meta, _} -> " :effects (Effects " <> labels <> " _)"
        end
      {:meta, _} -> " :effects _"
    end
    "(Fn " <> Enum.map_join(ps ++ [r], " ", &format/1) <> suffix <> ")"
  end
  def format([:forall, _binders, t]), do: format(t)
  def format([t]) when is_atom(t), do: Atom.to_string(t)
  def format([t | args]) when is_atom(t), do: "(#{t} " <> Enum.map_join(args, " ", &format/1) <> ")"
  def format(t), do: inspect(t)
end
