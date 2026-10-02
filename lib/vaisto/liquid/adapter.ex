defmodule Vaisto.Liquid.Adapter do
  @moduledoc """
  The Phase 0 adapter (RFC §9.3): today's typed AST to Liquid Core.

  It is temporary and untrusted. Core Lint checks what it produces, and the
  differential harness checks that the result runs as the backends do.
  Where the typed AST lacks information, as with `:any` or a type variable HM
  never resolved, the adapter writes `Dyn`, so Lint reports the places HM
  left untyped: the list of what Phase 1a has to fix. It is deleted when HM
  elaborates to Core directly.

  `module/2` returns `{:ok, core_module, skipped}`. `skipped` lists each
  definition left out because it uses something outside the pure fragment
  of Core version 0, with the reason, and each definition left out because
  it calls one of those.

  How the typed AST maps to Core:

    * a definition's free type variables become a `forall`. A call recovers
      the instantiation by matching the callee's type against the argument
      and result types the typed AST records. That is one-way matching, not
      inference;
    * a sum or record type recovers its type arguments the same way, against
      its declaration. A type variable nothing determines becomes `Dyn`;
    * every binder is renamed apart (liquid-core.md §10.12);
    * a `match` gets a final clause that crashes with `no_match` unless its
      last clause already matches anything, which is what a failed match does
      today (§6.5).
  """

  @type skipped :: [{atom(), String.t()}]

  # Builtins that are Core primitives over Int, and their Float versions.
  @int_ops %{:+ => :add, :- => :sub, :* => :mul}
  @float_ops %{:+ => :fadd, :- => :fsub, :* => :fmul}
  @cmp_ops %{:< => {:lt, :flt}, :> => {:gt, :fgt}, :<= => {:le, :fle}, :>= => {:ge, :fge}}

  defstruct decls: %{}, defs: %{}, delta: MapSet.new(), env: %{}

  @doc "Translate a typed module (or a single typed form) into a Core module named `name`."
  @spec module(term(), atom()) :: {:ok, Vaisto.Liquid.Canonical.tree(), skipped()}
  def module(typed, name \\ :Main) do
    forms =
      case typed do
        {:module, forms} -> forms
        form -> [form]
      end

    Process.put(:vaisto_liquid_adapter_prelude, MapSet.new())
    decls = for {:deftype, t, _def, type} <- forms, into: %{}, do: {t, declaration(type)}
    defs = for form <- forms, definition?(form), into: %{}, do: {elem(form, 1), scheme(elem(form, 4))}
    ctx = %__MODULE__{decls: decls, defs: defs}

    types = for {:deftype, t, _def, _type} <- forms, do: core_decl(ctx, t)

    {items, skipped} =
      for form <- forms, definition?(form), reduce: {[], []} do
        {items, skipped} ->
          try do
            {items ++ [definition(ctx, form)], skipped}
          catch
            {:unsupported, what} -> {items, skipped ++ [{elem(form, 1), what}]}
          end
      end

    {items, skipped} = drop_dependents(items, skipped)
    {:ok, [:module, name, [:"core-version", 0] | types ++ prelude(Map.keys(ctx.defs)) ++ items], skipped}
  end

  # `defn` (5 elements, or 6 with a guard) and `defn_multi`.
  defp definition?(form) when is_tuple(form) and tuple_size(form) in [5, 6], do: elem(form, 0) in [:defn, :defn_multi]
  defp definition?(_form), do: false

  # The builtins the program used, written in Core (Vaisto.Liquid.Prelude).
  defp prelude(avoid) do
    Vaisto.Liquid.Prelude.defs(Process.get(:vaisto_liquid_adapter_prelude, MapSet.new()), avoid)
  end

  defp use_prelude(name) do
    Process.put(:vaisto_liquid_adapter_prelude, MapSet.put(Process.get(:vaisto_liquid_adapter_prelude, MapSet.new()), name))
    name
  end

  # A definition that calls a skipped one is skipped too, until nothing changes.
  defp drop_dependents(items, skipped) do
    gone = MapSet.new(skipped, &elem(&1, 0))

    {keep, drop} =
      Enum.split_with(items, fn [:def, _f, _type, body] -> MapSet.disjoint?(gone, MapSet.new(symbols(body))) end)

    case drop do
      [] -> {keep, skipped}
      _ -> drop_dependents(keep, skipped ++ for([:def, f, _, _] <- drop, do: {f, "calls a definition outside the fragment"}))
    end
  end

  defp symbols(t) when is_atom(t), do: [t]
  defp symbols(t) when is_list(t), do: Enum.flat_map(t, &symbols/1)
  defp symbols(_t), do: []

  # --- Declarations ----------------------------------------------------------------

  # {kind, parameter tvar ids in order of first appearance, members with HM types}
  defp declaration({:sum, _t, ctors}) do
    {:sum, ctors |> Enum.flat_map(fn {_c, fs} -> fs end) |> tvars(), ctors}
  end

  defp declaration({:record, _t, fields}) do
    {:record, fields |> Enum.map(&elem(&1, 1)) |> tvars(), fields}
  end

  defp core_decl(ctx, t) do
    {kind, params, members} = Map.fetch!(ctx.decls, t)
    ctx = %{ctx | delta: MapSet.new(params)}
    names = Enum.map(params, &tv/1)

    body =
      case kind do
        :sum -> [:sum | for({c, fs} <- members, do: [c | Enum.map(fs, &ty(ctx, &1))])]
        :record -> [:record | for({l, t} <- members, do: [l, ty(ctx, t)])]
      end

    [:type, t, names, body]
  end

  # --- Definitions -------------------------------------------------------------------

  # A definition is polymorphic in the type variables of its parameters. One
  # that appears only in the result is unconstrained, not polymorphic: HM gives
  # the subterms that produce it fresh variables it never unifies, so it is
  # read as `Dyn` there and here alike.
  defp scheme({:fn, ptypes, _ret} = type), do: {tvars(ptypes), type}
  defp scheme(type), do: {[], type}

  defp definition(ctx, {:defn, f, params, body, {:fn, ptypes, _ret} = type}) do
    {ids, _} = scheme(type)
    ctx = %{ctx | delta: MapSet.new(ids)}
    start_names(ctx)
    {ctx, core_params} = bind_params(ctx, params, ptypes)
    [:def, f, generalize(ids, ty(ctx, type)), [:fn, core_params, tm(ctx, body)]]
  end

  # A guard that fails, or crashes, makes the call fail as no clause matching
  # does (function_clause today): a match on unit whose one clause is guarded.
  defp definition(ctx, {:defn, f, params, body, {:fn, ptypes, _ret} = type, guard}) do
    {ids, _} = scheme(type)
    ctx = %{ctx | delta: MapSet.new(ids)}
    start_names(ctx)
    {ctx, core_params} = bind_params(ctx, params, ptypes)
    guarded = [:match, [:unit], [:_, [:when, tm(ctx, guard)], tm(ctx, body)], [:_, [:perform, :crash, [:inj, [:Reason], :no_match]]]]
    [:def, f, generalize(ids, ty(ctx, type)), [:fn, core_params, guarded]]
  end

  defp definition(ctx, {:defn_multi, f, 1, clauses, {:fn, [ptype], _ret} = type}) do
    {ids, _} = scheme(type)
    ctx = %{ctx | delta: MapSet.new(ids)}
    start_names(ctx)
    arg = fresh(:arg)
    scrutinee_ctx = %{ctx | env: Map.put(ctx.env, arg, {arg, nil})}
    [:def, f, generalize(ids, ty(ctx, type)), [:fn, [[arg, ty(ctx, ptype)]], match_term(scrutinee_ctx, arg, ptype, clauses)]]
  end

  defp definition(_ctx, {:defn_multi, _f, arity, _clauses, _type}),
    do: throw({:unsupported, "a multi-clause function of #{arity} parameters"})

  defp generalize([], core_type), do: core_type
  defp generalize(ids, core_type), do: [:forall, Enum.map(ids, &[tv(&1), :Type]), core_type]

  defp bind_params(ctx, params, ptypes) do
    Enum.zip(params, ptypes)
    |> Enum.reduce({ctx, []}, fn {p, t}, {ctx, acc} ->
      # Lambdas from `for` name their parameters {:var, x, type}.
      p = with {:var, name, _} <- p, do: name
      x = fresh(p)
      {%{ctx | env: Map.put(ctx.env, p, {x, nil})}, acc ++ [[x, ty(ctx, t)]]}
    end)
  end

  # --- Types ---------------------------------------------------------------------------

  defp ty(_ctx, :int), do: :Int
  defp ty(_ctx, :float), do: :Float
  defp ty(_ctx, :string), do: :String
  defp ty(_ctx, :bool), do: [:Bool]
  defp ty(_ctx, :atom), do: :Atom
  defp ty(_ctx, {:atom, _}), do: :Atom
  defp ty(_ctx, :unit), do: :Unit
  defp ty(_ctx, :any), do: :Dyn
  defp ty(_ctx, :num), do: :Dyn
  defp ty(ctx, {:tvar, n}), do: if(MapSet.member?(ctx.delta, n), do: [:tvar, tv(n)], else: :Dyn)
  defp ty(ctx, {:list, t}), do: [:List, ty(ctx, t)]
  defp ty(_ctx, {:tuple, []}), do: :Unit
  defp ty(ctx, {:tuple, ts}), do: [:Tuple | Enum.map(ts, &ty(ctx, &1))]
  defp ty(ctx, {:fn, ps, r}), do: [:->, Enum.map(ps, &ty(ctx, &1)), [:eff, :closed], ty(ctx, r)]
  defp ty(ctx, {:record, t, _fields} = type), do: [t | type_args(ctx, t, type)]
  defp ty(ctx, {:sum, t, _ctors} = type), do: [t | type_args(ctx, t, type)]

  # D1: a parameter annotated with a declared type's name reaches the typed AST as that name.
  defp ty(ctx, t) when is_atom(t) and is_map_key(ctx.decls, t) do
    {_kind, params, _} = Map.fetch!(ctx.decls, t)
    [t | List.duplicate(:Dyn, length(params))]
  end

  defp ty(_ctx, t), do: throw({:unsupported, "the type #{inspect(t, limit: 6)}"})

  # The arguments of a named type, matched from an instance against its declaration.
  defp type_args(ctx, t, instance) do
    case Map.fetch(ctx.decls, t) do
      {:ok, {kind, params, members}} ->
        bound = hm_match({kind, t, members}, instance, %{})
        Enum.map(params, fn p -> if Map.has_key?(bound, p), do: ty(ctx, Map.fetch!(bound, p)), else: :Dyn end)

      :error ->
        throw({:unsupported, "the undeclared type #{t}"})
    end
  end

  # One-way matching of an HM type with type variables against an instance.
  defp hm_match({:tvar, n}, actual, acc), do: Map.put_new(acc, n, actual)
  defp hm_match({:list, a}, {:list, b}, acc), do: hm_match(a, b, acc)
  defp hm_match({:tuple, as}, {:tuple, bs}, acc) when length(as) == length(bs), do: hm_match_all(as, bs, acc)

  defp hm_match({:fn, ps, r}, {:fn, qs, s}, acc) when length(ps) == length(qs),
    do: hm_match(r, s, hm_match_all(ps, qs, acc))

  defp hm_match({:sum, t, cs}, {:sum, t, ds}, acc) do
    Enum.reduce(cs, acc, fn {c, fs}, acc ->
      case List.keyfind(ds, c, 0) do
        {^c, gs} when length(gs) == length(fs) -> hm_match_all(fs, gs, acc)
        _ -> acc
      end
    end)
  end

  defp hm_match({:record, t, fs}, {:record, t, gs}, acc) do
    Enum.reduce(fs, acc, fn {l, f}, acc ->
      case List.keyfind(gs, l, 0) do
        {^l, g} -> hm_match(f, g, acc)
        nil -> acc
      end
    end)
  end

  defp hm_match(_pattern, _actual, acc), do: acc

  defp hm_match_all(as, bs, acc), do: Enum.zip(as, bs) |> Enum.reduce(acc, fn {a, b}, acc -> hm_match(a, b, acc) end)

  # Type variable ids in order of first appearance.
  defp tvars(t), do: t |> collect_tvars() |> Enum.uniq()

  defp collect_tvars({:tvar, n}), do: [n]
  defp collect_tvars(t) when is_tuple(t), do: t |> Tuple.to_list() |> Enum.flat_map(&collect_tvars/1)
  defp collect_tvars(t) when is_list(t), do: Enum.flat_map(t, &collect_tvars/1)
  defp collect_tvars(_t), do: []

  defp tv(n), do: :"t#{n}"

  # --- Terms ------------------------------------------------------------------------------

  defp tm(_ctx, {:lit, :int, n}), do: n
  defp tm(_ctx, {:lit, :float, r}), do: r
  defp tm(_ctx, {:lit, :string, s}), do: s
  defp tm(_ctx, {:lit, :bool, b}), do: [:inj, [:Bool], b]
  defp tm(_ctx, {:lit, :atom, a}), do: [:atom, a]
  defp tm(_ctx, {:lit, :unit, _}), do: [:unit]
  defp tm(_ctx, n) when is_integer(n) or is_float(n), do: n

  defp tm(ctx, {:var, x, type}), do: reference(ctx, x, type)
  defp tm(ctx, {:fn_ref, f, _arity, type}), do: reference(ctx, f, type)

  defp tm(ctx, {:if, c, a, b, _type}), do: [:if, tm(ctx, c), tm(ctx, a), tm(ctx, b)]
  defp tm(_ctx, {:do, [], _type}), do: [:unit]
  defp tm(ctx, {:do, [e], _type}), do: tm(ctx, e)
  defp tm(ctx, {:do, [e | rest], type}), do: [:let, [[:_, ty(ctx, hm_type(e)), tm(ctx, e)]], tm(ctx, {:do, rest, type})]

  defp tm(ctx, {:let, bindings, body, _type}), do: let_term(ctx, bindings, body)

  defp tm(ctx, {:fn, params, body, {:fn, ptypes, _ret}}) do
    {ctx, core_params} = bind_params(ctx, params, ptypes)
    [:fn, core_params, tm(ctx, body)]
  end

  defp tm(ctx, {:apply, f, args, _type}), do: [:app, tm(ctx, f) | Enum.map(args, &tm(ctx, &1))]
  defp tm(ctx, {:tuple, [], _type}), do: tm(ctx, {:lit, :unit, nil})
  defp tm(ctx, {:tuple, es, _type}), do: [:tuple | Enum.map(es, &tm(ctx, &1))]

  defp tm(ctx, {:list, es, type}) do
    list_type = list_type(ctx, type, es)
    List.foldr(es, [:inj, list_type, :Nil], fn e, acc -> [:inj, list_type, :Cons, tm(ctx, e), acc] end)
  end

  defp tm(ctx, {:cons, h, t, type}), do: [:inj, list_type(ctx, type, [h]), :Cons, tm(ctx, h), tm(ctx, t)]
  defp tm(ctx, {:field_access, e, field, _type}), do: [:select, tm(ctx, e), field]
  defp tm(ctx, {:field_access, e, field, _ftype, _rtype}), do: [:select, tm(ctx, e), field]
  defp tm(ctx, {:match, e, clauses, _type}), do: match_term(ctx, tm(ctx, e), hm_type(e), clauses)

  defp tm(ctx, {:try, body, [{:error, {:var, e, _}, handler, _}], nil, _type}) do
    x = fresh(e)
    [:"handle-crash", tm(ctx, body), [[x, [:Reason]], tm(%{ctx | env: Map.put(ctx.env, e, {x, nil})}, handler)]]
  end

  defp tm(_ctx, {:try, _, _, _, _}), do: throw({:unsupported, "try with after, or catching a class other than :error"})
  defp tm(ctx, {:call, f, args, type}), do: call(ctx, f, args, type)
  defp tm(_ctx, node), do: throw({:unsupported, describe(node)})

  defp let_term(ctx, [], body), do: tm(ctx, body)

  # `(let [(Point x y) p] ...)` binds by a pattern: a match with one clause.
  defp let_term(ctx, [{pattern, e, _type} | rest], body) when not is_atom(pattern) do
    {core, binds} = pat(ctx, pattern, hm_type(e))
    rest_term = let_term(%{ctx | env: Map.merge(ctx.env, binds)}, rest, body)
    [:match, tm(ctx, e), [core, rest_term] | fallback(ctx, hm_type(e), [core])]
  end

  defp let_term(ctx, [{x, e, type} | rest], body) do
    free = type |> tvars() |> Enum.reject(&MapSet.member?(ctx.delta, &1))
    name = fresh(x)

    # A let-bound function with type variables of its own is polymorphic (§10.7).
    {core_type, rhs, scheme} =
      case {e, free} do
        {{:fn, _, _, _}, [_ | _]} ->
          inner = %{ctx | delta: MapSet.union(ctx.delta, MapSet.new(free))}
          {generalize(free, ty(inner, type)), tm(inner, e), {free, type}}

        _ ->
          {ty(ctx, type), tm(ctx, e), nil}
      end

    [:let, [[name, core_type, rhs]], let_term(%{ctx | env: Map.put(ctx.env, x, {name, scheme})}, rest, body)]
  end

  # A variable: a local, a let-bound polymorphic function, or a definition.
  defp reference(ctx, x, use_type) do
    case Map.fetch(ctx.env, x) do
      {:ok, {name, nil}} -> name
      {:ok, {name, {ids, type}}} -> instantiate(ctx, name, ids, type, use_type)
      :error ->
        case Map.fetch(ctx.defs, x) do
          {:ok, {ids, type}} -> instantiate(ctx, x, ids, type, use_type)
          :error -> throw({:unsupported, "the name #{x}, which is not a variable or a definition here"})
        end
    end
  end

  defp instantiate(_ctx, name, [], _type, _use_type), do: name

  defp instantiate(ctx, name, ids, type, use_type) do
    bound = hm_match(type, use_type, %{})
    [:inst, name | Enum.map(ids, fn id -> if Map.has_key?(bound, id), do: ty(ctx, Map.fetch!(bound, id)), else: :Dyn end)]
  end

  defp list_type(ctx, {:list, t}, _es), do: [:List, ty(ctx, t)]
  defp list_type(ctx, _type, [e | _]), do: [:List, ty(ctx, hm_type(e))]
  defp list_type(_ctx, _type, []), do: [:List, :Dyn]

  # --- Calls ------------------------------------------------------------------------------

  defp call(ctx, f, args, type) when is_atom(f) and is_map_key(ctx.env, f) do
    {:ok, {name, scheme}} = Map.fetch(ctx.env, f)
    fun_type = {:fn, Enum.map(args, &hm_type/1), type}
    head = if scheme, do: instantiate(ctx, name, elem(scheme, 0), elem(scheme, 1), fun_type), else: name
    [:app, head | Enum.map(args, &tm(ctx, &1))]
  end

  defp call(ctx, f, args, type) when is_map_key(@int_ops, f) and length(args) == 2, do: arithmetic(ctx, f, args, type)
  defp call(ctx, :/, [a, b], _type), do: [:prim, :fdiv, as_float(ctx, a), as_float(ctx, b)]
  defp call(ctx, :div, [a, b], _type), do: [:prim, :div, tm(ctx, a), tm(ctx, b)]
  defp call(ctx, :rem, [a, b], _type), do: [:prim, :rem, tm(ctx, a), tm(ctx, b)]

  defp call(ctx, :-, [a], _type) do
    if hm_type(a) == :float, do: [:prim, :fneg, tm(ctx, a)], else: [:prim, :neg, tm(ctx, a)]
  end

  defp call(ctx, f, [a, b], _type) when is_map_key(@cmp_ops, f) do
    {int_op, float_op} = Map.fetch!(@cmp_ops, f)

    case numeric_kind(a, b) do
      :int -> [:prim, int_op, tm(ctx, a), tm(ctx, b)]
      :float -> [:prim, float_op, as_float(ctx, a), as_float(ctx, b)]
      :other -> throw({:unsupported, "#{f} on #{inspect(hm_type(a))}, which compares in Erlang term order"})
    end
  end

  defp call(ctx, :==, [a, b], _type), do: [:prim, :eq, tm(ctx, a), tm(ctx, b)]
  defp call(ctx, :!=, [a, b], _type), do: [:prim, :ne, tm(ctx, a), tm(ctx, b)]
  defp call(ctx, :not, [a], _type), do: [:prim, :not, tm(ctx, a)]
  defp call(ctx, :and, [a, b], _type), do: [:if, tm(ctx, a), tm(ctx, b), [:inj, [:Bool], false]]
  defp call(ctx, :or, [a, b], _type), do: [:if, tm(ctx, a), [:inj, [:Bool], true], tm(ctx, b)]

  defp call(ctx, :++, [a, b], _type) do
    if hm_type(a) == :string and hm_type(b) == :string,
      do: [:prim, :concat, tm(ctx, a), tm(ctx, b)],
      else: throw({:unsupported, "++ on #{inspect(hm_type(a))}"})
  end

  defp call(ctx, prim, [xs], _type) when prim in [:head, :tail, :length, :empty?], do: [:prim, prim, tm(ctx, xs)]
  defp call(ctx, :cons, [h, t], type), do: [:inj, list_type(ctx, type, [h]), :Cons, tm(ctx, h), tm(ctx, t)]

  # The higher-order builtins call their prelude definition.
  defp call(ctx, hof, [f, xs], type) when hof in [:map, :filter, :flat_map] do
    a = list_elem(hm_type(xs))

    {insts, fparams, fresult} =
      case hof do
        :map -> {[a, list_elem(type)], [a], list_elem(type)}
        :filter -> {[a], [a], :bool}
        :flat_map -> {[a, list_elem(type)], [a], {:list, list_elem(type)}}
      end

    name = use_prelude(:"prelude.#{hof}")
    [:app, [:inst, name | Enum.map(insts, &ty(ctx, &1))], function_arg(ctx, f, fparams, fresult), tm(ctx, xs)]
  end

  defp call(ctx, :fold, [f, init, xs], type) do
    a = list_elem(hm_type(xs))
    name = use_prelude(:"prelude.fold")
    [:app, [:inst, name, ty(ctx, a), ty(ctx, type)], function_arg(ctx, f, [type, a], type), tm(ctx, init), tm(ctx, xs)]
  end

  defp call(_ctx, {:qualified, m, f}, _args, _type), do: throw({:unsupported, "the call #{m}:#{f} (externs need decode, Phase 1a)"})

  # A user definition, a constructor of the result's sum type, or the result's record type.
  defp call(ctx, f, args, type) when is_atom(f) do
    cond do
      Map.has_key?(ctx.defs, f) ->
        {ids, scheme_type} = Map.fetch!(ctx.defs, f)
        head = instantiate(ctx, f, ids, scheme_type, {:fn, Enum.map(args, &hm_type/1), type})
        [:app, head | Enum.map(args, &tm(ctx, &1))]

      match?({:sum, _, _}, type) and List.keymember?(elem(type, 2), f, 0) ->
        [:inj, ty(ctx, type), f | Enum.map(args, &tm(ctx, &1))]

      match?({:record, ^f, _}, type) ->
        [:record, ty(ctx, type) | Enum.zip_with(elem(type, 2), args, fn {l, _}, a -> [l, tm(ctx, a)] end)]

      true ->
        throw({:unsupported, "the builtin #{inspect(f)}"})
    end
  end

  defp call(_ctx, f, _args, _type), do: throw({:unsupported, "the call of #{inspect(f, limit: 4)}"})

  defp arithmetic(ctx, f, [a, b], _type) do
    case numeric_kind(a, b) do
      :float -> [:prim, Map.fetch!(@float_ops, f), as_float(ctx, a), as_float(ctx, b)]
      _ -> [:prim, Map.fetch!(@int_ops, f), tm(ctx, a), tm(ctx, b)]
    end
  end

  # Erlang promotes an Int operand to Float when the other is a Float.
  defp numeric_kind(a, b) do
    case {hm_type(a), hm_type(b)} do
      {:int, :int} -> :int
      {x, y} when x == :float and y in [:int, :float] -> :float
      {x, y} when y == :float and x in [:int, :float] -> :float
      {x, y} when x in [:int, :any, :num] or y in [:int, :any, :num] -> :int
      {{:tvar, _}, _} -> :int
      {_, {:tvar, _}} -> :int
      _ -> :other
    end
  end

  # The function given to a higher-order builtin: a lambda as it is, and a
  # definition, local or operator named by an atom wrapped in one, so that it
  # goes through the ordinary call rules.
  defp function_arg(ctx, f, params, result) when is_atom(f) do
    names = for i <- 1..length(params), do: :"p#{i}"
    body = {:call, f, Enum.zip_with(names, params, &{:var, &1, &2}), result}
    tm(ctx, {:fn, names, body, {:fn, params, result}})
  end

  defp function_arg(ctx, f, _params, _result), do: tm(ctx, f)

  defp as_float(ctx, e), do: if(hm_type(e) == :int, do: [:prim, :"int-to-float", tm(ctx, e)], else: tm(ctx, e))

  # --- Matching ---------------------------------------------------------------------------

  defp match_term(ctx, scrutinee, stype, clauses) do
    core_clauses =
      for clause <- clauses do
        {pattern, guard, body} =
          case clause do
            {p, b, _type} -> {p, nil, b}
            {p, g, b, _type} -> {p, g, b}
          end

        {core_pattern, binds} = pat(ctx, pattern, stype)
        cctx = %{ctx | env: Map.merge(ctx.env, binds)}

        case guard do
          nil -> [core_pattern, tm(cctx, body)]
          g -> [core_pattern, [:when, tm(cctx, g)], tm(cctx, body)]
        end
      end

    [:match, scrutinee | core_clauses ++ fallback(ctx, stype, for([p, _body] <- core_clauses, do: p))]
  end

  # A final clause that crashes with no_match, as a failed match does today,
  # unless the unguarded clauses already cover the type: then it could never
  # be chosen, and Core Lint rejects a clause that can never be chosen.
  defp fallback(ctx, stype, unguarded) do
    covered? =
      try do
        Vaisto.Liquid.Lint.exhaustive?(unguarded, ty(ctx, stype), types: for(t <- Map.keys(ctx.decls), do: core_decl(ctx, t)))
      catch
        {:unsupported, _} -> false
      end

    if covered?, do: [], else: [[:_, [:perform, :crash, [:inj, [:Reason], :no_match]]]]
  end

  # A pattern, typed against the scrutinee's HM type, and the names it binds.
  defp pat(_ctx, :_, _stype), do: {:_, %{}}

  defp pat(_ctx, {:var, x, _type}, _stype) do
    name = fresh(x)
    {name, %{x => {name, nil}}}
  end

  defp pat(_ctx, n, _stype) when is_integer(n) or is_float(n) or is_binary(n), do: {n, %{}}
  defp pat(_ctx, {:lit, :bool, b}, _stype), do: {[:inj, [:Bool], b], %{}}
  defp pat(_ctx, {:lit, :atom, a}, _stype), do: {[:atom, a], %{}}
  defp pat(_ctx, {:lit, :unit, _}, _stype), do: {[:unit], %{}}
  defp pat(_ctx, {:lit, _kind, v}, _stype), do: {v, %{}}
  defp pat(_ctx, b, _stype) when is_boolean(b), do: {[:inj, [:Bool], b], %{}}

  defp pat(ctx, {:tuple_pattern, ps, _type}, stype) do
    types = case stype do {:tuple, ts} when length(ts) == length(ps) -> ts; _ -> List.duplicate(:any, length(ps)) end
    {core, binds} = pats(ctx, ps, types)
    {[:tuple | core], binds}
  end

  defp pat(ctx, {:pattern, c, ps, type}, stype) do
    case pattern_type(ctx, c, type, stype) do
      {:sum, _t, ctors} = sum ->
        {^c, field_types} = List.keyfind(ctors, c, 0)
        {core, binds} = pats(ctx, ps, field_types)
        {[:inj, ty(ctx, sum), c | core], binds}

      {:record, ^c, fields} = record ->
        {core, binds} = pats(ctx, ps, Enum.map(fields, &elem(&1, 1)))
        {[:record, ty(ctx, record) | Enum.zip_with(fields, core, fn {l, _}, p -> [l, p] end)], binds}

      nil ->
        throw({:unsupported, "the pattern #{c}, whose type is not known"})
    end
  end

  defp pat(ctx, {:as_pattern, {:var, x, _}, p, _type}, stype) do
    name = fresh(x)
    {core, binds} = pat(ctx, p, stype)
    {[:as, name, core], Map.put(binds, x, {name, nil})}
  end

  # Multi-clause functions write list patterns in the shape of expressions.
  defp pat(ctx, {:list, ps, type}, stype), do: pat(ctx, {:list_pattern, ps, type}, stype)
  defp pat(ctx, {:cons, h, t, type}, stype), do: pat(ctx, {:cons_pattern, h, t, type}, stype)

  defp pat(ctx, {:list_pattern, ps, _type}, stype) do
    elem_type = list_elem(stype)
    list_type = [:List, ty(ctx, elem_type)]
    {core, binds} = pats(ctx, ps, List.duplicate(elem_type, length(ps)))
    {List.foldr(core, [:inj, list_type, :Nil], fn p, acc -> [:inj, list_type, :Cons, p, acc] end), binds}
  end

  defp pat(ctx, {:cons_pattern, h, t, _type}, stype) do
    elem_type = list_elem(stype)
    {[hp, tp], binds} = pats(ctx, [h, t], [elem_type, {:list, elem_type}])
    {[:inj, [:List, ty(ctx, elem_type)], :Cons, hp, tp], binds}
  end

  defp pat(_ctx, p, _stype), do: throw({:unsupported, "the pattern #{inspect(p, limit: 6)}"})

  defp pats(ctx, ps, types) do
    Enum.zip(ps, types)
    |> Enum.reduce({[], %{}}, fn {p, t}, {acc, binds} ->
      {core, more} = pat(ctx, p, t)
      {acc ++ [core], Map.merge(binds, more)}
    end)
  end

  defp list_elem({:list, t}), do: t
  defp list_elem(_), do: :any

  # A constructor pattern's type: its own, the scrutinee's, or, when a nested
  # pattern is typed `:any`, the declaration the constructor belongs to.
  defp pattern_type(ctx, c, type, stype) do
    Enum.find([type, stype], &owns?(&1, c)) ||
      Enum.find_value(ctx.decls, fn
        {t, {:sum, _params, ctors}} -> if List.keymember?(ctors, c, 0), do: {:sum, t, ctors}
        {^c, {:record, _params, fields}} -> {:record, c, fields}
        _ -> nil
      end)
  end

  defp owns?({:sum, _t, ctors}, c), do: List.keymember?(ctors, c, 0)
  defp owns?({:record, c, _fields}, c), do: true
  defp owns?(_type, _c), do: false

  # --- Helpers ----------------------------------------------------------------------------

  # The HM type the typed AST records for a node.
  defp hm_type({:lit, :bool, _}), do: :bool
  defp hm_type({:lit, kind, _}), do: kind
  defp hm_type({:field_access, _e, _field, field_type, _record_type}), do: field_type
  defp hm_type(n) when is_integer(n), do: :int
  defp hm_type(r) when is_float(r), do: :float
  defp hm_type(node) when is_tuple(node), do: elem(node, tuple_size(node) - 1)
  defp hm_type(_node), do: :any

  defp describe(node) when is_tuple(node), do: "the form #{inspect(elem(node, 0))}"
  defp describe(node), do: "#{inspect(node, limit: 4)}"

  # Binder names are unique within a definition (liquid-core.md §10.12).
  defp start_names(ctx), do: Process.put(:vaisto_liquid_adapter_names, MapSet.new(Map.keys(ctx.defs)))

  defp fresh(x) do
    used = Process.get(:vaisto_liquid_adapter_names, MapSet.new())

    name =
      if MapSet.member?(used, x),
        do: Stream.iterate(2, &(&1 + 1)) |> Stream.map(&:"#{x}'#{&1}") |> Enum.find(&(not MapSet.member?(used, &1))),
        else: x

    Process.put(:vaisto_liquid_adapter_names, MapSet.put(used, name))
    name
  end
end
