defmodule Vaisto.Refine.VCGen do
  @moduledoc """
  Verification-condition generation over Liquid Core (RFC §4.4, §10, §11.3;
  mechanisms M7, M8 and M10). It is part of the trusted base.

  It walks every definition of a Core module, in evaluation order, keeping
  the values it knows as IR terms and the facts that hold on the current path.
  Obligations arise:

    * at every call, anywhere, to a function with refined parameters;
    * where a refined function returns (its result refinement);
    * at `div`, `rem`, `head` and `tail` inside refined functions (§4.3);
    * where a function with refined parameters is used as a value;

  and, per refined function, satisfiability questions: its requirements can be
  met, its guarantee can hold, and no obligation is proved only because the
  facts at it contradict each other (§11.3).

  Facts are scoped to the branch that produced them (§4.4): leaving an `if`
  keeps only the guarded join of what each branch learned. A guard adds
  `def(g) ∧ g`, and a later clause learns only the negation of an earlier
  clause's *test* (§10.3); a test it cannot express over the scrutinee is
  dropped whole, never in part, since dropping part of a negated fact would
  strengthen it (§11.2).
  """

  alias Vaisto.Refine.{IR, Predicate, Render, Surface, VC}

  # Names in the logic. A Core binder keeps its own name. Fresh names and a
  # signature's placeholders contain a space, which no Vaisto name can, so a
  # parameter called `result` or `#2` can neither capture a placeholder nor
  # collide with a fresh name.
  @result :"the result"
  defp param(i), do: :"param #{i}"
  defp fresh_name(n), do: :"fresh #{n}"

  defmodule LogicSig do
    @moduledoc false
    # A refined signature in the logic. Predicates mention the placeholders
    # {:var, param(i), sort} and {:var, @result, sort}. A conjunct is
    # {surface_predicate, ir_predicate, binder_names}.
    defstruct [:name, :params, :result_sort, :result, :surface]
  end

  @doc """
  Build the refined signatures in the logic from the surface signatures and
  the Core definitions they belong to. Returns `{:ok, sigs}` or
  `{:error, problems}`, a problem being `{def_name, message}`.
  """
  @spec logic_sigs(%{atom() => Surface.Sig.t()}, %{atom() => term()}) ::
          {:ok, %{atom() => %LogicSig{}}} | {:error, [{atom(), String.t()}]}
  def logic_sigs(surface_sigs, core_types) do
    results =
      for {name, sig} <- Enum.sort(surface_sigs) do
        case Map.fetch(core_types, name) do
          {:ok, type} -> logic_sig(sig, type)
          :error -> {:error, {name, "`#{name}` has refinements but no definition the checker can read"}}
        end
      end

    case for({:error, problem} <- results, do: problem) do
      [] -> {:ok, Map.new(for {:ok, sig} <- results, do: {sig.name, sig})}
      problems -> {:error, problems}
    end
  end

  defp logic_sig(%Surface.Sig{name: name, params: params, result: {_rbase, rref}} = sig, core_type) do
    {param_types, result_type} = arrow(core_type)

    if length(param_types) != length(params) do
      {:error, {name, "`#{name}` has #{length(params)} parameters but its definition has #{length(param_types)}"}}
    else
      # Earlier parameters are in scope in later refinements and in the result.
      {logic_params, scope} =
        params
        |> Enum.zip(param_types)
        |> Enum.with_index()
        |> Enum.map_reduce(%{}, fn {{{pname, base, ref}, ptype}, i}, scope ->
          sort = sort_of(ptype)
          scope = if sort, do: Map.put(scope, pname, {{:var, param(i), sort}, sort}), else: scope
          conjuncts = conjuncts(name, pname, base, ref, scope, sort, param(i))
          {%{name: pname, sort: sort, conjuncts: conjuncts}, scope}
        end)

      result_sort = sort_of(result_type)
      result = conjuncts(name, :result, elem(sig.result, 0), rref, scope, result_sort, @result)

      problems = for p <- [result | Enum.map(logic_params, & &1.conjuncts)], match?({:error, _}, p), do: elem(p, 1)

      case problems do
        [] -> {:ok, %LogicSig{name: name, params: logic_params, result_sort: result_sort, result: result, surface: sig}}
        [message | _] -> {:error, {name, message}}
      end
    end
  end

  defp conjuncts(_f, _who, _base, nil, _scope, _sort, _placeholder), do: []

  defp conjuncts(f, who, base, {binder, pred}, scope, sort, placeholder) do
    with {:ok, base_sort} <- Predicate.sort_of_base(base),
         true <- base_sort == sort || {:error, "the refinement on `#{who}` in `#{f}` does not match its type"} do
      scope = Map.put(scope, binder, {{:var, placeholder, sort}, sort})

      Enum.reduce_while(Predicate.conjuncts(pred), [], fn conjunct, acc ->
        case Predicate.translate(conjunct, scope) do
          {:ok, ir} -> {:cont, acc ++ [{conjunct, ir, binder}]}
          {:error, message} -> {:halt, {:error, "in `#{f}`: #{message}"}}
        end
      end)
    end
    |> case do
      {:error, message} -> {:error, if(String.starts_with?(message, "in `"), do: message, else: "in `#{f}`: #{message}")}
      list -> list
    end
  end

  @doc """
  Generate every verification condition of a module. Returns the VCs, each
  with `meta` describing it for diagnostics, and the gate problems found on
  the way (constructs the bridge does not admit, §21.1).
  """
  @spec generate(term(), %{atom() => %LogicSig{}}, MapSet.t()) :: {[VC.t()], [{atom(), String.t()}]}
  def generate([:module, _name, [:"core-version", 0] | items], sigs, involved) do
    decls = for [:type | _] = d <- items, do: d
    defs = for [:def, f, type, _fn] <- items, into: %{}, do: {f, type}
    st = %{n: 0, vcs: [], gate: [], decls: decls, defs: defs, sigs: sigs}

    st =
      Enum.reduce(items, st, fn
        [:def, f, type, [:fn, params, body]], st -> definition(f, type, params, body, MapSet.member?(involved, f), st)
        _decl, st -> st
      end)

    {Enum.reverse(st.vcs), Enum.reverse(st.gate)}
  end

  # --- Definitions -------------------------------------------------------------------

  defp definition(f, _type, params, body, involved?, st) do
    sig = Map.get(st.sigs, f)
    {binds, st} = bind_params(params, st)
    env = %{def: f, refined?: sig != nil, involved?: involved?, binds: binds, facts: []}

    case sig do
      nil ->
        {_v, _env, st} = synth(body, env, st)
        st

      sig ->
        param_terms = for {[x, _], i} <- Enum.with_index(params), into: %{}, do: {param(i), term_of(binds[x])}
        requires = for p <- sig.params, {_s, ir, _b} <- p.conjuncts, do: fact(IR.subst(ir, param_terms))
        env = %{env | facts: Enum.reverse(requires)}
        st = vacuity(f, env, body, st)

        case sig.result do
          [] ->
            {_v, _env, st} = synth(body, env, st)
            st

          conjuncts ->
            goals = for {s, ir, b} <- conjuncts, do: {s, IR.subst(ir, param_terms), b}
            st = guarantee(f, sig, env, goals, st)
            check(body, env, st, goals)
        end
    end
  end

  defp bind_params(params, st) do
    Enum.map_reduce(params, st, fn [x, type], st ->
      case sort_of(type) do
        nil -> {{x, opaque(type)}, st}
        sort -> {{x, val({:var, x, sort}, sort, type)}, st}
      end
    end)
    |> then(fn {pairs, st} -> {Map.new(pairs), st} end)
  end

  # §11.3: requirements that can never be met (C7), counting a single-clause
  # guard (§10.4), and a guarantee that can never hold (C20).
  defp vacuity(f, env, body, st) do
    guard = guard_facts(body, env, st)
    emit(st, :vacuity, env.facts ++ guard, {:bool, false}, %{def: f, about: :requirements})
  end

  defp guarantee(f, sig, env, goals, st) do
    {r, st} = fresh(sig.result_sort, st)
    held = for {_s, ir, _b} <- goals, do: fact(IR.subst(ir, %{@result => r}))
    emit(st, :vacuity, env.facts ++ held, {:bool, false}, %{def: f, about: :guarantee})
  end

  defp guard_facts([:match, [:unit], [:_, [:when, g], _body] | _], env, _st) do
    case guard(g, env.binds) do
      {:ok, d, p} -> [fact(IR.and_([d, p]))]
      :unsupported -> []
    end
  end

  defp guard_facts(_body, _env, _st), do: []

  # --- Checking a body against the result refinement -----------------------------------

  # Checking is pushed into branches, so that an error names the branch (§4.4).
  # `within` is the `if` and branch a leaf sits in, for placing the diagnostic.
  defp check(e, env, st, goals, within \\ nil)

  defp check([:if, c, a, b] = e, env, st, goals, _within) do
    {cv, env, st} = synth(c, env, st)
    {phi, st} = bool_term(cv, st)
    st = check(a, add_fact(env, phi, branch_note(c, true)), st, goals, {e, 2})
    check(b, add_fact(env, IR.not_(phi), branch_note(c, false)), st, goals, {e, 3})
  end

  defp check([:let, [[x, type, e]], body], env, st, goals, within) do
    {v, env, st} = synth(e, env, st)

    case v do
      :bottom -> st
      v -> check(body, bind(env, x, retype(v, type)), st, goals, within)
    end
  end

  defp check([:match, s | clauses], env, st, goals, within) do
    {sv, env, st} = synth(s, env, st)

    case sv do
      :bottom ->
        st

      sv ->
        {_, st} =
          Enum.reduce(clauses, {[], st}, fn clause, {earlier, st} ->
            {clause_env, body, matched, st} = clause_env(clause, sv, earlier, env, st)
            {earlier ++ [matched], check(body, clause_env, st, goals, within)}
          end)

        st
    end
  end

  defp check([:perform, :crash | _], _env, st, _goals, _within), do: st

  defp check([:"handle-crash", e, [[x, _type], handler]], env, st, goals, within) do
    st = check(e, env, st, goals, within)
    check(handler, bind(env, x, opaque([:Reason])), st, goals, within)
  end

  defp check(e, env, st, goals, within) do
    {v, env, st} = synth(e, env, st)

    case v do
      :bottom ->
        st

      v ->
        {r, st} = logic_term(v, st, sort_of_goals(goals, v))

        Enum.reduce(goals, st, fn {surface, ir, binder}, st ->
          meta = %{def: env.def, about: :result, expr: e, within: within, conjunct: surface, subst: %{binder => "the result"}, notes: notes(env)}
          emit(st, :postcondition, env.facts, IR.subst(ir, %{@result => r}), meta)
        end)
    end
  end

  defp sort_of_goals([{_s, ir, _b} | _], v) do
    case IR.vars(ir) |> Enum.find(fn {name, _} -> name == @result end) do
      {@result, sort} -> sort
      nil -> v.s
    end
  end

  # --- Synthesis ---------------------------------------------------------------------
  # synth(e, env, st) -> {value | :bottom, env, st}; a value is %{t: ir | nil, s: sort | nil, ty: core type | nil}.
  # The env returned carries the facts that hold after e has been evaluated.

  defp synth(n, env, st) when is_integer(n), do: {val({:int, n}, :int, :Int), env, st}
  defp synth(f, env, st) when is_float(f), do: {opaque(:Float), env, st}
  defp synth(s, env, st) when is_binary(s), do: {opaque(:String), env, st}

  defp synth(x, env, st) when is_atom(x) do
    case Map.fetch(env.binds, x) do
      {:ok, v} -> {v, env, st}
      :error -> {opaque(Map.get(st.defs, x)), env, as_value(x, env, st)}
    end
  end

  defp synth([:atom, _], env, st), do: {opaque(:Atom), env, st}
  defp synth([:unit], env, st), do: {opaque(:Unit), env, st}
  defp synth([:inj, [:Bool], b], env, st) when is_boolean(b), do: {val({:bool, b}, :bool, [:Bool]), env, st}

  defp synth([:inj, [:List | _] = type, :Nil], env, st) do
    {v, st} = fresh(:list, st)
    {val(v, :list, type), add_fact(env, {:eq, {:len, v}, {:int, 0}}), st}
  end

  defp synth([:inj, [:List | _] = type, :Cons, h, t], env, st) do
    {_hv, env, st} = synth(h, env, st)
    {tv, env, st} = synth(t, env, st)

    case tv do
      :bottom ->
        {:bottom, env, st}

      tv ->
        {tt, st} = logic_term(tv, st, :list)
        {v, st} = fresh(:list, st)
        {val(v, :list, type), add_fact(env, {:eq, {:len, v}, {:arith, :add, [{:len, tt}, {:int, 1}]}}), st}
    end
  end

  defp synth([:inj, type, _ctor | fields], env, st), do: synth_all(fields, env, st, opaque(type))
  defp synth([:tuple | es], env, st), do: synth_all(es, env, st, opaque(nil))
  defp synth([:record, type | fields], env, st), do: synth_all(for([_l, e] <- fields, do: e), env, st, opaque(type))

  defp synth([:select, e, _label], env, st), do: synth_all([e], env, st, opaque(nil))
  defp synth([:prim, op | args], env, st), do: prim(op, args, env, st)
  defp synth([:app, f | args], env, st), do: call(f, args, env, st)
  defp synth([:inst, e | _types], env, st), do: synth(e, env, st)

  defp synth([:let, [[x, type, e]], body], env, st) do
    case synth(e, env, st) do
      {:bottom, env, st} -> {:bottom, env, st}
      {v, env, st} -> synth(body, bind(env, x, retype(v, type)), st)
    end
  end

  defp synth([:letrec, bindings, body], env, st) do
    env = Enum.reduce(bindings, env, fn [f, type, _fn], env -> bind(env, f, opaque(type)) end)
    st = Enum.reduce(bindings, st, fn [_f, _type, fun], st -> lambda(fun, env, st) end)
    synth(body, env, st)
  end

  defp synth([:fn, _params, _body] = fun, env, st), do: {opaque(nil), env, lambda(fun, env, st)}

  defp synth([:if, c, a, b], env, st) do
    {cv, env, st} = synth(c, env, st)

    case cv do
      :bottom ->
        {:bottom, env, st}

      cv ->
        {phi, st} = bool_term(cv, st)
        {va, env_a, st} = synth(a, add_fact(env, phi, branch_note(c, true)), st)
        {vb, env_b, st} = synth(b, add_fact(env, IR.not_(phi), branch_note(c, false)), st)
        arms = [%{sel: phi, v: va, facts: learned(env_a, env, 1)}, %{sel: IR.not_(phi), v: vb, facts: learned(env_b, env, 1)}]
        join_arms(arms, env, st)
    end
  end

  defp synth([:match, s | clauses], env, st) do
    case synth(s, env, st) do
      {:bottom, env, st} ->
        {:bottom, env, st}

      {sv, env, st} ->
        {{_matched, arms}, st} =
          Enum.reduce(clauses, {{[], []}, st}, fn clause, {{earlier, arms}, st} ->
            {clause_env, body, matched, st} = clause_env(clause, sv, earlier, env, st)
            {v, body_env, st} = synth(body, clause_env, st)
            arm = %{sel: selection(earlier, matched), v: v, facts: learned(clause_env, env, 0) ++ learned(body_env, clause_env, 0)}
            {{earlier ++ [matched], arms ++ [arm]}, st}
          end)

        join_arms(arms, env, st)
    end
  end

  defp synth([:perform, :crash | args], env, st) do
    {_v, env, st} = synth_all(args, env, st, opaque(nil))
    {:bottom, env, st}
  end

  defp synth([:perform, op | args], env, st) do
    case synth_all(args, env, st, opaque(nil)) do
      {:bottom, env, st} ->
        {:bottom, env, st}

      {_v, env, st} when op in [:now, :random] ->
        {v, st} = fresh(:int, st)
        {val(v, :int, :Int), env, st}

      {_v, env, st} ->
        {opaque(nil), env, gate(env, st, "the effect `#{op}`")}
    end
  end

  # No fact flows from the body into the handler, nor out of the try: the
  # crash may have happened anywhere in the body (§4.4). The result is a
  # fresh name, known only by its sort.
  defp synth([:"handle-crash", e, [[x, _type], handler]], env, st) do
    {ve, _env_e, st} = synth(e, env, st)
    {vh, _env_h, st} = synth(handler, bind(env, x, opaque([:Reason])), st)

    case Enum.reject([ve, vh], &(&1 == :bottom)) do
      [] -> {:bottom, env, st}
      [v | _] -> fresh_like(v, env, st)
    end
  end

  defp synth([op, _type, e], env, st) when op in [:up, :decode] do
    {_v, env, st} = synth(e, env, st)
    {opaque(nil), env, gate(env, st, "`#{op}`")}
  end

  defp synth(other, env, st), do: {opaque(nil), env, gate(env, st, "the Core form #{inspect(other, limit: 3)}")}

  defp synth_all(es, env, st, result) do
    Enum.reduce_while(es, {result, env, st}, fn e, {result, env, st} ->
      case synth(e, env, st) do
        {:bottom, env, st} -> {:halt, {:bottom, env, st}}
        {_v, env, st} -> {:cont, {result, env, st}}
      end
    end)
  end

  defp fresh_like(%{s: nil} = v, env, st), do: {v, env, st}

  defp fresh_like(v, env, st) do
    {t, st} = fresh(v.s, st)
    {val(t, v.s, v.ty), env, st}
  end

  # A lambda's body is checked where it is written, with its parameters
  # unrefined; what it learns never leaves it, since it may run later or never.
  defp lambda([:fn, params, body], env, st) do
    {binds, st} = bind_params(params, st)
    env = %{env | binds: Map.merge(env.binds, binds)}
    {_v, _env, st} = synth(body, env, st)
    st
  end

  # --- Joins ---------------------------------------------------------------------------

  # The guarded join of an `if` or a `match` in synthesis position (§4.4). An
  # arm is %{sel: its exact selection condition or nil, v: its value or
  # :bottom, facts: what holds when it is taken}.
  #
  # `sel ⇒ facts` is sound only when sel is sufficient for taking the arm, and
  # `¬sel` for a crashing arm only when sel is necessary too: so both need an
  # exact sel. Facts naming fresh binders go in the consequent, where a model
  # cannot falsify them to make the implication vacuous.
  defp join_arms(arms, env, st) do
    case Enum.reject(arms, &(&1.v == :bottom)) do
      [] ->
        {:bottom, env, st}

      [arm] ->
        # Only one way out, so it was taken: its facts hold afterwards.
        facts = if arm.sel, do: [arm.sel | arm.facts], else: arm.facts
        {arm.v, Enum.reduce(facts, env, &add_fact(&2, &1)), st}

      [first | _] = live ->
        sort = Enum.find_value(live, & &1.v.s)
        {r, st} = if sort, do: fresh(sort, st), else: {nil, st}

        guarded =
          for %{sel: sel, v: v, facts: facts} <- live, sel != nil do
            eq = if r && v.t && v.s == sort, do: [{:eq, r, v.t}], else: []
            IR.implies(sel, IR.and_(facts ++ eq))
          end

        dead = for %{sel: sel, v: :bottom} <- arms, sel != nil, do: IR.not_(sel)
        env = Enum.reduce(guarded ++ dead, env, &add_fact(&2, &1))
        {val(r, sort, first.v.ty), env, st}
    end
  end

  # A match arm is taken exactly when no earlier clause matched and its own
  # does. That is expressible only when every one of those match conditions
  # is; an arm with no exact selection condition guards nothing and is
  # negated by nothing (§10.3, §11.2).
  defp selection(earlier, matched) do
    if matched != nil and Enum.all?(earlier, &(&1 != nil)) do
      IR.and_(Enum.map(earlier, &IR.not_/1) ++ [matched])
    end
  end

  # The facts env gained since `base`, minus the first `skip` it was given on entry.
  defp learned(env, base, skip), do: env.facts |> Enum.take(length(env.facts) - length(base.facts) - skip) |> Enum.map(& &1.p) |> Enum.reverse()

  # --- Primitives (§4.3) ---------------------------------------------------------------

  defp prim(op, args, env, st) do
    case synth_values(args, env, st) do
      {:bottom, env, st} -> {:bottom, env, st}
      {vs, env, st} -> prim_value(op, vs, args, env, st)
    end
  end

  defp synth_values(args, env, st) do
    Enum.reduce_while(args, {[], env, st}, fn a, {vs, env, st} ->
      case synth(a, env, st) do
        {:bottom, env, st} -> {:halt, {:bottom, env, st}}
        {v, env, st} -> {:cont, {vs ++ [v], env, st}}
      end
    end)
  end

  @arith %{add: :add, sub: :sub, mul: :mul, div: :tdiv, rem: :trem}

  defp prim_value(op, [a, b], args, env, st) when is_map_key(@arith, op) do
    {ta, st} = logic_term(a, st, :int)
    {tb, st} = logic_term(b, st, :int)
    st = if op in [:div, :rem], do: safety(op, {:not, {:eq, tb, {:int, 0}}}, [:prim, op | args], env, st), else: st
    {val({:arith, Map.fetch!(@arith, op), [ta, tb]}, :int, :Int), env, st}
  end

  defp prim_value(:neg, [a], _args, env, st) do
    {ta, st} = logic_term(a, st, :int)
    {val({:arith, :neg, [ta]}, :int, :Int), env, st}
  end

  defp prim_value(op, [a, b], _args, env, st) when op in [:lt, :le, :gt, :ge] do
    {ta, st} = logic_term(a, st, :int)
    {tb, st} = logic_term(b, st, :int)

    p =
      case op do
        :lt -> {:lt, ta, tb}
        :le -> {:le, ta, tb}
        :gt -> {:lt, tb, ta}
        :ge -> {:le, tb, ta}
      end

    {val(p, :bool, [:Bool]), env, st}
  end

  defp prim_value(op, [a, b], _args, env, st) when op in [:eq, :ne] do
    st = if Float in [kind(a.ty), kind(b.ty)], do: gate(env, st, "`==` on Float, where the backends disagree (D25)"), else: st

    case {a, b} do
      {%{t: ta, s: s}, %{t: tb, s: s}} when ta != nil and tb != nil and s != nil ->
        p = {:eq, ta, tb}
        {val(if(op == :eq, do: p, else: IR.not_(p)), :bool, [:Bool]), env, st}

      _ ->
        {v, st} = fresh(:bool, st)
        {val(v, :bool, [:Bool]), env, st}
    end
  end

  defp prim_value(:not, [a], _args, env, st) do
    {ta, st} = bool_term(a, st)
    {val(IR.not_(ta), :bool, [:Bool]), env, st}
  end

  defp prim_value(:length, [xs], _args, env, st) do
    {t, st} = logic_term(xs, st, :list)
    {val({:len, t}, :int, :Int), env, st}
  end

  defp prim_value(:empty?, [xs], _args, env, st) do
    {t, st} = logic_term(xs, st, :list)
    {val({:eq, {:len, t}, {:int, 0}}, :bool, [:Bool]), env, st}
  end

  defp prim_value(:head, [xs], args, env, st) do
    {t, st} = logic_term(xs, st, :list)
    st = safety(:head, {:le, {:int, 1}, {:len, t}}, [:prim, :head | args], env, st)
    elem_type = element_type(xs.ty)

    case sort_of(elem_type) do
      nil ->
        {opaque(elem_type), env, st}

      sort ->
        {v, st} = fresh(sort, st)
        {val(v, sort, elem_type), env, st}
    end
  end

  defp prim_value(:tail, [xs], args, env, st) do
    {t, st} = logic_term(xs, st, :list)
    st = safety(:tail, {:le, {:int, 1}, {:len, t}}, [:prim, :tail | args], env, st)
    {v, st} = fresh(:list, st)
    {val(v, :list, xs.ty), add_fact(env, {:eq, {:len, v}, {:arith, :sub, [{:len, t}, {:int, 1}]}}), st}
  end

  defp prim_value(op, _vs, _args, env, st) do
    type =
      cond do
        op in [:fadd, :fsub, :fmul, :fdiv, :fneg, :"int-to-float"] -> :Float
        op == :concat -> :String
        true -> nil
      end

    case op do
      op when op in [:flt, :fle, :fgt, :fge] ->
        {v, st} = fresh(:bool, st)
        {val(v, :bool, [:Bool]), env, st}

      _ ->
        {opaque(type), env, st}
    end
  end

  # A primitive's crash must be unreachable, inside refined functions (§4.4).
  defp safety(_op, _goal, _expr, %{refined?: false}, st), do: st

  defp safety(op, goal, expr, env, st) do
    label =
      case op do
        op when op in [:div, :rem] -> "`#{op}` requires a non-zero divisor"
        op -> "`#{op}` requires a non-empty list"
      end

    emit(st, :primitive_safety, env.facts, goal, %{def: env.def, about: :primitive, expr: expr, label: label, notes: notes(env)})
  end

  # --- Calls ---------------------------------------------------------------------------

  defp call(f, args, env, st) do
    callee = callee(f, env)
    {fv, env, st} = if callee, do: {nil, env, st}, else: synth(f, env, st)

    case {fv, synth_values(args, env, st)} do
      {:bottom, {_, env, st}} -> {:bottom, env, st}
      {_, {:bottom, env, st}} -> {:bottom, env, st}
      {fv, {vs, env, st}} -> call_values(callee, fv, vs, [:app, f | args], args, env, st)
    end
  end

  defp callee(f, env) when is_atom(f), do: if(Map.has_key?(env.binds, f), do: nil, else: f)
  defp callee([:inst, f | _], env), do: callee(f, env)
  defp callee(_f, _env), do: nil

  defp call_values(callee, fv, vs, expr, args, env, st) do
    result_type =
      case callee do
        nil -> fv && result_of(fv.ty)
        f -> result_of(Map.get(st.defs, f))
      end

    case callee && Map.get(st.sigs, callee) do
      nil ->
        result_value(result_type, env, st)

      sig ->
        {terms, st} =
          vs
          |> Enum.zip(sig.params)
          |> Enum.with_index()
          |> Enum.map_reduce(st, fn {{v, p}, i}, st ->
            {t, st} = if p.sort, do: logic_term(v, st, p.sort), else: {nil, st}
            {{param(i), t}, st}
          end)

        terms = for {k, t} <- terms, t != nil, into: %{}, do: {k, t}
        rendered = for {p, a} <- Enum.zip(sig.params, args), into: %{}, do: {p.name, Render.core(a)}

        st =
          sig.params
          |> Enum.with_index()
          |> Enum.reduce(st, fn {p, i}, st ->
            Enum.reduce(p.conjuncts, st, fn {surface, ir, binder}, st ->
              subst = Map.put(rendered, binder, Map.fetch!(rendered, p.name))
              meta = %{def: env.def, about: :precondition, expr: expr, callee: callee, param: p.name, arg: Enum.at(args, i), conjunct: surface, subst: subst, notes: notes(env)}
              emit(st, :precondition, env.facts, IR.subst(ir, terms), meta)
            end)
          end)

        {rv, env, st} = result_value(result_type, env, st)

        env =
          case rv.t do
            nil -> env
            r -> Enum.reduce(sig.result, env, fn {_s, ir, _b}, env -> add_fact(env, IR.subst(ir, Map.put(terms, @result, r))) end)
          end

        {rv, env, st}
    end
  end

  defp result_value(type, env, st) do
    case sort_of(type) do
      nil ->
        {opaque(type), env, st}

      sort ->
        {v, st} = fresh(sort, st)
        {val(v, sort, type), env, st}
    end
  end

  # A function with refined parameters used other than at a call head: its
  # requirements must hold for every argument (§4.4, the last call rule).
  defp as_value(f, env, st) do
    case Map.get(st.sigs, f) do
      nil ->
        st

      sig ->
        {terms, st} =
          sig.params
          |> Enum.with_index()
          |> Enum.map_reduce(st, fn {p, i}, st ->
            {t, st} = if p.sort, do: fresh(p.sort, st), else: {nil, st}
            {{param(i), t}, st}
          end)

        terms = Map.new(terms)

        for {p, _i} <- Enum.with_index(sig.params), {surface, ir, binder} <- p.conjuncts, reduce: st do
          st ->
            meta = %{def: env.def, about: :as_value, expr: f, callee: f, param: p.name, conjunct: surface, subst: %{binder => Atom.to_string(p.name)}, notes: []}
            emit(st, :precondition, [], IR.subst(ir, terms), meta)
        end
    end
  end

  # --- Match clauses (§10.3) -----------------------------------------------------------

  # The env a clause body runs in, and the clause's exact match condition:
  # pattern and guard as a predicate over the scrutinee, nil when the logic
  # cannot state it. `earlier` holds the earlier clauses' match conditions,
  # whose negations hold in this clause.
  defp clause_env(clause, sv, earlier, env, st) do
    {pat, guard, body} =
      case clause do
        [pat, [:when, g], body] -> {pat, g, body}
        [pat, body] -> {pat, nil, body}
      end

    first_fresh = st.n + 1
    {%{tests: tests, local: local, binds: binds, exact?: exact?}, st} = pattern(pat, sv, st)

    env = Enum.reduce(for(m <- earlier, m != nil, do: IR.not_(m)), env, &add_fact(&2, &1))
    env = Enum.reduce(tests ++ local, env, &add_fact(&2, &1))
    env = %{env | binds: Map.merge(env.binds, binds)}

    {guard_fact, guard_exact?} =
      case guard && guard(guard, env.binds) do
        nil -> {nil, true}
        :unsupported -> {nil, false}
        {:ok, d, p} -> {IR.and_([d, p]), not mentions_fresh?([d, p], first_fresh, st.n)}
      end

    env = if guard_fact, do: add_fact(env, guard_fact), else: env

    matched =
      cond do
        not exact? or mentions_fresh?(tests, first_fresh, st.n) -> nil
        guard != nil and not guard_exact? -> nil
        guard_fact -> IR.and_(tests ++ [guard_fact])
        true -> IR.and_(tests)
      end

    {env, body, matched, st}
  end

  # A guard can be negated for later clauses only if it speaks about the
  # scrutinee, never about a name this pattern made up (fresh names first..last).
  defp mentions_fresh?(preds, first, last) do
    made_up = MapSet.new(first..last//1, &fresh_name/1)
    Enum.any?(IR.vars(preds), fn {name, _} -> MapSet.member?(made_up, name) end)
  end

  # pattern(p, value, st) -> {%{tests: preds over the scrutinee, local: binding
  # facts, binds: names to values, exact?: whether tests are complete}, st}
  defp pattern(:_, _v, st), do: {%{tests: [], local: [], binds: %{}, exact?: true}, st}

  defp pattern(x, v, st) when is_atom(x), do: {%{tests: [], local: [], binds: %{x => v}, exact?: true}, st}

  defp pattern(n, v, st) when is_integer(n) do
    case v do
      %{t: t, s: :int} when t != nil -> {%{tests: [{:eq, t, {:int, n}}], local: [], binds: %{}, exact?: true}, st}
      _ -> {%{tests: [], local: [], binds: %{}, exact?: false}, st}
    end
  end

  defp pattern([:unit], _v, st), do: {%{tests: [], local: [], binds: %{}, exact?: true}, st}

  defp pattern([:inj, [:Bool], b], v, st) when is_boolean(b) do
    case v do
      %{t: t, s: :bool} when t != nil -> {%{tests: [if(b, do: t, else: IR.not_(t))], local: [], binds: %{}, exact?: true}, st}
      _ -> {%{tests: [], local: [], binds: %{}, exact?: false}, st}
    end
  end

  defp pattern([:inj, [:List | _], :Nil], v, st) do
    case v do
      %{t: t, s: :list} when t != nil -> {%{tests: [{:eq, len_of(v), {:int, 0}}], local: [], binds: %{}, exact?: true}, st}
      _ -> {%{tests: [], local: [], binds: %{}, exact?: false}, st}
    end
  end

  defp pattern([:inj, [:List | _] = type, :Cons, ph, pt], v, st) do
    case v do
      %{t: t, s: :list} when t != nil ->
        len = len_of(v)
        {hv, st} = fresh_value(element_type(type), st)
        {tt, st} = fresh(:list, st)
        tail_len = {:arith, :sub, [len, {:int, 1}]}
        tv = %{t: tt, s: :list, ty: type, len: tail_len}
        {h, st} = pattern(ph, hv, st)
        {tl, st} = pattern(pt, tv, st)

        # The head's tests speak about a fresh name, so they are local facts;
        # the tail's are about its length, which is the scrutinee's minus one.
        result = %{
          tests: [{:le, {:int, 1}, len} | tl.tests],
          local: [{:eq, {:len, tt}, tail_len} | h.tests ++ h.local ++ tl.local],
          binds: Map.merge(h.binds, tl.binds),
          exact?: h.tests == [] and h.exact? and tl.exact?
        }

        {result, st}

      _ ->
        {r, st} = irrefutable([ph, pt], [element_type(type), type], st)
        {%{r | exact?: false}, st}
    end
  end

  defp pattern([:as, x, p], v, st) do
    {r, st} = pattern(p, v, st)
    {%{r | binds: Map.put(r.binds, x, v)}, st}
  end

  defp pattern([:tuple | ps], v, st) do
    types = case v.ty do
      [:Tuple | types] -> types
      _ -> Enum.map(ps, fn _ -> nil end)
    end

    irrefutable(ps, types, st)
  end

  defp pattern([:inj, type, ctor | ps], _v, st) do
    {r, st} = irrefutable(ps, ctor_fields(type, ctor, st.decls, length(ps)), st)
    {%{r | exact?: false}, st}
  end

  # A named record has one shape, so its pattern is exact when its fields' are.
  defp pattern([:record, type | fields], _v, st) do
    irrefutable(for([_l, p] <- fields, do: p), record_fields(type, fields, st.decls), st)
  end

  defp pattern(_literal, _v, st), do: {%{tests: [], local: [], binds: %{}, exact?: false}, st}

  # Sub-patterns over values the logic cannot name: their binders get fresh
  # values, and the test is exact only if every sub-pattern always matches.
  defp irrefutable(ps, types, st) do
    Enum.zip(ps, types)
    |> Enum.reduce({%{tests: [], local: [], binds: %{}, exact?: true}, st}, fn {p, type}, {acc, st} ->
      {v, st} = fresh_value(type, st)
      {r, st} = pattern(p, v, st)
      always? = r.tests == [] and r.exact? and (is_atom(p) or p == [:unit])
      {%{acc | local: acc.local ++ r.tests ++ r.local, binds: Map.merge(acc.binds, r.binds), exact?: acc.exact? and always?}, st}
    end)
  end

  defp len_of(%{len: len}), do: len
  defp len_of(%{t: t}), do: {:len, t}

  # --- Guards (§6.6, §10.4) -------------------------------------------------------------

  # A guard is guard-safe Core, so it translates whole: {:ok, def(g), g}.
  defp guard(g, binds) do
    case gexpr(g, binds) do
      {:ok, d, t, :bool} -> {:ok, d, t}
      _ -> :unsupported
    end
  catch
    :unsupported -> :unsupported
  end

  defp gexpr(n, _binds) when is_integer(n), do: {:ok, {:bool, true}, {:int, n}, :int}
  defp gexpr([:inj, [:Bool], b], _binds) when is_boolean(b), do: {:ok, {:bool, true}, {:bool, b}, :bool}

  defp gexpr(x, binds) when is_atom(x) do
    case Map.get(binds, x) do
      %{t: t, s: s} when t != nil and s != nil -> {:ok, {:bool, true}, t, s}
      _ -> throw(:unsupported)
    end
  end

  defp gexpr([:if, c, a, b], binds) do
    {:ok, dc, tc, :bool} = gexpr(c, binds)
    {:ok, da, ta, s} = gexpr(a, binds)
    {:ok, db, tb, ^s} = gexpr(b, binds)
    {:ok, IR.and_([dc, IR.implies(tc, da), IR.implies(IR.not_(tc), db)]), {:ite, tc, ta, tb}, s}
  end

  defp gexpr([:prim, op | args], binds) do
    parts = Enum.map(args, &gexpr(&1, binds))
    defs = for {:ok, d, _, _} <- parts, do: d
    terms = for {:ok, _, t, _} <- parts, do: t
    d = IR.and_(defs)

    case {op, terms} do
      {op, [a, b]} when op in [:add, :sub, :mul] -> {:ok, d, {:arith, op, [a, b]}, :int}
      {op, [a, b]} when op in [:div, :rem] -> {:ok, IR.and_([d, IR.not_({:eq, b, {:int, 0}})]), {:arith, Map.fetch!(@arith, op), [a, b]}, :int}
      {:neg, [a]} -> {:ok, d, {:arith, :neg, [a]}, :int}
      {:lt, [a, b]} -> {:ok, d, {:lt, a, b}, :bool}
      {:le, [a, b]} -> {:ok, d, {:le, a, b}, :bool}
      {:gt, [a, b]} -> {:ok, d, {:lt, b, a}, :bool}
      {:ge, [a, b]} -> {:ok, d, {:le, b, a}, :bool}
      {:eq, [a, b]} -> same_sort(parts) && {:ok, d, {:eq, a, b}, :bool}
      {:ne, [a, b]} -> same_sort(parts) && {:ok, d, IR.not_({:eq, a, b}), :bool}
      {:not, [a]} -> {:ok, d, IR.not_(a), :bool}
      {:length, [xs]} -> {:ok, d, {:len, xs}, :int}
      {:empty?, [xs]} -> {:ok, d, {:eq, {:len, xs}, {:int, 0}}, :bool}
      _ -> throw(:unsupported)
    end
    |> case do
      false -> throw(:unsupported)
      result -> result
    end
  end

  defp gexpr(_other, _binds), do: throw(:unsupported)

  defp same_sort(parts), do: match?([{:ok, _, _, s}, {:ok, _, _, s}], parts)

  # --- Values, sorts and types -------------------------------------------------------

  defp val(t, s, ty), do: %{t: t, s: s, ty: ty}
  defp opaque(ty), do: %{t: nil, s: sort_of(ty), ty: ty}

  defp retype(v, type), do: %{v | ty: type}

  defp fresh_value(type, st) do
    case sort_of(type) do
      nil ->
        {opaque(type), st}

      sort ->
        {v, st} = fresh(sort, st)
        {val(v, sort, type), st}
    end
  end

  # Every value of a logic sort is named by a term; one the logic knows
  # nothing about gets a fresh name (A-normal form, §4.4).
  defp logic_term(%{t: t}, st, _sort) when t != nil, do: {t, st}
  defp logic_term(_v, st, sort), do: fresh(sort, st)

  defp bool_term(v, st), do: logic_term(v, st, :bool)

  defp fresh(sort, st), do: {{:var, fresh_name(st.n + 1), sort}, %{st | n: st.n + 1}}

  @doc false
  def sort_of(:Int), do: :int
  def sort_of([:Bool]), do: :bool
  def sort_of([:List, _]), do: :list
  def sort_of(_), do: nil

  defp kind(:Float), do: Float
  defp kind(_), do: nil

  defp arrow([:forall, _binders, type]), do: arrow(type)
  defp arrow([:->, params, _eff, result]), do: {params, result}
  defp arrow([:pi, params, _eff, result]), do: {Enum.map(params, fn [_x, t] -> t end), result}

  defp result_of(nil), do: nil
  defp result_of(type) do
    case arrow(type) do
      {_params, result} -> result
    end
  rescue
    FunctionClauseError -> nil
  end

  defp element_type([:List, t]), do: t
  defp element_type(_), do: nil

  defp ctor_fields([name | args], ctor, decls, n) do
    with [:type, ^name, params, [:sum | ctors]] <- Enum.find(decls, &match?([:type, ^name | _], &1)),
         [^ctor | fields] <- Enum.find(ctors, &match?([^ctor | _], &1)) do
      subst = Enum.zip(params, args) |> Map.new()
      Enum.map(fields, &subst_tvars(&1, subst))
    else
      _ -> List.duplicate(nil, n)
    end
  end

  defp ctor_fields(_type, _ctor, _decls, n), do: List.duplicate(nil, n)

  defp record_fields([name | args], fields, decls) do
    with [:type, ^name, params, [:record | labelled]] <- Enum.find(decls, &match?([:type, ^name | _], &1)) do
      subst = Enum.zip(params, args) |> Map.new()
      for [l, _p] <- fields, do: labelled |> Enum.find_value(fn [^l, t] -> subst_tvars(t, subst); _ -> nil end)
    else
      _ -> Enum.map(fields, fn _ -> nil end)
    end
  end

  defp record_fields(_type, fields, _decls), do: Enum.map(fields, fn _ -> nil end)

  defp subst_tvars([:tvar, a], subst), do: Map.get(subst, a, [:tvar, a])
  defp subst_tvars(list, subst) when is_list(list), do: Enum.map(list, &subst_tvars(&1, subst))
  defp subst_tvars(t, _subst), do: t

  defp term_of(%{t: t}), do: t

  # --- Environments and facts -----------------------------------------------------------

  defp fact(p, note \\ nil), do: %{p: p, note: note}

  # Facts are newest first. A `true` fact is kept (emit/5 drops it), so that
  # learned/3 can count what a branch was given on entry.
  defp add_fact(env, p, note \\ nil), do: %{env | facts: [fact(p, note) | env.facts]}

  defp bind(env, :_, _v), do: env
  defp bind(env, x, v), do: %{env | binds: Map.put(env.binds, x, v)}

  defp branch_note(c, polarity), do: {Render.core(c), polarity}

  defp notes(env), do: for(%{note: note} <- Enum.reverse(env.facts), note != nil, do: note)

  defp gate(%{involved?: false}, st, _what), do: st
  defp gate(env, st, what), do: %{st | gate: [{env.def, "refinement checking does not admit #{what} yet"} | st.gate]}

  defp emit(st, kind, facts, goal, meta) do
    id = length(st.vcs) + 1
    hyps = facts |> Enum.reverse() |> Enum.map(fn %{p: p} -> p end) |> Enum.reject(&(&1 == {:bool, true}))
    vc = %VC{id: id, kind: kind, hyps: hyps, goal: goal, vars: IR.vars(hyps ++ [goal]), meta: meta}
    %{st | vcs: [vc | st.vcs]}
  end
end
