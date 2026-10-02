defmodule Vaisto.Elab do
  @moduledoc """
  The Phase 1a elaborator (liquid-types.md §8, §9; RFC §9.1): surface AST to
  Liquid Core, bidirectionally.

  Introduction forms are *checked* against the type the context supplies,
  and elimination forms *synthesize* one. When a synthesized type meets an
  expected one they must be equal. Inference is local unification: it only
  chooses the type arguments at a use site, inside one definition.

    * Every top-level definition carries a signature (Q35). Its parameters
      and result are annotated; a lowercase keyword such as `:a` is a type
      variable, quantified over the signature.
    * Local `let`s are not generalized (Q36).
    * A lambda takes its parameter types from the arrow it is checked
      against, so `(map (fn [x] (* x 2)) xs)` needs no annotation.
    * There is no `:any`. Where local inference cannot choose a type, the
      result is an error at that place, never `Dyn`.

  Core Lint re-checks the output independently (§8.1): a program the
  elaborator accepts and Lint rejects is an elaborator bug.

  This is slice 1 of Phase 1a: the fragment of Core version 0 the Phase 0
  adapter covers. Type classes, processes, externs and calls across modules
  are outside it; `module/2` says so rather than guessing.

  `module/2` returns:

    * `{:ok, core_module}`;
    * `{:error, errors}`, a list of `%Vaisto.Error{}`, for an ill-typed program;
    * `{:outside, reason}` for a program that uses something outside the fragment.

  The `:suggest` option maps definition names to Core types, used as the
  signature of a definition that has none: the migration of §13, where the
  old engine suggests and this one verifies.
  """

  alias Vaisto.Error
  alias Vaisto.Elab.Types
  alias Vaisto.Liquid.Prelude
  alias Vaisto.Parser.Loc

  @higher_order %{map: :"prelude.map", filter: :"prelude.filter", fold: :"prelude.fold", flat_map: :"prelude.flat_map"}
  @arith %{:+ => {:add, :fadd}, :- => {:sub, :fsub}, :* => {:mul, :fmul}}
  @compare %{:< => {:lt, :flt}, :> => {:gt, :fgt}, :<= => {:le, :fle}, :>= => {:ge, :fge}}
  @operators Map.keys(@arith) ++ Map.keys(@compare) ++ [:/, :div, :rem, :==, :!=, :and, :or, :++, :not]
  @no_match [:perform, :crash, [:inj, [:Reason], :no_match]]

  # decls: type name => %{params, kind, members}; ctors: constructor => type
  # name; sigs: definition => Core type; env: source name => {core name,
  # type}; s: the substitution; loc: the nearest source location.
  defstruct decls: %{}, ctors: %{}, sigs: %{}, env: %{}, s: %{}, next: 0, names: MapSet.new(), prelude: MapSet.new(), loc: nil, ground: []

  @doc "Elaborate a parsed program into a Core module named `name`. Options: `:suggest`, `:name`."
  @spec module(term(), keyword()) :: {:ok, term()} | {:error, [Error.t()]} | {:outside, String.t()}
  def module(ast, opts \\ []) do
    forms = if is_list(ast), do: ast, else: [ast]
    suggest = Keyword.get(opts, :suggest, %{})

    with {:ok, ctx} <- declarations(forms),
         {:ok, ctx, defs} <- signatures(forms, ctx, suggest) do
      {items, errors, prelude} =
        Enum.reduce(defs, {[], [], MapSet.new()}, fn {form, sig}, {items, errors, prelude} ->
          case definition(form, sig, ctx) do
            {:ok, item, used} -> {items ++ [item], errors, MapSet.union(prelude, used)}
            {:error, error} -> {items, errors ++ [error], prelude}
          end
        end)

      cond do
        errors != [] -> {:error, errors}
        true -> {:ok, [:module, Keyword.get(opts, :name, :Main), [:"core-version", 0] | core_decls(ctx) ++ Prelude.defs(prelude) ++ items]}
      end
    end
  catch
    {:outside, reason} -> {:outside, reason}
  end

  # --- Declarations ----------------------------------------------------------------

  defp declarations(forms) do
    arities = Map.new(forms |> Enum.filter(&match?({:deftype, _, _, _}, &1)), fn {:deftype, t, body, _} -> {t, length(decl_params(body))} end)

    {decls, errors} =
      Enum.reduce(forms, {%{}, []}, fn
        {:deftype, t, body, loc}, {decls, errors} ->
          case decl(t, body, arities) do
            {:ok, d} -> {Map.put(decls, t, d), errors}
            {:error, message} -> {decls, errors ++ [Error.new(message, span: span(loc))]}
          end

        _form, acc ->
          acc
      end)

    ctors = for {t, %{kind: :sum, members: ms}} <- decls, {c, _} <- ms, into: %{}, do: {c, t}

    if errors == [], do: {:ok, %__MODULE__{decls: decls, ctors: ctors}}, else: {:error, errors}
  end

  # A sum's fields and a record's field types are annotations; a bare
  # lowercase name in a sum field, as in (Ok v), is a type variable. The
  # declaration's parameters are its type variables in order of appearance.
  defp decl_params({:sum, variants}), do: variants |> Enum.flat_map(fn {_c, fs} -> Enum.map(fs, &field_tvars/1) end) |> List.flatten() |> Enum.uniq()
  defp decl_params({:product, fields}), do: fields |> Enum.map(fn {_l, t} -> field_tvars(t) end) |> List.flatten() |> Enum.uniq()

  defp field_tvars(t) when is_atom(t), do: if(String.match?(Atom.to_string(t), ~r/^[a-z]/), do: [t], else: [])
  defp field_tvars({:atom, t}), do: field_tvars(t) |> Enum.reject(&(&1 in [:int, :float, :string, :bool, :atom, :unit, :any, :num]))
  defp field_tvars({:call, _t, args, _loc}), do: Enum.flat_map(args, &field_tvars/1)
  defp field_tvars(_), do: []

  defp decl(t, body, arities) do
    params = decl_params(body)

    members =
      case body do
        {:sum, variants} -> for {c, fs} <- variants, do: {c, Enum.map(fs, &field_type(&1, arities))}
        {:product, fields} -> for {l, ft} <- fields, do: {l, field_type(ft, arities)}
      end

    kind = if match?({:sum, _}, body), do: :sum, else: :record
    {:ok, %{params: params, kind: kind, members: members}}
  catch
    {:type_error, message} -> {:error, "in the type `#{t}`: #{message}"}
  end

  defp field_type(t, arities) do
    case Types.from_surface(t, arities) do
      {:ok, type} -> type
      {:error, message} -> throw({:type_error, message})
    end
  end

  defp core_decls(ctx) do
    for {t, %{params: params, kind: kind, members: members}} <- Enum.sort(ctx.decls) do
      body =
        case kind do
          :sum -> [:sum | for({c, fs} <- members, do: [c | fs])]
          :record -> [:record | for({l, ft} <- members, do: [l, ft])]
        end

      [:type, t, params, body]
    end
  end

  # --- Signatures (Q35) ----------------------------------------------------------------

  defp signatures(forms, ctx, suggest) do
    arities = Map.new(ctx.decls, fn {t, d} -> {t, length(d.params)} end)

    {sigs, defs, errors} =
      Enum.reduce(forms, {%{}, [], []}, fn form, {sigs, defs, errors} ->
        case form do
          {:defn, f, params, _body, ret, loc} -> signature(f, params, ret, loc, arities, suggest, form, {sigs, defs, errors})
          {:defn, f, params, _body, ret, _guard, loc} -> signature(f, params, ret, loc, arities, suggest, form, {sigs, defs, errors})
          {:defn_multi, f, _clauses, loc} -> multi_signature(f, loc, suggest, form, {sigs, defs, errors})
          {:deftype, _, _, _} -> {sigs, defs, errors}
          {:ns, _, _} -> {sigs, defs, errors}
          other -> outside(other)
        end
      end)

    if errors == [], do: {:ok, %{ctx | sigs: sigs}, defs}, else: {:error, errors}
  end

  defp signature(f, params, ret, loc, arities, suggest, form, {sigs, defs, errors}) do
    annotated = Enum.map(params, fn {p, t} -> {p, Types.from_surface(t, arities)} end)
    result = Types.from_surface(ret, arities)

    case {Enum.find(annotated, &match?({_, {:error, _}}, &1)), result} do
      {nil, {:ok, rtype}} ->
        type = Types.generalize([:->, Enum.map(annotated, fn {_, {:ok, t}} -> t end), [:eff, :closed], rtype])
        {Map.put(sigs, f, type), defs ++ [{form, type}], errors}

      {missing, _} ->
        case Map.fetch(suggest, f) do
          {:ok, type} ->
            {Map.put(sigs, f, type), defs ++ [{form, type}], errors}

          :error ->
            what =
              case missing do
                {p, {:error, message}} -> "the parameter `#{p}`: #{message}"
                nil -> "the result: #{elem(result, 1)}"
              end

            error = Error.new("`#{f}` needs a signature", span: span(loc), note: what, hint: "annotate every parameter and the result, as in (defn #{f} [x :int] :int ...)")
            {sigs, defs, errors ++ [error]}
        end
    end
  end

  defp multi_signature(f, loc, suggest, form, {sigs, defs, errors}) do
    case Map.fetch(suggest, f) do
      {:ok, type} ->
        {Map.put(sigs, f, type), defs ++ [{form, type}], errors}

      :error ->
        error = Error.new("`#{f}` needs a signature", span: span(loc), note: "a multi-clause function has no syntax for one yet")
        {sigs, defs, errors ++ [error]}
    end
  end

  defp outside({tag, name, _, _}) when tag in [:process, :defclass], do: throw({:outside, "#{tag} #{name}"})
  defp outside(form) when is_tuple(form), do: throw({:outside, "the form #{inspect(elem(form, 0))}"})
  defp outside(form), do: throw({:outside, "#{inspect(form, limit: 4)}"})

  # --- Definitions -------------------------------------------------------------------

  defp definition(form, sig, ctx) do
    {name, loc} = {elem(form, 1), elem(form, tuple_size(form) - 1)}
    {_binders, arrow} = open(sig)
    [:->, ptypes, _eff, rtype] = arrow
    ctx = %{ctx | names: MapSet.new(Map.keys(ctx.sigs)), loc: loc, env: %{}, s: %{}, next: 0, prelude: MapSet.new(), ground: []}

    {fun, ctx} =
      case form do
        {:defn, _f, params, body, _ret, _loc} ->
          {core_params, ctx} = bind_params(Enum.map(params, &elem(&1, 0)), ptypes, ctx)
          {body, ctx} = check(body, rtype, ctx)
          {[:fn, core_params, body], ctx}

        {:defn, _f, params, body, _ret, guard, _loc} ->
          {core_params, ctx} = bind_params(Enum.map(params, &elem(&1, 0)), ptypes, ctx)
          {g, ctx} = check(guard, [:Bool], ctx)
          {body, ctx} = check(body, rtype, ctx)
          {[:fn, core_params, [:match, [:unit], [:_, [:when, g], body], [:_, @no_match]]], ctx}

        {:defn_multi, _f, clauses, _loc} ->
          [ptype] = ptypes
          {arg, ctx} = fresh_name(:arg, ctx)
          ctx = %{ctx | env: Map.put(ctx.env, :"$arg", {arg, ptype})}
          {m, ctx} = match_clauses(arg, ptype, clauses, {:check, rtype}, ctx)
          {[:fn, [[arg, ptype]], m], ctx}
      end

    {fun, ctx} = finish(fun, ctx)
    {:ok, [:def, name, sig, fun], ctx.prelude}
  catch
    {:elab, %Error{} = error} -> {:error, error}
  end

  defp open([:forall, binders, arrow]), do: {Enum.map(binders, &hd/1), arrow}
  defp open(arrow), do: {[], arrow}


  defp bind_params(names, types, ctx) do
    Enum.zip(names, types)
    |> Enum.reduce({[], ctx}, fn {p, t}, {acc, ctx} ->
      {x, ctx} = fresh_name(p, ctx)
      {acc ++ [[x, t]], %{ctx | env: Map.put(ctx.env, p, {x, t})}}
    end)
  end

  # The end of a definition. In order:
  #
  #   1. an arithmetic or comparison whose operands' type was open is resolved
  #      to its Int or Float primitive. Here a choice matters, so a type
  #      still unknown is an error;
  #   2. a type nothing constrains, such as the error type of `(Ok 42)` used
  #      only as an Int, becomes Unit. No choice there can change what the
  #      program does (parametricity), so it is not a type inference failed
  #      to find, and the elaborator chooses rather than asking for an
  #      annotation the surface has no syntax for;
  #   3. equality needs a ground type (liquid-core.md §10.11).
  defp finish(term, ctx) do
    term = resolve_numeric(term, ctx)
    term = term |> Types.zonk(ctx.s) |> default_unconstrained()

    for {type, loc} <- ctx.ground do
      t = type |> Types.zonk(ctx.s) |> default_unconstrained()

      if Types.tvars(t) != [] or function_type?(t) do
        fail(%{ctx | loc: loc}, "`==` needs a type without type variables or functions, but this is #{Types.format(t)}", note: "comparing values of a type variable needs an Eq dictionary, which comes with type classes")
      end
    end

    {term, ctx}
  end

  defp resolve_numeric({:numeric, int_op, float_op, type, op, loc}, ctx) do
    case Types.zonk(type, ctx.s) do
      :Int -> int_op
      :Float -> float_op
      {:meta, _} -> fail(%{ctx | loc: loc}, "cannot tell whether `#{op}` works on Int or Float here", hint: "annotate an operand's type")
      other -> fail(%{ctx | loc: loc}, "`#{op}` works on Int or Float, not #{Types.format(other)}")
    end
  end

  defp resolve_numeric(list, ctx) when is_list(list), do: Enum.map(list, &resolve_numeric(&1, ctx))
  defp resolve_numeric(t, _ctx), do: t

  defp default_unconstrained({:meta, _}), do: :Unit
  defp default_unconstrained(list) when is_list(list), do: Enum.map(list, &default_unconstrained/1)
  defp default_unconstrained(t), do: t

  defp function_type?([:->, _, _, _]), do: true
  defp function_type?(list) when is_list(list), do: Enum.any?(list, &function_type?/1)
  defp function_type?(_), do: false

  # --- Checking (⇐) ------------------------------------------------------------------
  # check(e, τ, ctx) -> {core, ctx}. Introduction forms take the expected type
  # inward; anything else synthesizes and must agree.

  defp check(e, type, ctx), do: at(e, ctx, &check_(&1, type, &2))

  defp check_({:fn, params, body, _loc} = e, type, ctx) do
    case Types.zonk(type, ctx.s) do
      [:->, ptypes, _eff, rtype] when length(ptypes) == length(params) ->
        {core_params, ctx} = bind_params(Enum.map(params, &param_name/1), ptypes, ctx)
        {body, ctx} = in_scope(ctx, fn ctx -> check(body, rtype, ctx) end)
        {[:fn, core_params, body], ctx}

      _ ->
        subsume(e, type, ctx)
    end
  end

  defp check_({:if, c, a, b, _loc}, type, ctx) do
    {c, ctx} = check(c, [:Bool], ctx)
    {a, ctx} = check(a, type, ctx)
    {b, ctx} = check(b, type, ctx)
    {[:if, c, a, b], ctx}
  end

  defp check_({:let, bindings, body, _loc}, type, ctx), do: let(bindings, body, {:check, type}, ctx)
  defp check_({:do, [], _loc}, type, ctx), do: subsume({:unit, nil}, type, ctx)
  defp check_({:do, [e], _loc}, type, ctx), do: check(e, type, ctx)

  defp check_({:do, [e | rest], loc}, type, ctx) do
    {core, t, ctx} = synth(e, ctx)
    {x, ctx} = fresh_name(:_, ctx)
    {rest, ctx} = check({:do, rest, loc}, type, ctx)
    {[:let, [[x, t, core]], rest], ctx}
  end

  defp check_({:match, s, clauses, _loc}, type, ctx) do
    {s, stype, ctx} = synth(s, ctx)
    match_clauses(s, stype, Enum.map(clauses, fn {p, b} -> {p, nil, b} end), {:check, type}, ctx)
  end

  defp check_({:list, elems, _loc}, type, ctx), do: check_({:bracket, elems}, type, ctx)

  defp check_({:bracket, elems}, type, ctx) when is_list(elems) do
    {elem_type, ctx} = list_elem(type, ctx)
    list(elems, elem_type, ctx)
  end

  defp check_({:try, body, [{:error, e, handler}], nil, _loc}, type, ctx) do
    {body, ctx} = check(body, type, ctx)
    {x, ctx} = fresh_name(e, ctx)
    {handler, ctx} = in_scope(%{ctx | env: Map.put(ctx.env, e, {x, [:Reason]})}, fn ctx -> check(handler, type, ctx) end)
    {[:"handle-crash", body, [[x, [:Reason]], handler]], ctx}
  end

  # A builtin operator given where a function is expected is eta-expanded.
  defp check_(op, type, ctx) when is_atom(op) and op in @operators do
    case Types.zonk(type, ctx.s) do
      [:->, ptypes, _eff, _r] when not is_map_key(ctx.env, op) ->
        params = for i <- 1..length(ptypes), do: :"$#{op}#{i}"
        check({:fn, params, {:call, op, params, ctx.loc}, ctx.loc}, type, ctx)

      _ ->
        subsume(op, type, ctx)
    end
  end

  defp check_(e, type, ctx), do: subsume(e, type, ctx)

  # A synthesized type meets an expected one: they must be equal (§8.1, modes).
  defp subsume(e, expected, ctx) do
    {core, actual, ctx} = synth(e, ctx)
    {core, unify!(expected, actual, ctx)}
  end

  defp list(elems, elem_type, ctx) do
    {cores, ctx} = Enum.map_reduce(elems, ctx, &check(&1, elem_type, &2))
    list_type = [:List, elem_type]
    {List.foldr(cores, [:inj, list_type, :Nil], fn h, t -> [:inj, list_type, :Cons, h, t] end), ctx}
  end

  defp list_elem(type, ctx) do
    {m, ctx} = meta(ctx)
    ctx = unify!(type, [:List, m], ctx)
    {m, ctx}
  end

  # --- Synthesis (⇒) -----------------------------------------------------------------
  # synth(e, ctx) -> {core, type, ctx}.

  defp synth(e, ctx), do: at(e, ctx, &synth_/2)

  defp synth_(n, ctx) when is_integer(n), do: {n, :Int, ctx}
  defp synth_(r, ctx) when is_float(r), do: {r, :Float, ctx}
  defp synth_(b, ctx) when is_boolean(b), do: {[:inj, [:Bool], b], [:Bool], ctx}
  defp synth_(nil, ctx), do: {[:unit], :Unit, ctx}
  defp synth_({:unit, _loc}, ctx), do: {[:unit], :Unit, ctx}
  defp synth_({:string, s}, ctx), do: {s, :String, ctx}
  defp synth_({:atom, a}, ctx), do: {[:atom, a], :Atom, ctx}

  defp synth_(x, ctx) when is_atom(x) do
    cond do
      Map.has_key?(ctx.env, x) ->
        {name, type} = Map.fetch!(ctx.env, x)
        {name, type, ctx}

      Map.has_key?(ctx.sigs, x) ->
        instantiate(x, Map.fetch!(ctx.sigs, x), ctx)

      Map.has_key?(@higher_order, x) ->
        prelude_ref(Map.fetch!(@higher_order, x), ctx)

      Map.has_key?(ctx.ctors, x) ->
        constructor(x, [], ctx)

      x in @operators ->
        fail(ctx, "`#{x}` is an operator; pass it where a function of known type is expected, or wrap it in (fn ...)")

      true ->
        fail(ctx, "undefined variable `#{x}`")
    end
  end

  defp synth_({:fn, params, body, _loc}, ctx) do
    {metas, ctx} = Enum.map_reduce(params, ctx, fn _p, ctx -> meta(ctx) end)
    {core_params, ctx} = bind_params(Enum.map(params, &param_name/1), metas, ctx)
    {body, rtype, ctx} = in_scope(ctx, fn ctx -> synth(body, ctx) end)
    {[:fn, core_params, body], [:->, metas, [:eff, :closed], rtype], ctx}
  end

  defp synth_({:if, c, a, b, _loc}, ctx) do
    {c, ctx} = check(c, [:Bool], ctx)
    {a, type, ctx} = synth(a, ctx)
    {b, ctx} = check(b, type, ctx)
    {[:if, c, a, b], type, ctx}
  end

  defp synth_({:let, bindings, body, _loc}, ctx) do
    {m, ctx} = meta(ctx)
    {core, ctx} = let(bindings, body, {:check, m}, ctx)
    {core, m, ctx}
  end

  defp synth_({:do, _, _} = e, ctx) do
    {m, ctx} = meta(ctx)
    {core, ctx} = check(e, m, ctx)
    {core, m, ctx}
  end

  defp synth_({:match, _, _, _} = e, ctx) do
    {m, ctx} = meta(ctx)
    {core, ctx} = check(e, m, ctx)
    {core, m, ctx}
  end

  defp synth_({:try, _, _, _, _} = e, ctx) do
    case e do
      {:try, _, [{:error, _, _}], nil, _} ->
        {m, ctx} = meta(ctx)
        {core, ctx} = check(e, m, ctx)
        {core, m, ctx}

      _ ->
        outside_term("try with after, or catching a class other than :error")
    end
  end

  defp synth_({:bracket, elems} = e, ctx) when is_list(elems) do
    {m, ctx} = meta(ctx)
    {core, ctx} = check(e, [:List, m], ctx)
    {core, [:List, m], ctx}
  end

  defp synth_({:list, elems, _loc}, ctx), do: synth_({:bracket, elems}, ctx)
  defp synth_({:bracket, {:cons, h, t}}, ctx), do: cons(h, t, ctx)
  defp synth_({:tuple, [], _loc}, ctx), do: {[:unit], :Unit, ctx}
  defp synth_({:tuple, es, _loc}, ctx), do: tuple(es, ctx)
  defp synth_({:tuple_pattern, []}, ctx), do: {[:unit], :Unit, ctx}
  defp synth_({:tuple_pattern, es}, ctx), do: tuple(es, ctx)

  defp synth_({:field_access, e, field, _loc}, ctx) do
    {core, type, ctx} = synth(e, ctx)

    zonked = Types.zonk(type, ctx.s)

    case zonked do
      [t | args] when is_atom(t) and is_map_key(ctx.decls, t) ->
        case Map.fetch!(ctx.decls, t) do
          %{kind: :record, params: params, members: fields} ->
            case List.keyfind(fields, field, 0) do
              {^field, ft} -> {[:select, core, field], Types.subst_tvars(ft, Map.new(Enum.zip(params, args))), ctx}
              nil -> fail(ctx, "the record `#{t}` has no field `#{field}`")
            end

          _ ->
            fail(ctx, "`.` reads a field of a record, but this is #{Types.format(zonked)}")
        end

      {:meta, _} ->
        fail(ctx, "the record type here cannot be determined", hint: "annotate it; reading a field of any record with that field (row polymorphism) comes later")

      other ->
        fail(ctx, "`.` reads a field of a record, but this is #{Types.format(other)}")
    end
  end

  defp synth_({:call, f, args, _loc}, ctx), do: call(f, args, ctx)
  defp synth_({:qualified, m, f}, _ctx), do: outside_term("the call #{m}/#{f} (externs need decode)")
  defp synth_(other, _ctx), do: outside_term("the form #{inspect(elem_or(other), limit: 3)}")

  defp elem_or(t) when is_tuple(t), do: elem(t, 0)
  defp elem_or(t), do: t

  defp tuple(es, ctx) do
    {pairs, ctx} = Enum.map_reduce(es, ctx, fn e, ctx -> {c, t, ctx} = synth(e, ctx); {{c, t}, ctx} end)
    {[:tuple | Enum.map(pairs, &elem(&1, 0))], [:Tuple | Enum.map(pairs, &elem(&1, 1))], ctx}
  end

  defp cons(h, t, ctx) do
    {h, htype, ctx} = synth(h, ctx)
    {t, ctx} = check(t, [:List, htype], ctx)
    {[:inj, [:List, htype], :Cons, h, t], [:List, htype], ctx}
  end

  # --- Calls ---------------------------------------------------------------------------

  defp call({:qualified, m, f}, _args, _ctx), do: outside_term("the call #{m}/#{f} (externs need decode)")

  defp call(f, args, ctx) when is_atom(f) do
    cond do
      Map.has_key?(ctx.env, f) -> apply_value(f, args, ctx)
      Map.has_key?(ctx.sigs, f) -> apply_value(f, args, ctx)
      Map.has_key?(ctx.ctors, f) -> constructor(f, args, ctx)
      Map.has_key?(ctx.decls, f) and ctx.decls[f].kind == :record -> record(f, args, ctx)
      Map.has_key?(@higher_order, f) -> apply_value(f, args, ctx)
      true -> builtin(f, args, ctx)
    end
  end

  defp call(f, _args, _ctx), do: outside_term("the call of #{inspect(f, limit: 3)}")

  # An application: the callee synthesizes an arrow, and the arguments are
  # checked against its parameters, which is what types a lambda argument.
  #
  # Function arguments (lambdas and operators) are checked after the others,
  # so that `(fold + 0 xs)` knows it folds Ints before it expands `+`. The
  # order only changes which unification runs first, never the result.
  defp apply_value(f, args, ctx) do
    {head, ftype, ctx} = synth(f, ctx)
    {ptypes, rtype, ctx} = arrow(ftype, length(args), f, ctx)
    {later, first} = args |> Enum.zip(ptypes) |> Enum.with_index() |> Enum.split_with(fn {{a, _t}, _i} -> function_arg?(a, ctx) end)

    {checked, ctx} = Enum.map_reduce(first ++ later, ctx, fn {{a, t}, i}, ctx -> {core, ctx} = check(a, t, ctx); {{i, core}, ctx} end)
    cores = checked |> Enum.sort() |> Enum.map(&elem(&1, 1))
    {[:app, head | cores], rtype, ctx}
  end

  defp function_arg?({:fn, _, _, _}, _ctx), do: true
  defp function_arg?(op, ctx) when is_atom(op), do: op in @operators and not Map.has_key?(ctx.env, op)
  defp function_arg?(_e, _ctx), do: false

  defp arrow(ftype, n, f, ctx) do
    case Types.zonk(ftype, ctx.s) do
      [:->, ptypes, _eff, rtype] when length(ptypes) == n ->
        {ptypes, rtype, ctx}

      [:->, ptypes, _eff, _rtype] ->
        fail(ctx, "wrong number of arguments", note: "`#{f}` takes #{length(ptypes)} argument(s), but #{n} were given")

      {:meta, _} = m ->
        {ps, ctx} = Enum.map_reduce(1..n//1, ctx, fn _, ctx -> meta(ctx) end)
        {r, ctx} = meta(ctx)
        ctx = unify!(m, [:->, ps, [:eff, :closed], r], ctx)
        {ps, r, ctx}

      other ->
        fail(ctx, "`#{f}` is not a function: it is #{Types.format(other)}")
    end
  end

  defp instantiate(name, [:forall, binders, type], ctx) do
    {metas, ctx} = Enum.map_reduce(binders, ctx, fn _, ctx -> meta(ctx) end)
    type = Types.subst_tvars(type, Map.new(Enum.zip(Enum.map(binders, &hd/1), metas)))
    {[:inst, name | metas], type, ctx}
  end

  defp instantiate(name, type, ctx), do: {name, type, ctx}

  defp prelude_ref(name, ctx) do
    instantiate(name, Prelude.type(name), %{ctx | prelude: MapSet.put(ctx.prelude, name)})
  end

  defp constructor(c, args, ctx) do
    t = Map.fetch!(ctx.ctors, c)
    %{params: params, members: ctors} = Map.fetch!(ctx.decls, t)
    {^c, fields} = List.keyfind(ctors, c, 0)

    if length(fields) != length(args) do
      fail(ctx, "wrong number of arguments", note: "the constructor `#{c}` takes #{length(fields)}, but #{length(args)} were given")
    end

    {metas, ctx} = Enum.map_reduce(params, ctx, fn _, ctx -> meta(ctx) end)
    inst = Map.new(Enum.zip(params, metas))
    {cores, ctx} = Enum.zip(args, fields) |> Enum.map_reduce(ctx, fn {a, ft}, ctx -> check(a, Types.subst_tvars(ft, inst), ctx) end)
    type = [t | metas]
    {[:inj, type, c | cores], type, ctx}
  end

  defp record(t, args, ctx) do
    %{params: params, members: fields} = Map.fetch!(ctx.decls, t)

    if length(fields) != length(args) do
      fail(ctx, "wrong number of arguments", note: "the record `#{t}` has #{length(fields)} fields, but #{length(args)} were given")
    end

    {metas, ctx} = Enum.map_reduce(params, ctx, fn _, ctx -> meta(ctx) end)
    inst = Map.new(Enum.zip(params, metas))
    {cores, ctx} = Enum.zip(args, fields) |> Enum.map_reduce(ctx, fn {a, {_l, ft}}, ctx -> check(a, Types.subst_tvars(ft, inst), ctx) end)
    type = [t | metas]
    {[:record, type | Enum.zip_with(fields, cores, fn {l, _}, c -> [l, c] end)], type, ctx}
  end

  # --- Builtins ------------------------------------------------------------------------

  defp builtin(op, [a, b], ctx) when is_map_key(@arith, op) do
    {int_op, float_op} = Map.fetch!(@arith, op)
    numeric(op, int_op, float_op, a, b, & &1, ctx)
  end

  defp builtin(op, [a, b], ctx) when is_map_key(@compare, op) do
    {int_op, float_op} = Map.fetch!(@compare, op)
    numeric(op, int_op, float_op, a, b, fn _ -> [:Bool] end, ctx)
  end

  defp builtin(:-, [a], ctx) do
    {core, type, ctx} = synth(a, ctx)

    case Types.zonk(type, ctx.s) do
      :Int -> {[:prim, :neg, core], :Int, ctx}
      :Float -> {[:prim, :fneg, core], :Float, ctx}
      {:meta, _} = m -> {[:prim, {:numeric, :neg, :fneg, m, :-, ctx.loc}, core], m, ctx}
      other -> fail(ctx, "`-` negates a number, but this is #{Types.format(other)}")
    end
  end

  defp builtin(:/, [a, b], ctx) do
    {a, ctx} = as_float(a, ctx)
    {b, ctx} = as_float(b, ctx)
    {[:prim, :fdiv, a, b], :Float, ctx}
  end

  defp builtin(op, [a, b], ctx) when op in [:div, :rem] do
    {a, ctx} = check(a, :Int, ctx)
    {b, ctx} = check(b, :Int, ctx)
    {[:prim, op, a, b], :Int, ctx}
  end

  defp builtin(op, [a, b], ctx) when op in [:==, :!=] do
    {a, type, ctx} = synth(a, ctx)
    {b, ctx} = check(b, type, ctx)
    {[:prim, if(op == :==, do: :eq, else: :ne), a, b], [:Bool], %{ctx | ground: [{type, ctx.loc} | ctx.ground]}}
  end

  defp builtin(:not, [a], ctx) do
    {a, ctx} = check(a, [:Bool], ctx)
    {[:prim, :not, a], [:Bool], ctx}
  end

  defp builtin(:and, [a, b], ctx) do
    {a, ctx} = check(a, [:Bool], ctx)
    {b, ctx} = check(b, [:Bool], ctx)
    {[:if, a, b, [:inj, [:Bool], false]], [:Bool], ctx}
  end

  defp builtin(:or, [a, b], ctx) do
    {a, ctx} = check(a, [:Bool], ctx)
    {b, ctx} = check(b, [:Bool], ctx)
    {[:if, a, [:inj, [:Bool], true], b], [:Bool], ctx}
  end

  defp builtin(:++, [a, b], ctx) do
    {a, ctx} = check(a, :String, ctx)
    {b, ctx} = check(b, :String, ctx)
    {[:prim, :concat, a, b], :String, ctx}
  end

  defp builtin(op, [xs], ctx) when op in [:head, :tail, :length, :empty?] do
    {core, type, ctx} = synth(xs, ctx)
    {elem, ctx} = list_elem(type, ctx)

    result =
      case op do
        :head -> elem
        :tail -> [:List, elem]
        :length -> :Int
        :empty? -> [:Bool]
      end

    {[:prim, op, core], result, ctx}
  end

  defp builtin(:cons, [h, t], ctx), do: cons(h, t, ctx)
  defp builtin(f, _args, ctx) when f in @operators, do: fail(ctx, "wrong number of arguments to `#{f}`")
  defp builtin(:str, _args, _ctx), do: outside_term("the builtin :str")
  defp builtin(f, _args, ctx), do: fail(ctx, "undefined function `#{f}`")

  # Arithmetic and comparison are on Int or on Float. An Int meeting a Float
  # is converted, as Erlang does. An operand of unknown type takes the other's.
  defp numeric(op, int_op, float_op, a, b, result, ctx) do
    {ac, at, ctx} = synth(a, ctx)
    {bc, bt, ctx} = synth(b, ctx)

    case {Types.zonk(at, ctx.s), Types.zonk(bt, ctx.s)} do
      {:Int, :Int} -> {[:prim, int_op, ac, bc], result.(:Int), ctx}
      {:Float, :Float} -> {[:prim, float_op, ac, bc], result.(:Float), ctx}
      {:Int, :Float} -> {[:prim, float_op, [:prim, :"int-to-float", ac], bc], result.(:Float), ctx}
      {:Float, :Int} -> {[:prim, float_op, ac, [:prim, :"int-to-float", bc]], result.(:Float), ctx}
      {{:meta, _} = m, t} when t in [:Int, :Float] -> numeric_again(op, int_op, float_op, ac, bc, t, result, unify!(m, t, ctx))
      {t, {:meta, _} = m} when t in [:Int, :Float] -> numeric_again(op, int_op, float_op, ac, bc, t, result, unify!(m, t, ctx))
      # Both open, as in (fn [a b] (+ a b)) before its call: the operands share
      # a type, and the primitive is chosen when the definition ends.
      {{:meta, _} = m, {:meta, _} = n} -> ctx = unify!(m, n, ctx); {[:prim, {:numeric, int_op, float_op, m, op, ctx.loc}, ac, bc], result.(m), ctx}
      {x, y} -> fail(ctx, "`#{op}` works on Int or Float, not #{Types.format(if x in [:Int, :Float], do: y, else: x)}")
    end
  end

  defp numeric_again(_op, int_op, _float_op, ac, bc, :Int, result, ctx), do: {[:prim, int_op, ac, bc], result.(:Int), ctx}
  defp numeric_again(_op, _int_op, float_op, ac, bc, :Float, result, ctx), do: {[:prim, float_op, ac, bc], result.(:Float), ctx}

  defp as_float(e, ctx) do
    {core, type, ctx} = synth(e, ctx)

    case Types.zonk(type, ctx.s) do
      :Float -> {core, ctx}
      :Int -> {[:prim, :"int-to-float", core], ctx}
      {:meta, _} -> fail(ctx, "cannot tell whether this is an Int or a Float", hint: "annotate it")
      other -> fail(ctx, "`/` divides numbers, not #{Types.format(other)}")
    end
  end

  # --- let (Q36: not generalized) -----------------------------------------------------

  defp let([], body, {:check, type}, ctx), do: check(body, type, ctx)

  defp let([{x, e} | rest], body, mode, ctx) when is_atom(x) do
    {core, type, ctx} = synth(e, ctx)
    {name, ctx} = fresh_name(x, ctx)
    {rest_core, ctx} = in_scope(%{ctx | env: Map.put(ctx.env, x, {name, type})}, fn ctx -> let(rest, body, mode, ctx) end)
    {[:let, [[name, type, core]], rest_core], ctx}
  end

  # `(let [(Point x y) p] ...)` binds by a pattern: a match with one clause.
  defp let([{pattern, e} | rest], body, mode, ctx) do
    {core, type, ctx} = synth(e, ctx)
    {p, binds, ctx} = pattern(pattern, type, ctx)
    {rest_core, ctx} = in_scope(%{ctx | env: Map.merge(ctx.env, binds)}, fn ctx -> let(rest, body, mode, ctx) end)
    {[:match, core, [p, rest_core], [:_, @no_match]], ctx}
  end

  # --- match ---------------------------------------------------------------------------

  # Patterns are checked against the scrutinee's type, which ties an
  # unannotated scrutinee to its patterns' type (D22).
  defp match_clauses(s, stype, clauses, {:check, type}, ctx) do
    {cores, ctx} =
      Enum.map_reduce(clauses, ctx, fn {pat, guard, body}, ctx ->
        {p, binds, ctx} = pattern(pat, stype, ctx)

        in_scope(%{ctx | env: Map.merge(ctx.env, binds)}, fn ctx ->
          case guard do
            nil ->
              {b, ctx} = check(body, type, ctx)
              {[p, b], ctx}

            g ->
              {g, ctx} = check(g, [:Bool], ctx)
              {b, ctx} = check(body, type, ctx)
              {[p, [:when, g], b], ctx}
          end
        end)
      end)

    total? = match?([p, _] when p == :_ or (is_atom(p) and p not in [true, false, nil]), List.last(cores))
    fallback = if total?, do: [], else: [[:_, @no_match]]
    {[:match, s | cores ++ fallback], ctx}
  end

  # pattern(p, τ, ctx) -> {core pattern, %{source name => {core name, type}}, ctx}
  defp pattern(p, type, ctx), do: at(p, ctx, &pattern_(&1, type, &2))

  defp pattern_(:_, _type, ctx), do: {:_, %{}, ctx}

  defp pattern_(b, type, ctx) when is_boolean(b), do: {[:inj, [:Bool], b], %{}, unify!(type, [:Bool], ctx)}

  defp pattern_(x, type, ctx) when is_atom(x) do
    if Map.has_key?(ctx.ctors, x) do
      pattern_({:call, x, [], ctx.loc}, type, ctx)
    else
      {name, ctx} = fresh_name(x, ctx)
      {name, %{x => {name, type}}, ctx}
    end
  end

  defp pattern_(n, type, ctx) when is_integer(n), do: {n, %{}, unify!(type, :Int, ctx)}
  defp pattern_(r, type, ctx) when is_float(r), do: {r, %{}, unify!(type, :Float, ctx)}
  defp pattern_({:string, s}, type, ctx), do: {s, %{}, unify!(type, :String, ctx)}
  defp pattern_({:atom, a}, type, ctx), do: {[:atom, a], %{}, unify!(type, :Atom, ctx)}
  defp pattern_({:unit, _}, type, ctx), do: {[:unit], %{}, unify!(type, :Unit, ctx)}

  defp pattern_({:bracket, []}, type, ctx) do
    {elem, ctx} = list_elem(type, ctx)
    {[:inj, [:List, elem], :Nil], %{}, ctx}
  end

  defp pattern_({:bracket, {:cons, h, t}}, type, ctx) do
    {elem, ctx} = list_elem(type, ctx)
    {hp, hb, ctx} = pattern(h, elem, ctx)
    {tp, tb, ctx} = pattern(t, [:List, elem], ctx)
    {[:inj, [:List, elem], :Cons, hp, tp], Map.merge(hb, tb), ctx}
  end

  defp pattern_({:bracket, ps}, type, ctx) when is_list(ps) do
    {elem, ctx} = list_elem(type, ctx)
    {cores, binds, ctx} = patterns(ps, List.duplicate(elem, length(ps)), ctx)
    list_type = [:List, elem]
    {List.foldr(cores, [:inj, list_type, :Nil], fn p, acc -> [:inj, list_type, :Cons, p, acc] end), binds, ctx}
  end

  defp pattern_({:tuple, ps, _loc}, type, ctx), do: pattern_({:tuple_pattern, ps}, type, ctx)

  defp pattern_({:tuple_pattern, ps}, type, ctx) do
    {metas, ctx} = Enum.map_reduce(ps, ctx, fn _, ctx -> meta(ctx) end)
    ctx = unify!(type, [:Tuple | metas], ctx)
    {cores, binds, ctx} = patterns(ps, metas, ctx)
    {[:tuple | cores], binds, ctx}
  end

  # (r @ (Ok v)) binds r to the whole value and matches it against (Ok v).
  defp pattern_({:call, x, [:@, p], _loc}, type, ctx) when is_atom(x) do
    {name, ctx} = fresh_name(x, ctx)
    {core, binds, ctx} = pattern(p, type, ctx)
    {[:as, name, core], Map.put(binds, x, {name, type}), ctx}
  end

  defp pattern_({:call, c, ps, _loc}, type, ctx) when is_atom(c) do
    cond do
      Map.has_key?(ctx.ctors, c) ->
        t = Map.fetch!(ctx.ctors, c)
        %{params: params, members: ctors} = Map.fetch!(ctx.decls, t)
        {^c, fields} = List.keyfind(ctors, c, 0)
        if length(fields) != length(ps), do: fail(ctx, "the constructor `#{c}` has #{length(fields)} fields, but the pattern has #{length(ps)}")
        {metas, ctx} = Enum.map_reduce(params, ctx, fn _, ctx -> meta(ctx) end)
        ctx = unify!(type, [t | metas], ctx)
        inst = Map.new(Enum.zip(params, metas))
        {cores, binds, ctx} = patterns(ps, Enum.map(fields, &Types.subst_tvars(&1, inst)), ctx)
        {[:inj, [t | metas], c | cores], binds, ctx}

      Map.has_key?(ctx.decls, c) and ctx.decls[c].kind == :record ->
        %{params: params, members: fields} = Map.fetch!(ctx.decls, c)
        if length(fields) != length(ps), do: fail(ctx, "the record `#{c}` has #{length(fields)} fields, but the pattern has #{length(ps)}")
        {metas, ctx} = Enum.map_reduce(params, ctx, fn _, ctx -> meta(ctx) end)
        ctx = unify!(type, [c | metas], ctx)
        inst = Map.new(Enum.zip(params, metas))
        {cores, binds, ctx} = patterns(ps, Enum.map(fields, fn {_l, ft} -> Types.subst_tvars(ft, inst) end), ctx)
        {[:record, [c | metas] | Enum.zip_with(fields, cores, fn {l, _}, p -> [l, p] end)], binds, ctx}

      true ->
        fail(ctx, "`#{c}` is not a constructor")
    end
  end

  defp pattern_(other, _type, _ctx), do: outside_term("the pattern #{inspect(other, limit: 4)}")

  defp patterns(ps, types, ctx) do
    Enum.zip(ps, types)
    |> Enum.reduce({[], %{}, ctx}, fn {p, t}, {cores, binds, ctx} ->
      {core, more, ctx} = pattern(p, t, ctx)
      {cores ++ [core], Map.merge(binds, more), ctx}
    end)
  end

  # --- Helpers ---------------------------------------------------------------------------

  defp unify!(expected, actual, ctx) do
    case Types.unify(expected, actual, ctx.s) do
      {:ok, s} ->
        %{ctx | s: s}

      :error ->
        e = Types.zonk(expected, ctx.s)
        a = Types.zonk(actual, ctx.s)
        fail(ctx, "type mismatch", note: "expected #{Types.format(e)}, found #{Types.format(a)}")
    end
  end

  defp meta(ctx), do: {{:meta, ctx.next}, %{ctx | next: ctx.next + 1}}

  # Binder names are unique within a definition (liquid-core.md §10.12).
  defp fresh_name(:_, ctx), do: {:_, ctx}

  defp fresh_name(x, ctx) do
    name =
      if MapSet.member?(ctx.names, x),
        do: Stream.iterate(2, &(&1 + 1)) |> Stream.map(&:"#{x}'#{&1}") |> Enum.find(&(not MapSet.member?(ctx.names, &1))),
        else: x

    {name, %{ctx | names: MapSet.put(ctx.names, name)}}
  end


  defp param_name({:var, name, _type}), do: name
  defp param_name(name) when is_atom(name), do: name

  # Names bound inside a scope do not outlive it; what the scope learned
  # about types (the substitution, fresh counters) does.
  defp in_scope(ctx, fun) do
    case fun.(ctx) do
      {core, ctx2} -> {core, %{ctx2 | env: ctx.env}}
      {core, type, ctx2} -> {core, type, %{ctx2 | env: ctx.env}}
    end
  end

  # Track the nearest source location, for errors.
  defp at(e, ctx, fun) do
    loc = loc_of(e) || ctx.loc

    case fun.(e, %{ctx | loc: loc}) do
      {a, ctx2} -> {a, %{ctx2 | loc: ctx.loc}}
      {a, b, ctx2} -> {a, b, %{ctx2 | loc: ctx.loc}}
    end
  end

  defp loc_of(e) when is_tuple(e) and tuple_size(e) > 1 do
    case elem(e, tuple_size(e) - 1) do
      %Loc{} = loc -> loc
      _ -> nil
    end
  end

  defp loc_of(_e), do: nil

  defp span(%Loc{} = loc), do: Error.span_from_loc(loc)
  defp span(_), do: nil

  defp fail(ctx, message, opts \\ []) do
    throw({:elab, Error.new(message, Keyword.merge([span: span(ctx.loc)], opts))})
  end

  defp outside_term(reason), do: throw({:outside, reason})
end
