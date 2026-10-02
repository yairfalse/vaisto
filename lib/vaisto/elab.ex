defmodule Vaisto.Elab do
  @moduledoc """
  The Phase 1a elaborator (liquid-types.md §8, §9; RFC §9.1): surface AST to
  Liquid Core, bidirectionally.

  Introduction forms are *checked* against the type the context supplies,
  and elimination forms *synthesize* one. When a synthesized type meets an
  expected one they must be equal: there is no other subsumption, and no
  coercion. Inference is local unification: it only chooses the type
  arguments at a use site, inside one definition.

    * Every top-level definition carries a signature (Q35). A lowercase
      keyword such as `:a` is a type variable, quantified over the signature
      and rigid in its body.
    * Local `let`s are not generalized (Q36).
    * A lambda takes its parameter types from the arrow it is checked
      against, or from its own annotations: `(fn [x :int] ...)`.
    * There is no `:any`, and no guessing. A type local inference cannot
      determine is an error at the place it arises; `(the τ e)` states it.
    * Arithmetic and comparison work on Int or on Float, never on a mix.
      The primitive is chosen once the operands' type is known, as a class
      instance is (§9); `/` divides Floats.
    * A `match` must be exhaustive, a `let` pattern irrefutable, and a guard
      guard-safe (liquid-core.md §6.6, §10.8). The only partiality is what the
      program writes: a guard on a definition.

  Arguments are checked in two passes, first-order ones first, so that type
  arguments are known before a lambda argument is checked against them
  (Pierce and Turner, "Local type inference", 2000).

  Core Lint re-checks the output with its own implementation of the same
  rules (§8.1). A program the elaborator accepts and Lint rejects is an
  elaborator bug.

  This is slice 1 of Phase 1a: the fragment of Core version 0 the Phase 0
  adapter covers. Type classes, processes, externs and calls across modules
  are outside it, and `module/2` says so rather than guessing:

    * `{:ok, core_module}`;
    * `{:error, errors}`, a list of `%Vaisto.Error{}`;
    * `{:outside, reason}`.

  The `:suggest` option maps definition names to Core types, used as the
  signature of a definition that has none: the migration of §13, where the
  old engine suggests and this one verifies.
  """

  alias Vaisto.Error
  alias Vaisto.Elab.{Exhaustive, Types}
  alias Vaisto.Liquid.Prelude
  alias Vaisto.Parser.Loc

  @higher_order %{map: :"prelude.map", filter: :"prelude.filter", fold: :"prelude.fold", flat_map: :"prelude.flat_map"}
  @arith %{:+ => {:add, :fadd}, :- => {:sub, :fsub}, :* => {:mul, :fmul}}
  @compare %{:< => {:lt, :flt}, :> => {:gt, :fgt}, :<= => {:le, :fle}, :>= => {:ge, :fge}}
  @operators Map.keys(@arith) ++ Map.keys(@compare) ++ [:/, :div, :rem, :==, :!=, :and, :or, :++, :not]
  @no_match [:perform, :crash, [:inj, [:Reason], :no_match]]

  # Names a declaration may not take: Core's reserved and built-in types.
  @reserved_types [:Int, :Float, :String, :Atom, :Ref, :Dyn, :Unit, :Tuple, :Record, :Pid, :Map, :Bool, :List, :Reason, :Fn]
  @primitive_names [:int, :float, :string, :bool, :atom, :unit, :any, :num]

  # decls: type => %{params, kind, members}; ctors: constructor => type;
  # sigs: definition => Core type; env: source name => {core name, type};
  # tvars: the signature's type variables; s: the substitution; origins: what
  # each metavariable stands for, and where; numeric, ground, matches: checks
  # that wait for the end of the definition; loc: the nearest location.
  defstruct decls: %{}, ctors: %{}, sigs: %{}, env: %{}, tvars: MapSet.new(), s: %{}, next: 0, origins: %{},
            names: MapSet.new(), prelude: MapSet.new(), loc: nil, ground: [], matches: []

  @doc "Elaborate a parsed program into a Core module. Options: `:suggest`, `:name`."
  @spec module(term(), keyword()) :: {:ok, term()} | {:error, [Error.t()]} | {:outside, String.t()}
  def module(ast, opts \\ []) do
    forms = if is_list(ast), do: ast, else: [ast]

    with :ok <- parse_errors(forms),
         {:ok, ctx} <- declarations(forms),
         {:ok, ctx, defs} <- signatures(forms, ctx, Keyword.get(opts, :suggest, %{})) do
      {items, errors, prelude} =
        Enum.reduce(defs, {[], [], MapSet.new()}, fn {form, sig}, {items, errors, prelude} ->
          case definition(form, sig, ctx) do
            {:ok, item, used} -> {items ++ [item], errors, MapSet.union(prelude, used)}
            {:error, error} -> {items, errors ++ [error], prelude}
          end
        end)

      if errors == [] do
        {:ok, [:module, Keyword.get(opts, :name, :Main), [:"core-version", 0] | core_decls(ctx) ++ Prelude.defs(prelude, Map.keys(ctx.sigs)) ++ items]}
      else
        {:error, errors}
      end
    end
  catch
    {:outside, reason} -> {:outside, reason}
  end

  # The parser reports a syntax error as an {:error, error, loc} node.
  defp parse_errors(forms) do
    case find_parse_errors(forms) do
      [] -> :ok
      errors -> {:error, errors}
    end
  end

  defp find_parse_errors({:error, %Error{} = e, _loc}), do: [e]
  defp find_parse_errors(t) when is_tuple(t), do: t |> Tuple.to_list() |> find_parse_errors()
  defp find_parse_errors(l) when is_list(l), do: Enum.flat_map(l, &find_parse_errors/1)
  defp find_parse_errors(_), do: []

  # --- Declarations ----------------------------------------------------------------

  defp declarations(forms) do
    deftypes = for {:deftype, t, body, loc} <- forms, do: {t, body, loc}
    arities = Map.new(deftypes, fn {t, body, _} -> {t, length(decl_params(body))} end)

    {decls, ctors, errors} =
      Enum.reduce(deftypes, {%{}, %{}, []}, fn {t, body, loc}, {decls, ctors, errors} ->
        case decl(t, body, arities, decls, ctors) do
          {:ok, d} ->
            new_ctors = if d.kind == :sum, do: for({c, _} <- d.members, into: %{}, do: {c, t}), else: %{}
            {Map.put(decls, t, d), Map.merge(ctors, new_ctors), errors}

          {:error, message} ->
            {decls, ctors, errors ++ [Error.new(message, span: span(loc))]}
        end
      end)

    if errors == [], do: {:ok, %__MODULE__{decls: decls, ctors: ctors}}, else: {:error, errors}
  end

  # A sum's fields and a record's field types are annotations; a bare
  # lowercase name in a sum field, as in (Ok v), is a type variable. The
  # declaration's parameters are its type variables in order of appearance.
  defp decl_params({:sum, variants}), do: variants |> Enum.flat_map(fn {_c, fs} -> Enum.flat_map(fs, &field_tvars/1) end) |> Enum.uniq()
  defp decl_params({:product, fields}), do: fields |> Enum.flat_map(fn {_l, t} -> field_tvars(t) end) |> Enum.uniq()

  defp field_tvars(t) when is_atom(t), do: if(lowercase?(t), do: [t], else: [])
  defp field_tvars({:atom, t}), do: if(lowercase?(t) and t not in @primitive_names, do: [t], else: [])
  defp field_tvars({:call, _t, args, _loc}), do: Enum.flat_map(args, &field_tvars/1)
  defp field_tvars(_), do: []

  defp decl(t, body, arities, decls, ctors) do
    params = decl_params(body)
    kind = if match?({:sum, _}, body), do: :sum, else: :record

    names =
      case body do
        {:sum, variants} -> Enum.map(variants, &elem(&1, 0))
        {:product, fields} -> Enum.map(fields, &elem(&1, 0))
      end

    cond do
      t in @reserved_types -> throw({:type_error, "`#{t}` is a built-in type and cannot be declared"})
      Map.has_key?(decls, t) -> throw({:type_error, "the type `#{t}` is declared twice"})
      Map.has_key?(ctors, t) -> throw({:type_error, "`#{t}` is already a constructor"})
      Enum.find(params, &(&1 in @primitive_names)) -> throw({:type_error, "`#{Enum.find(params, &(&1 in @primitive_names))}` is a type, so it cannot name a type variable: write :#{Enum.find(params, &(&1 in @primitive_names))}"})
      length(Enum.uniq(names)) != length(names) -> throw({:type_error, "`#{t}` names a #{if kind == :sum, do: "constructor", else: "field"} twice"})
      kind == :sum and Enum.find(names, &(Map.has_key?(ctors, &1) or Map.has_key?(decls, &1))) -> throw({:type_error, "the constructor `#{Enum.find(names, &(Map.has_key?(ctors, &1) or Map.has_key?(decls, &1)))}` is already defined"})
      true -> :ok
    end

    members =
      case body do
        {:sum, variants} -> for {c, fs} <- variants, do: {c, Enum.map(fs, &field_type(&1, arities))}
        {:product, fields} -> for {l, ft} <- fields, do: {l, field_type(ft, arities)}
      end

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
      Enum.reduce(forms, {%{}, [], []}, fn form, acc ->
        case form do
          {:defn, f, params, _body, ret, loc} -> signature(f, params, ret, loc, arities, suggest, form, acc)
          {:defn, f, params, _body, ret, _guard, loc} -> signature(f, params, ret, loc, arities, suggest, form, acc)
          {:defn_multi, f, _clauses, loc} -> multi_signature(f, loc, suggest, form, acc)
          {:deftype, _, _, _} -> acc
          {:ns, _, _} -> acc
          other -> outside(other)
        end
      end)

    if errors == [], do: {:ok, %{ctx | sigs: sigs}, defs}, else: {:error, errors}
  end

  defp signature(f, params, ret, loc, arities, suggest, form, acc) do
    annotated = Enum.map(params, fn {p, t} -> {p, Types.from_surface(t, arities)} end)
    result = Types.from_surface(ret, arities)

    case {Enum.find(annotated, &match?({_, {:error, _}}, &1)), result} do
      {nil, {:ok, rtype}} ->
        add_signature(f, Types.generalize([:->, Enum.map(annotated, fn {_, {:ok, t}} -> t end), [:eff, :closed], rtype]), loc, form, acc)

      {missing, _} ->
        case Map.fetch(suggest, f) do
          {:ok, type} ->
            add_signature(f, type, loc, form, acc)

          :error ->
            what =
              case missing do
                {p, {:error, message}} -> "the parameter `#{p}`: #{message}"
                nil -> "the result: #{elem(result, 1)}"
              end

            add_error(acc, Error.new("`#{f}` needs a signature", span: span(loc), note: what, hint: "annotate every parameter and the result, as in (defn #{f} [x :int] :int ...)"))
        end
    end
  end

  defp multi_signature(f, loc, suggest, form, acc) do
    case Map.fetch(suggest, f) do
      {:ok, type} -> add_signature(f, type, loc, form, acc)
      :error -> add_error(acc, Error.new("`#{f}` needs a signature", span: span(loc), note: "a multi-clause function has no syntax for one yet"))
    end
  end

  defp add_signature(f, type, loc, form, {sigs, defs, errors} = acc) do
    cond do
      Map.has_key?(sigs, f) -> add_error(acc, Error.new("`#{f}` is defined twice", span: span(loc)))
      f == :the -> add_error(acc, Error.new("`the` is the type ascription form and cannot be redefined", span: span(loc)))
      true -> {Map.put(sigs, f, type), defs ++ [{form, type}], errors}
    end
  end

  defp add_error({sigs, defs, errors}, error), do: {sigs, defs, errors ++ [error]}

  defp outside({tag, name, _, _}) when tag in [:process, :defclass], do: throw({:outside, "#{tag} #{name}"})
  defp outside(form) when is_tuple(form), do: throw({:outside, "the form #{inspect(elem(form, 0))}"})
  defp outside(form), do: throw({:outside, "#{inspect(form, limit: 4)}"})

  # --- Definitions -------------------------------------------------------------------

  defp definition(form, sig, ctx) do
    {name, loc} = {elem(form, 1), elem(form, tuple_size(form) - 1)}
    [:->, ptypes, _eff, rtype] = arrow = open(sig)
    reserved = MapSet.new(Map.keys(ctx.sigs) ++ Prelude.names())
    ctx = %{ctx | names: reserved, tvars: MapSet.new(Types.tvars(arrow)), loc: loc}

    {fun, ctx} =
      case form do
        {:defn, _f, params, body, _ret, _loc} ->
          {core_params, ctx} = bind_params(Enum.map(params, &elem(&1, 0)), ptypes, ctx)
          {body, ctx} = check(body, rtype, ctx)
          {[:fn, core_params, body], ctx}

        # A guard on a definition is the one partiality a program writes: the
        # call fails as no clause matching does when the guard is false.
        {:defn, _f, params, body, _ret, guard, _loc} ->
          {core_params, ctx} = bind_params(Enum.map(params, &elem(&1, 0)), ptypes, ctx)
          {g, ctx} = guard(guard, ctx)
          {body, ctx} = check(body, rtype, ctx)
          {[:fn, core_params, [:match, [:unit], [:_, [:when, g], body], [:_, @no_match]]], ctx}

        {:defn_multi, _f, clauses, _loc} ->
          [ptype] = ptypes
          {arg, ctx} = fresh_name(:arg, ctx)
          {m, ctx} = match_clauses(arg, ptype, clauses, rtype, ctx)
          {[:fn, [[arg, ptype]], m], ctx}
      end

    {fun, ctx} = finish(fun, ctx)
    {:ok, [:def, name, sig, fun], ctx.prelude}
  catch
    {:elab, %Error{} = error} -> {:error, error}
  end

  defp open([:forall, _binders, arrow]), do: arrow
  defp open(arrow), do: arrow

  defp bind_params(names, types, ctx) do
    {pairs, ctx} =
      Enum.zip(names, types)
      |> Enum.map_reduce(ctx, fn {p, t}, ctx ->
        {x, ctx} = fresh_name(p, ctx)
        {{p, x, t}, ctx}
      end)

    case Enum.find(Enum.frequencies(for({p, _, _} <- pairs, p != :_, do: p)), fn {_, n} -> n > 1 end) do
      {p, _} -> fail(ctx, "the parameter `#{p}` appears twice")
      nil -> :ok
    end

    env = for {p, x, t} <- pairs, p != :_, into: ctx.env, do: {p, {x, t}}
    {for({_, x, t} <- pairs, do: [x, t]), %{ctx | env: env}}
  end

  # The end of a definition: the checks that waited for its types.
  #   1. an Int/Float operator takes the primitive of its operands' type, as
  #      a class instance is chosen once its type is known;
  #   2. every type is determined, or that is an error where it arose;
  #   3. every match is exhaustive and every let pattern irrefutable;
  #   4. equality compares ground data (liquid-core.md §10.11).
  defp finish(term, ctx) do
    term = resolve_numeric(term, ctx)
    term = Types.zonk(term, ctx.s)
    if Types.unsolved?(term), do: undetermined(term, ctx)

    for {clauses, stype, kind, loc} <- Enum.reverse(ctx.matches) do
      stype = Types.zonk(stype, ctx.s)

      if i = Exhaustive.redundant(clauses, stype, ctx.decls) do
        fail(%{ctx | loc: loc}, "clause #{i} of this match can never be chosen", note: "the clauses before it match every value it does")
      end

      case Exhaustive.missing(for({p, false} <- clauses, do: p), stype, ctx.decls) do
        nil -> :ok
        witness when kind == :let -> fail(%{ctx | loc: loc}, "this pattern can fail to match", note: "it does not match #{witness}", hint: "use match, with a clause for every case")
        witness -> fail(%{ctx | loc: loc}, "this match is not exhaustive", note: "no clause matches #{witness}")
      end
    end

    for {type, loc} <- ctx.ground do
      t = Types.zonk(type, ctx.s)
      unless ground?(t, ctx.decls, MapSet.new()), do: fail(%{ctx | loc: loc}, "`==` compares ground data, but this is #{Types.format(t)}", note: "a type variable needs an Eq dictionary, which comes with type classes; a function cannot be compared")
    end

    {term, ctx}
  end

  defp resolve_numeric({:numeric, int_op, float_op, type, op, loc}, ctx) do
    case Types.zonk(type, ctx.s) do
      :Int -> int_op
      :Float -> float_op
      {:meta, _} -> fail(%{ctx | loc: loc}, "cannot tell whether `#{op}` works on Int or Float here", hint: "state an operand's type with (the :int ...) or (the :float ...)")
      other -> fail(%{ctx | loc: loc}, "`#{op}` works on Int or Float, not #{Types.format(other)}")
    end
  end

  defp resolve_numeric(list, ctx) when is_list(list), do: Enum.map(list, &resolve_numeric(&1, ctx))
  defp resolve_numeric(t, _ctx), do: t

  # The first undetermined type, reported where its metavariable was made.
  defp undetermined(term, ctx) do
    n = term |> metas() |> Enum.min()
    {loc, what} = Map.get(ctx.origins, n, {ctx.loc, "a type here"})
    fail(%{ctx | loc: loc}, "#{what} cannot be determined", hint: "state it with (the τ e), for example (the (List :int) [])")
  end

  defp metas({:meta, n}), do: [n]
  defp metas(list) when is_list(list), do: Enum.flat_map(list, &metas/1)
  defp metas(_), do: []

  # Ground data: no type variable and no function, through declarations too.
  defp ground?(t, _decls, _seen) when t in [:Int, :Float, :String, :Atom, :Unit], do: true
  defp ground?([:Bool], _decls, _seen), do: true
  defp ground?([:List, a], decls, seen), do: ground?(a, decls, seen)
  defp ground?([:Tuple | ts], decls, seen), do: Enum.all?(ts, &ground?(&1, decls, seen))

  defp ground?([t | args] = type, decls, seen) when is_atom(t) and is_map_key(decls, t) do
    if MapSet.member?(seen, type) do
      true
    else
      %{params: params, kind: kind, members: members} = Map.fetch!(decls, t)
      inst = Map.new(Enum.zip(params, args))

      # A sum's member carries a list of field types; a record's, one type.
      fields =
        case kind do
          :sum -> Enum.flat_map(members, &elem(&1, 1))
          :record -> Enum.map(members, &elem(&1, 1))
        end

      Enum.all?(args, &ground?(&1, decls, seen)) and Enum.all?(fields, &ground?(Types.subst_tvars(&1, inst), decls, MapSet.put(seen, type)))
    end
  end

  defp ground?(_type, _decls, _seen), do: false

  # --- Checking (⇐) ------------------------------------------------------------------

  defp check(e, type, ctx), do: at(e, ctx, &check_(&1, type, &2))

  defp check_({:fn, params, body, _loc} = e, type, ctx) do
    params = lambda_params(params, ctx)

    case Types.zonk(type, ctx.s) do
      [:->, ptypes, _eff, rtype] when length(ptypes) == length(params) ->
        ctx = Enum.zip(params, ptypes) |> Enum.reduce(ctx, fn {{_p, ann}, t}, ctx -> if ann, do: unify!(t, ann, ctx), else: ctx end)
        {core_params, ctx} = bind_params(Enum.map(params, &elem(&1, 0)), ptypes, ctx)
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

  defp check_({:let, bindings, body, _loc}, type, ctx), do: let(bindings, body, type, ctx)
  defp check_({:do, [], loc}, type, ctx), do: subsume({:unit, loc}, type, ctx)
  defp check_({:do, [e], _loc}, type, ctx), do: check(e, type, ctx)

  defp check_({:do, [e | rest], loc}, type, ctx) do
    {core, t, ctx} = synth(e, ctx)
    {rest, ctx} = check({:do, rest, loc}, type, ctx)
    {[:let, [[:_, t, core]], rest], ctx}
  end

  defp check_({:match, s, clauses, _loc}, type, ctx) do
    {s, stype, ctx} = synth(s, ctx)
    match_clauses(s, stype, Enum.map(clauses, fn {p, b} -> {p, nil, b} end), type, ctx)
  end

  defp check_({:list, elems, _loc}, type, ctx), do: check_({:bracket, elems}, type, ctx)

  defp check_({:bracket, elems}, type, ctx) when is_list(elems) do
    {elem_type, ctx} = list_elem(type, "the element type of this list", ctx)
    {cores, ctx} = Enum.map_reduce(elems, ctx, &check(&1, elem_type, &2))
    list_type = [:List, elem_type]
    {List.foldr(cores, [:inj, list_type, :Nil], fn h, t -> [:inj, list_type, :Cons, h, t] end), ctx}
  end

  defp check_({:try, body, [{:error, e, handler}], nil, _loc}, type, ctx) do
    unless is_atom(e) and not is_boolean(e) and e != nil, do: fail(ctx, "a catch clause binds the reason to a name, as in [catch [:error e ...]]")
    {body, ctx} = check(body, type, ctx)
    {x, ctx} = fresh_name(e, ctx)
    env = if e == :_, do: ctx.env, else: Map.put(ctx.env, e, {x, [:Reason]})
    {handler, ctx} = in_scope(%{ctx | env: env}, fn ctx -> check(handler, type, ctx) end)
    {[:"handle-crash", body, [[x, [:Reason]], handler]], ctx}
  end

  # An operator or a constructor given where a function is expected is
  # expanded to one: (fold + 0 xs), (map Some xs).
  defp check_(f, type, ctx) when is_atom(f) and not is_map_key(ctx.env, f) do
    with [:->, ptypes, _eff, _r] <- Types.zonk(type, ctx.s),
         true <- f in @operators or (Map.has_key?(ctx.ctors, f) and fields(ctx, f) != []) do
      params = for i <- 1..length(ptypes), do: :"#{f}_arg#{i}"
      check({:fn, params, {:call, f, params, ctx.loc}, ctx.loc}, type, ctx)
    else
      _ -> subsume(f, type, ctx)
    end
  end

  defp check_(e, type, ctx), do: subsume(e, type, ctx)

  # A synthesized type meets an expected one: they must be equal (§8.1).
  defp subsume(e, expected, ctx) do
    {core, actual, ctx} = synth(e, ctx)
    {core, unify!(expected, actual, ctx)}
  end

  defp list_elem(type, what, ctx) do
    {m, ctx} = meta(what, ctx)
    {m, unify!(type, [:List, m], ctx)}
  end

  # --- Synthesis (⇒) -----------------------------------------------------------------

  defp synth(e, ctx), do: at(e, ctx, &synth_/2)

  defp synth_(n, ctx) when is_integer(n), do: {n, :Int, ctx}
  defp synth_(r, ctx) when is_float(r), do: {r, :Float, ctx}
  defp synth_(b, ctx) when is_boolean(b), do: {[:inj, [:Bool], b], [:Bool], ctx}
  defp synth_(nil, ctx), do: {[:unit], :Unit, ctx}
  defp synth_({:unit, _loc}, ctx), do: {[:unit], :Unit, ctx}
  defp synth_({:string, s}, ctx), do: {s, :String, ctx}
  defp synth_({:atom, a}, ctx), do: {[:atom, a], :Atom, ctx}
  defp synth_(:_, ctx), do: fail(ctx, "`_` matches anything and binds nothing, so it cannot be used as a value")

  defp synth_(x, ctx) when is_atom(x) do
    cond do
      Map.has_key?(ctx.env, x) ->
        {name, type} = Map.fetch!(ctx.env, x)
        {name, type, ctx}

      Map.has_key?(ctx.sigs, x) ->
        instantiate(x, Map.fetch!(ctx.sigs, x), "the type arguments of `#{x}`", ctx)

      Map.has_key?(@higher_order, x) ->
        name = Map.fetch!(@higher_order, x)
        instantiate(name, Prelude.type(name), "the type arguments of `#{x}`", %{ctx | prelude: MapSet.put(ctx.prelude, name)})

      Map.has_key?(ctx.ctors, x) and fields(ctx, x) == [] ->
        constructor(x, [], ctx)

      Map.has_key?(ctx.ctors, x) or x in @operators ->
        fail(ctx, "`#{x}` is used as a value where its function type is not known", hint: "pass it where a function is expected, or wrap it in (fn ...)")

      true ->
        fail(ctx, "undefined variable `#{x}`")
    end
  end

  defp synth_({:fn, params, body, _loc}, ctx) do
    params = lambda_params(params, ctx)

    {ptypes, ctx} =
      Enum.map_reduce(params, ctx, fn
        {_p, ann}, ctx when ann != nil -> {ann, ctx}
        {p, nil}, ctx -> meta("the type of the parameter `#{p}` of this lambda", ctx)
      end)

    {core_params, ctx} = bind_params(Enum.map(params, &elem(&1, 0)), ptypes, ctx)
    {body, rtype, ctx} = in_scope(ctx, fn ctx -> synth(body, ctx) end)
    {[:fn, core_params, body], [:->, ptypes, [:eff, :closed], rtype], ctx}
  end

  defp synth_({:if, c, a, b, _loc}, ctx) do
    {c, ctx} = check(c, [:Bool], ctx)
    {a, type, ctx} = synth(a, ctx)
    {b, ctx} = check(b, type, ctx)
    {[:if, c, a, b], type, ctx}
  end

  defp synth_({:try, _, [{:error, _, _}], nil, _} = e, ctx), do: synth_by_check(e, "the type of this try", ctx)
  defp synth_({:try, _, _, _, _}, _ctx), do: outside_term("try with after, or catching a class other than :error")

  defp synth_({tag, _, _, _} = e, ctx) when tag in [:let, :match], do: synth_by_check(e, "the type of this #{tag}", ctx)
  defp synth_({:do, _, _} = e, ctx), do: synth_by_check(e, "the type of this do", ctx)

  defp synth_({:list, elems, _loc}, ctx), do: synth_({:bracket, elems}, ctx)

  defp synth_({:bracket, elems} = e, ctx) when is_list(elems) do
    {m, ctx} = meta("the element type of this list", ctx)
    {core, ctx} = check(e, [:List, m], ctx)
    {core, [:List, m], ctx}
  end

  defp synth_({:bracket, {:cons, h, t}}, ctx), do: cons(h, t, ctx)
  defp synth_({:tuple, [], _loc}, ctx), do: {[:unit], :Unit, ctx}
  defp synth_({:tuple, es, _loc}, ctx), do: tuple(es, ctx)
  defp synth_({:tuple_pattern, []}, ctx), do: {[:unit], :Unit, ctx}
  defp synth_({:tuple_pattern, es}, ctx), do: tuple(es, ctx)

  defp synth_({:field_access, e, field, _loc}, ctx) do
    {core, type, ctx} = synth(e, ctx)
    zonked = Types.zonk(type, ctx.s)

    with [t | args] when is_atom(t) and is_map_key(ctx.decls, t) <- zonked,
         %{kind: :record, params: params, members: fs} <- Map.fetch!(ctx.decls, t) do
      case List.keyfind(fs, field, 0) do
        {^field, ft} -> {[:select, core, field], Types.subst_tvars(ft, Map.new(Enum.zip(params, args))), ctx}
        nil -> fail(ctx, "the record `#{t}` has no field `#{field}`")
      end
    else
      {:meta, _} -> fail(ctx, "the record type here cannot be determined", hint: "state it with (the :T ...); reading a field of any record that has it (row polymorphism) comes later")
      _ -> fail(ctx, "`.` reads a field of a record, but this is #{Types.format(zonked)}")
    end
  end

  defp synth_({:call, f, args, _loc}, ctx), do: call(f, args, ctx)
  defp synth_(other, _ctx) do
    head = if is_tuple(other), do: elem(other, 0), else: other
    outside_term("the form #{inspect(head, limit: 3)}")
  end

  defp synth_by_check(e, what, ctx) do
    {m, ctx} = meta(what, ctx)
    {core, ctx} = check(e, m, ctx)
    {core, m, ctx}
  end

  defp tuple(es, ctx) do
    {pairs, ctx} = Enum.map_reduce(es, ctx, fn e, ctx -> {c, t, ctx} = synth(e, ctx); {{c, t}, ctx} end)
    {[:tuple | Enum.map(pairs, &elem(&1, 0))], [:Tuple | Enum.map(pairs, &elem(&1, 1))], ctx}
  end

  defp cons(h, t, ctx) do
    {h, htype, ctx} = synth(h, ctx)
    {t, ctx} = check(t, [:List, htype], ctx)
    {[:inj, [:List, htype], :Cons, h, t], [:List, htype], ctx}
  end

  # Lambda parameters: names, each optionally annotated, as defn's are (D20).
  defp lambda_params(raw, ctx), do: lambda_params(raw, ctx, [])

  defp lambda_params([], _ctx, acc), do: Enum.reverse(acc)
  defp lambda_params([{:var, x, _t} | rest], ctx, acc), do: lambda_params(rest, ctx, [{x, nil} | acc])

  defp lambda_params([x, ann | rest], ctx, acc) when is_atom(x) and not is_boolean(x) and x != nil do
    if annotation?(ann) do
      lambda_params(rest, ctx, [{x, surface_type(ann, ctx)} | acc])
    else
      lambda_params([ann | rest], ctx, [{x, nil} | acc])
    end
  end

  defp lambda_params([x], ctx, acc) when is_atom(x) and not is_boolean(x) and x != nil, do: lambda_params([], ctx, [{x, nil} | acc])
  defp lambda_params(_other, ctx, _acc), do: fail(ctx, "a lambda parameter is a name, optionally followed by its type", hint: "to take a value apart, match on it in the body")

  defp annotation?({:atom, _}), do: true
  defp annotation?({:call, t, _, _}) when is_atom(t), do: capitalized?(t)
  defp annotation?(_), do: false

  # A type written in a body: its type variables are the signature's.
  defp surface_type(ann, ctx) do
    arities = Map.new(ctx.decls, fn {t, d} -> {t, length(d.params)} end)

    case Types.from_surface(ann, arities) do
      {:ok, type} ->
        case Enum.reject(Types.tvars(type), &MapSet.member?(ctx.tvars, &1)) do
          [] -> type
          [a | _] -> fail(ctx, "the type variable `#{a}` is not in scope here", note: "a type in a body may use only its definition's type variables")
        end

      {:error, message} ->
        fail(ctx, message)
    end
  end

  # --- Calls ---------------------------------------------------------------------------

  defp call(:the, [ann, e], ctx) do
    type = surface_type(ann, ctx)
    {core, ctx} = check(e, type, ctx)
    {core, type, ctx}
  end

  defp call(:the, _args, ctx), do: fail(ctx, "a type ascription is written (the τ e)")
  defp call({:qualified, m, f}, _args, _ctx), do: outside_term("the call #{m}/#{f} (externs need decode)")

  defp call(f, args, ctx) when is_atom(f) do
    cond do
      Map.has_key?(ctx.env, f) or Map.has_key?(ctx.sigs, f) or Map.has_key?(@higher_order, f) -> apply_value(f, args, ctx)
      Map.has_key?(ctx.ctors, f) -> constructor(f, args, ctx)
      record?(ctx, f) -> record(f, args, ctx)
      true -> builtin(f, args, ctx)
    end
  end

  # A call whose head is an expression: ((head fs) x), ((fn [x] x) 1).
  defp call(f, args, ctx) when is_tuple(f), do: apply_value(f, args, ctx)
  defp call(f, _args, _ctx), do: outside_term("the call of #{inspect(f, limit: 3)}")

  # The callee synthesizes an arrow and the arguments are checked against its
  # parameters: first-order ones first, then functions (Pierce and Turner).
  defp apply_value(f, args, ctx) do
    {head, ftype, ctx} = synth(f, ctx)
    {ptypes, rtype, ctx} = arrow(ftype, length(args), callee(f), ctx)
    {later, first} = args |> Enum.zip(ptypes) |> Enum.with_index() |> Enum.split_with(fn {{a, _t}, _i} -> function_arg?(a, ctx) end)
    {checked, ctx} = Enum.map_reduce(first ++ later, ctx, fn {{a, t}, i}, ctx -> {core, ctx} = check(a, t, ctx); {{i, core}, ctx} end)
    {[:app, head | checked |> Enum.sort() |> Enum.map(&elem(&1, 1))], rtype, ctx}
  end

  defp function_arg?({:fn, _, _, _}, _ctx), do: true
  defp function_arg?(f, ctx) when is_atom(f), do: not Map.has_key?(ctx.env, f) and (f in @operators or Map.has_key?(ctx.ctors, f))
  defp function_arg?(_e, _ctx), do: false

  defp callee(f) when is_atom(f), do: "`#{f}`"
  defp callee(_f), do: "this function"

  defp arrow(ftype, n, name, ctx) do
    case Types.zonk(ftype, ctx.s) do
      [:->, ptypes, _eff, rtype] when length(ptypes) == n ->
        {ptypes, rtype, ctx}

      [:->, ptypes, _eff, _rtype] ->
        fail(ctx, "wrong number of arguments", note: "#{name} takes #{length(ptypes)}, but #{n} were given")

      {:meta, _} = m ->
        {ps, ctx} = Enum.map_reduce(1..n//1, ctx, fn i, ctx -> meta("the type of argument #{i} of #{name}", ctx) end)
        {r, ctx} = meta("the result type of #{name}", ctx)
        {ps, r, unify!(m, [:->, ps, [:eff, :closed], r], ctx)}

      other ->
        fail(ctx, "#{name} is not a function: it is #{Types.format(other)}")
    end
  end

  defp instantiate(name, [:forall, binders, type], what, ctx) do
    {metas, ctx} = Enum.map_reduce(binders, ctx, fn _, ctx -> meta(what, ctx) end)
    type = Types.subst_tvars(type, Map.new(Enum.zip(Enum.map(binders, &hd/1), metas)))
    {[:inst, name | metas], type, ctx}
  end

  defp instantiate(name, type, _what, ctx), do: {name, type, ctx}

  defp fields(ctx, c) do
    %{members: ctors} = Map.fetch!(ctx.decls, Map.fetch!(ctx.ctors, c))
    {^c, fts} = List.keyfind(ctors, c, 0)
    fts
  end

  defp record?(ctx, t), do: match?(%{kind: :record}, Map.get(ctx.decls, t))

  defp constructor(c, args, ctx) do
    t = Map.fetch!(ctx.ctors, c)
    fts = fields(ctx, c)
    if length(fts) != length(args), do: fail(ctx, "wrong number of arguments", note: "the constructor `#{c}` takes #{length(fts)}, but #{length(args)} were given")
    {type, inst, ctx} = instance(t, "the type arguments of `#{c}`", ctx)
    {cores, ctx} = Enum.zip(args, fts) |> Enum.map_reduce(ctx, fn {a, ft}, ctx -> check(a, Types.subst_tvars(ft, inst), ctx) end)
    {[:inj, type, c | cores], type, ctx}
  end

  defp record(t, args, ctx) do
    %{members: fs} = Map.fetch!(ctx.decls, t)
    if length(fs) != length(args), do: fail(ctx, "wrong number of arguments", note: "the record `#{t}` has #{length(fs)} fields, but #{length(args)} were given")
    {type, inst, ctx} = instance(t, "the type arguments of `#{t}`", ctx)
    {cores, ctx} = Enum.zip(args, fs) |> Enum.map_reduce(ctx, fn {a, {_l, ft}}, ctx -> check(a, Types.subst_tvars(ft, inst), ctx) end)
    {[:record, type | Enum.zip_with(fs, cores, fn {l, _}, c -> [l, c] end)], type, ctx}
  end

  # A declared type applied to fresh type arguments.
  defp instance(t, what, ctx) do
    %{params: params} = Map.fetch!(ctx.decls, t)
    {metas, ctx} = Enum.map_reduce(params, ctx, fn _, ctx -> meta(what, ctx) end)
    {[t | metas], Map.new(Enum.zip(params, metas)), ctx}
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
    {[:prim, numeric_op(:neg, :fneg, type, :-, ctx), core], type, ctx}
  end

  defp builtin(:/, [a, b], ctx) do
    {a, ctx} = check(a, :Float, ctx)
    {b, ctx} = check(b, :Float, ctx)
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
    {elem, ctx} = list_elem(type, "the element type of this list", ctx)
    result = %{head: elem, tail: [:List, elem], length: :Int, empty?: [:Bool]} |> Map.fetch!(op)
    {[:prim, op, core], result, ctx}
  end

  defp builtin(:cons, [h, t], ctx), do: cons(h, t, ctx)
  defp builtin(f, _args, ctx) when f in @operators, do: fail(ctx, "wrong number of arguments to `#{f}`")
  defp builtin(:str, _args, _ctx), do: outside_term("the builtin :str")
  defp builtin(f, _args, ctx), do: fail(ctx, "undefined function `#{f}`")

  # Both operands have one type, Int or Float: no coercion. The primitive is
  # that type's, chosen now if the type is known and at the end otherwise.
  defp numeric(op, int_op, float_op, a, b, result, ctx) do
    {ac, type, ctx} = synth(a, ctx)
    {bc, ctx} = check(b, type, ctx)
    {[:prim, numeric_op(int_op, float_op, type, op, ctx), ac, bc], result.(type), ctx}
  end

  defp numeric_op(int_op, float_op, type, op, ctx) do
    case Types.zonk(type, ctx.s) do
      :Int -> int_op
      :Float -> float_op
      {:meta, _} = m -> {:numeric, int_op, float_op, m, op, ctx.loc}
      other -> fail(ctx, "`#{op}` works on Int or Float, not #{Types.format(other)}")
    end
  end

  # --- let (Q36: not generalized) -----------------------------------------------------

  defp let([], body, type, ctx), do: check(body, type, ctx)

  defp let([{x, e} | rest], body, type, ctx) when is_atom(x) and not is_boolean(x) and x != nil do
    {core, etype, ctx} = synth(e, ctx)
    {name, ctx} = fresh_name(x, ctx)
    env = if x == :_, do: ctx.env, else: Map.put(ctx.env, x, {name, etype})
    {rest_core, ctx} = in_scope(%{ctx | env: env}, fn ctx -> let(rest, body, type, ctx) end)
    {[:let, [[name, etype, core]], rest_core], ctx}
  end

  # `(let [(Point x y) p] ...)` binds by a pattern that cannot fail.
  defp let([{pattern, e} | rest], body, type, ctx) do
    {core, etype, ctx} = synth(e, ctx)
    {p, binds, ctx} = pattern(pattern, etype, ctx)
    ctx = %{ctx | matches: [{[{p, false}], etype, :let, ctx.loc} | ctx.matches]}
    {rest_core, ctx} = in_scope(%{ctx | env: Map.merge(ctx.env, binds)}, fn ctx -> let(rest, body, type, ctx) end)
    {[:match, core, [p, rest_core]], ctx}
  end

  # --- match ---------------------------------------------------------------------------

  # Patterns are checked against the scrutinee's type, which ties it to them
  # (D22). The unguarded clauses must cover the type (§10.8).
  defp match_clauses(s, stype, clauses, type, ctx) do
    loc = ctx.loc

    {cores, ctx} =
      Enum.map_reduce(clauses, ctx, fn {pat, guard, body}, ctx ->
        {p, binds, ctx} = pattern(pat, stype, ctx)

        in_scope(%{ctx | env: Map.merge(ctx.env, binds)}, fn ctx ->
          case guard do
            nil ->
              {b, ctx} = check(body, type, ctx)
              {[p, b], ctx}

            g ->
              {g, ctx} = guard(g, ctx)
              {b, ctx} = check(body, type, ctx)
              {[p, [:when, g], b], ctx}
          end
        end)
      end)

    clauses = for [p | rest] <- cores, do: {p, length(rest) == 2}
    {[:match, s | cores], %{ctx | matches: [{clauses, stype, :match, loc} | ctx.matches]}}
  end

  # A guard is guard-safe Core (liquid-core.md §6.6): no application, perform,
  # match or handle-crash, and no concat.
  defp guard(g, ctx) do
    {core, ctx} = check(g, [:Bool], ctx)

    case unsafe(core) do
      nil -> {core, ctx}
      what -> fail(ctx, "a guard cannot #{what}", note: "a guard may use variables, literals, comparisons, arithmetic, `and`, `or`, `not`, fields and constructors")
    end
  end

  defp unsafe([:app | _]), do: "call a function"
  defp unsafe([:perform | _]), do: "perform an effect"
  defp unsafe([:match | _]), do: "match"
  defp unsafe([:let | _]), do: "bind a name"
  defp unsafe([:"handle-crash" | _]), do: "catch a crash"
  defp unsafe([:fn | _]), do: "build a function"
  defp unsafe([:prim, :concat | _]), do: "concatenate strings"
  defp unsafe(list) when is_list(list), do: Enum.find_value(list, &unsafe/1)
  defp unsafe(_), do: nil

  # pattern(p, τ, ctx) -> {core pattern, %{source name => {core name, type}}, ctx}
  defp pattern(p, type, ctx), do: at(p, ctx, &pattern_(&1, type, &2))

  defp pattern_(:_, _type, ctx), do: {:_, %{}, ctx}
  defp pattern_(b, type, ctx) when is_boolean(b), do: {[:inj, [:Bool], b], %{}, unify!(type, [:Bool], ctx)}
  defp pattern_(nil, type, ctx), do: {[:unit], %{}, unify!(type, :Unit, ctx)}

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
    {elem, ctx} = list_elem(type, "the element type of this list", ctx)
    {[:inj, [:List, elem], :Nil], %{}, ctx}
  end

  defp pattern_({:bracket, {:cons, h, t}}, type, ctx) do
    {elem, ctx} = list_elem(type, "the element type of this list", ctx)
    {[hp, tp], binds, ctx} = patterns([h, t], [elem, [:List, elem]], ctx)
    {[:inj, [:List, elem], :Cons, hp, tp], binds, ctx}
  end

  defp pattern_({:bracket, ps}, type, ctx) when is_list(ps) do
    {elem, ctx} = list_elem(type, "the element type of this list", ctx)
    {cores, binds, ctx} = patterns(ps, List.duplicate(elem, length(ps)), ctx)
    list_type = [:List, elem]
    {List.foldr(cores, [:inj, list_type, :Nil], fn p, acc -> [:inj, list_type, :Cons, p, acc] end), binds, ctx}
  end

  defp pattern_({:tuple, ps, _loc}, type, ctx), do: pattern_({:tuple_pattern, ps}, type, ctx)
  defp pattern_({:tuple_pattern, []}, type, ctx), do: {[:unit], %{}, unify!(type, :Unit, ctx)}

  defp pattern_({:tuple_pattern, ps}, type, ctx) do
    {metas, ctx} = Enum.map_reduce(ps, ctx, fn _, ctx -> meta("the type of a component of this tuple", ctx) end)
    ctx = unify!(type, [:Tuple | metas], ctx)
    {cores, binds, ctx} = patterns(ps, metas, ctx)
    {[:tuple | cores], binds, ctx}
  end

  # (r @ (Ok v)) binds r to the whole value and matches it against (Ok v).
  defp pattern_({:call, x, [:@, p], _loc}, type, ctx) when is_atom(x) do
    {name, ctx} = fresh_name(x, ctx)
    {core, binds, ctx} = pattern(p, type, ctx)
    {[:as, name, core], disjoint(%{x => {name, type}}, binds, ctx), ctx}
  end

  defp pattern_({:call, c, ps, _loc}, type, ctx) when is_atom(c) do
    cond do
      Map.has_key?(ctx.ctors, c) ->
        fts = fields(ctx, c)
        if length(fts) != length(ps), do: fail(ctx, "the constructor `#{c}` has #{length(fts)} fields, but the pattern has #{length(ps)}")
        {ptype, inst, ctx} = instance(Map.fetch!(ctx.ctors, c), "the type arguments of `#{c}`", ctx)
        ctx = unify!(type, ptype, ctx)
        {cores, binds, ctx} = patterns(ps, Enum.map(fts, &Types.subst_tvars(&1, inst)), ctx)
        {[:inj, ptype, c | cores], binds, ctx}

      record?(ctx, c) ->
        %{members: fs} = Map.fetch!(ctx.decls, c)
        if length(fs) != length(ps), do: fail(ctx, "the record `#{c}` has #{length(fs)} fields, but the pattern has #{length(ps)}")
        {ptype, inst, ctx} = instance(c, "the type arguments of `#{c}`", ctx)
        ctx = unify!(type, ptype, ctx)
        {cores, binds, ctx} = patterns(ps, Enum.map(fs, fn {_l, ft} -> Types.subst_tvars(ft, inst) end), ctx)
        {[:record, ptype | Enum.zip_with(fs, cores, fn {l, _}, p -> [l, p] end)], binds, ctx}

      true ->
        fail(ctx, "`#{c}` is not a constructor")
    end
  end

  defp pattern_(other, _type, _ctx), do: outside_term("the pattern #{inspect(other, limit: 4)}")

  defp patterns(ps, types, ctx) do
    Enum.zip(ps, types)
    |> Enum.reduce({[], %{}, ctx}, fn {p, t}, {cores, binds, ctx} ->
      {core, more, ctx} = pattern(p, t, ctx)
      {cores ++ [core], disjoint(binds, more, ctx), ctx}
    end)
  end

  # A pattern binds each name once (liquid-core.md §4.2).
  defp disjoint(a, b, ctx) do
    case Enum.find(Map.keys(b), &Map.has_key?(a, &1)) do
      nil -> Map.merge(a, b)
      x -> fail(ctx, "`#{x}` is bound twice in this pattern", hint: "use a different name, and compare them with == in the body")
    end
  end

  # --- Helpers ---------------------------------------------------------------------------

  defp unify!(expected, actual, ctx) do
    case Types.unify(expected, actual, ctx.s) do
      {:ok, s} -> %{ctx | s: s}
      :error -> fail(ctx, "type mismatch", note: "expected #{Types.format(Types.zonk(expected, ctx.s))}, found #{Types.format(Types.zonk(actual, ctx.s))}")
    end
  end

  # A metavariable, with what it stands for, so an undetermined one is
  # reported where it arose.
  defp meta(what, ctx) do
    n = ctx.next
    {{:meta, n}, %{ctx | next: n + 1, origins: Map.put(ctx.origins, n, {ctx.loc, what})}}
  end

  # Binder names are unique within a definition, and distinct from every
  # definition's (liquid-core.md §10.12).
  defp fresh_name(:_, ctx), do: {:_, ctx}

  defp fresh_name(x, ctx) do
    name =
      if MapSet.member?(ctx.names, x),
        do: Stream.iterate(2, &(&1 + 1)) |> Stream.map(&:"#{x}'#{&1}") |> Enum.find(&(not MapSet.member?(ctx.names, &1))),
        else: x

    {name, %{ctx | names: MapSet.put(ctx.names, name)}}
  end

  # Names bound in a scope do not outlive it; what it learned about types does.
  defp in_scope(ctx, fun) do
    case fun.(ctx) do
      {core, ctx2} -> {core, %{ctx2 | env: ctx.env}}
      {core, type, ctx2} -> {core, type, %{ctx2 | env: ctx.env}}
    end
  end

  # The nearest source location, for errors.
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

  defp lowercase?(t), do: String.match?(Atom.to_string(t), ~r/^[a-z]/)
  defp capitalized?(t), do: String.match?(Atom.to_string(t), ~r/^[A-Z]/)

  defp span(%Loc{} = loc), do: Error.span_from_loc(loc)
  defp span(_), do: nil

  defp fail(ctx, message, opts \\ []), do: throw({:elab, Error.new(message, Keyword.merge([span: span(ctx.loc)], opts))})

  defp outside_term(reason), do: throw({:outside, reason})
end
