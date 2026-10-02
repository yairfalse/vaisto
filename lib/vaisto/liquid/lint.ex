defmodule Vaisto.Liquid.Lint do
  @moduledoc """
  Core Lint: the static semantics of `docs/design/liquid-core.md` §10.

  It checks Core and infers nothing: every binder carries its type and every
  instantiation is explicit. It is the trusted check on whatever produced the
  Core (RFC §9.2), so a problem it finds in elaborated code is a compiler bug.

    * `check_module/1` checks a module (§10.13);
    * `synth/2` and `check/3` check one term (§10.4, §10.5), for tests and tools.

  A problem is `%{message: text, in: definition_name, span: span_or_nil}`,
  where the span is the nearest enclosing `(meta (span ...))`.
  """

  alias Vaisto.Liquid.Canonical

  # The built-in declarations, exactly as liquid-core.md §3.3 writes them.
  @builtins Canonical.parse!(
              """
              ((type Bool () (sum (true) (false)))
               (type List (a) (sum (Nil) (Cons (tvar a) (List (tvar a)))))
               (type Reason () (sum (badarith) (badarg) (no_match) (bad_decode) (user Dyn) (raised Atom Dyn))))
              """,
              atoms: :create
            )

  @base [:Int, :Float, :String, :Atom, :Ref, :Dyn, :Unit]
  @reserved @base ++ [:tvar, :Tuple, :Record, :->, :pi, :Pid, :Map, :forall, :refine, :row, :eff]
  @bool [:Bool]

  # §10.4: the operations of version 0.
  @sigma %{
    now: {[], :Int},
    random: {[], :Int},
    unique: {[], :Ref},
    external: {[:Atom, :Atom, [:List, :Dyn]], :Dyn}
  }

  # §10.10: primitives with fixed operand types.
  @prims Map.merge(
           Map.new([:add, :sub, :mul, :div, :rem], &{&1, {[:Int, :Int], :Int}}),
           Map.new([:lt, :le, :gt, :ge], &{&1, {[:Int, :Int], [:Bool]}})
         )
         |> Map.merge(Map.new([:fadd, :fsub, :fmul, :fdiv], &{&1, {[:Float, :Float], :Float}}))
         |> Map.merge(Map.new([:flt, :fle, :fgt, :fge], &{&1, {[:Float, :Float], [:Bool]}}))
         |> Map.merge(%{
           neg: {[:Int], :Int},
           fneg: {[:Float], :Float},
           "int-to-float": {[:Int], :Float},
           not: {[[:Bool]], [:Bool]},
           concat: {[:String, :String], :String}
         })

  defstruct types: %{}, delta: %{}, gamma: %{}, span: nil

  @type problem :: %{message: String.t(), in: atom() | nil, span: Canonical.tree() | nil}

  # --- Public interface --------------------------------------------------------

  @doc "Check a module (§10.13). Returns `:ok` or every problem found."
  @spec check_module(Canonical.tree()) :: :ok | {:error, [problem()]}
  def check_module(module) do
    {module, span} = split_meta(module)

    case module do
      [:module, name, [:"core-version", 0] | items] when is_atom(name) ->
        check_items(items)

      _ ->
        {:error, [%{message: "not a Liquid Core version 0 module", in: nil, span: span}]}
    end
  end

  @doc """
  Synthesize the type of a term (§10.4): `{:ok, type}`, `:none` for a term that
  never returns, or `{:error, problems}`. Options: `types:` a list of `(type ...)`
  declarations, `bind:` a list of `(x τ)` variables in scope.
  """
  @spec synth(Canonical.tree(), keyword()) :: {:ok, Canonical.tree()} | :none | {:error, [problem()]}
  def synth(term, opts \\ []) do
    ctx = context(opts)

    run(nil, fn ->
      check_binders(ctx, term, Map.keys(ctx.gamma))
      synth_term(ctx, term)
    end)
  end

  @doc "Check a term against a type (§10.5): `:ok` or `{:error, problems}`."
  @spec check(Canonical.tree(), Canonical.tree(), keyword()) :: :ok | {:error, [problem()]}
  def check(term, type, opts \\ []) do
    ctx = context(opts)

    run(nil, fn ->
      type = norm(type)
      wf(ctx, type)
      check_binders(ctx, term, Map.keys(ctx.gamma))
      check_term(ctx, term, type)
      :ok
    end)
  end

  defp context(opts) do
    {types, []} = declare(Keyword.get(opts, :types, []))
    ctx = %__MODULE__{types: types}
    gamma = for [x, type] <- Keyword.get(opts, :bind, []), into: %{}, do: {x, norm(type)}
    %{ctx | gamma: gamma}
  end

  # Runs one unit of checking, turning the first problem into a result.
  defp run(name, fun) do
    fun.()
  catch
    {:lint, message, span} -> {:error, [%{message: message, in: name, span: span}]}
  end

  defp fail(ctx, message), do: throw({:lint, message, ctx.span})

  # --- §10.13 Modules ------------------------------------------------------------

  defp check_items(items) do
    items = for item <- items, not meta?(item), do: split_meta(item)
    decls = for {[:type, t, params, _body] = d, _span} <- items, is_atom(t) and is_list(params), do: d
    defs = for {[:def, f, _type, _fun], _span} = d <- items, is_atom(f), do: d
    others = for {item, _span} <- items, item not in decls and item not in Enum.map(defs, &elem(&1, 0)), do: item

    {types, decl_problems} = declare(decls)
    ctx = %__MODULE__{types: types}

    item_problems = for other <- others, do: %{message: "not a module item: #{show(other)}", in: nil, span: nil}

    # Every definition's type is checked first, so the bodies can all refer to all of them.
    {gamma, broken, sig_problems} =
      Enum.reduce(defs, {%{}, MapSet.new(), []}, fn {[:def, f, type, _fun], span}, {gamma, broken, problems} ->
        cond do
          Map.has_key?(gamma, f) ->
            {gamma, broken, problems ++ [%{message: "#{show(f)} is defined twice", in: f, span: span}]}

          true ->
            case run(f, fn -> wf(%{ctx | span: span}, norm(type)) end) do
              {:error, ps} -> {Map.put(gamma, f, norm(type)), MapSet.put(broken, f), problems ++ ps}
              _ -> {Map.put(gamma, f, norm(type)), broken, problems}
            end
        end
      end)

    ctx = %{ctx | gamma: gamma}

    # Definitions are checked only against well-formed declarations: a type
    # that names a constructor twice has no meaning to check a match against.
    checkable = if decl_problems == [], do: Enum.uniq_by(defs, fn {[:def, f | _], _} -> f end), else: []

    def_problems =
      for {[:def, f, type, fun], span} <- checkable, f not in broken do
        case run(f, fn ->
               ctx = %{ctx | span: span}
               unless match?([:fn | _], strip_own_meta(fun)), do: fail(ctx, "the definition of #{show(f)} is not a fn")
               check_binders(ctx, fun, Map.keys(gamma))
               bind_check(ctx, norm(type), fun)
             end) do
          {:error, ps} -> ps
          _ -> []
        end
      end
      |> List.flatten()

    case item_problems ++ decl_problems ++ sig_problems ++ def_problems do
      [] -> :ok
      problems -> {:error, problems}
    end
  end

  # Collect declarations (built-in first), then check each against all of them.
  defp declare(decls) do
    builtin = Map.new(@builtins, &header/1)

    {types, problems} =
      Enum.reduce(decls, {builtin, []}, fn decl, {types, problems} ->
        case decl do
          [:type, t, params, [kind | _]] when is_atom(t) and is_list(params) and kind in [:sum, :record] ->
            cond do
              t in @reserved or Map.has_key?(builtin, t) ->
                {types, problems ++ [%{message: "#{show(t)} is a reserved or built-in type name", in: t, span: nil}]}

              Map.has_key?(types, t) ->
                {types, problems ++ [%{message: "type #{show(t)} is declared twice", in: t, span: nil}]}

              true ->
                {t, header} = header(decl)
                {Map.put(types, t, header), problems}
            end

          _ ->
            {types, problems ++ [%{message: "malformed type declaration: #{show(decl)}", in: nil, span: nil}]}
        end
      end)

    ctx = %__MODULE__{types: types}

    body_problems =
      for [:type, t, _params, body] <- decls, Map.has_key?(types, t), not Map.has_key?(builtin, t) do
        case run(t, fn -> check_decl(ctx, t, body, Map.fetch!(types, t)) end) do
          {:error, ps} -> ps
          _ -> []
        end
      end

    {types, problems ++ List.flatten(body_problems)}
  end

  defp header([:type, t, params, [:sum | ctors]]),
    do: {t, {:sum, params, for([c | fields] <- ctors, do: {c, Enum.map(fields, &norm/1)})}}

  defp header([:type, t, params, [:record | fields]]),
    do: {t, {:record, params, for([l, type] <- fields, do: {l, norm(type)})}}

  defp check_decl(ctx, t, body, {kind, params, members}) do
    well_formed? =
      case body do
        [:sum | ctors] -> Enum.all?(ctors, &match?([c | _] when is_atom(c), &1))
        [:record | fields] -> Enum.all?(fields, &match?([l, _] when is_atom(l), &1))
      end

    unless well_formed?, do: fail(ctx, "malformed members in the declaration of #{show(t)}")
    unless Enum.all?(params, &is_atom/1) and distinct?(params), do: fail(ctx, "the parameters of #{show(t)} must be distinct names")
    ctx = %{ctx | delta: Map.new(params, &{&1, :Type})}
    names = Enum.map(members, &elem(&1, 0))
    unless distinct?(names), do: fail(ctx, "#{show(t)} names a #{if kind == :sum, do: "constructor", else: "field"} twice")
    if kind == :sum and members == [], do: fail(ctx, "the sum #{show(t)} has no constructors")

    for {name, field_types} <- members do
      if kind == :record and brand?(name), do: fail(ctx, "field #{show(name)} of #{show(t)} may not begin with #")
      for type <- List.wrap(if kind == :sum, do: field_types, else: [field_types]), do: wf(ctx, type)
    end

    :ok
  end

  # --- §10.2 Well-formed types ---------------------------------------------------

  defp wf(_ctx, t) when t in @base, do: :ok

  defp wf(ctx, [:tvar, a]) do
    unless Map.get(ctx.delta, a) == :Type, do: fail(ctx, "type variable #{show(a)} is not in scope")
    :ok
  end

  defp wf(ctx, [:Tuple | ts]) when ts != [], do: Enum.each(ts, &wf(ctx, &1))
  defp wf(ctx, [:Record, row]), do: wf_row(ctx, row)
  defp wf(ctx, [:Pid, t]), do: wf(ctx, t)
  defp wf(ctx, [:Map, k, v]), do: wf(ctx, k) && wf(ctx, v)

  defp wf(ctx, [:->, params, eff, result]) when is_list(params) do
    Enum.each(params, &wf(ctx, &1))
    wf_eff(ctx, eff)
    wf(ctx, result)
  end

  defp wf(ctx, [:forall, binders, body]) when is_list(binders) and binders != [] do
    names = for b <- binders, do: (match?([a, k] when is_atom(a) and k in [:Type, :Row, :Eff], b) && hd(b)) || fail(ctx, "malformed binder #{show(b)}")
    unless distinct?(names), do: fail(ctx, "a forall binds #{show(names)} with a name twice")
    wf(%{ctx | delta: Map.merge(ctx.delta, Map.new(binders, fn [a, k] -> {a, k} end))}, body)
  end

  defp wf(ctx, [:refine | _]), do: fail(ctx, "refinements are not in Liquid Core version 0")

  defp wf(ctx, [t | args] = type) when is_atom(t) and t not in @reserved do
    case Map.get(ctx.types, t) do
      {_kind, params, _} when length(params) == length(args) -> Enum.each(args, &wf(ctx, &1))
      {_kind, params, _} -> fail(ctx, "#{show(t)} takes #{length(params)} type arguments: #{show(type)}")
      nil -> fail(ctx, "unknown type #{show(t)}")
    end
  end

  defp wf(ctx, type), do: fail(ctx, "not a type: #{show(type)}")

  defp wf_row(ctx, [:row | items] = row) when items != [] do
    {fields, tail} = split_row(row)
    labels = for f <- fields, do: (match?([l, _] when is_atom(l), f) && hd(f)) || fail(ctx, "malformed row field #{show(f)}")
    unless distinct?(labels), do: fail(ctx, "row #{show(row)} has a label twice")
    for [_l, type] <- fields, do: wf(ctx, type)
    wf_tail(ctx, tail, :rvar, :Row)
  end

  defp wf_row(ctx, row), do: fail(ctx, "not a row: #{show(row)}")

  defp wf_eff(ctx, [:eff | items] = eff) when items != [] do
    {labels, [tail]} = Enum.split(items, -1)
    unless Enum.all?(labels, &is_atom/1) and distinct?(labels), do: fail(ctx, "malformed effect row #{show(eff)}")
    wf_tail(ctx, tail, :evar, :Eff)
  end

  defp wf_eff(ctx, eff), do: fail(ctx, "not an effect row: #{show(eff)}")

  defp wf_tail(_ctx, :closed, _var, _kind), do: :ok

  defp wf_tail(ctx, [var, a], var, kind) do
    unless Map.get(ctx.delta, a) == kind, do: fail(ctx, "#{var} #{show(a)} is not a #{kind} variable in scope")
    :ok
  end

  defp wf_tail(ctx, tail, _var, _kind), do: fail(ctx, "malformed tail #{show(tail)}")

  defp split_row([:row | items]) do
    {fields, [tail]} = Enum.split(items, -1)
    {fields, tail}
  end

  # --- §10.3 Type equality and substitution ---------------------------------------

  # Strip metadata and read a dependent arrow as the arrow it abbreviates: no
  # refinement can mention its names in version 0.
  defp norm([:pi, params, eff, result] = t) when is_list(params) do
    if Enum.all?(params, &match?([_x, _t], &1)),
      do: [:->, Enum.map(params, fn [_x, t] -> norm(t) end), norm(eff), norm(result)],
      else: t
  end
  defp norm(t) when is_list(t), do: for(c <- t, not meta?(c), do: norm(c))
  defp norm(t), do: t

  defp equal?(ctx, a, b), do: eqt(ctx, a, b, 0)

  defp eqt(_ctx, a, a, _depth), do: true
  defp eqt(ctx, [:Record, r1], [:Record, r2], depth), do: eqrow(ctx, r1, r2, depth)

  defp eqt(ctx, [:->, p1, _e1, r1], [:->, p2, _e2, r2], depth),
    do: length(p1) == length(p2) and all_eq?(ctx, p1, p2, depth) and eqt(ctx, r1, r2, depth)

  defp eqt(ctx, [:forall, b1, t1], [:forall, b2, t2], depth) do
    length(b1) == length(b2) and Enum.map(b1, &List.last/1) == Enum.map(b2, &List.last/1) and
      (
        fresh = for i <- 1..length(b1), do: :"$#{depth}_#{i}"
        rename = fn bs, t -> subst(t, Map.new(Enum.zip(bs, fresh), fn {[a, k], f} -> {a, renamed(k, f)} end)) end
        eqt(ctx, rename.(b1, t1), rename.(b2, t2), depth + 1)
      )
  end

  defp eqt(ctx, [:Tuple | a], [:Tuple | b], depth), do: length(a) == length(b) and all_eq?(ctx, a, b, depth)
  defp eqt(ctx, [:Pid, a], [:Pid, b], depth), do: eqt(ctx, a, b, depth)
  defp eqt(ctx, [:Map, k1, v1], [:Map, k2, v2], depth), do: eqt(ctx, k1, k2, depth) and eqt(ctx, v1, v2, depth)

  defp eqt(ctx, [t | args], [:Record, _] = record, depth) when t not in @reserved and is_atom(t) do
    record?(ctx, t) and eqt(ctx, unfold(ctx, t, args), record, depth)
  end

  defp eqt(ctx, [:Record, _] = record, [t | args], depth) when t not in @reserved and is_atom(t) do
    record?(ctx, t) and eqt(ctx, record, unfold(ctx, t, args), depth)
  end

  defp eqt(ctx, [t | a1], [t | a2], depth) when t not in @reserved and is_atom(t),
    do: length(a1) == length(a2) and all_eq?(ctx, a1, a2, depth)

  defp eqt(_ctx, _a, _b, _depth), do: false

  defp all_eq?(ctx, as, bs, depth), do: Enum.zip(as, bs) |> Enum.all?(fn {a, b} -> eqt(ctx, a, b, depth) end)

  defp eqrow(ctx, r1, r2, depth) do
    {f1, t1} = split_row(r1)
    {f2, t2} = split_row(r2)
    m1 = Map.new(f1, fn [l, t] -> {l, t} end)
    m2 = Map.new(f2, fn [l, t] -> {l, t} end)

    t1 == t2 and Map.keys(m1) |> Enum.sort() == Map.keys(m2) |> Enum.sort() and
      Enum.all?(m1, fn {l, t} -> eqt(ctx, t, Map.fetch!(m2, l), depth) end)
  end

  defp renamed(:Type, f), do: {:type, [:tvar, f]}
  defp renamed(:Row, f), do: {:row, [:row, [:rvar, f]]}
  defp renamed(:Eff, f), do: {:eff, [:eff, [:evar, f]]}

  # `s` maps a variable to {:type, τ}, {:row, row} or {:eff, eff}.
  defp subst(t, s) when map_size(s) == 0, do: t
  defp subst(t, _s) when is_atom(t), do: t

  defp subst([:tvar, a] = t, s) do
    case Map.get(s, a) do
      {:type, replacement} -> replacement
      _ -> t
    end
  end

  defp subst([:row | _] = row, s) do
    {fields, tail} = split_row(row)
    fields = for [l, t] <- fields, do: [l, subst(t, s)]

    case {tail, tail_replacement(tail, s, :rvar, :row)} do
      {_, nil} -> [:row | fields ++ [tail]]
      {_, [:row | _] = r} -> splice(fields, split_row(r), :row)
    end
  end

  defp subst([:eff | items], s) do
    {labels, [tail]} = Enum.split(items, -1)

    case tail_replacement(tail, s, :evar, :eff) do
      nil -> [:eff | labels ++ [tail]]
      [:eff | more] -> (fn {l2, [t2]} -> splice_labels(labels, l2, t2) end).(Enum.split(more, -1))
    end
  end

  defp subst([:forall, binders, body], s) do
    names = Enum.map(binders, &hd/1)
    s = Map.drop(s, names)
    range_free = s |> Map.values() |> Enum.flat_map(fn {_, t} -> free(t) end) |> MapSet.new()

    # Rename binders that the substitution would capture.
    {binders, body} =
      Enum.reduce(binders, {[], body}, fn [a, k], {acc, body} ->
        if MapSet.member?(range_free, a) do
          fresh = fresh_name(a, MapSet.union(range_free, MapSet.new(free(body))))
          {acc ++ [[fresh, k]], subst(body, %{a => renamed(k, fresh)})}
        else
          {acc ++ [[a, k]], body}
        end
      end)

    [:forall, binders, subst(body, s)]
  end

  defp subst([:->, params, eff, result], s), do: [:->, Enum.map(params, &subst(&1, s)), subst(eff, s), subst(result, s)]
  defp subst([:Record, row], s), do: [:Record, subst(row, s)]
  defp subst([head | args], s) when is_atom(head), do: [head | Enum.map(args, &subst(&1, s))]
  defp subst(t, _s), do: t

  defp tail_replacement([var, a], s, var, kind) do
    case Map.get(s, a) do
      {^kind, replacement} -> replacement
      _ -> nil
    end
  end

  defp tail_replacement(_tail, _s, _var, _kind), do: nil

  defp splice(fields, {more, tail}, :row) do
    labels = Enum.map(fields ++ more, &hd/1)
    unless distinct?(labels), do: throw({:lint, "substitution gives a row with a label twice: #{show(labels)}", nil})
    [:row | fields ++ more ++ [tail]]
  end

  defp splice_labels(labels, more, tail) do
    unless distinct?(labels ++ more), do: throw({:lint, "substitution gives an effect row with a label twice", nil})
    [:eff | labels ++ more ++ [tail]]
  end

  defp free([:tvar, a]), do: [a]
  defp free([:rvar, a]), do: [a]
  defp free([:evar, a]), do: [a]
  defp free([:forall, binders, body]), do: free(body) -- Enum.map(binders, &hd/1)
  defp free(t) when is_list(t), do: Enum.flat_map(t, &free/1)
  defp free(_t), do: []

  defp fresh_name(a, taken) do
    Stream.iterate(1, &(&1 + 1))
    |> Stream.map(&:"#{a}'#{&1}")
    |> Enum.find(&(not MapSet.member?(taken, &1)))
  end

  # A named record is its branded row (§10.3, rule 4).
  defp unfold(ctx, t, args) do
    fields = for {l, type} <- record_fields(ctx, t, args), do: [l, type]
    [:Record, [:row, [brand(t), :Unit] | fields ++ [:closed]]]
  end

  defp brand(t), do: :"##{t}"
  defp brand?(l), do: String.starts_with?(Atom.to_string(l), "#")

  defp record?(ctx, t), do: match?({:record, _, _}, Map.get(ctx.types, t))

  defp instantiate(params, args), do: Map.new(Enum.zip(params, args), fn {p, a} -> {p, {:type, a}} end)

  defp record_fields(ctx, t, args) do
    {:record, params, fields} = Map.fetch!(ctx.types, t)
    s = instantiate(params, args)
    for {l, type} <- fields, do: {l, subst(type, s)}
  end

  defp constructors(ctx, t, args) do
    {:sum, params, ctors} = Map.fetch!(ctx.types, t)
    s = instantiate(params, args)
    for {c, fields} <- ctors, do: {c, Enum.map(fields, &subst(&1, s))}
  end

  # --- §10.4 Synthesis -----------------------------------------------------------

  defp synth_term(ctx, e) do
    {e, ctx} = enter(ctx, e)
    synth_form(ctx, e)
  end

  defp synth_form(ctx, x) when is_atom(x) do
    case Map.fetch(ctx.gamma, x) do
      {:ok, type} -> {:ok, type}
      :error -> fail(ctx, "unbound variable #{show(x)}")
    end
  end

  defp synth_form(_ctx, n) when is_integer(n), do: {:ok, :Int}
  defp synth_form(_ctx, r) when is_float(r), do: {:ok, :Float}
  defp synth_form(_ctx, s) when is_binary(s), do: {:ok, :String}
  defp synth_form(_ctx, [:atom, a]) when is_atom(a), do: {:ok, :Atom}
  defp synth_form(_ctx, [:unit]), do: {:ok, :Unit}

  defp synth_form(ctx, [:fn, params, body]) when is_list(params) do
    {ctx, types} = bind_params(ctx, params)

    case synth_term(ctx, body) do
      {:ok, result} -> {:ok, [:->, types, [:eff, :closed], result]}
      :none -> :none
    end
  end

  defp synth_form(ctx, [:app, f | args]) do
    case synth_term(ctx, f) do
      {:ok, [:->, params, _eff, result]} when length(params) == length(args) ->
        Enum.zip(args, params) |> Enum.each(fn {a, p} -> check_term(ctx, a, p) end)
        {:ok, result}

      {:ok, [:->, params, _eff, _result]} ->
        fail(ctx, "the function takes #{length(params)} arguments, not #{length(args)}")

      {:ok, other} ->
        fail(ctx, "cannot apply a value of type #{show(other)}")

      :none ->
        fail(ctx, "the function in an application never returns")
    end
  end

  defp synth_form(ctx, [:let, [[x, type, e1]], e2]) when is_atom(x) do
    type = norm(type)
    wf(ctx, type)
    bind_check(ctx, type, e1)
    synth_term(bind(ctx, x, type), e2)
  end

  defp synth_form(ctx, [:letrec, group, body]) when is_list(group), do: synth_term(check_group(ctx, group), body)

  defp synth_form(ctx, [:inst, e | args]) when args != [] do
    case synth_term(ctx, e) do
      {:ok, [:forall, binders, body]} when length(binders) == length(args) ->
        args = Enum.map(args, &norm/1)

        s =
          Map.new(Enum.zip(binders, args), fn {[a, k], arg} ->
            {a, kinded(ctx, k, arg)}
          end)

        {:ok, subst(body, s)}

      {:ok, [:forall, binders, _]} ->
        fail(ctx, "inst gives #{length(args)} arguments to a forall of #{length(binders)}")

      {:ok, other} ->
        fail(ctx, "inst of a term whose type #{show(other)} is not a forall")

      :none ->
        fail(ctx, "inst of a term that never returns")
    end
  end

  defp synth_form(ctx, [:tuple | es]) when es != [] do
    {:ok, [:Tuple | Enum.map(es, &must_synth(ctx, &1, "a tuple element"))]}
  end

  defp synth_form(ctx, [:record, type | fields]) do
    type = norm(type)
    wf(ctx, type)
    expected = record_shape(ctx, type)
    given = for f <- fields, do: (match?([l, _] when is_atom(l), f) && hd(f)) || fail(ctx, "malformed field #{show(f)}")
    unless distinct?(given), do: fail(ctx, "a record gives a field twice")

    unless Enum.sort(given) == expected |> Map.keys() |> Enum.sort(),
      do: fail(ctx, "a record of #{show(type)} must give exactly the fields #{show(Map.keys(expected))}")

    for [l, e] <- fields, do: check_term(ctx, e, Map.fetch!(expected, l))
    {:ok, type}
  end

  defp synth_form(ctx, [:select, e, l]) do
    type = must_synth(ctx, e, "the record in a select")

    case select_type(ctx, type, l) do
      nil -> fail(ctx, "#{show(type)} has no field #{show(l)}")
      field_type -> {:ok, field_type}
    end
  end

  defp synth_form(ctx, [:inj, type, c | es]) do
    type = norm(type)
    wf(ctx, type)

    case type do
      [t | args] when is_atom(t) and t not in @reserved ->
        unless match?({:sum, _, _}, Map.get(ctx.types, t)), do: fail(ctx, "#{show(t)} is not a sum type")

        case List.keyfind(constructors(ctx, t, args), c, 0) do
          {^c, field_types} when length(field_types) == length(es) ->
            Enum.zip(es, field_types) |> Enum.each(fn {e, ft} -> check_term(ctx, e, ft) end)
            {:ok, type}

          {^c, field_types} ->
            fail(ctx, "constructor #{show(c)} takes #{length(field_types)} fields, not #{length(es)}")

          nil ->
            fail(ctx, "#{show(c)} is not a constructor of #{show(t)}")
        end

      _ ->
        fail(ctx, "inj needs a named sum type, not #{show(type)}")
    end
  end

  defp synth_form(ctx, [:match, e | clauses]) when clauses != [] do
    common(clause_branches(ctx, e, clauses))
  end

  defp synth_form(ctx, [:if, c, e1, e2]) do
    check_term(ctx, c, @bool)
    common([{ctx, e1}, {ctx, e2}])
  end

  defp synth_form(ctx, [:prim, op | es]), do: {:ok, prim_type(ctx, op, es)}

  defp synth_form(ctx, [:perform, :crash, e]) do
    check_term(ctx, e, [:Reason])
    :none
  end

  defp synth_form(ctx, [:perform, op | es]) do
    case Map.fetch(@sigma, op) do
      {:ok, {params, result}} when length(params) == length(es) ->
        Enum.zip(es, params) |> Enum.each(fn {e, p} -> check_term(ctx, e, p) end)
        {:ok, result}

      {:ok, {params, _}} ->
        fail(ctx, "#{show(op)} takes #{length(params)} operands, not #{length(es)}")

      :error ->
        fail(ctx, "#{show(op)} is not an operation of Liquid Core version 0")
    end
  end

  defp synth_form(ctx, [:"handle-crash", e, [[x, reason], handler]]) when is_atom(x) do
    unless equal?(ctx, norm(reason), [:Reason]), do: fail(ctx, "handle-crash binds a (Reason), not #{show(reason)}")
    common([{ctx, e}, {bind(ctx, x, [:Reason]), handler}])
  end

  defp synth_form(ctx, [:up, type, e]) do
    type = norm(type)
    wf(ctx, type)
    unless ground_data?(ctx, type), do: not_ground(ctx, "up of", type)
    check_term(ctx, e, type)
    {:ok, :Dyn}
  end

  defp synth_form(ctx, [:decode | _]), do: fail(ctx, "decode is not in Liquid Core version 0")
  defp synth_form(ctx, term), do: fail(ctx, "not a Core term: #{show(term)}")

  defp must_synth(ctx, e, what) do
    case synth_term(ctx, e) do
      {:ok, type} -> type
      :none -> fail(ctx, "#{what} never returns, so its type is unknown")
    end
  end

  defp bind_params(ctx, params) do
    names = for p <- params, do: (match?([x, _] when is_atom(x), p) && hd(p)) || fail(ctx, "malformed parameter #{show(p)}")
    unless distinct?(names -- [:_]), do: fail(ctx, "a fn names a parameter twice")
    types = for [_x, t] <- params, do: norm(t)
    Enum.each(types, &wf(ctx, &1))
    {Enum.zip(names, types) |> Enum.reduce(ctx, fn {x, t}, acc -> bind(acc, x, t) end), types}
  end

  # `_` binds nothing (§10.12).
  defp bind(ctx, :_, _type), do: ctx
  defp bind(ctx, x, type), do: %{ctx | gamma: Map.put(ctx.gamma, x, type)}

  # §10.12: every binder of a definition is distinct, and none reuses a name
  # already in scope (the module's definitions, or the variables given).
  defp check_binders(ctx, term, outer) do
    names = binders(term)
    repeated = Enum.uniq(names -- Enum.uniq(names))

    case repeated ++ Enum.filter(Enum.uniq(names), &(&1 in outer)) do
      [] -> :ok
      [x | _] -> fail(ctx, "#{show(x)} is bound more than once: binders must be unique within a definition (liquid-core.md §10.12)")
    end
  end

  defp binders(term) do
    case strip_own_meta(term) do
      [:fn, params, body] when is_list(params) -> Enum.flat_map(params, &named/1) ++ binders(body)
      [:let, [[x, _type, e1]], e2] -> named([x]) ++ binders(e1) ++ binders(e2)
      [:letrec, group, body] when is_list(group) -> Enum.flat_map(group, &binding_binders/1) ++ binders(body)
      [:match, e | clauses] -> binders(e) ++ Enum.flat_map(clauses, &clause_binders/1)
      [:"handle-crash", e, [[x, _type], h]] -> named([x]) ++ binders(e) ++ binders(h)
      [form, _type, e] when form in [:up, :decode] -> binders(e)
      [:select, e, _label] -> binders(e)
      [:inst, e | _types] -> binders(e)
      [:record, _type | fields] -> Enum.flat_map(fields, &field_binders/1)
      [:inj, _type, _c | es] -> Enum.flat_map(es, &binders/1)
      [form, _op | es] when form in [:prim, :perform] -> Enum.flat_map(es, &binders/1)
      [form | es] when form in [:app, :tuple, :if] -> Enum.flat_map(es, &binders/1)
      _ -> []
    end
  end

  defp named([x | _]) when is_atom(x) and x != :_, do: [x]
  defp named(_), do: []

  defp binding_binders([f, _type, e]), do: named([f]) ++ binders(e)
  defp binding_binders(_), do: []

  defp field_binders([_label, e]), do: binders(e)
  defp field_binders(_), do: []

  defp clause_binders(clause) do
    case strip_own_meta(clause) do
      [p, [:when, g], body] -> pattern_vars(p) ++ binders(g) ++ binders(body)
      [p, body] -> pattern_vars(p) ++ binders(body)
      _ -> []
    end
  end

  defp pattern_vars(p) do
    case strip_own_meta(p) do
      :_ -> []
      x when is_atom(x) -> [x]
      [:as, x, q] when is_atom(x) -> [x | pattern_vars(q)]
      [:tuple | ps] -> Enum.flat_map(ps, &pattern_vars/1)
      [:record, _type | fields] -> Enum.flat_map(fields, fn [_l, q] -> pattern_vars(q); _ -> [] end)
      [:inj, _type, _c | ps] -> Enum.flat_map(ps, &pattern_vars/1)
      _ -> []
    end
  end

  # The field types a record of `type` must give.
  defp record_shape(ctx, [:Record, row] = type) do
    case split_row(row) do
      {fields, :closed} -> Map.new(fields, fn [l, t] -> {l, t} end)
      _ -> fail(ctx, "a record value needs a closed row, not #{show(type)}")
    end
  end

  defp record_shape(ctx, [t | args] = type) when is_atom(t) and t not in @reserved do
    if record?(ctx, t), do: Map.new(record_fields(ctx, t, args)), else: fail(ctx, "#{show(type)} is not a record type")
  end

  defp record_shape(ctx, type), do: fail(ctx, "#{show(type)} is not a record type")

  defp select_type(ctx, [t | args], l) when is_atom(t) and t not in @reserved and is_atom(l) do
    if record?(ctx, t), do: ctx |> record_fields(t, args) |> List.keyfind(l, 0) |> then(&(&1 && elem(&1, 1)))
  end

  defp select_type(_ctx, [:Record, row], l) when is_atom(l) do
    {fields, _tail} = split_row(row)
    Enum.find_value(fields, fn [label, t] -> label == l && t end)
  end

  defp select_type(_ctx, [:Tuple | ts], l) when is_integer(l) and l >= 0 and l < length(ts), do: Enum.at(ts, l)
  defp select_type(_ctx, _type, _l), do: nil

  defp kinded(ctx, :Type, arg), do: wf(ctx, arg) && {:type, arg}
  defp kinded(ctx, :Row, [:row | _] = arg), do: wf_row(ctx, arg) && {:row, arg}
  defp kinded(ctx, :Eff, [:eff | _] = arg), do: wf_eff(ctx, arg) && {:eff, arg}
  defp kinded(ctx, kind, arg), do: fail(ctx, "#{show(arg)} is not of kind #{kind}")

  # §10.10
  defp prim_type(ctx, op, es) when op in [:eq, :ne] do
    case es do
      [a, b] ->
        type = must_synth(ctx, a, "the first operand of #{show(op)}")
        unless ground_data?(ctx, type), do: not_ground(ctx, "#{show(op)} compares values of", type)
        check_term(ctx, b, type)
        @bool

      _ ->
        fail(ctx, "#{show(op)} takes 2 operands")
    end
  end

  defp prim_type(ctx, op, es) when op in [:length, :empty?, :head, :tail] do
    case es do
      [xs] ->
        case must_synth(ctx, xs, "the operand of #{show(op)}") do
          [:List, elem] -> %{length: :Int, empty?: @bool, head: elem, tail: [:List, elem]}[op]
          other -> fail(ctx, "#{show(op)} needs a list, not #{show(other)}")
        end

      _ ->
        fail(ctx, "#{show(op)} takes 1 operand")
    end
  end

  defp prim_type(ctx, op, es) do
    case Map.fetch(@prims, op) do
      {:ok, {params, result}} when length(params) == length(es) ->
        Enum.zip(es, params) |> Enum.each(fn {e, p} -> check_term(ctx, e, p) end)
        result

      {:ok, {params, _}} ->
        fail(ctx, "#{show(op)} takes #{length(params)} operands, not #{length(es)}")

      :error ->
        fail(ctx, "#{show(op)} is not a primitive")
    end
  end

  # §10.11: no arrow and no type, row or effect variable, through the
  # declarations of named types. A recursive occurrence is assumed ground,
  # which is what the rest of its declaration then decides.
  defp ground_data?(ctx, type), do: ground_data?(ctx, type, MapSet.new())

  defp ground_data?(_ctx, t, _seen) when t in @base, do: true
  defp ground_data?(ctx, [:Tuple | ts], seen), do: Enum.all?(ts, &ground_data?(ctx, &1, seen))
  defp ground_data?(ctx, [:Pid, t], seen), do: ground_data?(ctx, t, seen)
  defp ground_data?(ctx, [:Map, k, v], seen), do: ground_data?(ctx, k, seen) and ground_data?(ctx, v, seen)

  defp ground_data?(ctx, [:Record, row], seen) do
    {fields, tail} = split_row(row)
    tail == :closed and Enum.all?(fields, fn [_, t] -> ground_data?(ctx, t, seen) end)
  end

  defp ground_data?(ctx, [t | args], seen) when is_atom(t) and t not in @reserved do
    cond do
      not Enum.all?(args, &ground_data?(ctx, &1, seen)) ->
        false

      MapSet.member?(seen, t) ->
        true

      true ->
        seen = MapSet.put(seen, t)

        member_types =
          case Map.get(ctx.types, t) do
            {:record, _, _} -> ctx |> record_fields(t, args) |> Enum.map(&elem(&1, 1))
            {:sum, _, _} -> ctx |> constructors(t, args) |> Enum.flat_map(&elem(&1, 1))
          end

        Enum.all?(member_types, &ground_data?(ctx, &1, seen))
    end
  end

  # Arrows, foralls and variables.
  defp ground_data?(_ctx, _type, _seen), do: false

  defp not_ground(ctx, what, type) do
    fail(ctx, "#{what} #{show(type)}, which is not ground data: a function type or a type variable " <>
      "has no primitive equality or embedding into Dyn; pass an Eq dictionary instead (liquid-core.md §10.11)")
  end

  # --- §10.5 Checking -------------------------------------------------------------

  defp check_term(ctx, e, type) do
    {e, ctx} = enter(ctx, e)
    check_form(ctx, e, type)
  end

  defp check_form(ctx, [:perform, :crash, e], _type), do: check_term(ctx, e, [:Reason])

  defp check_form(ctx, [:if, c, e1, e2], type) do
    check_term(ctx, c, @bool)
    check_term(ctx, e1, type)
    check_term(ctx, e2, type)
  end

  defp check_form(ctx, [:match, e | clauses], type) when clauses != [] do
    for {ctx, body} <- clause_branches(ctx, e, clauses), do: check_term(ctx, body, type)
  end

  defp check_form(ctx, [:"handle-crash", e, [[x, reason], handler]], type) when is_atom(x) do
    unless equal?(ctx, norm(reason), [:Reason]), do: fail(ctx, "handle-crash binds a (Reason), not #{show(reason)}")
    check_term(ctx, e, type)
    check_term(bind(ctx, x, [:Reason]), handler, type)
  end

  defp check_form(ctx, [:let, [[x, xtype, e1]], e2], type) when is_atom(x) do
    xtype = norm(xtype)
    wf(ctx, xtype)
    bind_check(ctx, xtype, e1)
    check_term(bind(ctx, x, xtype), e2, type)
  end

  defp check_form(ctx, [:letrec, group, body], type) when is_list(group), do: check_term(check_group(ctx, group), body, type)

  defp check_form(ctx, [:fn, params, body], [:->, ptypes, _eff, result] = type) when is_list(params) do
    {ctx, types} = bind_params(ctx, params)

    unless length(types) == length(ptypes) and Enum.zip(types, ptypes) |> Enum.all?(fn {a, b} -> equal?(ctx, a, b) end),
      do: fail(ctx, "a fn with parameters #{show(types)} does not have type #{show(type)}")

    check_term(ctx, body, result)
  end

  defp check_form(ctx, e, type) do
    case synth_form(ctx, e) do
      {:ok, actual} ->
        unless equal?(ctx, actual, type), do: fail(ctx, "expected #{show(type)}, found #{show(actual)}")
        :ok

      :none ->
        fail(ctx, "expected #{show(type)}, found a term that never returns")
    end
  end

  # §10.6
  defp common(branches) do
    results = Enum.map(branches, fn {ctx, e} -> {ctx, e, synth_term(ctx, e)} end)

    case Enum.find(results, &match?({_, _, {:ok, _}}, &1)) do
      nil ->
        :none

      {_, _, {:ok, type}} ->
        for {ctx, e, result} <- results do
          case result do
            {:ok, other} -> unless equal?(ctx, other, type), do: fail(ctx, "branches have types #{show(type)} and #{show(other)}")
            :none -> check_term(ctx, e, type)
          end
        end

        {:ok, type}
    end
  end

  # §10.7
  defp bind_check(ctx, [:forall, binders, body], e) do
    unless value?(e), do: fail(ctx, "a polymorphic binding must bind a value")
    check_term(%{ctx | delta: Map.merge(ctx.delta, Map.new(binders, fn [a, k] -> {a, k} end))}, e, body)
  end

  defp bind_check(ctx, type, e), do: check_term(ctx, e, type)

  defp check_group(ctx, group) do
    binds = for b <- group, do: (match?([f, _, _] when is_atom(f), b) && b) || fail(ctx, "malformed letrec binding #{show(b)}")
    names = Enum.map(binds, &hd/1)
    unless distinct?(names), do: fail(ctx, "a letrec binds a name twice")
    types = for [_f, t, _] <- binds, do: norm(t)
    Enum.each(types, &wf(ctx, &1))
    ctx = %{ctx | gamma: Map.merge(ctx.gamma, Map.new(Enum.zip(names, types)))}

    for {[f, _, fun], type} <- Enum.zip(binds, types) do
      unless match?([:fn | _], strip_own_meta(fun)), do: fail(ctx, "letrec binds #{show(f)} to something other than a fn")
      bind_check(ctx, type, fun)
    end

    ctx
  end

  defp value?(e) do
    case strip_own_meta(e) do
      [:fn | _] -> true
      [:atom, _] -> true
      [:unit] -> true
      [:inst, v | _] -> value?(v)
      [:tuple | es] -> Enum.all?(es, &value?/1)
      [:record, _type | fields] -> Enum.all?(fields, fn [_l, v] -> value?(v); _ -> false end)
      [:inj, _type, _c | es] -> Enum.all?(es, &value?/1)
      x -> not is_list(x)
    end
  end

  # --- Match clauses, guards (§6.6), patterns (§10.9), exhaustiveness (§10.8) -----

  defp clause_branches(ctx, scrutinee, clauses) do
    stype = must_synth(ctx, scrutinee, "the scrutinee of a match")

    branches =
      for clause <- clauses do
        {clause, cctx} = enter(ctx, clause)

        {pattern, guard, body} =
          case clause do
            [p, [:when, g], body] -> {p, g, body}
            [p, body] -> {p, nil, body}
            _ -> fail(cctx, "malformed clause #{show(clause)}")
          end

        pattern = Canonical.strip(pattern)
        binds = pattern_binds(cctx, pattern, stype, %{})
        cctx = %{cctx | gamma: Map.merge(cctx.gamma, binds)}

        if guard do
          unless guard_safe?(guard), do: fail(cctx, "guard #{show(Canonical.strip(guard))} is not guard-safe")
          check_term(cctx, guard, @bool)
        end

        {cctx, body, pattern, guard}
      end

    unguarded = for {_, _, pattern, nil} <- branches, do: [normalize(ctx, pattern, stype)]

    case missing(ctx, unguarded, [stype]) do
      :covered -> :ok
      {:missing, [witness]} -> fail(ctx, "the match is not exhaustive: #{show_witness(witness)} is not matched")
    end

    for {cctx, body, _, _} <- branches, do: {cctx, body}
  end

  @guard_prims (Map.keys(@prims) ++ [:eq, :ne, :length, :empty?, :head, :tail]) -- [:concat]

  defp guard_safe?(g) do
    case strip_own_meta(g) do
      x when is_atom(x) or is_integer(x) or is_float(x) or is_binary(x) -> true
      [:atom, _] -> true
      [:unit] -> true
      [:select, e, _l] -> guard_safe?(e)
      [:tuple | es] -> Enum.all?(es, &guard_safe?/1)
      [:inj, _type, _c | es] -> Enum.all?(es, &guard_safe?/1)
      [:if, c, a, b] -> guard_safe?(c) and guard_safe?(a) and guard_safe?(b)
      [:prim, op | es] -> op in @guard_prims and Enum.all?(es, &guard_safe?/1)
      _ -> false
    end
  end

  defp pattern_binds(_ctx, :_, _type, binds), do: binds

  defp pattern_binds(ctx, x, type, binds) when is_atom(x) do
    if Map.has_key?(binds, x), do: fail(ctx, "a pattern binds #{show(x)} twice")
    Map.put(binds, x, type)
  end

  defp pattern_binds(ctx, p, type, binds) when is_integer(p) or is_float(p) or is_binary(p) or p == [:unit] do
    expect_pattern_type(ctx, p, literal_type(p), type)
    binds
  end

  defp pattern_binds(ctx, [:atom, a] = p, type, binds) when is_atom(a) do
    expect_pattern_type(ctx, p, :Atom, type)
    binds
  end

  defp pattern_binds(ctx, [:as, x, p], type, binds) when is_atom(x),
    do: pattern_binds(ctx, x, type, pattern_binds(ctx, p, type, binds))

  defp pattern_binds(ctx, [:tuple | ps] = p, type, binds) do
    case type do
      [:Tuple | ts] when length(ts) == length(ps) ->
        Enum.zip(ps, ts) |> Enum.reduce(binds, fn {p, t}, acc -> pattern_binds(ctx, p, t, acc) end)

      _ ->
        fail(ctx, "pattern #{show(p)} does not match values of #{show(type)}")
    end
  end

  defp pattern_binds(ctx, [:record, ptype | fields] = p, type, binds) do
    ptype = norm(ptype)
    wf(ctx, ptype)
    unless Enum.all?(fields, &match?([l, _] when is_atom(l), &1)), do: fail(ctx, "malformed record pattern #{show(p)}")

    case ptype do
      [t | args] when is_atom(t) and t not in @reserved ->
        unless record?(ctx, t), do: fail(ctx, "pattern #{show(p)}: #{show(t)} is not a record type")
        unless equal?(ctx, ptype, type), do: fail(ctx, "pattern of #{show(ptype)} does not match values of #{show(type)}")
        declared = Map.new(record_fields(ctx, t, args))
        labels = for [l, _] <- fields, do: l
        unless distinct?(labels), do: fail(ctx, "pattern #{show(p)} names a field twice")

        Enum.reduce(fields, binds, fn [l, fp], acc ->
          case Map.fetch(declared, l) do
            {:ok, ft} -> pattern_binds(ctx, fp, ft, acc)
            :error -> fail(ctx, "#{show(t)} has no field #{show(l)}")
          end
        end)

      _ ->
        fail(ctx, "patterns over anonymous records are not in version 0")
    end
  end

  defp pattern_binds(ctx, [:inj, ptype, c | ps] = p, type, binds) do
    ptype = norm(ptype)
    wf(ctx, ptype)

    case ptype do
      [t | args] when is_atom(t) and t not in @reserved ->
        unless match?({:sum, _, _}, Map.get(ctx.types, t)), do: fail(ctx, "pattern #{show(p)}: #{show(t)} is not a sum type")
        unless equal?(ctx, ptype, type), do: fail(ctx, "pattern of #{show(ptype)} does not match values of #{show(type)}")

        case List.keyfind(constructors(ctx, t, args), c, 0) do
          {^c, fts} when length(fts) == length(ps) ->
            Enum.zip(ps, fts) |> Enum.reduce(binds, fn {p, ft}, acc -> pattern_binds(ctx, p, ft, acc) end)

          {^c, fts} ->
            fail(ctx, "constructor #{show(c)} has #{length(fts)} fields, not #{length(ps)}")

          nil ->
            fail(ctx, "#{show(c)} is not a constructor of #{show(t)}")
        end

      _ ->
        fail(ctx, "inj pattern needs a named sum type, not #{show(ptype)}")
    end
  end

  defp pattern_binds(ctx, p, _type, _binds), do: fail(ctx, "not a Core pattern: #{show(p)}")

  defp literal_type(lit) when is_integer(lit), do: :Int
  defp literal_type(lit) when is_float(lit), do: :Float
  defp literal_type(lit) when is_binary(lit), do: :String
  defp literal_type([:unit]), do: :Unit

  defp expect_pattern_type(ctx, p, actual, type) do
    unless equal?(ctx, actual, type), do: fail(ctx, "pattern #{show(p)} does not match values of #{show(type)}")
    true
  end

  # Patterns as the usefulness algorithm sees them: :wild, or {:con, key, subpatterns}.
  defp normalize(_ctx, p, _type) when is_atom(p), do: :wild
  defp normalize(ctx, [:as, _x, p], type), do: normalize(ctx, p, type)
  defp normalize(_ctx, lit, _type) when is_integer(lit) or is_float(lit) or is_binary(lit), do: {:con, {:lit, lit}, []}
  defp normalize(_ctx, [:atom, a], _type), do: {:con, {:lit, {:atom, a}}, []}
  defp normalize(_ctx, [:unit], _type), do: {:con, :unit, []}
  defp normalize(ctx, [:tuple | ps], [:Tuple | ts]), do: {:con, :tuple, Enum.zip_with(ps, ts, &normalize(ctx, &1, &2))}

  defp normalize(ctx, [:record, _ptype | fields], [t | args]) do
    given = Map.new(fields, fn [l, p] -> {l, p} end)
    {:con, :record, for({l, ft} <- record_fields(ctx, t, args), do: normalize(ctx, Map.get(given, l, :_), ft))}
  end

  defp normalize(ctx, [:inj, _ptype, c | ps], [t | args]) do
    {^c, fts} = List.keyfind(constructors(ctx, t, args), c, 0)
    {:con, {:ctor, c}, Enum.zip_with(ps, fts, &normalize(ctx, &1, &2))}
  end

  # The constructors of a type (§10.8): {:finite, [{key, field_types}]} or :infinite.
  defp signature(ctx, [t | args] = _type) when is_atom(t) and t not in @reserved do
    case Map.get(ctx.types, t) do
      {:sum, _, _} -> {:finite, for({c, fts} <- constructors(ctx, t, args), do: {{:ctor, c}, fts})}
      {:record, _, _} -> {:finite, [{:record, ctx |> record_fields(t, args) |> Enum.map(&elem(&1, 1))}]}
    end
  end

  defp signature(_ctx, [:Tuple | ts]), do: {:finite, [{:tuple, ts}]}
  defp signature(_ctx, :Unit), do: {:finite, [{:unit, []}]}
  defp signature(_ctx, _type), do: :infinite

  # Is the all-wildcard row useful against `rows` (Maranget 2007)? If so, a
  # witness: a vector of patterns matched by no row.
  defp missing(_ctx, rows, []), do: if(rows == [], do: {:missing, []}, else: :covered)

  defp missing(ctx, rows, [type | types]) do
    heads = rows |> Enum.flat_map(fn [p | _] -> if match?({:con, _, _}, p), do: [elem(p, 1)], else: [] end) |> Enum.uniq()
    sig = signature(ctx, type)

    complete? =
      case sig do
        {:finite, ctors} -> Enum.all?(ctors, fn {k, _} -> k in heads end)
        :infinite -> false
      end

    if complete? do
      {:finite, ctors} = sig

      Enum.find_value(ctors, :covered, fn {k, fts} ->
        n = length(fts)

        case missing(ctx, specialize(rows, k, n), fts ++ types) do
          {:missing, w} -> {:missing, [{:con, k, Enum.take(w, n)} | Enum.drop(w, n)]}
          :covered -> nil
        end
      end)
    else
      case missing(ctx, for([:wild | rest] <- rows, do: rest), types) do
        :covered ->
          :covered

        {:missing, w} ->
          head =
            case sig do
              {:finite, ctors} ->
                {k, fts} = Enum.find(ctors, fn {k, _} -> k not in heads end)
                {:con, k, List.duplicate(:wild, length(fts))}

              :infinite ->
                :wild
            end

          {:missing, [head | w]}
      end
    end
  end

  defp specialize(rows, k, n) do
    Enum.flat_map(rows, fn
      [{:con, ^k, args} | rest] -> [args ++ rest]
      [{:con, _other, _} | _rest] -> []
      [:wild | rest] -> [List.duplicate(:wild, n) ++ rest]
    end)
  end

  defp show_witness(:wild), do: "_"
  defp show_witness({:con, {:ctor, c}, []}), do: "(#{show(c)})"
  defp show_witness({:con, {:ctor, c}, args}), do: "(#{show(c)} #{Enum.map_join(args, " ", &show_witness/1)})"
  defp show_witness({:con, :tuple, args}), do: "(tuple #{Enum.map_join(args, " ", &show_witness/1)})"
  defp show_witness({:con, :record, args}), do: "(record #{Enum.map_join(args, " ", &show_witness/1)})"
  defp show_witness({:con, :unit, []}), do: "(unit)"

  # --- Metadata --------------------------------------------------------------------

  defp meta?([:meta | _]), do: true
  defp meta?(_), do: false

  defp split_meta(node) when is_list(node) do
    {metas, rest} = Enum.split_with(node, &meta?/1)
    span = Enum.find_value(metas, fn [:meta | items] -> Enum.find(items, &match?([:span | _], &1)) end)
    {rest, span}
  end

  defp split_meta(leaf), do: {leaf, nil}

  defp strip_own_meta(node), do: node |> split_meta() |> elem(0)

  defp enter(ctx, e) do
    case split_meta(e) do
      {e, nil} -> {e, ctx}
      {e, span} -> {e, %{ctx | span: span}}
    end
  end

  defp distinct?(xs), do: length(Enum.uniq(xs)) == length(xs)
  defp show(tree), do: tree |> Canonical.strip() |> Canonical.render()
end
