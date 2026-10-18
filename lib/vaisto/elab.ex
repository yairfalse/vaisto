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

  The native path covers the original Core fragment, checked externs and
  `Dyn` boundaries, typed processes, selective receive, supervision, local
  type classes, canonical typed imports, prompt pipelines and full try/after.
  Values and functions carry signatures; expression programs synthesize a
  local `main/0`. Missing constructs are reported rather than guessed:

    * `{:ok, core_module}`;
    * `{:error, errors}`, a list of `%Vaisto.Error{}`;
    * `{:outside, reason}`.

  """

  alias Vaisto.Error
  alias Vaisto.Elab.{Classes, Exhaustive, Types}
  alias Vaisto.Liquid.Prelude
  alias Vaisto.Parser.Loc

  @higher_order %{map: :"prelude.map", filter: :"prelude.filter", fold: :"prelude.fold", flat_map: :"prelude.flat_map"}
  @arith %{:+ => {:add, :fadd}, :- => {:sub, :fsub}, :* => {:mul, :fmul}}
  @compare %{:< => {:lt, :flt}, :> => {:gt, :fgt}, :<= => {:le, :fle}, :>= => {:ge, :fge}}
  @operators Map.keys(@arith) ++ Map.keys(@compare) ++ [:/, :div, :rem, :==, :!=, :and, :or, :andalso, :orelse, :++, :not, :cons]
  @no_match [:perform, :crash, [:inj, [:Reason], :no_match]]
  @decode_decls %{
    "Core.DecodeError": %{params: [], kind: :sum, members: [{:"Core.BadDecode", []}]},
    "Core.Result": %{params: [:e, :a], kind: :sum, members: [{:"Core.Err", [[:tvar, :e]]}, {:"Core.Ok", [[:tvar, :a]]}]},
    "Core.TaskResult": %{params: [:a], kind: :sum, members: [{:ok, [[:tvar, :a]]}, {:error, [:Dyn]}]}
  }

  # Names a declaration may not take: Core's reserved and built-in types.
  @reserved_types [:Int, :Float, :String, :Atom, :Ref, :Dyn, :Unit, :Tuple, :Record, :Pid, :Map, :Bool, :List, :Reason, :Fn] ++ Map.keys(@decode_decls)
  @primitive_names [:int, :float, :string, :binary, :bool, :atom, :unit, :ref, :dyn, :any, :num]

  # decls: type => %{params, kind, members}; ctors: constructor => type;
  # sigs: definition => Core type; env: source name => {core name, type};
  # tvars: the signature's type variables; s: the substitution; origins: what
  # each metavariable stands for, and where; numeric, ground, matches: checks
  # that wait for the end of the definition; loc: the nearest location.
  defstruct decls: %{}, ctors: %{}, sigs: %{}, env: %{}, tvars: MapSet.new(), s: %{}, next: 0, origins: %{},
            names: MapSet.new(), prelude: MapSet.new(), loc: nil, ground: [], matches: [],
            externs: %{}, eff: [:eff, :closed], effect_checks: [], data: [], processes: %{}, type_probe: false, in_guard: false,
            classes: %{}, methods: %{}, instances: %{}, dictionaries: %{}, imports: %{}, type_aliases: %{}, prompts: %{}, values: MapSet.new(), class_defaults: %{}, function_constraints: %{}

  @doc "Elaborate a parsed program into a Core module with explicit definition signatures."
  @spec module(term(), keyword()) :: {:ok, term()} | {:error, [Error.t()]} | {:outside, String.t()}
  def module(ast, opts \\ []) do
    forms = Classes.expand(if is_list(ast), do: ast, else: [ast])

    with :ok <- parse_errors(forms),
         {:ok, imports, aliases, imported_types} <- imports(forms, opts),
         {:ok, ctx} <- declarations(forms, imported_types, aliases),
         ctx = %{ctx | imports: imports, type_aliases: aliases, type_probe: Keyword.get(opts, :type_probe, false)},
         {:ok, ctx} <- Classes.collect(forms, ctx),
         {:ok, ctx} <- prompts(forms, ctx),
         {:ok, ctx} <- externs(forms, ctx),
         {:ok, ctx, defs} <- signatures(forms, ctx),
         {:ok, ctx, defs} <- class_factories(ctx, defs),
         {:ok, ctx, defs} <- processes(forms, ctx, defs),
         {:ok, ctx, defs} <- expressions(forms, ctx, defs, Keyword.get(opts, :expression_entry, :main)) do
      {ctx, defs} = infer_effects(ctx, defs)
      {items, errors, prelude} =
        Enum.reduce(defs, {[], [], MapSet.new()}, fn {form, sig}, {items, errors, prelude} ->
          case definition(form, sig, ctx) do
            {:ok, item, used} -> {items ++ [item], errors, MapSet.union(prelude, used)}
            {:error, error} -> {items, errors ++ List.wrap(error), prelude}
          end
        end)

      if errors == [] do
        items = Classes.prune_builtins(items, ctx.instances, ctx.class_defaults)
        headers = ctx.imports |> Map.values() |> Enum.uniq_by(& &1.module) |> Enum.sort_by(& &1.module) |> Enum.map(&[:import, &1.module, &1.digest, &1.exports])
        metadata = Classes.export_constraints(ctx)
        {:ok, [:module, Keyword.get(opts, :name, :Main), [:"core-version", 0] | core_decls(ctx) ++ headers ++ metadata ++ Prelude.defs(prelude, Map.keys(ctx.sigs)) ++ items]}
      else
        {:error, errors}
      end
    end
  catch
    {:outside, reason} -> {:outside, reason}
    {:classes, error} -> {:error, [error]}
    {:elab, error} -> {:error, [error]}
    {:elab_errors, errors} -> {:error, errors}
    {:type_error, message} -> {:error, [Error.new(message)]}
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

  defp imports(forms, opts) do
    Enum.reduce_while(forms, {:ok, %{}, %{}, []}, fn
      {:import, name, alias_name, loc}, {:ok, imports, aliases, types} ->
        supplied = Keyword.get(opts, :interfaces, %{})
        result = case Map.fetch(supplied, name) do
          {:ok, tree} -> Vaisto.Interface.validate_core(tree)
          :error -> Vaisto.Interface.load_core(name, Keyword.get(opts, :search_paths, ["."]), Keyword.take(opts, [:atoms]))
        end
        case result do
          {:ok, interface} ->
            bare = fn m -> m |> Atom.to_string() |> String.replace_prefix("Elixir.", "") end
            if bare.(name) != bare.(interface.module), do: throw({:elab, Error.new("imported interface names a different module", span: span(loc))})
            if Enum.any?(imports, fn {_, i} -> i.module == interface.module and i.digest != interface.digest end),
              do: throw({:elab, Error.new("conflicting interface digests for `#{interface.module}`", span: span(loc))})
            alias_name = alias_name || name
            unless is_atom(alias_name), do: throw({:elab, Error.new("an import alias must be a name", span: span(loc))})
            if Map.has_key?(imports, alias_name), do: throw({:elab, Error.new("duplicate import alias `#{alias_name}`", span: span(loc))})
            aliases = Enum.reduce(interface.types, aliases, fn [:type, t, params, _], aliases -> Map.put(aliases, :"#{alias_name}.#{t}", {t, length(params)}) end)
            {:cont, {:ok, Map.put(imports, alias_name, interface), aliases, Enum.uniq(types ++ interface.types)}}
          {:error, error} -> {:halt, {:error, [error]}}
        end
      _, acc -> {:cont, acc}
    end)
  catch
    {:elab, error} -> {:error, [error]}
  end

  defp declarations(forms, imported_types, aliases) do
    deftypes = for {:deftype, t, body, loc} <- forms, do: {t, body, loc}
    protocols = for {:process, name, _, handlers, loc} <- forms do
      members = Enum.map(handlers, fn {msg, _} -> {message_name(msg), []} end)
      body = if members == [], do: {:product, []}, else: {:sum, members}
      {protocol_name(name), body, loc}
    end
    deftypes = deftypes ++ protocols
    arities = Map.new(@decode_decls, fn {t, d} -> {t, length(d.params)} end)
      |> Map.merge(Map.new(deftypes, fn {t, body, _} -> {t, length(decl_params(body))} end))
      |> Map.merge(Map.new(imported_types, fn [:type, t, ps, _] -> {t, length(ps)} end))
      |> Map.merge(aliases)
    builtin_ctors = for {t, d} <- @decode_decls, t != :"Core.TaskResult", {c, _} <- d.members, into: %{}, do: {c, t}

    {decls, ctors, errors} =
      Enum.reduce(deftypes, {@decode_decls, builtin_ctors, []}, fn {t, body, loc}, {decls, ctors, errors} ->
        protocol? = Enum.any?(protocols, fn {name, _, _} -> name == t end)
        case decl(t, body, arities, decls, ctors, protocol?) do
          {:ok, d} ->
            # Protocol constructors are internal: two actors can both accept
            # :get without claiming the same surface constructor name.
            new_ctors = if d.kind == :sum and not protocol?, do: for({c, _} <- d.members, into: %{}, do: {c, t}), else: %{}
            {Map.put(decls, t, d), Map.merge(ctors, new_ctors), errors}

          {:error, message} ->
            {decls, ctors, errors ++ [Error.new(message, span: span(loc))]}
        end
      end)

    {decls, ctors, errors} = Enum.reduce(imported_types, {decls, ctors, errors}, fn [:type, t, params, [kind | members]], {decls, ctors, errors} ->
      members = if kind == :sum, do: Enum.map(members, fn [c | fs] -> {c, fs} end), else: Enum.map(members, fn [l, type] -> {l, type} end)
      d = %{params: params, kind: kind, members: members}
      cond do
        Map.has_key?(decls, t) and Map.fetch!(decls, t) != d -> {decls, ctors, errors ++ [Error.new("conflicting imported type declaration `#{t}`")]}
        kind == :sum and Enum.any?(members, fn {c, _} -> Map.has_key?(ctors, c) and Map.fetch!(ctors, c) != t end) ->
          {decls, ctors, errors ++ [Error.new("conflicting imported constructors for `#{t}`")]}
        true ->
          ctors = if kind == :sum, do: Enum.reduce(members, ctors, fn {c, _}, acc -> Map.put(acc, c, t) end), else: ctors
          {Map.put(decls, t, d), ctors, errors}
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

  defp decl(t, body, arities, decls, ctors, protocol?) do
    params = decl_params(body)
    kind = if match?({:sum, _}, body), do: :sum, else: :record

    names =
      case body do
        {:sum, variants} -> Enum.map(variants, &elem(&1, 0))
        {:product, fields} -> Enum.map(fields, &elem(&1, 0))
      end

    cond do
      not is_atom(t) -> throw({:type_error, "a type declaration needs an unqualified name"})
      t in @reserved_types -> throw({:type_error, "`#{t}` is a built-in type and cannot be declared"})
      Map.has_key?(decls, t) -> throw({:type_error, "the type `#{t}` is declared twice"})
      Map.has_key?(ctors, t) -> throw({:type_error, "`#{t}` is already a constructor"})
      Enum.find(params, &(&1 in @primitive_names)) -> throw({:type_error, "`#{Enum.find(params, &(&1 in @primitive_names))}` is a type, so it cannot name a type variable: write :#{Enum.find(params, &(&1 in @primitive_names))}"})
      length(Enum.uniq(names)) != length(names) -> throw({:type_error, "`#{t}` names a #{if kind == :sum, do: "constructor", else: "field"} twice"})
      not Enum.all?(names, &is_atom/1) -> throw({:type_error, "constructor and field names must be unqualified"})
      kind == :sum and not protocol? and Enum.find(names, &(Map.has_key?(ctors, &1) or Map.has_key?(decls, &1))) -> throw({:type_error, "the constructor `#{Enum.find(names, &(Map.has_key?(ctors, &1) or Map.has_key?(decls, &1)))}` is already defined"})
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
    for {t, %{params: params, kind: kind, members: members}} <- Enum.sort(ctx.decls), not is_map_key(@decode_decls, t) do
      body =
        case kind do
          :sum -> [:sum | for({c, fs} <- members, do: [c | fs])]
          :record -> [:record | for({l, ft} <- members, do: [l, ft])]
        end

      [:type, t, params, body]
    end
  end

  # --- Signatures (Q35) ----------------------------------------------------------------

  defp externs(forms, ctx) do
    Enum.reduce_while(forms, {:ok, ctx}, fn
      {:extern, m, f, args, ret, loc}, {:ok, ctx} ->
        ctx = %{ctx | loc: loc}
        try do
          if Map.has_key?(ctx.externs, {m, f, length(args)}), do: fail(ctx, "the extern `#{m}:#{f}/#{length(args)}` is declared twice")
          types = Enum.map(args ++ [ret], &surface_type(&1, ctx))
          for type <- types, do: require_data(type, "an extern type", ctx)
          {params, [result]} = Enum.split(types, -1)
          {:cont, {:ok, %{ctx | externs: Map.put(ctx.externs, {m, f, length(args)}, {params, result})}}}
        catch
          {:elab, error} -> {:halt, {:error, [error]}}
        end

      _, acc -> {:cont, acc}
    end)
  end

  # Propagate effects when no explicit contract was written. Explicit rows
  # remain contracts: checking a body may not silently widen them.
  defp infer_effects(ctx, defs) do
    next = Enum.reduce(defs, ctx.sigs, fn {form, sig}, sigs ->
      f = elem(form, 1)
      effects = effects(form, %{ctx | sigs: sigs})
      explicit = case form do
        {:defn, _, _, _, {:effectful_result, _, _, _}, _} -> true
        {:defn, _, _, _, {:effectful_result, _, _, _}, _, _} -> true
        {:defn_constrained, _, _, _, {:effectful_result, _, _, _}, _, _, _} -> true
        {:defn_multi, _, _, {:call, :Fn, [_, _, {:atom, :effects}, _], _}, _} -> true
        _ -> false
      end
      type = cond do
        explicit -> sig
        Map.has_key?(ctx.function_constraints, f) -> with_inner_effects(sig, effects)
        true -> with_effects(sig, effects)
      end
      Map.put(sigs, f, type)
    end)
    if next == ctx.sigs do
      {ctx, Enum.map(defs, fn {form, _} -> {form, Map.fetch!(next, elem(form, 1))} end)}
    else
      infer_effects(%{ctx | sigs: next}, defs)
    end
  end

  defp with_effects([:forall, vars, type], effects), do: [:forall, vars, with_effects(type, effects)]
  defp with_effects([:->, ps, [:eff | labels], r], effects) do
    {labels, [tail]} = Enum.split(labels, -1)
    [:->, ps, [:eff | Enum.sort(Enum.uniq(labels ++ effects)) ++ [tail]], r]
  end
  defp with_inner_effects([:forall, vars, type], effects), do: [:forall, vars, with_inner_effects(type, effects)]
  defp with_inner_effects([:->, dicts, eff, inner], effects), do: [:->, dicts, eff, with_effects(inner, effects)]

  defp effects({:qualified, m, f}, ctx) do
    case Map.get(ctx.imports, m) do
      nil -> if(Enum.any?(ctx.externs, fn {{module, function, _}, _} -> module == m and function == f end), do: [:external], else: [])
      i ->
        case Enum.find(i.exports, fn [name, _, _] -> name == f end) do
          [^f, type, _] ->
            type = if Map.has_key?(i.constraints, f), do: List.last(open(type)), else: type
            [:->, _, [:eff | labels], _] = open(type)
            Enum.drop(labels, -1)
          _ -> []
        end
    end
  end
  defp effects({:call, :spawn, args, _}, ctx), do: Enum.uniq([:spawn | effects(args, ctx)])
  defp effects({:call, f, args, _}, ctx) when f in [:"!", :"!!"], do: Enum.uniq([:send | effects(args, ctx)])
  defp effects({:receive, clauses, _}, ctx), do: Enum.uniq([:receive | effects(clauses, ctx)])
  defp effects({:supervise, _, children, _}, ctx), do: Enum.uniq([:supervise, :spawn, :link | effects(children, ctx)])
  defp effects({:supervisor_start, _, _, children, _}, ctx), do: Enum.uniq([:supervise, :spawn, :link | effects(children, ctx)])
  defp effects({:instance_factory, _, _, _}, _ctx), do: []
  defp effects({:class_default, _, _, _}, _ctx), do: []
  defp effects({:forward, f, module, _}, ctx) do
    i = ctx.imports |> Map.values() |> Enum.find(&(&1.module == module))
    [^f, type, _] = Enum.find(i.exports, fn [name, _, _] -> name == f end)
    [:->, _, [:eff | labels], _] = open(type)
    Enum.drop(labels, -1)
  end
  defp effects({:pipeline, _, _, _, _, _}, _ctx), do: [:external]
  defp effects(x, ctx) when is_atom(x) do
    type = case Map.get(ctx.env, x) do
      {_, t} -> t
      nil -> Map.get(ctx.sigs, x)
    end
    type = if not Map.has_key?(ctx.env, x) and Map.has_key?(ctx.function_constraints, x), do: List.last(open(type)), else: type
    case open(type) do
      [:->, _, [:eff | labels], _] -> Enum.drop(labels, -1)
      _ -> []
    end
  end
  defp effects(t, ctx) when is_tuple(t), do: t |> Tuple.to_list() |> effects(ctx)
  defp effects(l, ctx) when is_list(l), do: l |> Enum.flat_map(&effects(&1, ctx)) |> Enum.uniq()
  defp effects(_, _ctx), do: []

  defp require_data(type, what, ctx) do
    unless Vaisto.Liquid.Lint.decodable?(type, types: core_decls(ctx)),
      do: fail(ctx, "#{what} must be decodable ground data, but this is #{Types.format(type)}")
  end

  defp allow_external(ctx) do
    allow_effects([:external], ctx)
  end

  defp allow_effects(labels, ctx) do
    allow_effect_row([:eff | Enum.sort(Enum.uniq(labels)) ++ [:closed]], ctx)
  end

  defp allow_effect_row(row, ctx) do
    %{ctx | effect_checks: [{row, ctx.eff, ctx.loc} | ctx.effect_checks]}
  end

  defp check_effect_row(row, allowed, ctx) do
    {labels, [tail]} = row |> tl() |> Enum.split(-1)
    {permitted, [own_tail]} = allowed |> tl() |> Enum.split(-1)
    for label <- labels, label not in permitted,
      do: fail(ctx, "this call performs `#{label}`, but a pure function is expected")
    if tail != :closed and tail != own_tail,
      do: fail(ctx, "this call has an open effect row that this function does not allow")
  end

  defp signatures(forms, ctx) do
    arities = Map.new(ctx.decls, fn {t, d} -> {t, length(d.params)} end) |> Map.merge(ctx.type_aliases)
    constraints = for {:defn_constrained, f, _, _, _, _, constraints, loc} <- forms, into: %{} do
      constraints = Enum.map(constraints, fn {class, annotation} ->
        unless Map.has_key?(ctx.classes, class), do: fail(%{ctx | loc: loc}, "unknown constraint class `#{class}`")
        case Types.from_surface(annotation, arities) do
          {:ok, t} -> {class, t}
          {:error, message} -> fail(%{ctx | loc: loc}, message)
        end
      end)
      if Enum.uniq(constraints) != constraints, do: fail(%{ctx | loc: loc}, "duplicate function constraint")
      {f, constraints}
    end

    {sigs, defs, errors} =
      Enum.reduce(forms, {%{}, [], []}, fn form, acc ->
        case form do
          {:defn, f, params, _body, ret, loc} -> signature(f, params, ret, loc, arities, form, acc)
          {:defn, f, params, _body, ret, _guard, loc} -> signature(f, params, ret, loc, arities, form, acc)
          {:defn_constrained, f, params, _, ret, _, _, loc} ->
            case signature(f, params, ret, loc, arities, form, {%{}, [], []}) do
              {sigs, _, []} ->
                user = open(Map.fetch!(sigs, f))
                constraints = Map.fetch!(constraints, f)
                for {_, t} <- constraints, a <- Types.tvars(t), a not in Types.tvars(user), do: fail(%{ctx | loc: loc}, "constraint variable `#{a}` is absent from the function signature")
                dicts = Enum.map(constraints, fn {c, t} -> Classes.dictionary_type(ctx.classes, c, t) end)
                type = Types.generalize([:->, dicts, [:eff, :closed], user])
                add_signature(f, type, loc, form, acc)
              {_, _, errors} -> Enum.reduce(errors, acc, &add_error(&2, &1))
            end
          {:defn_multi, f, _clauses, loc} ->
            add_error(acc, Error.new("`#{f}` needs a signature", span: span(loc), hint: "write (defn #{f} (Fn argument-type result-type) [pattern body] ...)"))
          {:defn_multi, f, _clauses, annotation, loc} ->
            case Types.from_surface(annotation, arities) do
              {:ok, [:->, [_], _, _] = type} -> add_signature(f, Types.generalize(type), loc, form, acc)
              {:ok, _} -> add_error(acc, Error.new("`#{f}` needs a one-argument function signature", span: span(loc)))
              {:error, message} -> add_error(acc, Error.new("in signature of `#{f}`: #{message}", span: span(loc)))
            end
          {:defval, f, {:call, :the, [annotation, body], _}, loc} ->
            case Types.from_surface(annotation, arities) do
              {:ok, type} -> add_signature(f, Types.generalize([:->, [], [:eff, :closed], type]), loc, {:value_def, f, body, loc}, acc)
              {:error, message} -> add_error(acc, Error.new("in signature of `#{f}`: #{message}", span: span(loc)))
            end
          {:defval, f, _value, loc} ->
            add_error(acc, Error.new("`#{f}` needs a signature", span: span(loc), hint: "write (def #{f} TYPE value)"))
          {:supervise, strategy, children, loc} ->
            type = [:->, [:Dyn], [:eff, :link, :spawn, :supervise, :closed], [:Tuple, :Atom, [:Pid, :Dyn]]]
            add_signature(:start_link, type, loc, {:supervisor_start, :start_link, strategy, children, loc}, acc)
          {:deftype, _, _, _} -> acc
          {:extern, _, _, _, _, _} -> acc
          {:defclass, _, _, _, _} -> acc
          {:instance, _, _, _, _} -> acc
          {:instance_constrained, _, _, _, _, _, _} -> acc
          {:derive, _, _, _, _} -> acc
          {:import, _, _, _} -> acc
          {:defprompt, _, _, _, _, _} -> acc
          {:pipeline, f, input, output, _ops, loc} ->
            type = [:->, [surface_type(input, %{ctx | loc: loc})], [:eff, :external, :closed], [:"Core.TaskResult", surface_type(output, %{ctx | loc: loc})]]
            add_signature(f, type, loc, form, acc)
          {:process, _, _, _, _} -> acc
          {:ns, _, _} -> acc
          _expression -> acc
        end
      end)

    values = for {:defval, f, _, _} <- forms, into: MapSet.new(), do: f
    if errors == [], do: {:ok, %{ctx | sigs: sigs, values: values, function_constraints: constraints}, defs}, else: {:error, errors}
  end

  defp signature(f, params, ret, loc, arities, form, acc) do
    {ret, eff} = case ret do
      {:effectful_result, ret, row, _} ->
        case Types.effect_from_surface(row) do
          {:ok, eff} -> {ret, eff}
          {:error, message} -> fail(%__MODULE__{loc: loc}, message)
        end
      ret -> {ret, [:eff, :closed]}
    end
    annotated = Enum.map(params, fn {p, t} -> {p, Types.from_surface(t, arities)} end)
    result = Types.from_surface(ret, arities)

    case {Enum.find(annotated, &match?({_, {:error, _}}, &1)), result} do
      {nil, {:ok, rtype}} ->
        add_signature(f, Types.generalize([:->, Enum.map(annotated, fn {_, {:ok, t}} -> t end), eff, rtype]), loc, form, acc)

      {missing, _} ->
        what =
          case missing do
            {p, {:error, message}} -> "the parameter `#{p}`: #{message}"
            nil -> "the result: #{elem(result, 1)}"
          end

        add_error(acc, Error.new("`#{f}` needs a signature", span: span(loc), note: what, hint: "annotate every parameter and the result, as in (defn #{f} [x :int] :int ...)"))
    end
  end

  defp add_signature(f, type, loc, form, {sigs, defs, errors} = acc) do
    cond do
      Map.has_key?(sigs, f) -> add_error(acc, Error.new("`#{f}` is defined twice", span: span(loc)))
      reserved_arity?(f, type, form) -> add_error(acc, Error.new("`#{f}` is a reserved form at this arity and cannot be redefined", span: span(loc)))
      true -> {Map.put(sigs, f, type), defs ++ [{form, type}], errors}
    end
  end

  defp reserved_arity?(f, type, form) do
    arity = Map.get(%{:the => 2, :up => 2, :decode => 2, :crash => 1, :spawn => 2, :"!" => 2, :"!!" => 2}, f)
    user = if elem(form, 0) == :defn_constrained, do: List.last(open(type)), else: open(type)
    case user do
      [:->, params, _, _] -> arity == length(params)
      _ -> false
    end
  end

  defp add_error({sigs, defs, errors}, error), do: {sigs, defs, errors ++ [error]}

  defp class_factories(ctx, defs) do
    acc = Enum.reduce(Enum.sort(ctx.class_defaults), {ctx.sigs, [], []}, fn {f, info}, acc ->
      form = if Map.has_key?(info, :remote), do: {:forward, f, info.remote, info.loc}, else: {:class_default, f, info, info.loc}
      add_signature(f, info.type, info.loc, form, acc)
    end)
    {sigs, factories, errors} = Enum.reduce(Enum.sort(ctx.instances), acc, fn {_, i}, acc ->
      dict = Classes.dictionary_type(ctx.classes, i.class, i.type)
      constraints = Enum.map(i.constraints, fn {c, t} -> Classes.dictionary_type(ctx.classes, c, t) end)
      type = Types.generalize([:->, constraints, [:eff, :closed], dict])
      form = if Map.has_key?(i, :remote), do: {:forward, i.factory, i.remote, i.loc}, else: {:instance_factory, i.factory, i, i.loc}
      add_signature(i.factory, type, i.loc, form, acc)
    end)
    clashes = Map.keys(ctx.methods) |> Enum.filter(&Map.has_key?(ctx.sigs, &1))
    errors = errors ++ Enum.map(clashes, &Error.new("`#{&1}` is already a class method"))
    if errors == [], do: {:ok, %{ctx | sigs: sigs}, factories ++ defs}, else: {:error, errors}
  end

  defp expressions(forms, ctx, defs, entry) when is_atom(entry) do
    expressions = Enum.reject(forms, fn form ->
      is_tuple(form) and elem(form, 0) in [:defn, :defn_constrained, :defn_multi, :defval, :deftype, :defclass, :instance, :instance_constrained, :derive, :extern, :process, :supervise, :ns, :import, :defprompt, :pipeline]
    end)
    if expressions == [] do
      {:ok, ctx, defs}
    else
      loc = Enum.find_value(expressions, &loc_of/1)
      body = {:do, expressions, loc}
      local = %{ctx | loc: loc, eff: [:eff, :external, :link, :receive, :send, :spawn, :supervise, :closed]}
      {core, type, local} = synth(body, local)
      finish(core, local)
      type = Types.zonk(type, local.s)
      {sigs, defs, errors} = add_signature(entry, [:->, [], [:eff, :closed], type], loc, {:expression_main, entry, body, loc}, {ctx.sigs, defs, []})
      if errors == [], do: {:ok, %{ctx | sigs: sigs}, defs}, else: {:error, errors}
    end
  catch
    {:elab, error} -> {:error, [error]}
  end

  defp prompts(forms, ctx) do
    prompts = Enum.reduce(forms, %{}, fn
      {:defprompt, name, input, output, template, loc}, prompts ->
        if Map.has_key?(prompts, name), do: fail(%{ctx | loc: loc}, "prompt `#{name}` is defined twice")
        input = surface_type(input, %{ctx | loc: loc})
        output = surface_type(output, %{ctx | loc: loc})
        require_data(input, "a prompt input", ctx)
        require_data(output, "a prompt output", ctx)
        for t <- [input, output] do
          unless Vaisto.LLM.OpenAI.Schema.core_supported?(t, core_decls(ctx)), do: fail(%{ctx | loc: loc}, "prompt types must have supported closed JSON schemas")
        end
        Map.put(prompts, name, %{input: input, output: output, template: template})
      _, prompts -> prompts
    end)
    {:ok, %{ctx | prompts: prompts}}
  catch
    {:elab, error} -> {:error, [error]}
  end

  defp task_projection(value, actual, expected, ctx) do
    if actual == expected do
      {value, ctx}
    else
      case {actual, expected} do
        {[:List, a], [:List, b]} ->
          {element, ctx} = fresh_name(:element, ctx)
          {projected, ctx} = task_projection(element, a, b, ctx)
          term = [:app, [:inst, :"prelude.map", a, b, [:eff, :closed]], [:fn, [[element, a]], projected], value]
          {term, %{ctx | prelude: MapSet.put(ctx.prelude, :"prelude.map")}}
        {[a | aa], [b | ba]} when is_map_key(ctx.decls, a) and is_map_key(ctx.decls, b) ->
          ad = Map.fetch!(ctx.decls, a)
          bd = Map.fetch!(ctx.decls, b)
          unless ad.kind == :record and bd.kind == :record, do: fail(ctx, "task projection requires record types")
          %{params: ap, members: af} = ad
          %{params: bp, members: bf} = bd
          as = Map.new(Enum.zip(ap, aa))
          bs = Map.new(Enum.zip(bp, ba))
          {fields, ctx} = Enum.map_reduce(bf, ctx, fn {label, t}, ctx ->
            {_, at} = List.keyfind(af, label, 0) || fail(ctx, "task projection is missing field `#{label}`")
            {field, ctx} = task_projection([:select, value, label], Types.subst_tvars(at, as), Types.subst_tvars(t, bs), ctx)
            {[label, field], ctx}
          end)
          {[:record, expected | fields], ctx}
        _ -> {value, unify!(expected, actual, ctx)}
      end
    end
  end

  defp pipeline_ops(value, type, [], output, ctx) do
    {value, ctx} = task_projection(value, type, output, ctx)
    {[:inj, [:"Core.TaskResult", output], :ok, value], ctx}
  end
  defp pipeline_ops(value, type, [{:generate, name, extract, loc} | ops], output, ctx) do
    ctx = %{ctx | loc: loc}
    prompt = Map.get(ctx.prompts, name) || throw({:elab, %{Vaisto.Errors.undefined_prompt(name, Map.keys(ctx.prompts)) | primary_span: span(loc)}})
    if prompt.template == nil, do: throw({:elab, Vaisto.Errors.prompt_missing_template(name, span: span(loc))})
    descriptor = Vaisto.LLM.OpenAI.Schema.core_descriptor(prompt.input, core_decls(ctx))
    fields = case descriptor do
      {:record, _, fields} -> fields
      _ -> []
    end
    for [field] <- Regex.scan(~r/\{([^}]+)\}/, prompt.template, capture: :all_but_first) do
      case Enum.find(fields, fn {label, _} -> Atom.to_string(label) == field end) do
        {_, t} when t in [:int, :float, :bool, :string] -> :ok
        _ -> fail(ctx, "template field `#{field}` must name a scalar field of the prompt input")
      end
    end
    target = surface_type(extract, ctx)
    {_, ctx} = task_projection(value, type, prompt.input, ctx)
    {_, ctx} = task_projection(value, prompt.output, target, ctx)
    for t <- [type, target] do
      unless Vaisto.LLM.OpenAI.Schema.core_supported?(t, core_decls(ctx)), do: fail(ctx, "task input and extract types must have a supported closed JSON schema")
    end
    result = [:"Core.TaskResult", target]
    {next, ctx} = fresh_name(:payload, ctx)
    {error, ctx} = fresh_name(:error, ctx)
    {rest, ctx} = pipeline_ops(next, target, ops, output, ctx)
    {[:match, [:perform, :generate, type, target, prompt.template, value],
      [[:inj, result, :ok, next], rest],
      [[:inj, result, :error, error], [:inj, [:"Core.TaskResult", output], :error, error]]], ctx}
  end
  defp pipeline_ops(_, _, _, _, ctx), do: fail(ctx, "this pipeline contains an unsupported operator")

  # A process's initializer determines one local state type; every handler
  # must return exactly that type. There is no inference across definitions.
  defp protocol_name(name) when is_atom(name), do: :"Process.#{name}.Message"
  defp protocol_name(_), do: throw({:outside, "a process whose name is not an atom"})
  defp message_name({:atom, name}), do: name
  defp message_name(name) when is_atom(name), do: name
  defp message_name(_), do: throw({:outside, "a process handler whose message is not an atom"})

  defp processes(forms, ctx, defs) do
    ps = for {:process, _, _, _, _} = p <- forms, do: p
    Enum.reduce_while(ps, {:ok, ctx, defs}, fn {:process, name, initial, handlers, loc}, {:ok, ctx, defs} ->
      try do
        local = %{ctx | loc: loc, eff: [:eff, :external, :send, :spawn, :closed]}
        {core, state, local} = synth(initial, local)
        {_, local} = finish(core, local)
        state = Types.zonk(state, local.s)
        require_data(state, "a process state", local)
        protocol = [protocol_name(name)]
        step = :"Process.#{name}.step"
        info = %{state: state, protocol: protocol, handlers: handlers, step: step}
        additions = [
          {{:process_start, name, loc}, [:->, [state], [:eff, :spawn, :closed], [:Pid, protocol]]},
          {{:process_step, step, handlers, loc}, [:->, [state, protocol], [:eff, :closed], state]}
        ]
        additions = if length(ps) == 1 and name != :start_link and not Map.has_key?(ctx.sigs, :start_link), do: additions ++ [
          {{:process_link, :start_link, name, loc}, [:->, [state], [:eff, :link, :spawn, :closed], [:Tuple, :Atom, [:Pid, protocol]]]}
        ], else: additions
        sigs = Enum.reduce(additions, ctx.sigs, fn {form, sig}, sigs ->
          f = elem(form, 1)
          if Map.has_key?(sigs, f), do: fail(local, "`#{f}` is defined twice")
          Map.put(sigs, f, sig)
        end)
        ctx = %{ctx | sigs: sigs, processes: Map.put(ctx.processes, name, info)}
        {:cont, {:ok, ctx, defs ++ additions}}
      catch
        {:elab, error} -> {:halt, {:error, [error]}}
      end
    end)
  end

  # --- Definitions -------------------------------------------------------------------

  defp definition(form, sig, ctx) do
    {name, loc} = {elem(form, 1), elem(form, tuple_size(form) - 1)}
    [:->, ptypes, eff, rtype] = arrow = open(sig)
    reserved = MapSet.new(Map.keys(ctx.sigs) ++ Prelude.names())
    ctx = %{ctx | names: reserved, tvars: MapSet.new(Enum.map(Types.variables(arrow), &elem(&1, 0))), loc: loc, eff: eff}

    {fun, ctx} =
      case form do
        {:forward, f, module, _} ->
          {params, ctx} = Enum.map_reduce(ptypes, ctx, fn type, ctx ->
            {name, ctx} = fresh_name(:arg, ctx)
            {[name, type], ctx}
          end)
          remote = case sig do
            [:forall, binders, _] -> [:inst, [:remote, module, f] | Enum.map(binders, fn [a, _] -> [:tvar, a] end)]
            _ -> [:remote, module, f]
          end
          {[:fn, params, [:app, remote | Enum.map(params, &hd/1)]], ctx}

        {:defn, _f, params, body, _ret, _loc} ->
          {core_params, ctx} = bind_params(Enum.map(params, &elem(&1, 0)), ptypes, ctx)
          {body, ctx} = check(body, rtype, ctx)
          {[:fn, core_params, body], ctx}

        {:expression_main, _, body, _} ->
          {body, ctx} = check(body, rtype, ctx)
          {[:fn, [], body], ctx}

        {:value_def, _, body, _} ->
          {body, ctx} = check(body, rtype, ctx)
          {[:fn, [], body], ctx}

        {:defn_constrained, f, source_params, body, _, guard, _, _} ->
          {dicts, ctx} = Enum.map_reduce(ptypes, ctx, fn t, ctx ->
            {name, ctx} = fresh_name(:dictionary, ctx)
            {[name, t], ctx}
          end)
          entries = Map.new(Enum.zip(Map.fetch!(ctx.function_constraints, f), dicts), fn {key, [x, _]} -> {key, x} end)
          [:->, types, inner_eff, result] = rtype
          ctx = %{ctx | dictionaries: entries, eff: inner_eff}
          {params, ctx} = bind_params(Enum.map(source_params, &elem(&1, 0)), types, ctx)
          {body, ctx} = check(body, result, ctx)
          {body, ctx} = if guard do
            {g, ctx} = guard(guard, ctx)
            {[:if, g, body, @no_match], ctx}
          else
            {body, ctx}
          end
          {[:fn, dicts, [:fn, params, body]], ctx}

        {:pipeline, _, _, _, ops, _} ->
          {params, ctx} = bind_params([:payload], ptypes, ctx)
          [[value, input]] = params
          [:"Core.TaskResult", output] = rtype
          {body, ctx} = pipeline_ops(value, input, ops, output, ctx)
          {[:fn, params, body], ctx}

        # A guard on a definition is the one partiality a program writes: the
        # call fails as no clause matching does when the guard is false.
        {:defn, _f, params, body, _ret, guard, _loc} ->
          {core_params, ctx} = bind_params(Enum.map(params, &elem(&1, 0)), ptypes, ctx)
          {g, ctx} = guard(guard, ctx)
          {body, ctx} = check(body, rtype, ctx)
          {[:fn, core_params, [:match, [:unit], [:_, [:when, g], body], [:_, @no_match]]], ctx}

        multi when elem(multi, 0) == :defn_multi ->
          clauses = elem(multi, 2)
          [ptype] = ptypes
          {arg, ctx} = fresh_name(:arg, ctx)
          {m, ctx} = match_clauses(arg, ptype, clauses, rtype, ctx)
          {[:fn, [[arg, ptype]], m], ctx}

        {:process_start, p, _} ->
          info = Map.fetch!(ctx.processes, p)
          {params, ctx} = bind_params([:initial_state], ptypes, ctx)
          [[arg, _]] = params
          tags = Enum.map(info.handlers, fn {m, _} -> message_name(m) end)
          {[:fn, params, [:perform, :spawn, info.protocol, info.state, tags, info.step, arg]], ctx}

        {:process_link, _, p, _} ->
          {params, ctx} = bind_params([:initial_state], ptypes, ctx)
          [[arg, _]] = params
          protocol = Map.fetch!(ctx.processes, p).protocol
          {pid, ctx} = fresh_name(:pid, ctx)
          {[:fn, params, [:let, [[pid, [:Pid, protocol], [:app, p, arg]]],
            [:let, [[:_, :Unit, [:perform, :link, protocol, pid]]], [:tuple, [:atom, :ok], pid]]]], ctx}

        {:supervisor_start, _, strategy, children, loc} ->
          {params, ctx} = bind_params([:_], ptypes, ctx)
          {body, ctx} = check({:supervise, strategy, children, loc}, rtype, ctx)
          {[:fn, params, body], ctx}

        {:instance_factory, _, instance, _} ->
          {params, ctx} = Enum.map_reduce(ptypes, ctx, fn t, ctx ->
            {name, ctx} = fresh_name(:dictionary, ctx)
            {[name, t], ctx}
          end)
          dictionaries = Map.new(Enum.zip(instance.constraints, params), fn {key, [x, _]} -> {key, x} end)
          ctx = %{ctx | dictionaries: dictionaries}
          {methods, ctx} = Enum.map_reduce(instance.methods, ctx, fn {_method, signature, names, body}, ctx ->
            [:->, types, eff, result] = signature
            outer_env = ctx.env
            outer_eff = ctx.eff
            {ps, ctx} = bind_params(names, types, ctx)
            {body, ctx} = in_scope(%{ctx | eff: eff}, fn ctx -> check(body, result, ctx) end, outer_env)
            {[:fn, ps, body], %{ctx | eff: outer_eff}}
          end)
          {[:fn, params, [:tuple | methods]], ctx}

        {:class_default, _, info, _} ->
          [dict_type | types] = ptypes
          {dict, ctx} = fresh_name(:dictionary, ctx)
          {params, ctx} = bind_params(info.names, types, ctx)
          ctx = %{ctx | dictionaries: %{{info.class, [:tvar, info.param]} => dict}}
          {body, ctx} = check(info.body, rtype, ctx)
          {[:fn, [[dict, dict_type] | params], body], ctx}

        {:process_step, _, handlers, _} ->
          [state, protocol] = ptypes
          {params, ctx} = bind_params([:state, :message], ptypes, ctx)
          [[_, _], [msg, _]] = params
          {clauses, ctx} = Enum.map_reduce(handlers, ctx, fn {tag, body}, ctx ->
            {body, ctx} = check(body, state, ctx)
            {[[:inj, protocol, message_name(tag)], body], ctx}
          end)
          body = if clauses == [], do: @no_match, else: [:match, msg | clauses]
          {[:fn, params, body], ctx}
      end

    {fun, ctx} = finish(fun, ctx)
    {:ok, [:def, name, sig, fun], ctx.prelude}
  catch
    {:elab, %Error{} = error} -> {:error, error}
    {:elab_errors, errors} -> {:error, errors}
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
    term = resolve_dictionaries(term, ctx)

    for {row, allowed, loc} <- ctx.effect_checks do
      row = Types.zonk(row, ctx.s)
      allowed = Types.zonk(allowed, ctx.s)
      if Types.unsolved?(row) or Types.unsolved?(allowed), do: undetermined([row, allowed], ctx)
      check_effect_row(row, allowed, %{ctx | loc: loc})
    end

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

    for {type, operation, loc} <- ctx.ground do
      t = Types.zonk(type, ctx.s)
      unless ground?(t, ctx.decls, MapSet.new()), do: fail(%{ctx | loc: loc}, "#{operation}, but this is #{Types.format(t)}", note: "an abstract type needs a class dictionary; a function is not ground data")
    end

    for {type, what, loc} <- ctx.data do
      require_data(Types.zonk(type, ctx.s), what, %{ctx | loc: loc})
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
  defp ground?(t, _decls, _seen) when t in [:Int, :Float, :String, :Atom, :Unit, :Ref, :Dyn], do: true
  defp ground?([:Bool], _decls, _seen), do: true
  defp ground?([:Reason], _decls, _seen), do: true
  defp ground?([:List, a], decls, seen), do: ground?(a, decls, seen)
  defp ground?([:Tuple | ts], decls, seen), do: Enum.all?(ts, &ground?(&1, decls, seen))
  defp ground?([:Map, k, v], decls, seen), do: ground?(k, decls, seen) and ground?(v, decls, seen)
  defp ground?([:Record, [:row | fields]], decls, seen) do
    case Enum.split(fields, -1) do
      {fields, [:closed]} -> Enum.all?(fields, fn [_, t] -> ground?(t, decls, seen) end)
      _ -> false
    end
  end
  defp ground?([:Pid, p], decls, seen), do: ground?(p, decls, seen)

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

  # Editor queries are typed identity expressions, never holes or a bypass.
  # The entire modified program still passes elaboration, Lint and refinement checking.
  defp check_({:call, :__vaisto_type_probe__, [id, expression], _}, type, %{type_probe: true} = ctx) when is_integer(id) do
    {core, ctx} = check(expression, type, ctx)
    type_probe(core, type, id, expression, ctx)
  end

  defp check_({:call, :crash, [reason], _}, _type, ctx) do
    {reason, ctx} = case reason do
      {:atom, tag} when tag in [:badarith, :badarg, :no_match, :bad_decode] -> {[:inj, [:Reason], tag], ctx}
      _ -> check(reason, [:Reason], ctx)
    end
    {[:perform, :crash, reason], ctx}
  end

  defp check_({:imported_default, class, target, f, names, _loc}, type, ctx) do
    signature = Map.fetch!(ctx.sigs, f)
    head = case signature do
      [:forall, [[_, :Type]], _] -> [:inst, f, target]
      _ -> f
    end
    args = Enum.map(names, fn name -> elem(Map.fetch!(ctx.env, name), 0) end)
    dict = {:dictionary, class, target, ctx.loc}
    [:->, _, _, result] = Types.subst_tvars(open(signature), %{hd(Types.tvars(signature)) => target})
    ctx = unify!(result, type, ctx)
    {[:app, head, dict | args], ctx}
  end

  defp check_({:map, entries, _} = literal, type, ctx) do
    case Types.zonk(type, ctx.s) do
      [:Map, key, value] ->
        {entries, ctx} = Enum.map_reduce(entries, ctx, fn {k, v}, ctx ->
          {k, ctx} = check(k, key, ctx)
          {v, ctx} = check(v, value, ctx)
          {[k, v], ctx}
        end)
        {[:map, key, value | entries], ctx}
      [:Record, [:row | fields]] = type ->
        if List.last(fields) == :closed do
        {fields, [:closed]} = Enum.split(fields, -1)
        names = Enum.map(entries, fn
          {{:atom, name}, _} -> name
          _ -> fail(ctx, "an anonymous record needs literal field names")
        end)
        if Enum.sort(names) != Enum.sort(Enum.map(fields, &hd/1)), do: fail(ctx, "an anonymous record must give exactly its declared fields")
        {entries, ctx} = Enum.map_reduce(entries, ctx, fn {{:atom, name}, value}, ctx ->
          [^name, t] = Enum.find(fields, &match?([^name, _], &1))
          {value, ctx} = check(value, t, ctx)
          {[name, value], ctx}
        end)
        {[:record, type | entries], ctx}
        else
          subsume(literal, type, ctx)
        end
      _ -> subsume(literal, type, ctx)
    end
  end

  defp check_(n, type, ctx) when is_integer(n) do
    type = Types.zonk(type, ctx.s)
    if match?([:tvar, _], type) and Map.has_key?(ctx.dictionaries, {:Num, type}) do
      {converted, actual, ctx} = class_call(:"num-from-int", [n], ctx)
      {converted, unify!(actual, type, ctx)}
    else
      subsume(n, type, ctx)
    end
  end

  defp check_({:fn, params, body, _loc} = e, type, ctx) do
    params = lambda_params(params, ctx)

    case Types.zonk(type, ctx.s) do
      [:->, ptypes, eff, rtype] when length(ptypes) == length(params) ->
        ctx = Enum.zip(params, ptypes) |> Enum.reduce(ctx, fn {{_p, ann}, t}, ctx -> if ann, do: unify!(t, ann, ctx), else: ctx end)
        outer_env = ctx.env
        {core_params, ctx} = bind_params(Enum.map(params, &elem(&1, 0)), ptypes, ctx)
        ctx = if Types.unsolved?(Types.zonk(eff, ctx.s)) do
          prefix = case Types.zonk(eff, ctx.s) do
            [:eff | items] -> Enum.drop(items, -1)
            _ -> []
          end
          actual = [:eff | Enum.sort(Enum.uniq(prefix ++ effects(body, ctx))) ++ [:closed]]
          unify!(eff, actual, ctx)
        else
          ctx
        end
        eff = Types.zonk(eff, ctx.s)
        {body, inner} = in_scope(%{ctx | eff: eff}, fn ctx -> check(body, rtype, ctx) end, outer_env)
        ctx = %{inner | eff: ctx.eff}
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

  defp check_({:receive, clauses, _loc}, type, ctx) do
    ctx = allow_effects([:receive], ctx)
    {clauses, ctx} = Enum.map_reduce(clauses, ctx, fn {p, body}, ctx ->
      {target, ctx} = receive_type(p, ctx)
      {p, binds, ctx} = pattern(p, target, ctx)
      {body, ctx} = in_scope(%{ctx | env: Map.merge(ctx.env, binds)}, fn ctx -> check(body, type, ctx) end, ctx.env)
      ctx = %{ctx | data: [{target, "a receive target", ctx.loc} | ctx.data]}
      {[target, p, body], ctx}
    end)
    {[:receive | clauses], ctx}
  end

  defp check_({:list, elems, _loc}, type, ctx), do: check_({:bracket, elems}, type, ctx)

  defp check_({:bracket, elems}, type, ctx) when is_list(elems) do
    {elem_type, ctx} = list_elem(type, "the element type of this list", ctx)
    {cores, ctx} = Enum.map_reduce(elems, ctx, &check(&1, elem_type, &2))
    list_type = [:List, elem_type]
    {List.foldr(cores, [:inj, list_type, :Nil], fn h, t -> [:inj, list_type, :Cons, h, t] end), ctx}
  end

  defp check_({:try, body, handlers, after_body, _loc}, type, ctx) do
    {body, ctx} = check(body, type, ctx)
    {reason, ctx} = fresh_name(:reason, ctx)
    {handlers, ctx} = Enum.map_reduce(handlers, ctx, fn {class, e, handler}, ctx ->
      unless class in [:error, :throw, :exit] and is_atom(e) and not is_boolean(e) and e != nil, do: fail(ctx, "a catch clause binds the reason to a name")
      env = if e == :_, do: ctx.env, else: Map.put(ctx.env, e, {reason, [:Reason]})
      {handler, ctx} = in_scope(%{ctx | env: env}, fn ctx -> check(handler, type, ctx) end, ctx.env)
      {{class, handler}, ctx}
    end)
    # Dispatch can select a source handler from both the normalized and
    # foreign-exception paths. Bind its Core once: duplicating the tree also
    # duplicates any nested decoder/lambda binders (§10.12).
    {handler_defs, ctx} = Enum.map_reduce(handlers, ctx, fn {class, handler}, ctx ->
      {name, ctx} = fresh_name(:catch_handler, ctx)
      {{class, name, [name, [:->, [], ctx.eff, type], [:fn, [], handler]]}, ctx}
    end)
    handlers = Enum.map(handler_defs, fn {class, name, _} -> {class, [:app, name]} end)
    {class, ctx} = fresh_name(:class, ctx)
    reraised = [:perform, :crash, reason]
    raised = Enum.reduce(Enum.reverse(handlers), reraised, fn {tag, handler}, rest ->
      [:if, [:prim, :eq, class, [:atom, tag]], handler, rest]
    end)
    ordinary = case List.keyfind(handlers, :error, 0) do
      {:error, handler} -> handler
      nil -> reraised
    end
    dispatch = [:match, reason,
      [[:inj, [:Reason], :raised, class, :_], raised],
      [:_, ordinary]]
    dispatch = Enum.reduce(Enum.reverse(handler_defs), dispatch, fn {_, _, binding}, rest -> [:let, [binding], rest] end)
    handled = [:"handle-crash", body, [[reason, [:Reason]], dispatch]]
    if after_body == nil do
      {handled, ctx}
    else
      {after_core, after_type, ctx} = synth(after_body, ctx)
      {outcome, ctx} = fresh_name(:outcome, ctx)
      {result, ctx} = fresh_name(:result, ctx)
      {failure, ctx} = fresh_name(:failure, ctx)
      {caught, ctx} = fresh_name(:caught, ctx)
      outcome_type = [:"Core.Result", [:Reason], type]
      outcome_core = [:"handle-crash", [:inj, outcome_type, :"Core.Ok", handled],
        [[caught, [:Reason]], [:inj, outcome_type, :"Core.Err", caught]]]
      choose = [:match, outcome,
        [[:inj, outcome_type, :"Core.Ok", result], result],
        [[:inj, outcome_type, :"Core.Err", failure], [:perform, :crash, failure]]]
      {[:let, [[outcome, outcome_type, outcome_core]], [:let, [[:_, after_type, after_core]], choose]], ctx}
    end
  end

  # An operator or a constructor given where a function is expected is
  # expanded to one: (fold + 0 xs), (map Some xs).
  defp check_(f, type, ctx) when is_atom(f) and not is_map_key(ctx.env, f) do
    with [:->, ptypes, _eff, _r] <- Types.zonk(type, ctx.s),
         true <- f in @operators or Map.has_key?(ctx.methods, f) or (Map.has_key?(ctx.ctors, f) and fields(ctx, f) != []) do
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
  defp synth_({:qualified, m, f}, ctx) when is_map_key(ctx.imports, m) do
    i = Map.fetch!(ctx.imports, m)
    case Enum.find(i.exports, fn [name, _, _] -> name == f end) do
      [^f, type, :plain] ->
        case Map.fetch(i.constraints, f) do
          {:ok, constraints} -> constrained_value([:remote, i.module, f], type, constraints, ctx)
          :error -> instantiate([:remote, i.module, f], type, "the type arguments of `#{m}/#{f}`", ctx)
        end
      [^f, _, :refined] -> fail(ctx, "`#{m}/#{f}` has refinements, and imported refinements cannot be erased")
      nil -> fail(ctx, "`#{m}` does not export `#{f}`")
    end
  end
  defp synth_(:_, ctx), do: fail(ctx, "`_` matches anything and binds nothing, so it cannot be used as a value")

  defp synth_(x, ctx) when is_atom(x) do
    cond do
      Map.has_key?(ctx.env, x) ->
        {name, type} = Map.fetch!(ctx.env, x)
        {name, type, ctx}

      MapSet.member?(ctx.values, x) ->
        {head, ftype, ctx} = instantiate(x, Map.fetch!(ctx.sigs, x), "the type arguments of value `#{x}`", ctx)
        {[], rtype, ctx} = arrow(ftype, 0, "value `#{x}`", ctx)
        {[:app, head], rtype, ctx}

      Map.has_key?(ctx.function_constraints, x) ->
        constrained_value(x, Map.fetch!(ctx.sigs, x), ctx.function_constraints[x], ctx)

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
        hint = Vaisto.Errors.undefined_variable(x, Map.keys(ctx.env) ++ Map.keys(ctx.sigs)).hint
        fail(ctx, "undefined variable `#{x}`", hint: hint)
    end
  end

  defp synth_({:fn, params, body, _loc}, ctx) do
    params = lambda_params(params, ctx)

    {ptypes, ctx} =
      Enum.map_reduce(params, ctx, fn
        {_p, ann}, ctx when ann != nil -> {ann, ctx}
        {p, nil}, ctx -> meta("the type of the parameter `#{p}` of this lambda", ctx)
      end)

    outer_env = ctx.env
    {core_params, ctx} = bind_params(Enum.map(params, &elem(&1, 0)), ptypes, ctx)
    eff = [:eff | Enum.sort(effects(body, ctx)) ++ [:closed]]
    {body, rtype, inner} = in_scope(%{ctx | eff: eff}, fn ctx -> synth(body, ctx) end, outer_env)
    {[:fn, core_params, body], [:->, ptypes, eff, rtype], %{inner | eff: ctx.eff}}
  end

  defp synth_({:if, c, a, b, _loc}, ctx) do
    {c, ctx} = check(c, [:Bool], ctx)
    {a, type, ctx} = synth(a, ctx)
    {b, ctx} = check(b, type, ctx)
    {[:if, c, a, b], type, ctx}
  end

  defp synth_({:try, _, _, _, _} = e, ctx), do: synth_by_check(e, "the type of this try", ctx)

  defp synth_({tag, _, _, _} = e, ctx) when tag in [:let, :match], do: synth_by_check(e, "the type of this #{tag}", ctx)
  defp synth_({:receive, _, _} = e, ctx), do: synth_by_check(e, "the result type of this receive", ctx)
  defp synth_({:supervise, strategy, children, _}, ctx) do
    ctx = allow_effects([:supervise, :spawn, :link], ctx)
    strategy = case strategy do
      {:atom, s} -> s
      s -> s
    end
    unless strategy in [:one_for_one, :one_for_all, :rest_for_one, :all_for_one], do: fail(ctx, "unknown supervision strategy #{inspect(strategy)}")
    # Keep the historical spelling, but pass OTP its actual strategy name.
    strategy = if strategy == :all_for_one, do: :one_for_all, else: strategy
    {children, ctx} = Enum.map_reduce(children, ctx, fn
      {:call, name, [initial], _}, ctx when is_map_key(ctx.processes, name) ->
        info = Map.fetch!(ctx.processes, name)
        {initial, ctx} = check(initial, info.state, ctx)
        tags = Enum.map(info.handlers, fn {m, _} -> message_name(m) end)
        {[name, info.protocol, info.state, tags, info.step, initial], ctx}
      _, ctx -> fail(ctx, "a supervised child must be a local process with one initial-state argument")
    end)
    ids = Enum.map(children, &hd/1)
    unless length(Enum.uniq(ids)) == length(ids), do: fail(ctx, "a supervision tree names the same child twice")
    {[:perform, :supervise, strategy | children], [:Tuple, :Atom, [:Pid, :Dyn]], ctx}
  end
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

  defp synth_({:map, entries, _loc}, ctx) do
    if Enum.all?(entries, fn {key, _} -> match?({:atom, _}, key) end) and entries != [] do
      names = Enum.map(entries, fn {{:atom, key}, _} -> key end)
      if Enum.uniq(names) != names, do: fail(ctx, "an anonymous record gives a field twice")
      {fields, ctx} = Enum.map_reduce(entries, ctx, fn {{:atom, key}, value}, ctx ->
        {core, type, ctx} = synth(value, ctx)
        {{key, type, core}, ctx}
      end)
      type = [:Record, [:row | Enum.sort(Enum.map(fields, fn {k, t, _} -> [k, t] end)) ++ [:closed]]]
      {[:record, type | Enum.map(fields, fn {k, _, v} -> [k, v] end)], type, ctx}
    else
      {key, ctx} = meta("the key type of this map", ctx)
      {value, ctx} = meta("the value type of this map", ctx)
      type = [:Map, key, value]
      {core, ctx} = check_({:map, entries, nil}, type, ctx)
      {core, type, ctx}
    end
  end

  defp synth_({:field_access, e, field, _loc}, ctx) do
    {core, type, ctx} = synth(e, ctx)
    zonked = Types.zonk(type, ctx.s)

    case zonked do
      [:Record, [:row | fields]] ->
        case Enum.find(fields, &match?([^field, _], &1)) do
          [^field, ft] -> {[:select, core, field], ft, ctx}
          nil -> fail(ctx, "the record has no field `#{field}`")
        end
      _ ->
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
  end

  defp synth_({:call, :__vaisto_type_probe__, [id, expression], _}, %{type_probe: true} = ctx) when is_integer(id) do
    {core, type, ctx} = synth(expression, ctx)
    {core, ctx} = type_probe(core, type, id, expression, ctx)
    {core, type, ctx}
  end
  defp synth_({:call, :crash, [_], _} = e, ctx), do: synth_by_check(e, "the result type at this crash", ctx)
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
  defp annotation?({:call, {:module_path, ["Core", "Result"]}, _, _}), do: true
  defp annotation?({:call, t, _, _}) when is_atom(t), do: capitalized?(t)
  defp annotation?(_), do: false

  # A type written in a body: its type variables are the signature's.
  defp surface_type(ann, ctx) do
    arities = Map.new(ctx.decls, fn {t, d} -> {t, length(d.params)} end) |> Map.merge(ctx.type_aliases)

    case Types.from_surface(ann, arities) do
      {:ok, type} ->
        case Enum.reject(Enum.map(Types.variables(type), &elem(&1, 0)), &MapSet.member?(ctx.tvars, &1)) do
          [] -> type
          [a | _] -> fail(ctx, "the type variable `#{a}` is not in scope here", note: "a type in a body may use only its definition's type variables")
        end

      {:error, message} ->
        fail(ctx, message)
    end
  end

  # --- Calls ---------------------------------------------------------------------------

  defp call(:spawn, [p, initial], ctx) when is_atom(p) do
    case Map.fetch(ctx.processes, p) do
      {:ok, _} -> apply_value(p, [initial], ctx)
      :error ->
        hint = Vaisto.Errors.unknown_process(p, Map.keys(ctx.processes)).hint
        fail(ctx, "unknown process `#{p}`", hint: hint)
    end
  end
  defp call(:spawn, args, ctx) do
    if Map.has_key?(ctx.sigs, :spawn) or Map.has_key?(ctx.env, :spawn), do: apply_value(:spawn, args, ctx), else: fail(ctx, "write (spawn process initial-state)")
  end

  defp call(op, [pid, message], ctx) when op in [:"!", :"!!"] do
    ctx = allow_effects([:send], ctx)
    {pid, pid_type, ctx} = synth(pid, ctx)
    protocol = case Types.zonk(pid_type, ctx.s) do
      [:Pid, p] -> p
      other -> fail(ctx, "sending requires a PID, not #{Types.format(other)}")
    end
    if op == :"!!" do
      {message, type, ctx} = synth(message, ctx)
      ctx = %{ctx | data: [{type, "an unchecked message", ctx.loc} | ctx.data]}
      {untyped, ctx} = decode_or_crash([:Pid, :Dyn], [:up, [:Pid, protocol], pid], ctx)
      {[:perform, :send, :Dyn, untyped, [:up, type, message]], :Unit, ctx}
    else
      info = Enum.find_value(ctx.processes, fn {_, info} -> if info.protocol == protocol, do: info end)
      {message, ctx} = case {info, message} do
        {%{handlers: handlers}, {:atom, tag}} ->
          unless Enum.any?(handlers, fn {m, _} -> message_name(m) == tag end),
            do: fail(ctx, "process does not accept `:#{tag}`", note: "accepted messages: #{Enum.map_join(handlers, ", ", fn {m, _} -> ":#{message_name(m)}" end)}")
          {[:inj, protocol, tag], ctx}
        {%{handlers: []}, _} -> fail(ctx, "this process accepts no messages")
        _ -> check(message, protocol, ctx)
      end
      {[:perform, :send, protocol, pid, message], :Unit, ctx}
    end
  end
  defp call(op, args, ctx) when op in [:"!", :"!!"] do
    if Map.has_key?(ctx.sigs, op) or Map.has_key?(ctx.env, op), do: apply_value(op, args, ctx), else: fail(ctx, "write (#{op} pid message)")
  end

  defp call(:the, [ann, e], ctx) do
    type = surface_type(ann, ctx)
    {core, ctx} = check(e, type, ctx)
    {core, type, ctx}
  end

  defp call(:the, args, ctx) do
    if Map.has_key?(ctx.sigs, :the) or Map.has_key?(ctx.env, :the), do: apply_value(:the, args, ctx), else: fail(ctx, "a type ascription is written (the τ e)")
  end

  defp call(form, [ann, e], ctx) when form in [:up, :decode] do
    type = surface_type(ann, ctx)
    {core, ctx} = check(e, if(form == :up, do: type, else: :Dyn), ctx)
    ctx = %{ctx | data: [{type, "a `#{form}` target", ctx.loc} | ctx.data]}
    result = if form == :up, do: :Dyn, else: [:"Core.Result", [:"Core.DecodeError"], type]
    {[form, type, core], result, ctx}
  end

  defp call(form, args, ctx) when form in [:up, :decode] do
    if Map.has_key?(ctx.sigs, form) or Map.has_key?(ctx.env, form), do: apply_value(form, args, ctx), else: fail(ctx, "write (#{form} τ e)")
  end

  defp call({:module_path, ["Core", c]}, args, ctx) when c in ["Ok", "Err", "BadDecode"] do
    constructor(decode_constructor(c), args, ctx)
  end
  defp call({:module_path, path}, args, ctx) do
    {module, [f]} = Enum.split(path, -1)
    call({:qualified, module |> Enum.join(".") |> String.to_atom(), String.to_atom(f)}, args, ctx)
  end

  defp call({:qualified, m, f} = target, args, ctx) when is_map_key(ctx.imports, m) do
    i = Map.fetch!(ctx.imports, m)
    cond do
      Enum.any?(i.exports, fn [name, _, _] -> name == f end) -> apply_value(target, args, ctx)
      Enum.any?(i.types, fn
        [:type, ^f, _, [:record | _]] -> true
        _ -> false
      end) -> record(f, args, ctx)
      Enum.any?(i.types, fn
        [:type, _, _, [:sum | cs]] -> Enum.any?(cs, fn [c | _] -> c == f end)
        _ -> false
      end) -> constructor(f, args, ctx)
      true -> fail(ctx, "`#{m}` does not export `#{f}`")
    end
  end

  defp call({:qualified, m, f}, args, ctx) do
    case Map.fetch(ctx.externs, {m, f, length(args)}) do
      {:ok, {params, result}} ->
        if length(args) != length(params), do: fail(ctx, "wrong number of arguments", note: "the extern `#{m}:#{f}` takes #{length(params)}, but #{length(args)} were given")
        ctx = allow_external(ctx)
        {cores, ctx} = Enum.zip(args, params) |> Enum.map_reduce(ctx, fn {a, t}, ctx -> {c, ctx} = check(a, t, ctx); {[:up, t, c], ctx} end)
        list = List.foldr(cores, [:inj, [:List, :Dyn], :Nil], fn c, rest -> [:inj, [:List, :Dyn], :Cons, c, rest] end)
        request = [:perform, :external, [:atom, foreign_module(m)], [:atom, f], list]
        {term, ctx} = decode_or_crash(result, request, ctx)
        {term, result, ctx}

      :error ->
        arities = for {{module, function, arity}, _} <- ctx.externs, module == m and function == f, do: arity
        if arities != [], do: fail(ctx, "wrong number of arguments", note: "the extern `#{m}:#{f}` declares arities #{inspect(Enum.sort(arities))}, but #{length(args)} arguments were given")
        fail(ctx, "undeclared foreign call `#{m}:#{f}`", hint: "declare its argument and result types with (extern #{m}:#{f} [...] τ)")
    end
  end

  defp call(f, args, ctx) when is_atom(f) do
    cond do
      Map.has_key?(ctx.env, f) or Map.has_key?(ctx.sigs, f) or Map.has_key?(@higher_order, f) -> apply_value(f, args, ctx)
      Map.has_key?(ctx.methods, f) -> class_call(f, args, ctx)
      Map.has_key?(ctx.ctors, f) -> constructor(f, args, ctx)
      record?(ctx, f) -> record(f, args, ctx)
      true -> builtin(f, args, ctx)
    end
  end

  # A call whose head is an expression: ((head fs) x), ((fn [x] x) 1).
  defp call(f, args, ctx) when is_tuple(f), do: apply_value(f, args, ctx)
  defp call(f, _args, _ctx), do: outside_term("the call of #{inspect(f, limit: 3)}")

  defp foreign_module(m) do
    name = Atom.to_string(m)
    cond do
      String.starts_with?(name, "Elixir.") -> m
      String.contains?(name, ".") or String.match?(name, ~r/^[A-Z]/) -> Module.concat([name])
      true -> m
    end
  end

  defp decode_constructor("Ok"), do: :"Core.Ok"
  defp decode_constructor("Err"), do: :"Core.Err"
  defp decode_constructor("BadDecode"), do: :"Core.BadDecode"

  defp constrained_value(value, signature, constraints, ctx) do
    {head, [:->, _, _, user], ctx} = instantiate(value, signature, "the type arguments of a constrained function", ctx)
    subst = case {signature, head} do
      {[:forall, binders, _], [:inst, _ | args]} -> Map.new(Enum.zip_with(binders, args, fn [a, _], t -> {a, t} end))
      _ -> %{}
    end
    {dicts, ctx} = Enum.map_reduce(constraints, ctx, fn entry, ctx ->
      {c, t, schema} = case entry do
        {c, t} -> {c, t, nil}
        {c, t, d} -> {c, t, d}
      end
      unless Map.has_key?(ctx.classes, c), do: fail(ctx, "unknown imported constraint class `#{c}`")
      t = Types.subst_tvars(t, subst)
      ctx = if schema == nil, do: ctx, else: unify!(Classes.dictionary_type(ctx.classes, c, t), Types.subst_tvars(schema, subst), ctx)
      {{:dictionary, c, t, ctx.loc}, ctx}
    end)
    {[:app, head | dicts], user, ctx}
  end

  defp class_call(method, args, ctx) do
    class = Map.get(ctx.methods, method) || fail(ctx, "unknown class method `#{method}`")
    info = Map.fetch!(ctx.classes, class)
    index = Enum.find_index(info.methods, fn {m, _, _, _} -> m == method end)
    {_, sig, _, _} = Enum.at(info.methods, index)
    {target, ctx} = meta("the instance of `#{class}`", ctx)
    [:->, params, eff, result] = Types.subst_tvars(sig, %{info.param => target})
    if length(params) != length(args), do: fail(ctx, "wrong number of arguments to class method `#{method}`")
    ctx = allow_effects(Enum.drop(tl(eff), -1), ctx)
    {args, ctx} = Enum.zip(args, params) |> Enum.map_reduce(ctx, fn {arg, type}, ctx -> check(arg, type, ctx) end)
    dict = {:dictionary, class, target, ctx.loc}
    {[:app, [:select, dict, index] | args], result, ctx}
  end

  defp resolve_dictionaries({:dictionary, class, type, loc}, ctx) do
    type = Types.zonk(type, ctx.s)
    if Types.unsolved?(type), do: undetermined(type, %{ctx | loc: loc})
    dictionary(class, type, %{ctx | loc: loc})
  end
  defp resolve_dictionaries(xs, ctx) when is_list(xs), do: Enum.map(xs, &resolve_dictionaries(&1, ctx))
  defp resolve_dictionaries(t, _ctx), do: t

  defp dictionary(class, type, ctx, seen \\ MapSet.new()) do
    case Map.fetch(ctx.dictionaries, {class, type}) do
      {:ok, variable} -> variable
      :error ->
        size = fn t -> t |> List.wrap() |> List.flatten() |> length() end
        if Enum.any?(seen, fn {c, previous} -> c == class and Classes.head(previous) == Classes.head(type) and size.(type) >= size.(previous) end),
          do: fail(ctx, "recursive instance resolution for #{class} #{Types.format(type)}")
        seen = MapSet.put(seen, {class, type})
        i = Map.get(ctx.instances, {class, Classes.head(type)}) ||
          fail(ctx, "no instance for #{class} #{Types.format(type)}",
            note: if(MapSet.size(seen) > 1, do: "required by constrained instance " <> (seen |> Enum.reject(&(&1 == {class, type})) |> Enum.sort() |> Enum.map_join(", ", fn {c, t} -> "#{c} #{Types.format(t)}" end))))
        subst = case {i.type, type} do
          {[name | vars], [name | args]} -> Map.new(Enum.zip(vars, args), fn {[:tvar, a], t} -> {a, t} end)
          {^type, ^type} -> %{}
          _ -> fail(ctx, "the instance of #{class} does not match #{Types.format(type)}")
        end
        signature = Map.fetch!(ctx.sigs, i.factory)
        head = case signature do
          [:forall, binders, _] -> [:inst, i.factory | Enum.map(binders, fn [a, _] -> Map.fetch!(subst, a) end)]
          _ -> i.factory
        end
        args = Enum.map(i.constraints, fn {c, t} -> dictionary(c, Types.subst_tvars(t, subst), ctx, seen) end)
        [:app, head | args]
    end
  end

  defp decode_or_crash(type, core, ctx) do
    {value, ctx} = fresh_name(:decoded, ctx)
    result = [:"Core.Result", [:"Core.DecodeError"], type]
    {[:match, [:decode, type, core],
      [[:inj, result, :"Core.Ok", value], value],
      [[:inj, result, :"Core.Err", :_], [:perform, :crash, [:inj, [:Reason], :bad_decode]]]], ctx}
  end

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
  defp function_arg?(f, ctx) when is_atom(f), do: not Map.has_key?(ctx.env, f) and (f in @operators or Map.has_key?(ctx.methods, f) or Map.has_key?(ctx.ctors, f))
  defp function_arg?(_e, _ctx), do: false

  defp callee(f) when is_atom(f), do: "`#{f}`"
  defp callee(_f), do: "this function"

  defp arrow(ftype, n, name, ctx) do
    case Types.zonk(ftype, ctx.s) do
      [:->, ptypes, eff, rtype] when length(ptypes) == n ->
        ctx = allow_effect_row(eff, ctx)
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
    {metas, ctx} = Enum.map_reduce(binders, ctx, fn [_, kind], ctx ->
      {m, ctx} = meta(what, ctx)
      argument = case kind do
        :Row -> [:row, m]
        :Eff -> [:eff, m]
        :Type -> m
      end
      {argument, ctx}
    end)
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
    if Map.has_key?(ctx.dictionaries, {:Num, Types.zonk(type, ctx.s)}) do
      {core, ctx} = dictionary_operation(:Num, :"num-neg", type, [core], type, ctx)
      {core, type, ctx}
    else
      {[:prim, numeric_op(:neg, :fneg, type, :-, ctx), core], type, ctx}
    end
  end

  defp builtin(:"int-to-float", [a], ctx) do
    {a, ctx} = check(a, :Int, ctx)
    {[:prim, :"int-to-float", a], :Float, ctx}
  end
  defp builtin(op, [a], ctx) when op in [:"float-round", :"float-trunc"] do
    {a, ctx} = check(a, :Float, ctx)
    {[:prim, op, a], :Int, ctx}
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
    if Map.has_key?(ctx.dictionaries, {:Eq, Types.zonk(type, ctx.s)}) do
      {core, ctx} = dictionary_operation(:Eq, if(op == :==, do: :eq, else: :neq), type, [a, b], [:Bool], ctx)
      {core, [:Bool], ctx}
    else
      {[:prim, if(op == :==, do: :eq, else: :ne), a, b], [:Bool], %{ctx | ground: [{type, "`==` compares ground data", ctx.loc} | ctx.ground]}}
    end
  end

  defp builtin(:not, [a], ctx) do
    {a, ctx} = check(a, [:Bool], ctx)
    {[:prim, :not, a], [:Bool], ctx}
  end

  defp builtin(op, [a, b], ctx) when op in [:and, :andalso] do
    {a, ctx} = check(a, [:Bool], ctx)
    {b, ctx} = check(b, [:Bool], ctx)
    {[:if, a, b, [:inj, [:Bool], false]], [:Bool], ctx}
  end

  defp builtin(op, [a, b], ctx) when op in [:or, :orelse] do
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
  defp builtin(:"map-new", [], ctx) do
    {key, ctx} = meta("the key type of this map", ctx)
    {value, ctx} = meta("the value type of this map", ctx)
    {[:map, key, value], [:Map, key, value], ctx}
  end
  defp builtin(:"map-from-list", [pairs], ctx) do
    {key, ctx} = meta("the key type of the entries", ctx)
    {value, ctx} = meta("the value type of the entries", ctx)
    {pairs, ctx} = check(pairs, [:List, [:Tuple, key, value]], ctx)
    {[:prim, :"map-from-list", pairs], [:Map, key, value], ctx}
  end
  defp builtin(op, [map | args], ctx) when op in [:"map-get", :"map-put", :"map-delete", :"map-has?", :"map-keys", :"map-values", :"map-size", :"map-merge", :"map-to-list"] do
    {map, type, ctx} = synth(map, ctx)
    {key, ctx} = meta("the key type of this map", ctx)
    {value, ctx} = meta("the value type of this map", ctx)
    ctx = unify!(type, [:Map, key, value], ctx)
    {params, result} = case op do
      :"map-get" -> {[key], value}
      :"map-put" -> {[key, value], type}
      :"map-delete" -> {[key], type}
      :"map-has?" -> {[key], [:Bool]}
      :"map-keys" -> {[], [:List, key]}
      :"map-values" -> {[], [:List, value]}
      :"map-size" -> {[], :Int}
      :"map-merge" -> {[type], type}
      :"map-to-list" -> {[], [:List, [:Tuple, key, value]]}
    end
    if length(args) != length(params), do: fail(ctx, "wrong number of arguments to `#{op}`")
    {args, ctx} = Enum.zip(args, params) |> Enum.map_reduce(ctx, fn {arg, t}, ctx -> check(arg, t, ctx) end)
    {[:prim, op, map | args], result, ctx}
  end

  defp builtin(:str, args, ctx) do
    {args, ctx} = Enum.map_reduce(args, ctx, fn e, ctx ->
      {e, type, ctx} = synth(e, ctx)
      ctx = %{ctx | ground: [{type, "`str` converts ground data", ctx.loc} | ctx.ground]}
      {[:up, type, e], ctx}
    end)
    {[:prim, :str | args], :String, ctx}
  end
  defp builtin(f, _args, ctx) do
    candidates = Map.keys(ctx.sigs) ++ Map.keys(ctx.methods) ++ @operators ++ Map.keys(@higher_order) ++ [:head, :tail, :length, :empty?, :list, :tuple, :str]
    hint = Vaisto.Errors.unknown_function(f, candidates).hint
    fail(ctx, "undefined function `#{f}`", hint: hint)
  end

  defp type_probe(core, type, id, expression, ctx) do
    kind = cond do
      is_atom(expression) and Map.has_key?(ctx.env, expression) -> :variable
      is_atom(expression) and MapSet.member?(ctx.values, expression) -> :variable
      is_atom(expression) and (Map.has_key?(ctx.sigs, expression) or Map.has_key?(ctx.methods, expression)) -> :function
      match?({:qualified, _, _}, expression) -> :function
      true -> :expression
    end
    metadata = [:meta, [:"type-at", id, type, kind]]
    if ctx.in_guard do
      {[:select, [:tuple, core], 0, metadata], ctx}
    else
      {name, ctx} = fresh_name(:probe_result, ctx)
      {[:let, [[name, type, core]], name, metadata], ctx}
    end
  end

  # Both operands have one type, Int or Float: no coercion. The primitive is
  # that type's, chosen now if the type is known and at the end otherwise.
  defp numeric(op, int_op, float_op, a, b, result, ctx) do
    {ac, at, ctx} = synth(a, ctx)
    {bc, bt, ctx} = synth(b, ctx)
    {ac, bc, type, ctx} = cond do
      is_integer(a) and Map.has_key?(ctx.dictionaries, {:Num, Types.zonk(bt, ctx.s)}) ->
        {ac, ctx} = check(a, bt, ctx)
        {ac, bc, bt, ctx}
      is_integer(b) and Map.has_key?(ctx.dictionaries, {:Num, Types.zonk(at, ctx.s)}) ->
        {bc, ctx} = check(b, at, ctx)
        {ac, bc, at, ctx}
      true -> {ac, bc, at, unify!(at, bt, ctx)}
    end
    class = if Map.has_key?(@compare, op), do: :Ord, else: :Num
    if Map.has_key?(ctx.dictionaries, {class, Types.zonk(type, ctx.s)}) do
      method = Map.fetch!(%{:+ => :"num-add", :- => :"num-sub", :* => :"num-mul", :< => :"ord-lt", :> => :"ord-gt", :<= => :"ord-le", :>= => :"ord-ge"}, op)
      {core, ctx} = dictionary_operation(class, method, type, [ac, bc], result.(type), ctx)
      {core, result.(type), ctx}
    else
      {[:prim, numeric_op(int_op, float_op, type, op, ctx), ac, bc], result.(type), ctx}
    end
  end

  defp dictionary_operation(class, method, type, args, result, ctx) do
    info = Map.fetch!(ctx.classes, class)
    index = Enum.find_index(info.methods, fn {m, _, _, _} -> m == method end) || fail(ctx, "`#{class}` must declare method `#{method}`")
    {_, signature, _, _} = Enum.at(info.methods, index)
    actual = Types.subst_tvars(signature, %{info.param => type})
    expected = [:->, List.duplicate(type, length(args)), [:eff, :closed], result]
    ctx = unify!(actual, expected, ctx)
    {[:app, [:select, {:dictionary, class, type, ctx.loc}, index] | args], ctx}
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
    {rest_core, ctx} = in_scope(%{ctx | env: env}, fn ctx -> let(rest, body, type, ctx) end, ctx.env)
    {[:let, [[name, etype, core]], rest_core], ctx}
  end

  # `(let [(Point x y) p] ...)` binds by a pattern that cannot fail.
  defp let([{pattern, e} | rest], body, type, ctx) do
    {core, etype, ctx} = synth(e, ctx)
    {p, binds, ctx} = pattern(pattern, etype, ctx)
    ctx = %{ctx | matches: [{[{p, false}], etype, :let, ctx.loc} | ctx.matches]}
    {rest_core, ctx} = in_scope(%{ctx | env: Map.merge(ctx.env, binds)}, fn ctx -> let(rest, body, type, ctx) end, ctx.env)
    {[:match, core, [p, rest_core]], ctx}
  end

  # --- match ---------------------------------------------------------------------------

  # Mailbox data starts at Dyn. Concrete patterns select a decode target;
  # an explicit (the T pattern) also supplies one. A bare variable does not
  # silently become Int merely because its body uses it as one.
  defp receive_type({:call, :the, [ann, _p], _}, ctx), do: {surface_type(ann, ctx), ctx}
  defp receive_type({:atom, _}, ctx), do: {:Atom, ctx}
  defp receive_type({:string, _}, ctx), do: {:String, ctx}
  defp receive_type(n, ctx) when is_integer(n), do: {:Int, ctx}
  defp receive_type(n, ctx) when is_float(n), do: {:Float, ctx}
  defp receive_type(b, ctx) when is_boolean(b), do: {[:Bool], ctx}
  defp receive_type(nil, ctx), do: {:Unit, ctx}
  defp receive_type({:unit, _}, ctx), do: {:Unit, ctx}
  defp receive_type(x, ctx) when is_atom(x) do
    if Map.has_key?(ctx.ctors, x), do: receive_type({:call, x, [], ctx.loc}, ctx), else: {:Dyn, ctx}
  end
  defp receive_type({:tuple, ps, _}, ctx), do: receive_type({:tuple_pattern, ps}, ctx)
  defp receive_type({:tuple_pattern, []}, ctx), do: {:Unit, ctx}
  defp receive_type({:tuple_pattern, ps}, ctx) do
    {ts, ctx} = Enum.map_reduce(ps, ctx, &receive_type/2)
    {[:Tuple | ts], ctx}
  end
  defp receive_type({:bracket, []}, ctx), do: {[:List, :Dyn], ctx}
  defp receive_type({:bracket, {:cons, h, _}}, ctx) do
    {t, ctx} = receive_type(h, ctx)
    {[:List, t], ctx}
  end
  defp receive_type({:bracket, [h | _]}, ctx) do
    {t, ctx} = receive_type(h, ctx)
    {[:List, t], ctx}
  end
  defp receive_type({:call, c, _, _}, ctx) when is_map_key(ctx.ctors, c) do
    {t, _, ctx} = instance(Map.fetch!(ctx.ctors, c), "the type of this received constructor", ctx)
    {t, ctx}
  end
  defp receive_type({:call, t, _, _}, ctx) when is_map_key(ctx.decls, t) do
    {t, _, ctx} = instance(t, "the type of this received record", ctx)
    {t, ctx}
  end
  defp receive_type(p, ctx), do: fail(ctx, "unsupported receive pattern: #{inspect(p, limit: 4)}")

  # Patterns are checked against the scrutinee's type, which ties it to them
  # (D22). The unguarded clauses must cover the type (§10.8).
  defp match_clauses(s, stype, clauses, type, ctx) do
    loc = ctx.loc

    {cores, ctx, errors} =
      Enum.reduce(clauses, {[], ctx, []}, fn {pat, guard, body}, {cores, ctx, errors} ->
        try do
          {p, binds, inner} = pattern(pat, stype, ctx)

          {clause, inner} = in_scope(%{inner | env: Map.merge(inner.env, binds)}, fn inner ->
            case guard do
              nil ->
                {b, inner} = check(body, type, inner)
                {[p, b], inner}

              g ->
                {g, inner} = guard(g, inner)
                {b, inner} = check(body, type, inner)
                {[p, [:when, g], b], inner}
            end
          end, ctx.env)
          {cores ++ [clause], inner, errors}
        catch
          {:elab, error} -> {cores, ctx, errors ++ [error]}
          {:elab_errors, nested} -> {cores, ctx, errors ++ nested}
        end
      end)

    if errors != [], do: throw({:elab_errors, errors})
    clauses = for [p | rest] <- cores, do: {p, length(rest) == 2}
    {[:match, s | cores], %{ctx | matches: [{clauses, stype, :match, loc} | ctx.matches]}}
  end

  # A guard is guard-safe Core (liquid-core.md §6.6): no application, perform,
  # match or handle-crash, and no concat.
  defp guard(g, ctx) do
    old_guard = ctx.in_guard
    {core, ctx} = check(g, [:Bool], %{ctx | in_guard: true})
    ctx = %{ctx | in_guard: old_guard}

    case unsafe(core) do
      nil -> {core, ctx}
      what -> fail(ctx, "a guard cannot #{what}", note: "a guard may use variables, literals, comparisons, arithmetic, `and`, `or`, `not`, fields and constructors")
    end
  end

  defp unsafe([:app | _]), do: "call a function"
  defp unsafe([:perform | _]), do: "perform an effect"
  defp unsafe([:match | _]), do: "match"
  defp unsafe([:receive | _]), do: "receive a message"
  defp unsafe([:let | _]), do: "bind a name"
  defp unsafe([:"handle-crash" | _]), do: "catch a crash"
  defp unsafe([:fn | _]), do: "build a function"
  defp unsafe([:prim, :concat | _]), do: "concatenate strings"
  defp unsafe([:prim, :str | _]), do: "convert a value to a string"
  defp unsafe(list) when is_list(list), do: Enum.find_value(list, &unsafe/1)
  defp unsafe(_), do: nil

  # pattern(p, τ, ctx) -> {core pattern, %{source name => {core name, type}}, ctx}
  defp pattern(p, type, ctx), do: at(p, ctx, &pattern_(&1, type, &2))

  defp pattern_(:_, _type, ctx), do: {:_, %{}, ctx}
  defp pattern_({:call, :the, [ann, p], _}, type, ctx) do
    ctx = unify!(type, surface_type(ann, ctx), ctx)
    pattern(p, type, ctx)
  end
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
  defp pattern_({:call, :tuple, ps, _loc}, type, ctx), do: pattern_({:tuple_pattern, ps}, type, ctx)
  defp pattern_({:tuple_pattern, []}, type, ctx), do: {[:unit], %{}, unify!(type, :Unit, ctx)}
  defp pattern_({:tuple_pattern, [{:atom, tag}, p]}, [:"Core.TaskResult", target] = type, ctx) when tag in [:ok, :error] do
    {p, binds, ctx} = pattern(p, if(tag == :ok, do: target, else: :Dyn), ctx)
    {[:inj, type, tag, p], binds, ctx}
  end

  defp pattern_({:tuple_pattern, ps}, type, ctx) do
    {metas, ctx} = Enum.map_reduce(ps, ctx, fn _, ctx -> meta("the type of a component of this tuple", ctx) end)
    ctx = unify!(type, [:Tuple | metas], ctx)
    {cores, binds, ctx} = patterns(ps, metas, ctx)
    {[:tuple | cores], binds, ctx}
  end

  defp pattern_({:call, {:module_path, ["Core", c]}, ps, loc}, type, ctx) when c in ["Ok", "Err", "BadDecode"] do
    pattern_({:call, decode_constructor(c), ps, loc}, type, ctx)
  end
  defp pattern_({:call, {:qualified, m, c}, ps, loc}, type, ctx) when is_map_key(ctx.imports, m) do
    i = Map.fetch!(ctx.imports, m)
    found = Enum.any?(i.types, fn
      [:type, ^c, _, [:record | _]] -> true
      [:type, _, _, [:sum | cs]] -> Enum.any?(cs, fn [tag | _] -> tag == c end)
      _ -> false
    end)
    unless found, do: fail(ctx, "`#{m}` does not export constructor `#{c}`")
    pattern_({:call, c, ps, loc}, type, ctx)
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
    case Types.unify(expected, actual, ctx.s, ctx.next) do
      {:ok, s, next} -> %{ctx | s: s, next: next}
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
  defp in_scope(ctx, fun, outer_env) do
    case fun.(ctx) do
      {core, ctx2} -> {core, %{ctx2 | env: outer_env}}
      {core, type, ctx2} -> {core, type, %{ctx2 | env: outer_env}}
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

  defp fail(ctx, message, opts \\ []), do: throw({:elab, Error.new(message, Keyword.merge([span: span(ctx.loc), file: ctx.loc && ctx.loc.file], opts))})

  defp outside_term(reason), do: throw({:outside, reason})
end
