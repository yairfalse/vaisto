defmodule Vaisto.Liquid.Lower do
  @moduledoc """
  Liquid Core to Erlang abstract format to BEAM (RFC §7, M13).

  Core Lint must accept the module before lowering starts. No HM types, old
  typed AST, old emitter or reference evaluator participates in compilation.
  Refinements are rejected by Lint until a verified erasure path is supplied.

  The runtime supplies only primitives and effect/decoding boundaries; the
  generated module implements its own functions, closures and control flow.
  """

  alias Vaisto.Errors
  alias Vaisto.Liquid.{Canonical, Lint, Runtime}

  defstruct types: %{}, funs: %{}, extra: [], next: 0, reserved: MapSet.new(), remotes: %{}

  @doc "Compile a Linted Core module; `load: false` returns BEAM without loading it."
  @spec compile(term(), atom(), keyword()) :: {:ok, atom(), binary()} | {:error, term()}
  def compile(core, module_name, opts \\ []) do
    with {:ok, forms} <- to_forms(core, module_name),
         {:ok, binary} <- beam(forms, module_name),
         :ok <- load(module_name, binary, Keyword.get(opts, :load, true)) do
      {:ok, module_name, binary}
    end
  end

  @doc "Lint and lower a Core module into the Erlang abstract format."
  @spec to_forms(term(), atom()) :: {:ok, [term()]} | {:error, term()}
  def to_forms(core, module_name) when is_atom(module_name) do
    with :ok <- Lint.check_module(core) do
      [:module, _, [:"core-version", 0] | items] = Canonical.strip(core)

      types =
        for [:type, name, params, [kind | members]] <- items,
            into: %{},
            do: {name, {params, kind, members}}

      types =
        Map.merge(types, %{
          "Core.DecodeError": {[], :sum, [[:"Core.BadDecode"]]},
          "Core.Result":
            {[:e, :a], :sum, [[:"Core.Err", [:tvar, :e]], [:"Core.Ok", [:tvar, :a]]]},
          "Core.TaskResult": {[:a], :sum, [[:ok, [:tvar, :a]], [:error, :Dyn]]}
        })

      defs = for [:def, f, _, [:fn, params, _]] = d <- items, do: {f, length(params), d}
      funs = Map.new(defs, fn {f, arity, _} -> {f, {:fun, 0, {:function, f, arity}}} end)

      remotes =
        for [:import, m, _, exports] <- items,
            [f, type, _] <- exports,
            into: %{},
            do: {{m, f}, type}

      ctx = %__MODULE__{
        types: types,
        funs: funs,
        reserved: MapSet.new(Map.keys(funs)),
        remotes: remotes
      }

      {functions, ctx} =
        Enum.map_reduce(defs, ctx, fn {name, arity, [:def, _, _, [:fn, params, body]]}, ctx ->
          {ps, env, ctx} = parameters(params, %{}, ctx)
          {body, ctx} = expr(body, env, ctx)
          {{:function, 0, name, arity, [{:clause, 0, ps, [], [body]}]}, ctx}
        end)

      {:ok,
       [
         {:attribute, 0, :module, module_name},
         {:attribute, 0, :export, Enum.map(defs, fn {f, arity, _} -> {f, arity} end)}
         | functions ++ Enum.reverse(ctx.extra)
       ]}
    end
  end

  defp beam(forms, module_name) do
    case :compile.forms(forms, [:binary, :return_errors, :return_warnings]) do
      {:ok, ^module_name, binary} ->
        {:ok, binary}

      {:ok, ^module_name, binary, _warnings} ->
        {:ok, binary}

      {:error, errors, warnings} ->
        {:error, Errors.beam_compilation_failed(inspect({errors, warnings}))}
    end
  end

  defp load(_, _, false), do: :ok

  defp load(module_name, binary, true) do
    case :code.load_binary(module_name, ~c"vaisto-liquid", binary) do
      {:module, ^module_name} ->
        :ok

      {:error, reason} ->
        {:error, Errors.compilation_error("cannot load #{module_name}: #{inspect(reason)}")}
    end
  end

  defp literal(value), do: :erl_parse.abstract(value)
  defp runtime(f, args), do: {:call, 0, {:remote, 0, literal(Runtime), literal(f)}, args}

  defp list(xs),
    do: Enum.reduce(Enum.reverse(xs), {nil, 0}, fn x, tail -> {:cons, 0, x, tail} end)

  defp tuple(xs), do: {:tuple, 0, xs}
  defp clause(pattern, body), do: {:clause, 0, [pattern], [], [body]}
  defp block(xs), do: {:block, 0, xs}
  defp crash(reason), do: runtime(:crash, [literal(reason)])

  defp fresh(ctx) do
    {{:var, 0, :"V#{ctx.next}"}, %{ctx | next: ctx.next + 1}}
  end

  defp helper_name(ctx) do
    name = :"__liquid_rec_#{ctx.next}"
    ctx = %{ctx | next: ctx.next + 1}

    if MapSet.member?(ctx.reserved, name),
      do: helper_name(ctx),
      else: {name, %{ctx | reserved: MapSet.put(ctx.reserved, name)}}
  end

  defp parameters(params, env, ctx) do
    Enum.reduce(params, {[], env, ctx}, fn [name, _type], {ps, env, ctx} ->
      {v, ctx} = fresh(ctx)
      {ps ++ [v], Map.put(env, name, v), ctx}
    end)
  end

  defp expr(x, env, ctx) when is_atom(x) do
    {Map.get_lazy(env, x, fn -> Map.fetch!(ctx.funs, x) end), ctx}
  end

  defp expr(x, _env, ctx) when is_number(x) or is_binary(x), do: {literal(x), ctx}
  defp expr([:atom, a], _env, ctx), do: {literal(a), ctx}
  defp expr([:unit], _env, ctx), do: {literal(nil), ctx}

  defp expr([:remote, m, f], _env, ctx) do
    type = Map.fetch!(ctx.remotes, {m, f})

    arrow =
      case type do
        [:forall, _, arrow] -> arrow
        arrow -> arrow
      end

    [:->, params, _, _] = arrow
    {{:fun, 0, {:function, literal(m), literal(f), literal(length(params))}}, ctx}
  end

  defp expr([:inst, e | _], env, ctx), do: expr(e, env, ctx)
  defp expr([:up, _, e], env, ctx), do: expr(e, env, ctx)

  defp expr([:fn, params, body], env, ctx) do
    {ps, env, ctx} = parameters(params, env, ctx)
    {body, ctx} = expr(body, env, ctx)
    {{:fun, 0, {:clauses, [{:clause, 0, ps, [], [body]}]}}, ctx}
  end

  defp expr([:app, f | args], env, ctx) do
    {f, ctx} = expr(f, env, ctx)
    {args, ctx} = expressions(args, env, ctx)
    {{:call, 0, f, args}, ctx}
  end

  defp expr([:let, [[x, _, value]], body], env, ctx) do
    {value, ctx} = expr(value, env, ctx)
    {v, ctx} = fresh(ctx)
    {body, ctx} = expr(body, Map.put(env, x, v), ctx)
    {block([{:match, 0, v, value}, body]), ctx}
  end

  defp expr([:letrec, group, body], env, ctx) do
    # Closure conversion: each recursive function becomes a private function
    # whose first parameters are the captured values. The local function
    # values close over those captures and call the private functions.
    captures = env |> Enum.sort() |> Enum.map(&elem(&1, 1))
    old_funs = ctx.funs

    {heads, ctx} =
      Enum.map_reduce(group, ctx, fn [f, _, [:fn, params, _]], ctx ->
        {name, ctx} = helper_name(ctx)
        {{f, name, params}, ctx}
      end)

    {funs, ctx} =
      Enum.map_reduce(heads, ctx, fn {f, name, params}, ctx ->
        {ps, _, ctx} = parameters(params, %{}, ctx)

        closure =
          {:fun, 0,
           {:clauses, [{:clause, 0, ps, [], [{:call, 0, literal(name), captures ++ ps}]}]}}

        {{f, closure}, ctx}
      end)

    ctx = %{ctx | funs: Map.merge(old_funs, Map.new(funs))}

    ctx =
      Enum.zip(group, heads)
      |> Enum.reduce(ctx, fn {[_, _, [:fn, params, function_body]], {_, name, _}}, ctx ->
        {ps, inner_env, ctx} = parameters(params, env, ctx)
        {function_body, ctx} = expr(function_body, inner_env, ctx)

        function =
          {:function, 0, name, length(captures) + length(ps),
           [{:clause, 0, captures ++ ps, [], [function_body]}]}

        %{ctx | extra: [function | ctx.extra]}
      end)

    {body, ctx} = expr(body, env, ctx)
    {body, %{ctx | funs: old_funs}}
  end

  defp expr([:tuple | es], env, ctx) do
    {es, ctx} = expressions(es, env, ctx)
    {tuple(es), ctx}
  end

  defp expr([:inj, type, ctor | es], env, ctx) do
    {es, ctx} = expressions(es, env, ctx)
    {constructor(type, ctor, es), ctx}
  end

  defp expr([:record, type | fields], env, ctx) do
    # The tuple's layout follows its declaration; effects follow source order.
    {bindings, values, ctx} =
      Enum.reduce(fields, {[], %{}, ctx}, fn [l, e], {bindings, values, ctx} ->
        {e, ctx} = expr(e, env, ctx)
        {v, ctx} = fresh(ctx)
        {bindings ++ [{:match, 0, v, e}], Map.put(values, l, v), ctx}
      end)

    record =
      case type do
        [:Record, _] ->
          {:map, 0,
           Enum.map(fields, fn [l, _] ->
             {:map_field_assoc, 0, literal(l), Map.fetch!(values, l)}
           end)}

        [name | _] ->
          {_, :record, declared} = Map.fetch!(ctx.types, name)
          tuple([literal(name) | Enum.map(declared, fn [l, _] -> Map.fetch!(values, l) end)])
      end

    {block(bindings ++ [record]), ctx}
  end

  defp expr([:select, e, label], env, ctx) do
    {e, ctx} = expr(e, env, ctx)
    {runtime(:select, [e, literal(label), literal(ctx.types)]), ctx}
  end

  defp expr([:if, condition, yes, no], env, ctx) do
    {condition, ctx} = expr(condition, env, ctx)
    {yes, ctx} = expr(yes, env, ctx)
    {no, ctx} = expr(no, env, ctx)
    {{:case, 0, condition, [clause(literal(true), yes), clause(literal(false), no)]}, ctx}
  end

  defp expr([:match, value | clauses], env, ctx) do
    {value, ctx} = expr(value, env, ctx)
    {v, ctx} = fresh(ctx)
    {body, ctx} = match_clauses(clauses, v, env, ctx)
    {block([{:match, 0, v, value}, body]), ctx}
  end

  defp expr([:map, _, _ | entries], env, ctx) do
    {bindings, fields, ctx} =
      Enum.reduce(entries, {[], [], ctx}, fn [key, value], {bindings, fields, ctx} ->
        {key, ctx} = expr(key, env, ctx)
        {k, ctx} = fresh(ctx)
        {value, ctx} = expr(value, env, ctx)
        {v, ctx} = fresh(ctx)

        {bindings ++ [{:match, 0, k, key}, {:match, 0, v, value}],
         fields ++ [{:map_field_assoc, 0, k, v}], ctx}
      end)

    {block(bindings ++ [{:map, 0, fields}]), ctx}
  end

  defp expr([:prim, op | args], env, ctx) do
    {args, ctx} = expressions(args, env, ctx)
    {runtime(:primitive, [literal(op), list(args)]), ctx}
  end

  defp expr([:receive | clauses], env, ctx) do
    {loop, ctx} = fresh(ctx)
    retry = {:call, 0, loop, []}

    {branches, ctx} =
      Enum.map_reduce(clauses, ctx, fn [type, p, body], ctx ->
        {raw, ctx} = fresh(ctx)
        {p, binds, ctx} = pattern(p, %{}, ctx)
        {body, ctx} = expr(body, Map.merge(env, binds), ctx)
        decode = runtime(:decode, [raw, literal(type), literal(ctx.types)])

        {{raw, {:match, 0, raw, p}, receive_guard(raw, type, ctx.types, MapSet.new()), decode,
          body}, ctx}
      end)

    native =
      Enum.map(branches, fn {_raw, p, guard, decode, body} ->
        checked =
          {:case, 0, decode,
           [
             clause(tuple([literal(:"Core.Ok"), {:var, 0, :_}]), body),
             clause({:var, 0, :_}, retry)
           ]}

        {:clause, 0, [p], [[guard]], [checked]}
      end)

    native =
      {:call, 0, {:named_fun, 0, elem(loop, 2), [{:clause, 0, [], [], [{:receive, 0, native}]}]},
       []}

    scripted =
      Enum.map(branches, fn {_raw, p, guard, decode, body} ->
        checked =
          {:case, 0, decode,
           [
             clause(tuple([literal(:"Core.Ok"), {:var, 0, :_}]), body),
             clause({:var, 0, :_}, runtime(:invalid_receive, []))
           ]}

        {:clause, 0, [p], [[guard]], [checked]}
      end)

    selectors = Enum.map(clauses, fn [type, p, _] -> [type, p] end)
    message = runtime(:perform, [literal(:receive), literal([selectors])])

    scripted =
      {:case, 0, message, scripted ++ [clause({:var, 0, :_}, runtime(:invalid_receive, []))]}

    {{:case, 0, runtime(:effect_mode, []),
      [clause(literal(:native), native), clause({:var, 0, :_}, scripted)]}, ctx}
  end

  defp expr([:perform, :crash, reason], env, ctx) do
    {reason, ctx} = expr(reason, env, ctx)
    {runtime(:crash, [reason]), ctx}
  end

  defp expr([:perform, :spawn, protocol, state, tags, step, initial], env, ctx) do
    {step, ctx} = expr(step, env, ctx)
    {initial, ctx} = expr(initial, env, ctx)

    {runtime(:spawn_process, [
       literal(protocol),
       literal(state),
       literal(tags),
       step,
       initial,
       literal(ctx.types)
     ]), ctx}
  end

  defp expr([:perform, :generate, input, output, template, payload], env, ctx) do
    {payload, ctx} = expr(payload, env, ctx)

    {runtime(:generate, [
       literal(input),
       literal(output),
       literal(template),
       payload,
       literal(ctx.types)
     ]), ctx}
  end

  defp expr([:perform, :supervise, strategy | children], env, ctx) do
    {children, ctx} =
      Enum.map_reduce(children, ctx, fn [id, protocol, state, tags, step, initial], ctx ->
        {step, ctx} = expr(step, env, ctx)
        {initial, ctx} = expr(initial, env, ctx)

        {list([literal(id), literal(protocol), literal(state), literal(tags), step, initial]),
         ctx}
      end)

    {runtime(:start_supervisor, [literal(strategy), list(children), literal(ctx.types)]), ctx}
  end

  defp expr([:perform, :send, _protocol, pid, message], env, ctx) do
    {pid, ctx} = expr(pid, env, ctx)
    {message, ctx} = expr(message, env, ctx)
    {runtime(:perform, [literal(:send), list([pid, message])]), ctx}
  end

  defp expr([:perform, :link, _protocol, pid], env, ctx) do
    {pid, ctx} = expr(pid, env, ctx)
    {runtime(:perform, [literal(:link), list([pid])]), ctx}
  end

  defp expr([:perform, op | args], env, ctx) do
    {args, ctx} = expressions(args, env, ctx)
    {runtime(:perform, [literal(op), list(args)]), ctx}
  end

  defp expr([:decode, type, value], env, ctx) do
    {value, ctx} = expr(value, env, ctx)
    {runtime(:decode, [value, literal(type), literal(ctx.types)]), ctx}
  end

  defp expr([:"handle-crash", value, [[x, _], handler]], env, ctx) do
    {value, ctx} = expr(value, env, ctx)
    {v, ctx} = fresh(ctx)
    {handler, ctx} = expr(handler, Map.put(env, x, v), ctx)
    {catch_crash(value, v, handler), ctx}
  end

  defp expressions(es, env, ctx), do: Enum.map_reduce(es, ctx, &expr(&1, env, &2))

  # Only guard-safe checks belong in selective receive. Recursive containers
  # are decoded fully after selection, before any typed binding is used.
  defp receive_guard(v, t, types, seen) do
    bif = fn f, args -> {:call, 0, {:remote, 0, literal(:erlang), literal(f)}, args} end
    both = fn a, b -> {:op, 0, :andalso, a, b} end

    tuple_guard = fn fields ->
      shape =
        both.(
          bif.(:is_tuple, [v]),
          {:op, 0, :"=:=", bif.(:tuple_size, [v]), literal(length(fields))}
        )

      Enum.with_index(fields, 1)
      |> Enum.reduce(shape, fn {type, i}, acc ->
        both.(acc, receive_guard(bif.(:element, [literal(i), v]), type, types, seen))
      end)
    end

    case t do
      :Dyn ->
        literal(true)

      :Unit ->
        {:op, 0, :"=:=", v, literal(nil)}

      scalar when scalar in [:Int, :Float, :String, :Atom, :Ref] ->
        bif.(
          Map.fetch!(
            %{
              Int: :is_integer,
              Float: :is_float,
              String: :is_binary,
              Atom: :is_atom,
              Ref: :is_reference
            },
            scalar
          ),
          [v]
        )

      [:Bool] ->
        bif.(:is_boolean, [v])

      [:Pid, :Dyn] ->
        bif.(:is_pid, [v])

      [:List, _] ->
        bif.(:is_list, [v])

      [:Map, _, _] ->
        bif.(:is_map, [v])

      [:atom, a] ->
        {:op, 0, :"=:=", v, literal(a)}

      [:Record, [:row | items]] ->
        fields = Enum.filter(items, &is_list/1)

        shape =
          both.(
            bif.(:is_map, [v]),
            {:op, 0, :"=:=", bif.(:map_size, [v]), literal(length(fields))}
          )

        Enum.reduce(fields, shape, fn [label, type], acc ->
          both.(
            both.(acc, bif.(:is_map_key, [literal(label), v])),
            receive_guard(bif.(:map_get, [literal(label), v]), type, types, seen)
          )
        end)

      [:Tuple | fields] ->
        tuple_guard.(fields)

      [:Reason] ->
        atoms =
          Enum.map(
            [:badarith, :badarg, :no_match, :bad_decode],
            &{:op, 0, :"=:=", v, literal(&1)}
          )

        tuples =
          Enum.map([[[:atom, :user], :Dyn], [[:atom, :raised], :Atom, :Dyn]], &tuple_guard.(&1))

        Enum.reduce(atoms ++ tuples, literal(false), &{:op, 0, :orelse, &2, &1})

      [name | args] ->
        if MapSet.member?(seen, name) do
          literal(true)
        else
          {params, kind, members} = Map.fetch!(types, name)
          subst = Map.new(Enum.zip(params, args))
          members = receive_subst(members, subst)

          alternatives =
            if kind == :record, do: [[name | Enum.map(members, &List.last/1)]], else: members

          Enum.reduce(alternatives, literal(false), fn [tag | fields], acc ->
            tagged = [[:atom, tag] | fields]
            guard = receive_guard(v, [:Tuple | tagged], types, MapSet.put(seen, name))
            {:op, 0, :orelse, acc, guard}
          end)
        end
    end
  end

  defp receive_subst([:tvar, a], subst), do: Map.fetch!(subst, a)
  defp receive_subst(xs, subst) when is_list(xs), do: Enum.map(xs, &receive_subst(&1, subst))
  defp receive_subst(t, _subst), do: t

  defp constructor([:Bool], c, []), do: literal(c)
  defp constructor([:List, _], :Nil, []), do: {nil, 0}
  defp constructor([:List, _], :Cons, [h, t]), do: {:cons, 0, h, t}

  defp constructor([:Reason], c, []) when c in [:badarith, :badarg, :no_match, :bad_decode],
    do: literal(c)

  defp constructor(_, ctor, fields), do: tuple([literal(ctor) | fields])

  defp catch_crash(value, v, handler) do
    pattern = tuple([literal(:throw), tuple([literal(:liquid_crash), v]), {:var, 0, :_}])
    {:try, 0, [value], [], [clause(pattern, handler)], []}
  end

  defp match_clauses([], _v, _env, ctx), do: {crash(:no_match), ctx}

  defp match_clauses([clause | rest], v, env, ctx) do
    {p, guard, body} =
      case clause do
        [p, [:when, g], b] -> {p, g, b}
        [p, b] -> {p, nil, b}
      end

    {p, binds, ctx} = pattern(p, %{}, ctx)
    inner = Map.merge(env, binds)
    {body, ctx} = expr(body, inner, ctx)
    {fallback, ctx} = match_clauses(rest, v, env, ctx)

    if guard do
      # A crash in a guard is false, not a crash of the program. Compiling
      # the continuation once also avoids duplicating the remaining clauses.
      {next, ctx} = fresh(ctx)
      continuation = {:fun, 0, {:clauses, [{:clause, 0, [], [], [fallback]}]}}
      retry = {:call, 0, next, []}
      {guard, ctx} = expr(guard, inner, ctx)
      {ignored, ctx} = fresh(ctx)
      guard = catch_crash(guard, ignored, literal(false))
      choose = {:case, 0, guard, [clause(literal(true), body), clause(literal(false), retry)]}
      select = {:case, 0, v, [clause(p, choose), clause({:var, 0, :_}, retry)]}
      {block([{:match, 0, next, continuation}, select]), ctx}
    else
      {{:case, 0, v, [clause(p, body), clause({:var, 0, :_}, fallback)]}, ctx}
    end
  end

  defp pattern(:_, binds, ctx), do: {{:var, 0, :_}, binds, ctx}

  defp pattern(x, binds, ctx) when is_atom(x) do
    {v, ctx} = fresh(ctx)
    {v, Map.put(binds, x, v), ctx}
  end

  defp pattern(x, binds, ctx) when is_number(x) or is_binary(x), do: {literal(x), binds, ctx}
  defp pattern([:atom, a], binds, ctx), do: {literal(a), binds, ctx}
  defp pattern([:unit], binds, ctx), do: {literal(nil), binds, ctx}

  defp pattern([:as, x, p], binds, ctx) do
    {v, ctx} = fresh(ctx)
    {p, binds, ctx} = pattern(p, Map.put(binds, x, v), ctx)
    {{:match, 0, v, p}, binds, ctx}
  end

  defp pattern([:tuple | ps], binds, ctx) do
    {ps, binds, ctx} = patterns(ps, binds, ctx)
    {tuple(ps), binds, ctx}
  end

  defp pattern([:inj, type, ctor | ps], binds, ctx) do
    {ps, binds, ctx} = patterns(ps, binds, ctx)
    {constructor(type, ctor, ps), binds, ctx}
  end

  defp pattern([:record, [name | _] | fields], binds, ctx) do
    {_, :record, declared} = Map.fetch!(ctx.types, name)
    fields = Map.new(fields, fn [l, p] -> {l, p} end)
    ps = Enum.map(declared, fn [l, _] -> Map.get(fields, l, :_) end)
    {ps, binds, ctx} = patterns(ps, binds, ctx)
    {tuple([literal(name) | ps]), binds, ctx}
  end

  defp patterns(ps, binds, ctx) do
    Enum.reduce(ps, {[], binds, ctx}, fn p, {ps, binds, ctx} ->
      {p, binds, ctx} = pattern(p, binds, ctx)
      {ps ++ [p], binds, ctx}
    end)
  end
end
