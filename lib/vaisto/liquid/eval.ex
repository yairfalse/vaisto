defmodule Vaisto.Liquid.Eval do
  @moduledoc """
  The reference evaluator: the executable form of `docs/design/liquid-core.md`
  §5 to §8. It is the definition of what a Core program means, so it favours
  clarity over speed: one clause per construct, primitives exactly as §7
  defines them.

  Values are their BEAM representations (§5), except closures, so an outcome
  can be compared with the outcome of the lowered program directly (§9).

  Outcomes (§6.1):

    * `{:ok, value}` - the term produced a value;
    * `{:crash, reason}` - the term crashed and nothing caught it.

  A term that Core Lint would reject makes the evaluator raise `Stuck`: that is
  a bug in whatever produced the term, never a crash of the program.
  """

  alias Vaisto.Liquid.Canonical

  defmodule Closure do
    @moduledoc "A function value: parameters, body, environment and, for `letrec`, its group."
    defstruct [:params, :body, :env, group: []]
  end

  defmodule Stuck do
    @moduledoc "The evaluator met a term that is not well formed Core (§6.1)."
    defexception [:message]
  end

  # The built-in named types (§3.3).
  @builtin_types %{
    Bool: {:sum, [], [true: [], false: []]},
    List: {:sum, [:a], [Nil: [], Cons: [[:tvar, :a], [:List, [:tvar, :a]]]]},
    Reason:
      {:sum, [],
       [badarith: [], badarg: [], no_match: [], bad_decode: [], user: [:Dyn], raised: [:Atom, :Dyn]]}
  }

  @typedoc "`:pure`, or `{:script, [{op, args, outcome}]}` with outcome `{:ok, v}` or `{:raised, class, reason}` (§8)."
  @type handler :: :pure | {:script, [{atom(), [term()], term()}]}
  @type outcome :: {:ok, term()} | {:crash, term()}

  @doc """
  Run function `entry` of a module (§4.3) on argument values.

  Options: `handler:` (default `:pure`).
  """
  @spec run(Canonical.tree(), atom(), [term()], keyword()) :: outcome()
  def run(module, entry, args, opts \\ []) do
    [:module, _name, [:"core-version", 0] | items] = Canonical.strip(module)
    types = Map.merge(@builtin_types, for([:type, t, params, body] <- items, into: %{}, do: {t, decl(params, body)}))
    defs = for [:def, f, ty, fun] <- items, do: [f, ty, fun]
    env = bind_group(%{}, defs)

    with_handler(opts, fn ->
      case Map.fetch(env, entry) do
        {:ok, closure} -> apply_closure(closure, args, types)
        :error -> stuck("no definition named #{entry}")
      end
    end)
  end

  @doc """
  Evaluate a closed term (§6). Options: `handler:` (default `:pure`) and
  `types:`, a list of `(type ...)` declarations the term may use.
  """
  @spec eval(Canonical.tree(), keyword()) :: outcome()
  def eval(term, opts \\ []) do
    declared = for [:type, t, params, body] <- Keyword.get(opts, :types, []), into: %{}, do: {t, decl(params, body)}
    types = Map.merge(@builtin_types, declared)
    with_handler(opts, fn -> ev(Canonical.strip(term), %{}, types) end)
  end

  defp decl(params, [:sum | ctors]), do: {:sum, params, for([c | fields] <- ctors, do: {c, fields})}
  defp decl(params, [:record | fields]), do: {:record, params, for([l, ty] <- fields, do: {l, ty})}

  # --- §5.1 Value typing -------------------------------------------------------

  @doc """
  Value typing (§5.1): is the BEAM term `value` the representation of a value
  of the closed type `type`? Options: `types:` declarations, as for `eval/2`.
  """
  @spec value_of?(term(), Canonical.tree(), keyword()) :: boolean()
  def value_of?(value, type, opts \\ []) do
    declared = for [:type, t, params, body] <- Keyword.get(opts, :types, []), into: %{}, do: {t, decl(params, body)}
    has_type?(value, Canonical.strip(type), Map.merge(@builtin_types, declared))
  end

  @reasons [:badarith, :badarg, :no_match, :bad_decode]

  defp has_type?(v, :Int, _types), do: is_integer(v)
  defp has_type?(v, :Float, _types), do: is_float(v)
  defp has_type?(v, :String, _types), do: is_binary(v)
  defp has_type?(v, :Atom, _types), do: is_atom(v)
  defp has_type?(v, :Unit, _types), do: v === nil
  defp has_type?(v, :Ref, _types), do: is_reference(v)
  defp has_type?(_v, :Dyn, _types), do: true
  defp has_type?(_v, [:tvar, _a], _types), do: true
  defp has_type?(v, [:forall, _binders, body], types), do: has_type?(v, body, types)
  defp has_type?(v, [:Pid, _protocol], _types), do: is_pid(v)

  defp has_type?(v, [:Tuple | ts], types),
    do: is_tuple(v) and tuple_size(v) == length(ts) and all_typed?(Tuple.to_list(v), ts, types)

  defp has_type?(v, [:Record, [:row | items]], types) when is_map(v) do
    {fields, [tail]} = Enum.split(items, -1)

    (tail != :closed or map_size(v) == length(fields)) and
      Enum.all?(fields, fn [l, t] -> is_map_key(v, l) and has_type?(Map.fetch!(v, l), t, types) end)
  end

  defp has_type?(v, [:Map, k, t], types) when is_map(v),
    do: Enum.all?(v, fn {key, val} -> has_type?(key, k, types) and has_type?(val, t, types) end)

  defp has_type?(v, [arrow, params | _], _types) when arrow in [:->, :pi],
    do: (match?(%Closure{}, v) and length(v.params) == length(params)) or is_function(v, length(params))

  defp has_type?(v, [:Bool], _types), do: is_boolean(v)
  defp has_type?([], [:List, _t], _types), do: true
  defp has_type?([h | t], [:List, et] = type, types), do: has_type?(h, et, types) and has_type?(t, type, types)
  defp has_type?(v, [:Reason], _types) when v in @reasons, do: true
  defp has_type?({:user, _v}, [:Reason], _types), do: true
  defp has_type?({:raised, c, _v}, [:Reason], _types), do: is_atom(c)

  defp has_type?(v, [t | args], types) when is_tuple(v) and tuple_size(v) > 0 and is_atom(t) do
    case Map.get(types, t) do
      {:record, params, fields} ->
        elem(v, 0) === t and tuple_size(v) == length(fields) + 1 and
          all_typed?(v |> Tuple.to_list() |> tl(), Enum.map(fields, &with_args(elem(&1, 1), params, args)), types)

      {:sum, params, ctors} when t not in [:Bool, :List, :Reason] ->
        case List.keyfind(ctors, elem(v, 0), 0) do
          {_c, fields} ->
            tuple_size(v) == length(fields) + 1 and
              all_typed?(v |> Tuple.to_list() |> tl(), Enum.map(fields, &with_args(&1, params, args)), types)

          nil ->
            false
        end

      _ ->
        false
    end
  end

  defp has_type?(_v, _type, _types), do: false

  defp all_typed?(vs, ts, types), do: Enum.zip(vs, ts) |> Enum.all?(fn {v, t} -> has_type?(v, t, types) end)

  # A declared field type with the declaration's parameters replaced by `args`.
  defp with_args(type, params, args), do: replace_tvars(type, Map.new(Enum.zip(params, args)))

  defp replace_tvars([:tvar, a] = t, s), do: Map.get(s, a, t)
  defp replace_tvars([:forall, binders, body], s), do: [:forall, binders, replace_tvars(body, Map.drop(s, Enum.map(binders, &hd/1)))]
  defp replace_tvars(t, s) when is_list(t), do: Enum.map(t, &replace_tvars(&1, s))
  defp replace_tvars(t, _s), do: t

  # --- §6 Evaluation: one clause per construct ---------------------------------

  defp ev(x, env, _types) when is_atom(x) do
    case Map.fetch(env, x) do
      {:ok, v} -> v
      :error -> stuck("unbound variable #{x}")
    end
  end

  defp ev(n, _env, _types) when is_integer(n) or is_float(n) or is_binary(n), do: n
  defp ev([:atom, a], _env, _types) when is_atom(a), do: a
  defp ev([:unit], _env, _types), do: nil

  defp ev([:fn, params, body], env, _types), do: %Closure{params: Enum.map(params, &hd/1), body: body, env: env}

  defp ev([:app, f | args], env, types) do
    closure = ev(f, env, types)
    values = Enum.map(args, &ev(&1, env, types))
    apply_closure(closure, values, types)
  end

  defp ev([:let, [[x, _ty, e1]], e2], env, types), do: ev(e2, Map.put(env, x, ev(e1, env, types)), types)
  defp ev([:letrec, group, body], env, types), do: ev(body, bind_group(env, group), types)
  defp ev([:inst, e | _tys], env, types), do: ev(e, env, types)
  defp ev([:up, _ty, e], env, types), do: ev(e, env, types)
  defp ev([:decode, _ty, _e], _env, _types), do: stuck("decode is not in Liquid Core version 0")

  defp ev([:tuple | es], env, types) when es != [], do: es |> Enum.map(&ev(&1, env, types)) |> List.to_tuple()

  defp ev([:record, [:Record, _row] | fields], env, types), do: Map.new(fields, fn [l, e] -> {l, ev(e, env, types)} end)

  defp ev([:record, [t | _args] | fields], env, types) when is_atom(t) do
    # Fields are evaluated in the order written (§6.2), stored in declaration order (§5).
    values = Map.new(fields, fn [l, e] -> {l, ev(e, env, types)} end)

    case Map.get(types, t) do
      {:record, _params, decl_fields} -> List.to_tuple([t | Enum.map(decl_fields, fn {l, _} -> field!(values, l, t) end)])
      _ -> stuck("#{t} is not a record type")
    end
  end

  defp ev([:select, e, l], env, types), do: select(ev(e, env, types), l, types)
  defp ev([:inj, [t | _args], c | es], env, types), do: build(t, c, Enum.map(es, &ev(&1, env, types)), types)

  defp ev([:if, c, e1, e2], env, types) do
    case ev(c, env, types) do
      true -> ev(e1, env, types)
      false -> ev(e2, env, types)
      other -> stuck("if on a non-Bool value #{inspect(other)}")
    end
  end

  defp ev([:match, e | clauses], env, types), do: match_clauses(ev(e, env, types), clauses, env, types)
  defp ev([:prim, op | es], env, types), do: prim(op, Enum.map(es, &ev(&1, env, types)))
  defp ev([:perform, :crash, e], env, types), do: crash(ev(e, env, types))
  defp ev([:perform, op | es], env, types), do: perform(op, Enum.map(es, &ev(&1, env, types)))

  defp ev([:"handle-crash", e, [[x, _reason_ty], handler]], env, types) do
    try do
      ev(e, env, types)
    catch
      {:liquid_crash, reason} -> ev(handler, Map.put(env, x, reason), types)
    end
  end

  defp ev(term, _env, _types), do: stuck("not a Core term: #{inspect(term, limit: 8)}")

  # --- §6.3 Functions ------------------------------------------------------------

  defp apply_closure(%Closure{params: params, body: body, env: env, group: group}, args, types)
       when length(params) == length(args) do
    env = env |> bind_group(group) |> Map.merge(Map.new(Enum.zip(params, args)))
    ev(body, env, types)
  end

  defp apply_closure(other, args, _types), do: stuck("cannot apply #{inspect(other, limit: 4)} to #{length(args)} arguments")

  # Every function of a letrec group sees the whole group (§6.3). The closures are
  # rebuilt at each application, so no value is ever cyclic.
  defp bind_group(env, group) do
    Enum.reduce(group, env, fn [f, _ty, [:fn, params, body]], acc ->
      Map.put(acc, f, %Closure{params: Enum.map(params, &hd/1), body: body, env: env, group: group})
    end)
  end

  # --- §5 and §6.4 Data ------------------------------------------------------------

  defp field!(values, l, t) do
    case Map.fetch(values, l) do
      {:ok, v} -> v
      :error -> stuck("record #{t} is missing field #{l}")
    end
  end

  defp build(:Bool, c, [], _types) when c in [true, false], do: c
  defp build(:List, :Nil, [], _types), do: []
  defp build(:List, :Cons, [h, t], _types), do: [h | t]
  defp build(:Reason, c, [], _types), do: c

  defp build(t, c, values, types) do
    case Map.get(types, t) do
      {:sum, _params, ctors} ->
        case List.keyfind(ctors, c, 0) do
          {^c, fields} when length(fields) == length(values) -> List.to_tuple([c | values])
          _ -> stuck("#{c} is not a constructor of #{t} with #{length(values)} fields")
        end

      _ ->
        stuck("#{t} is not a sum type")
    end
  end

  # The inverse of build/4: the fields of `v` if constructor `c` of `t` built it.
  defp unbuild(:Bool, c, v, _types), do: if(v === c, do: {:ok, []}, else: :no)
  defp unbuild(:List, :Nil, v, _types), do: if(v == [], do: {:ok, []}, else: :no)
  defp unbuild(:List, :Cons, [h | t], _types), do: {:ok, [h, t]}
  defp unbuild(:List, :Cons, _v, _types), do: :no
  defp unbuild(:Reason, c, v, _types) when is_atom(v), do: if(v === c, do: {:ok, []}, else: :no)

  defp unbuild(_t, c, v, _types) when is_tuple(v) and tuple_size(v) > 0 and elem(v, 0) === c,
    do: {:ok, v |> Tuple.to_list() |> tl()}

  defp unbuild(_t, _c, _v, _types), do: :no

  # A named record, an anonymous record (a map), or a tuple position. The value
  # says which, so a row-polymorphic select needs no type information (§6.4).
  defp select(v, l, _types) when is_tuple(v) and is_integer(l) and l >= 0 and l < tuple_size(v), do: elem(v, l)
  defp select(v, l, _types) when is_map(v) and is_map_key(v, l), do: Map.fetch!(v, l)

  defp select(v, l, types) when is_tuple(v) and tuple_size(v) > 0 do
    with t when is_atom(t) <- elem(v, 0),
         {:record, _params, fields} <- Map.get(types, t),
         index when is_integer(index) <- Enum.find_index(fields, fn {name, _} -> name == l end) do
      elem(v, index + 1)
    else
      _ -> stuck("no field #{inspect(l)} in #{inspect(v, limit: 4)}")
    end
  end

  defp select(v, l, _types), do: stuck("no field #{inspect(l)} in #{inspect(v, limit: 4)}")

  # --- §6.5 and §6.6 Matching ------------------------------------------------------

  defp match_clauses(_v, [], _env, _types), do: crash(:no_match)
  defp match_clauses(v, [clause | rest], env, types) do
    {pattern, guard, body} =
      case clause do
        [p, [:when, g], body] -> {p, g, body}
        [p, body] -> {p, nil, body}
      end

    with {:ok, binds} <- match(pattern, v, types),
         env = Map.merge(env, binds),
         true <- guard == nil or guard_holds?(guard, env, types) do
      ev(body, env, types)
    else
      _ -> match_clauses(v, rest, env, types)
    end
  end

  # A guard that crashes counts as false (§6.6).
  defp guard_holds?(guard, env, types) do
    ev(guard, env, types) === true
  catch
    {:liquid_crash, _reason} -> false
  end

  defp match(:_, _v, _types), do: {:ok, %{}}
  defp match(x, v, _types) when is_atom(x), do: {:ok, %{x => v}}
  defp match(lit, v, _types) when is_integer(lit) or is_float(lit) or is_binary(lit), do: if(v === lit, do: {:ok, %{}}, else: :no)
  defp match([:atom, a], v, _types), do: if(v === a, do: {:ok, %{}}, else: :no)
  defp match([:unit], v, _types), do: if(v === nil, do: {:ok, %{}}, else: :no)

  defp match([:as, x, p], v, types) do
    with {:ok, binds} <- match(p, v, types), do: {:ok, Map.put(binds, x, v)}
  end

  defp match([:tuple | ps], v, types) when is_tuple(v) and tuple_size(v) == length(ps),
    do: match_all(ps, Tuple.to_list(v), types)

  defp match([:tuple | _], _v, _types), do: :no

  defp match([:record, [t | _args] | fields], v, types) when is_atom(t) do
    with {:record, _params, decl_fields} <- Map.get(types, t),
         true <- is_tuple(v) and tuple_size(v) == length(decl_fields) + 1 and elem(v, 0) === t do
      {labels, ps} = Enum.unzip(Enum.map(fields, fn [l, p] -> {l, p} end))
      match_all(ps, Enum.map(labels, &select(v, &1, types)), types)
    else
      _ -> :no
    end
  end

  defp match([:inj, [t | _args], c | ps], v, types) do
    with {:ok, fields} <- unbuild(t, c, v, types),
         true <- length(fields) == length(ps) do
      match_all(ps, fields, types)
    else
      _ -> :no
    end
  end

  defp match(p, _v, _types), do: stuck("not a Core pattern: #{inspect(p, limit: 8)}")

  defp match_all(ps, vs, types) do
    Enum.zip(ps, vs)
    |> Enum.reduce_while({:ok, %{}}, fn {p, v}, {:ok, acc} ->
      case match(p, v, types) do
        {:ok, binds} -> {:cont, {:ok, Map.merge(acc, binds)}}
        :no -> {:halt, :no}
      end
    end)
  end

  # --- §7 Primitives ------------------------------------------------------------------

  defp prim(:add, [x, y]) when is_integer(x) and is_integer(y), do: x + y
  defp prim(:sub, [x, y]) when is_integer(x) and is_integer(y), do: x - y
  defp prim(:mul, [x, y]) when is_integer(x) and is_integer(y), do: x * y
  defp prim(:neg, [x]) when is_integer(x), do: -x
  defp prim(:div, [x, 0]) when is_integer(x), do: crash(:badarith)
  defp prim(:div, [x, y]) when is_integer(x) and is_integer(y), do: tdiv(x, y)
  defp prim(:rem, [x, 0]) when is_integer(x), do: crash(:badarith)
  defp prim(:rem, [x, y]) when is_integer(x) and is_integer(y), do: trem(x, y)
  defp prim(:lt, [x, y]) when is_integer(x) and is_integer(y), do: x < y
  defp prim(:le, [x, y]) when is_integer(x) and is_integer(y), do: x <= y
  defp prim(:gt, [x, y]) when is_integer(x) and is_integer(y), do: x > y
  defp prim(:ge, [x, y]) when is_integer(x) and is_integer(y), do: x >= y

  defp prim(:fadd, [x, y]) when is_float(x) and is_float(y), do: finite(fn -> x + y end)
  defp prim(:fsub, [x, y]) when is_float(x) and is_float(y), do: finite(fn -> x - y end)
  defp prim(:fmul, [x, y]) when is_float(x) and is_float(y), do: finite(fn -> x * y end)
  defp prim(:fneg, [x]) when is_float(x), do: -x
  defp prim(:fdiv, [x, y]) when is_float(x) and is_float(y) and y == 0.0, do: crash(:badarith)
  defp prim(:fdiv, [x, y]) when is_float(x) and is_float(y), do: finite(fn -> x / y end)
  defp prim(:flt, [x, y]) when is_float(x) and is_float(y), do: x < y
  defp prim(:fle, [x, y]) when is_float(x) and is_float(y), do: x <= y
  defp prim(:fgt, [x, y]) when is_float(x) and is_float(y), do: x > y
  defp prim(:fge, [x, y]) when is_float(x) and is_float(y), do: x >= y

  defp prim(:"int-to-float", [x]) when is_integer(x) do
    :erlang.float(x)
  rescue
    ArgumentError -> crash(:badarg)
  end

  defp prim(:eq, [x, y]), do: x === y
  defp prim(:ne, [x, y]), do: x !== y
  defp prim(:not, [x]) when is_boolean(x), do: not x
  defp prim(:concat, [x, y]) when is_binary(x) and is_binary(y), do: x <> y

  defp prim(:length, [xs]) when is_list(xs), do: len(xs, 0)
  defp prim(:empty?, [[]]), do: true
  defp prim(:empty?, [[_ | _]]), do: false
  defp prim(:head, [[h | _]]), do: h
  defp prim(:head, [[]]), do: crash(:badarg)
  defp prim(:tail, [[_ | t]]), do: t
  defp prim(:tail, [[]]), do: crash(:badarg)

  defp prim(op, args), do: stuck("primitive #{op} is not defined on #{inspect(args, limit: 4)}")

  # By recursion over Cons cells, not with the host's length/1 (§7, independence).
  defp len([], n), do: n
  defp len([_ | t], n), do: len(t, n + 1)

  # BEAM raises badarith for a non-finite float result; §7 calls that a crash.
  defp finite(f) do
    f.()
  rescue
    ArithmeticError -> crash(:badarith)
  end

  # tdiv(x, y): the quotient of x and y rounded toward zero; trem(x, y) = x - y * tdiv(x, y).
  # Both are called only with y != 0. §7 asks for them from their definitions, not
  # from Erlang's div and rem: those are what the lowering will be compared with.
  # Sign and magnitude: on non-negative operands truncated, floored and Euclidean
  # division agree, so the host operator is used only there, and the sign, where
  # the conventions differ, is applied here.
  defp tdiv(x, y) do
    magnitude = div(abs(x), abs(y))

    if (x < 0) != (y < 0) do
      -magnitude
    else
      magnitude
    end
  end

  defp trem(x, y) do
    x - y * tdiv(x, y)
  end

  # --- §6.7 Crashes and §8 Handlers -------------------------------------------------

  defp crash(reason), do: throw({:liquid_crash, reason})

  defp perform(op, args) do
    case Process.get(:vaisto_liquid_handler) do
      :pure ->
        stuck("perform #{op} under the pure handler")

      {:script, [{^op, ^args, outcome} | rest]} ->
        Process.put(:vaisto_liquid_handler, {:script, rest})
        answer(outcome)

      {:script, script} ->
        stuck("the script does not fit: got #{inspect({op, args})}, expected #{inspect(List.first(script))}")
    end
  end

  defp answer({:ok, v}), do: v
  defp answer({:raised, class, reason}), do: crash({:raised, class, reason})

  defp with_handler(opts, fun) do
    previous = Process.put(:vaisto_liquid_handler, Keyword.get(opts, :handler, :pure))

    try do
      outcome =
        try do
          {:ok, fun.()}
        catch
          {:liquid_crash, reason} -> {:crash, reason}
        end

      case Process.get(:vaisto_liquid_handler) do
        {:script, [_ | _] = unused} -> stuck("the script was not used up: #{inspect(unused)}")
        _ -> outcome
      end
    after
      if previous, do: Process.put(:vaisto_liquid_handler, previous), else: Process.delete(:vaisto_liquid_handler)
    end
  end

  defp stuck(message), do: raise(Stuck, message: message)
end
