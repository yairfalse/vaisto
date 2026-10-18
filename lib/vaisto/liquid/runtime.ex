defmodule Vaisto.Liquid.Runtime do
  @moduledoc """
  Runtime operations for native Liquid Core modules. This is not an evaluator:
  functions, closures, recursion, matches and control flow run as BEAM code.
  It supplies structural decoding, normalized crashes and effect boundaries.

  The handler is process-local and restored even on failure. Ordinary native
  entry calls use live spawn/send/link; `run/4` explicitly chooses `:pure`,
  `:native` or a script. Native foreign calls cross the declared extern
  boundary; the compiled caller decodes every result before using it.
  """

  @handler {__MODULE__, :handler}
  @behaviour :gen_server

  @doc "Run under `:pure` (default), `:native`, `{:script, steps}`, or `:observe` (stop before the first effect)."
  def run(module, entry, args, opts \\ []) do
    previous = Process.get(@handler, :absent)
    Process.put(@handler, Keyword.get(opts, :handler, :pure))

    try do
      outcome =
        try do
          {:ok, apply(module, entry, args)}
        catch
          {:liquid_crash, reason} -> {:crash, reason}
          {:liquid_suspended, op, args} -> {:effect, op, args}
        end

      case Process.get(@handler) do
        {:script, [_ | _]} -> raise ArgumentError, "native effect script was not used up"
        _ -> outcome
      end
    after
      if previous == :absent, do: Process.delete(@handler), else: Process.put(@handler, previous)
    end
  end

  @doc false
  def crash(reason), do: throw({:liquid_crash, reason})

  @doc false
  def effect_mode, do: Process.get(@handler, :native)

  @doc false
  def invalid_receive, do: raise(ArgumentError, "scripted receive did not select a valid message")

  @doc false
  def generate(input, output, template, payload, types) do
    value =
      if effect_mode() == :native do
        generate_native(input, output, template, payload, types)
      else
        perform(:generate, [input, output, template, payload])
      end

    if data?(value, [:"Core.TaskResult", output], types), do: value, else: crash(:bad_decode)
  end

  defp generate_native(input, output, template, payload, types) do
    unless data?(payload, input, types), do: crash(:bad_decode)
    in_schema = Vaisto.LLM.OpenAI.Schema.core_descriptor(input, types)
    out_schema = Vaisto.LLM.OpenAI.Schema.core_descriptor(output, types)
    data = task_data(payload, in_schema, :encode)

    text =
      Regex.replace(~r/\{([^}]+)\}/, template, fn _, name ->
        {_field, value} =
          Enum.find(data, fn {label, _} -> Atom.to_string(label) == name end) ||
            raise(ArgumentError, "missing template field")

        to_string(value)
      end)

    case Vaisto.LLM.call(text, data, out_schema, []) do
      {:ok, result} -> {:ok, task_data(result, out_schema, :decode)}
      {:error, reason} -> {:error, reason}
      _ -> crash(:bad_decode)
    end
  rescue
    _ in [ArgumentError, KeyError, Protocol.UndefinedError] -> crash(:bad_decode)
  end

  defp task_data(value, {:record, _name, fields}, :encode) do
    fields
    |> Enum.with_index(1)
    |> Map.new(fn {{label, type}, i} -> {label, task_data(elem(value, i), type, :encode)} end)
  end

  defp task_data(value, {:record, name, fields}, :decode) when is_map(value) do
    [
      name
      | Enum.map(fields, fn {label, type} ->
          v =
            case Map.fetch(value, label) do
              {:ok, v} -> v
              :error -> Map.fetch!(value, Atom.to_string(label))
            end

          task_data(v, type, :decode)
        end)
    ]
    |> List.to_tuple()
  end

  defp task_data([], {:list, _}, _direction), do: []

  defp task_data([h | t], {:list, type} = schema, direction),
    do: [task_data(h, type, direction) | task_data(t, schema, direction)]

  defp task_data(_, {:list, _}, _direction), do: crash(:bad_decode)
  defp task_data(value, _, _), do: value

  @doc false
  def perform(op, args) do
    case Process.get(@handler, :native) do
      :observe ->
        throw({:liquid_suspended, op, args})

      :native when op == :external ->
        [module, function, arguments] = args

        try do
          apply(module, function, arguments)
        catch
          class, reason -> crash({:raised, class, reason})
        end

      :native when op == :send ->
        [pid, message] = args
        send(pid, message)
        nil

      :native when op == :link ->
        [pid] = args
        Process.link(pid)
        nil

      {:script, [{^op, ^args, outcome} | rest]} ->
        Process.put(@handler, {:script, rest})

        case outcome do
          {:ok, value} -> value
          {:raised, class, reason} -> crash({:raised, class, reason})
          _ -> raise ArgumentError, "invalid native effect outcome"
        end

      _ ->
        raise ArgumentError, "native effect #{op} has no matching scripted handler"
    end
  end

  @doc false
  def spawn_process(protocol, state_type, tags, step, initial, types) do
    case Process.get(@handler, :native) do
      :native ->
        unless data?(initial, state_type, types), do: crash(:bad_decode)

        config = %{
          protocol: protocol,
          tags: tags,
          state_type: state_type,
          step: step,
          value: initial,
          types: types
        }

        case :gen_server.start(__MODULE__, config, []) do
          {:ok, pid} -> pid
          {:error, reason} -> crash({:raised, :error, reason})
        end

      _ ->
        perform(:spawn, [protocol, state_type, tags, step, initial])
    end
  end

  @impl true
  def init(config) do
    Process.put(@handler, :native)
    {:ok, config}
  end

  @doc false
  def start_supervisor(strategy, children, types) do
    case effect_mode() do
      :native ->
        specs =
          Enum.map(children, fn [id, protocol, state, tags, step, initial] ->
            unless data?(initial, state, types), do: crash(:bad_decode)

            config = %{
              protocol: protocol,
              state_type: state,
              tags: tags,
              step: step,
              value: initial,
              types: types
            }

            %{id: id, start: {__MODULE__, :start_link_child, [config]}}
          end)

        case Supervisor.start_link(specs, strategy: strategy) do
          {:ok, pid} -> {:ok, pid}
          {:error, reason} -> crash({:raised, :error, reason})
        end

      _ ->
        perform(:supervise, [strategy, children])
    end
  end

  @doc false
  def start_link_child(config), do: :gen_server.start_link(__MODULE__, config, [])

  @impl true
  def handle_call(message, _from, config) do
    case transition(message, config) do
      {:ok, config} -> {:reply, config.value, config}
      :stray -> {:noreply, config}
    end
  end

  @impl true
  def handle_cast(message, config) do
    case transition(message, config) do
      {:ok, config} -> {:noreply, config}
      :stray -> {:noreply, config}
    end
  end

  @impl true
  def handle_info({message, from} = packet, config) when is_pid(from) do
    # Keep the raw Core backend's request/reply transport usable too.
    case transition(message, config) do
      {:ok, config} ->
        send(from, config.value)
        {:noreply, config}

      :stray ->
        handle_message(packet, config)
    end
  end

  def handle_info(message, config), do: handle_message(message, config)

  defp handle_message(message, config) do
    case transition(message, config) do
      {:ok, config} -> {:noreply, config}
      :stray -> {:noreply, config}
    end
  end

  defp transition(message, config) do
    # The legacy foreign driver sends atom commands; Core sends typed sum
    # values. Conversion is allowed only for a declared nullary constructor.
    {typed, tag} = message_view(message, config.protocol)
    selected? = tag in config.tags

    if selected? and data?(typed, config.protocol, config.types) do
      value = config.step.(config.value, typed)
      unless data?(value, config.state_type, config.types), do: crash(:bad_decode)
      {:ok, %{config | value: value}}
    else
      :stray
    end
  end

  defp message_view(b, [:Bool]) when is_boolean(b), do: {b, b}
  defp message_view([], [:List, _]), do: {[], :Nil}
  defp message_view([_ | _] = list, [:List, _]), do: {list, :Cons}

  defp message_view(r, [:Reason]) when r in [:badarith, :badarg, :no_match, :bad_decode],
    do: {r, r}

  defp message_view(a, _) when is_atom(a), do: {{a}, a}
  defp message_view(t, _) when is_tuple(t) and tuple_size(t) > 0, do: {t, elem(t, 0)}
  defp message_view(other, _), do: {other, :no_constructor}

  @doc false
  def decode(value, type, types) do
    if data?(value, type, types),
      do: {:"Core.Ok", value},
      else: {:"Core.Err", {:"Core.BadDecode"}}
  end

  defp data?(v, :Int, _), do: is_integer(v)
  defp data?(v, :Float, _), do: is_float(v)
  defp data?(v, :String, _), do: is_binary(v)
  defp data?(v, :Atom, _), do: is_atom(v)
  defp data?(v, :Ref, _), do: is_reference(v)
  defp data?(v, :Unit, _), do: v === nil
  defp data?(_, :Dyn, _), do: true
  defp data?(v, [:Bool], _), do: is_boolean(v)
  defp data?(v, [:Pid, :Dyn], _), do: is_pid(v)
  defp data?(v, [:List, t], types), do: list?(v, t, types)

  defp data?(v, [:Tuple | ts], types),
    do: is_tuple(v) and tuple_size(v) == length(ts) and fields?(Tuple.to_list(v), ts, types)

  defp data?(v, [:Map, k, t], types),
    do:
      is_map(v) and
        Enum.all?(v, fn {key, value} -> data?(key, k, types) and data?(value, t, types) end)

  defp data?(v, [:Record, [:row | row]], types) do
    {fields, [:closed]} = Enum.split(row, -1)

    is_map(v) and map_size(v) == length(fields) and
      Enum.all?(fields, fn [l, t] -> Map.has_key?(v, l) and data?(Map.fetch!(v, l), t, types) end)
  end

  defp data?(v, [:Reason], _) when v in [:badarith, :badarg, :no_match, :bad_decode], do: true
  defp data?({:user, _}, [:Reason], _), do: true
  defp data?({:raised, c, _}, [:Reason], _), do: is_atom(c)

  defp data?(v, [name | args], types) when is_tuple(v) and tuple_size(v) > 0 do
    case Map.fetch(types, name) do
      {:ok, {params, :record, fields}} ->
        elem(v, 0) == name and tuple_size(v) == length(fields) + 1 and
          fields?(
            tl(Tuple.to_list(v)),
            Enum.map(fields, fn [_, t] -> subst(t, params, args) end),
            types
          )

      {:ok, {params, :sum, ctors}} ->
        case Enum.find(ctors, fn [c | _] -> c == elem(v, 0) end) do
          [_ | fields] ->
            tuple_size(v) == length(fields) + 1 and
              fields?(tl(Tuple.to_list(v)), Enum.map(fields, &subst(&1, params, args)), types)

          nil ->
            false
        end

      :error ->
        false
    end
  end

  defp data?(_, _, _), do: false
  defp list?([], _, _), do: true
  defp list?([h | t], type, types), do: data?(h, type, types) and list?(t, type, types)
  defp list?(_, _, _), do: false

  defp fields?(values, ts, types),
    do: Enum.zip(values, ts) |> Enum.all?(fn {v, t} -> data?(v, t, types) end)

  defp subst([:tvar, a], params, args), do: Enum.zip(params, args) |> Map.new() |> Map.fetch!(a)
  defp subst(xs, params, args) when is_list(xs), do: Enum.map(xs, &subst(&1, params, args))
  defp subst(t, _, _), do: t

  @doc false
  def select(v, label, types) when is_tuple(v) do
    if is_integer(label) do
      elem(v, label)
    else
      {_, :record, fields} = Map.fetch!(types, elem(v, 0))
      elem(v, 1 + Enum.find_index(fields, fn [l, _] -> l == label end))
    end
  end

  def select(v, label, _types) when is_map(v), do: Map.fetch!(v, label)

  @doc false
  def primitive(op, args) do
    primitive!(op, args)
  rescue
    ArithmeticError -> crash(:badarith)
    ArgumentError -> crash(:badarg)
  end

  defp primitive!(:add, [a, b]), do: a + b
  defp primitive!(:sub, [a, b]), do: a - b
  defp primitive!(:mul, [a, b]), do: a * b
  defp primitive!(:neg, [a]), do: -a
  defp primitive!(:div, [a, b]), do: div(a, b)
  defp primitive!(:rem, [a, b]), do: rem(a, b)
  defp primitive!(:lt, [a, b]), do: a < b
  defp primitive!(:le, [a, b]), do: a <= b
  defp primitive!(:gt, [a, b]), do: a > b
  defp primitive!(:ge, [a, b]), do: a >= b
  defp primitive!(:fadd, [a, b]), do: a + b
  defp primitive!(:fsub, [a, b]), do: a - b
  defp primitive!(:fmul, [a, b]), do: a * b
  defp primitive!(:fneg, [a]), do: -a
  defp primitive!(:fdiv, [a, b]), do: a / b
  defp primitive!(:flt, [a, b]), do: a < b
  defp primitive!(:fle, [a, b]), do: a <= b
  defp primitive!(:fgt, [a, b]), do: a > b
  defp primitive!(:fge, [a, b]), do: a >= b

  defp primitive!(:"int-to-float", [a]) do
    :erlang.float(a)
  rescue
    ArithmeticError -> crash(:badarg)
  end

  defp primitive!(:eq, [a, b]), do: a === b
  defp primitive!(:ne, [a, b]), do: a !== b
  defp primitive!(:not, [a]), do: not a
  defp primitive!(:concat, [a, b]), do: a <> b
  defp primitive!(:"float-round", [a]), do: round(a)
  defp primitive!(:"float-trunc", [a]), do: trunc(a)

  defp primitive!(:"map-get", [map, key]) do
    case Map.fetch(map, key) do
      {:ok, value} -> value
      :error -> crash(:badarg)
    end
  end

  defp primitive!(:"map-put", [map, key, value]), do: :maps.put(key, value, map)
  defp primitive!(:"map-delete", [map, key]), do: :maps.remove(key, map)
  defp primitive!(:"map-has?", [map, key]), do: :maps.is_key(key, map)
  defp primitive!(:"map-keys", [map]), do: :maps.keys(map)
  defp primitive!(:"map-values", [map]), do: :maps.values(map)
  defp primitive!(:"map-size", [map]), do: :maps.size(map)
  defp primitive!(:"map-merge", [a, b]), do: :maps.merge(a, b)
  defp primitive!(:"map-to-list", [map]), do: :maps.to_list(map)
  defp primitive!(:"map-from-list", [pairs]), do: :maps.from_list(pairs)

  defp primitive!(:str, values) do
    Enum.map_join(values, "", fn
      v when is_binary(v) -> v
      v when is_integer(v) -> Integer.to_string(v)
      v when is_float(v) -> Float.to_string(v)
      v when is_atom(v) -> Atom.to_string(v)
      v -> IO.iodata_to_binary(:io_lib.format(~c"~p", [v]))
    end)
  end

  defp primitive!(:length, [xs]), do: length(xs)
  defp primitive!(:empty?, [xs]), do: xs == []
  defp primitive!(:head, [xs]), do: hd(xs)
  defp primitive!(:tail, [xs]), do: tl(xs)
end
