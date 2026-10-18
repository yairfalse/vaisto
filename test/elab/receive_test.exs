defmodule Vaisto.Elab.ReceiveTest do
  use ExUnit.Case, async: true

  alias Vaisto.{Compilation, Elab, Parser}
  alias Vaisto.Liquid.{Canonical, Eval, Lint, Lower, Runtime}

  defp compile(source) do
    name = :"NativeReceive#{System.unique_integer([:positive])}"
    assert {:ok, core} = source |> Parser.parse() |> Elab.module(name: name)
    assert :ok = Lint.check_module(core)
    assert {:ok, ^name, _} = Compilation.compile_liquid(source, name, format_errors: false)

    on_exit(fn ->
      :code.purge(name)
      :code.delete(name)
    end)

    {name, core}
  end

  test "literal selective receive leaves unrelated messages queued" do
    {name, _} = compile("(defn next [] :int (receive [:ping 42] [:pong 99]))")
    send(self(), :unrelated)
    send(self(), :pong)
    assert apply(name, :next, []) == 99
    assert_receive :unrelated
  end

  test "typed variables use guards and plain variables remain Dyn" do
    {name, _} = compile("(defn next [] :int (receive [(the :int n) (+ n 1)]))")
    send(self(), :unrelated)
    send(self(), 41)
    assert apply(name, :next, []) == 42
    assert_receive :unrelated

    {name, _} = compile("(defn next [] :dyn (receive [x x]))")
    send(self(), {:unknown, [1, 2]})
    assert apply(name, :next, []) == {:unknown, [1, 2]}

    assert {:error, _} =
             "(defn next [] :int (receive [x (+ x 1)]))" |> Parser.parse() |> Elab.module()
  end

  test "deep malformed data is dropped before exposing bindings" do
    {name, _} = compile("(defn next [] (List :int) (receive [(the (List :int) xs) xs]))")
    send(self(), [1, :bad])
    send(self(), [2, 3])
    assert apply(name, :next, []) == [2, 3]
    refute_receive [1, :bad], 0
  end

  test "named constructors and recursive data decode before use" do
    source = """
    (deftype Tree (Leaf :int) (Branch :Tree :Tree))
    (defn next [] :int (receive [(Leaf n) n]))
    """

    {name, _} = compile(source)
    send(self(), {:Leaf, :bad})
    send(self(), {:Leaf, 7})
    assert apply(name, :next, []) == 7
    # The scalar guard can reject this without consuming it.
    assert_receive {:Leaf, :bad}
  end

  test "scripted native receive agrees with Core and rejects invalid answers" do
    {name, core} = compile("(defn next [] :int (receive [(the :int n) (+ n 1)]))")
    [:module, _, _ | items] = Canonical.strip(core)

    [:def, :next, _, [:fn, [], [:receive | clauses]]] =
      Enum.find(items, &match?([:def, :next | _], &1))

    selectors = Enum.map(clauses, fn [type, p, _] -> [type, p] end)
    handler = {:script, [{:receive, [selectors], {:ok, 41}}]}
    assert Runtime.run(name, :next, [], handler: handler) == {:ok, 42}
    assert Eval.run(core, :next, [], handler: handler) == {:ok, 42}
    bad = {:script, [{:receive, [selectors], {:ok, :bad}}]}
    assert_raise ArgumentError, fn -> Runtime.run(name, :next, [], handler: bad) end
    assert_raise Eval.Stuck, fn -> Eval.run(core, :next, [], handler: bad) end
    assert_raise ArgumentError, fn -> Runtime.run(name, :next, []) end
  end

  test "branch results agree and branch bindings do not leak" do
    for source <- [
          "(defn next [] :int (receive [:ping 1] [:pong false]))",
          "(defn next [] :int (do (receive [(the :int n) n]) n))",
          "(defn next [] :int (receive [(the :int n) n] [:pong n]))",
          "(defn next [] :int (do (let [n 1] n) n))",
          "(defn next [] :int (do (fn [(n :int)] n) n))",
          "(defn next [] :int (do (match 1 [n n]) n))",
          "(defn next [] :int (do (try 1 [catch [:error e 2]]) e))"
        ] do
      assert {:error, _} = source |> Parser.parse() |> Elab.module()
    end
  end

  test "a selected malformed message cannot fall through to a later catchall" do
    {name, core} = compile("(defn next [] :int (receive [(the (List :int) xs) 1] [other 2]))")
    send(self(), [1, :bad])
    send(self(), :next)
    assert apply(name, :next, []) == 2
    refute_receive [1, :bad], 0
    [:module, _, _ | items] = Canonical.strip(core)

    [:def, :next, _, [:fn, [], [:receive | clauses]]] =
      Enum.find(items, &match?([:def, :next | _], &1))

    selectors = Enum.map(clauses, fn [type, p, _] -> [type, p] end)
    bad = {:script, [{:receive, [selectors], {:ok, [1, :bad]}}]}
    assert_raise ArgumentError, fn -> Runtime.run(name, :next, [], handler: bad) end
    assert_raise Eval.Stuck, fn -> Eval.run(core, :next, [], handler: bad) end
  end

  test "tuple guards retain wrong scalar shapes and recursive decode rejects deep corruption" do
    {name, _} =
      compile("(defn next [] (Tuple :atom :int) (receive [(the (Tuple :atom :int) pair) pair]))")

    send(self(), {:ok, :bad})
    send(self(), {:ok, 42})
    assert apply(name, :next, []) == {:ok, 42}
    assert_receive {:ok, :bad}

    {name, core} =
      compile("""
      (deftype Tree (Leaf :int) (Branch :Tree :Tree))
      (defn next [] :Tree (receive [(the :Tree tree) tree]))
      """)

    send(self(), {:Branch, {:Leaf, :bad}, {:Leaf, 2}})
    value = {:Branch, {:Leaf, 1}, {:Leaf, 2}}
    send(self(), value)
    assert apply(name, :next, []) == value
    refute_receive {:Branch, {:Leaf, :bad}, {:Leaf, 2}}, 0
    [:module, _, _ | items] = Canonical.strip(core)

    [:def, :next, _, [:fn, [], [:receive | clauses]]] =
      Enum.find(items, &match?([:def, :next | _], &1))

    selectors = Enum.map(clauses, fn [type, p, _] -> [type, p] end)
    handler = {:script, [{:receive, [selectors], {:ok, value}}]}
    assert Eval.run(core, :next, [], handler: handler) == {:ok, value}
  end

  test "nullary constructors infer their protocol rather than Dyn" do
    {name, _} =
      compile("(deftype Signal (Go) (Stop)) (defn next [] :int (receive [Go 1] [Stop 0]))")

    send(self(), {:Go})
    assert apply(name, :next, []) == 1
  end

  test "independent Core lint requires receive effects and decodable targets" do
    assert {:error, _} =
             Lint.check_module([
               :module,
               :Bad,
               [:"core-version", 0],
               [
                 :def,
                 :next,
                 [:->, [], [:eff, :closed], :Int],
                 [:fn, [], [:receive, [:Int, :n, :n]]]
               ]
             ])

    assert {:error, _} =
             Lint.check_module([
               :module,
               :Bad,
               [:"core-version", 0],
               [
                 :def,
                 :next,
                 [:->, [], [:eff, :receive, :closed], :Dyn],
                 [
                   :fn,
                   [],
                   [
                     :receive,
                     [
                       [:->, [], [:eff, :closed], :Int],
                       :f,
                       [:up, [:->, [], [:eff, :closed], :Int], :f]
                     ]
                   ]
                 ]
               ]
             ])
  end

  test "native selectors and the independent evaluator cover decodable type shapes" do
    for {type, value, unrelated} <- [
          {:Int, 42, :wrong},
          {:Float, 1.5, 1},
          {:String, "ok", :wrong},
          {:Atom, :ok, 1},
          {:Unit, nil, 1},
          {:Ref, make_ref(), :wrong},
          {[:Bool], true, :wrong},
          {[:Pid, :Dyn], self(), :wrong},
          {[:Tuple, :Int, :String], {1, "ok"}, {1, :wrong}},
          {[:Map, :Atom, :Int], %{ok: 1}, :wrong},
          {[:Record, [:row, [:n, :Int], :closed]], %{n: 1}, %{n: :wrong}},
          {[:Reason], {:raised, :error, :anything}, :wrong},
          {[:"Core.Result", :String, :Int], {:"Core.Ok", 1}, {:"Core.Ok", :wrong}}
        ] do
      name = :"ReceiveShape#{System.unique_integer([:positive])}"

      core = [
        :module,
        name,
        [:"core-version", 0],
        [
          :def,
          :next,
          [:->, [], [:eff, :receive, :closed], type],
          [:fn, [], [:receive, [type, :value, :value]]]
        ]
      ]

      assert {:ok, ^name, _} = Lower.compile(core, name)

      try do
        send(self(), unrelated)
        send(self(), value)
        assert apply(name, :next, []) === value
        assert_receive ^unrelated
        handler = {:script, [{:receive, [[[type, :value]]], {:ok, value}}]}
        assert Runtime.run(name, :next, [], handler: handler) == {:ok, value}
        assert Eval.run(core, :next, [], handler: handler) == {:ok, value}
      after
        :code.purge(name)
        :code.delete(name)
      end
    end
  end
end
