defmodule Vaisto.Elab.EffectsTest do
  use ExUnit.Case, async: true
  alias Vaisto.{Compilation, Elab, Interface, Parser}
  alias Vaisto.Liquid.{Eval, Lint, Runtime}

  defp compile(source, opts \\ []) do
    name = :"NativeEffects#{System.unique_integer([:positive])}"
    assert {:ok, core} = source |> Parser.parse() |> Elab.module(Keyword.put(opts, :name, name))
    assert :ok = Lint.check_module(core)
    assert {:ok, ^name, _} = Compilation.compile_liquid(source, name, opts)

    on_exit(fn ->
      :code.purge(name)
      :code.delete(name)
    end)

    {name, core}
  end

  test "explicit effect variables instantiate for pure and effectful callbacks" do
    {name, core} =
      compile("""
      (extern erlang:abs [:int] :int)
      (defn positive [x :int] :int (erlang:abs x))
      (defn apply [f (Fn :a :b :effects :e) x :a] :b :effects :e (f x))
      (defn main [] (Tuple :int :int :int)
        {(apply (fn [x] (+ x 1)) 41)
         (apply positive -42)
         (apply (fn [x] (erlang:abs x)) -7)})
      """)

    script =
      {:script,
       [
         {:external, [:erlang, :abs, [-42]], {:ok, 42}},
         {:external, [:erlang, :abs, [-7]], {:ok, 7}}
       ]}

    assert Runtime.run(name, :main, [], handler: script) == {:ok, {42, 42, 7}}
    assert Eval.run(core, :main, [], handler: script) == {:ok, {42, 42, 7}}

    assert [:def, :apply, [:forall, binders, _], _] =
             Enum.find(core, &match?([:def, :apply | _], &1))

    assert [:e, :Eff] in binders
  end

  test "recursive higher-order functions retain the same rigid row" do
    {name, core} =
      compile("""
      (extern erlang:abs [:int] :int)
      (defn map-effect [f (Fn :a :b :effects :e) xs (List :a)] (List :b) :effects :e
        (if (empty? xs) [] (cons (f (head xs)) (map-effect f (tail xs)))))
      (defn main [] (List :int) (map-effect (fn [x] (erlang:abs x)) [-1 -2 -3]))
      """)

    events = for n <- [-1, -2, -3], do: {:external, [:erlang, :abs, [n]], {:ok, -n}}
    assert Runtime.run(name, :main, [], handler: {:script, events}) == {:ok, [1, 2, 3]}
    assert Eval.run(core, :main, [], handler: {:script, events}) == {:ok, [1, 2, 3]}
  end

  test "explicit contracts reject hidden effects and mismatched open rows" do
    for source <- [
          "(extern erlang:abs [:int] :int) (defn f [x :int] :int :effects [] (erlang:abs x))",
          "(defn f [g (Fn :int :int :effects :e)] :int :effects [] (g 1))",
          "(defn f [g (Fn :int :int :effects :e)] :int :effects :other (g 1))",
          "(defn f [x :e g (Fn :int :int :effects :e)] :e :effects :e x)",
          "(defn f [] :int :effects [external external] 1)"
        ] do
      assert {:error, [_ | _]} = source |> Parser.parse() |> Elab.module()
    end
  end

  test "Core higher-order builtins propagate callback effects and dictionaries" do
    {name, core} =
      compile("""
      (extern erlang:abs [:int] :int)
      (defn render [x :a] :string :effects [external] where [(Show a)]
        (do (erlang:abs -1) (show x)))
      (defn main [] (Tuple (List :int) (List :int) :int (List :int) (List :string))
        {(map (fn [x] (erlang:abs x)) [-1 -2])
         (filter (fn [x] (> (erlang:abs x) 1)) [-1 -2])
         (fold (fn [a x] (+ a (erlang:abs x))) 0 [-1 -2])
         (flat_map (fn [x] [(erlang:abs x)]) [-1 -2])
         (map render [1 2])})
      """)

    events =
      for(_ <- 1..4, n <- [-1, -2], do: {:external, [:erlang, :abs, [n]], {:ok, -n}}) ++
        List.duplicate({:external, [:erlang, :abs, [-1]], {:ok, 1}}, 2)

    expected = {[1, 2], [-2], 3, [1, 2], ["1", "2"]}
    assert Runtime.run(name, :main, [], handler: {:script, events}) == {:ok, expected}
    assert Eval.run(core, :main, [], handler: {:script, events}) == {:ok, expected}
  end

  test "effect-polymorphic interfaces remain independently checked across modules" do
    {provider, core} =
      compile("""
      (defn apply [f (Fn :a :b :effects :e) x :a] :b :effects :e (f x))
      """)

    assert {:ok, tree} = Interface.from_core(core)

    {name, consumer} =
      compile(
        """
        (import #{provider} :as Lib)
        (extern erlang:abs [:int] :int)
        (defn main [] :int (Lib/apply (fn [x] (erlang:abs x)) -42))
        """,
        interfaces: %{provider => tree}
      )

    script = {:script, [{:external, [:erlang, :abs, [-42]], {:ok, 42}}]}
    assert Runtime.run(name, :main, [], handler: script) == {:ok, 42}

    assert Eval.run(consumer, :main, [], handler: script, imports: %{provider => core}) ==
             {:ok, 42}
  end

  test "an explicit effect prefix combines storage with the callback's remaining effects" do
    {name, core} =
      compile("""
      (extern foreign:tick [] :unit)
      (defn call-with-storage [f (Fn :int :effects (Effects [external] :fx))] :int :effects (Effects [external] :fx)
        (do (foreign:tick) (f)))
      (defn sender [] :int :effects [send] 7)
      (defn main [] (Tuple :int :int)
        {(call-with-storage (fn [] 42)) (call-with-storage (fn [] (sender)))})
      """)

    events = List.duplicate({:external, [:foreign, :tick, []], {:ok, nil}}, 2)
    assert Runtime.run(name, :main, [], handler: {:script, events}) == {:ok, {42, 7}}
    assert Eval.run(core, :main, [], handler: {:script, events}) == {:ok, {42, 7}}
    assert {:ok, _} = Interface.from_core(core)

    assert {:error, [_ | _]} =
             "(defn bad [f (Fn :int :effects (Effects :e :fx))] :int 1)"
             |> Parser.parse()
             |> Elab.module()
  end
end
