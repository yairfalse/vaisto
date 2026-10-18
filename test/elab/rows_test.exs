defmodule Vaisto.Elab.RowsTest do
  use ExUnit.Case, async: true
  alias Vaisto.{Compilation, Elab, Interface, Parser}
  alias Vaisto.Liquid.{Eval, Lint}

  defp compile(source, opts \\ []) do
    name = :"NativeRows#{System.unique_integer([:positive])}"
    assert {:ok, core} = source |> Parser.parse() |> Elab.module(Keyword.put(opts, :name, name))
    assert :ok = Lint.check_module(core)
    assert {:ok, ^name, _} = Compilation.compile_liquid(source, name, opts)

    on_exit(fn ->
      :code.purge(name)
      :code.delete(name)
    end)

    {name, core}
  end

  test "explicit record rows instantiate per call and preserve remaining fields" do
    {name, core} =
      compile(~S"""
      (defn name [p (Record [name :string] :r)] :string (. p :name))
      (defn identity [p (Record [name :string] :r)] (Record [name :string] :r) p)
      (defn main [] (Tuple :string :string (Record [name :string age :int]))
        {(name #{:age 42 :name "Ada"}) (name #{:name "Grace" :active true})
         (identity #{:name "Ada" :age 42})})
      """)

    expected = {"Ada", "Grace", %{name: "Ada", age: 42}}
    assert apply(name, :main, []) == expected
    assert Eval.run(core, :main, []) == {:ok, expected}

    assert [:def, :name, [:forall, [[:r, :Row]], _], _] =
             Enum.find(core, &match?([:def, :name | _], &1))
  end

  test "rigid rows pass through generic calls, recursion and higher-order callbacks" do
    {name, core} =
      compile(~S"""
      (defn name [p (Record [name :string] :r)] :string (. p :name))
      (defn names [xs (List (Record [name :string] :r))] (List :string)
        (if (empty? xs) [] (cons (name (head xs)) (names (tail xs)))))
      (defn main [] (Tuple (List :string) (List :string))
        {(names [#{:name "Ada" :age 42} #{:age 85 :name "Grace"}])
         (map name [#{:name "one" :flag true} #{:flag false :name "two"}])})
      """)

    expected = {["Ada", "Grace"], ["one", "two"]}
    assert apply(name, :main, []) == expected
    assert Eval.run(core, :main, []) == {:ok, expected}
  end

  test "row-polymorphic canonical interfaces execute without layout guessing" do
    {provider, provider_core} =
      compile("(defn name [p (Record [name :string] :r)] :string (. p :name))")

    assert {:ok, tree} = Interface.from_core(provider_core)

    source =
      "(import #{provider} :as Lib) " <>
        ~S|(defn main [] :string (Lib/name #{:age 42 :name "Ada"}))|

    {name, core} = compile(source, interfaces: %{provider => tree})
    assert apply(name, :main, []) == "Ada"
    assert Eval.run(core, :main, [], imports: %{provider => provider_core}) == {:ok, "Ada"}
  end

  test "rows cannot hide absent fields, different field types or width coercions" do
    for source <- [
          ~S"(defn name [p (Record [name :string] :r)] :string (. p :name)) (name #{:age 42})",
          ~S"(defn name [p (Record [name :string] :r)] :string (. p :name)) (name #{:name 42})",
          "(defn f [p (Record [name :string] :r)] :int (. p :age))",
          "(defn f [p (Record [name :string] :r)] (Record [name :string]) p)",
          ~S|(defn f [p (Record [name :string] :r)] (Record [name :string] :r) #{:name "fixed"})|,
          "(defn f [p (Record [name :string] :r) x :r] :string (. p :name))",
          "(defn f [p (Record [name :string] :r)] :string (let [x (the (Record [name :string] :unbound) p)] (. x :name)))",
          "(extern foreign:row [] (Record [name :string] :r))"
        ] do
      assert {:error, [_ | _]} = source |> Parser.parse() |> Elab.module()
    end
  end

  test "local bindings remain monomorphic in their row arguments" do
    source = ~S"""
    (defn name [p (Record [name :string] :r)] :string (. p :name))
    (let [f name]
      {(f #{:name "Ada" :age 42}) (f #{:name "Grace" :active true})})
    """

    assert {:error, [_ | _]} = source |> Parser.parse() |> Elab.module()
  end

  test "local row inference combines independent field requirements without losing either" do
    {name, core} =
      compile(~S"""
      (defn with-x [p (Record [x :int] :r)] (Record [x :int] :r) p)
      (defn with-y [p (Record [y :int] :s)] (Record [y :int] :s) p)
      (defn main [] (Tuple (Record [x :int y :int]) (Record [x :int y :int]))
        (let [both (fn [p] {(with-x p) (with-y p)})]
          (both #{:x 1 :y 2})))
      """)

    expected = {%{x: 1, y: 2}, %{x: 1, y: 2}}
    assert apply(name, :main, []) == expected
    assert Eval.run(core, :main, []) == {:ok, expected}
  end
end
