defmodule Vaisto.Elab.MapTest do
  use ExUnit.Case, async: true
  alias Vaisto.{Compilation, Elab, Parser}
  alias Vaisto.Liquid.{Eval, Lint, Runtime}

  defp compile(source) do
    name = :"NativeMap#{System.unique_integer([:positive])}"
    assert {:ok, core} = source |> Parser.parse() |> Elab.module(name: name)
    assert :ok = Lint.check_module(core)
    assert {:ok, ^name, _} = Compilation.compile_liquid(source, name, format_errors: false)

    on_exit(fn ->
      :code.purge(name)
      :code.delete(name)
    end)

    {name, core}
  end

  test "homogeneous maps and generic operations preserve exact key and value types" do
    {name, core} =
      compile(~S"""
      (defn insert [m (Map :k :v) key :k value :v] (Map :k :v) (map-put m key value))
      (defn main [] (Tuple :int :bool (List :atom) (List :int) (Map :atom :int) (List (Tuple :atom :int)))
        (let [m (insert (the (Map :atom :int) #{}) :x 1)
              m2 (map-merge m (the (Map :atom :int) #{:x 2 :y 3}))
              m3 (map-delete m2 :y)]
          {(map-get m2 :x) (map-has? m2 :y) (map-keys m3) (map-values m3)
           (map-from-list (map-to-list m2)) (map-to-list m3)}))
      """)

    expected = {2, true, [:x], [2], %{x: 2, y: 3}, [x: 2]}
    assert apply(name, :main, []) == expected
    assert Eval.run(core, :main, []) == {:ok, expected}
  end

  test "map literals evaluate every key then value in source order and last key wins" do
    {name, core} =
      compile(~S"""
      (extern foreign:tick [:int] :int)
      (defn main [] (Map :int :int)
        #{(foreign:tick 1) (foreign:tick 2)
          (foreign:tick 3) (foreign:tick 4)})
      """)

    events =
      for n <- 1..4,
          do: {:external, [:foreign, :tick, [n]], {:ok, if(rem(n, 2) == 1, do: 1, else: n)}}

    script = {:script, events}
    assert Runtime.run(name, :main, [], handler: script) == {:ok, %{1 => 4}}
    assert Eval.run(core, :main, [], handler: script) == {:ok, %{1 => 4}}
  end

  test "anonymous records preserve heterogeneous field types and map representation" do
    {name, core} =
      compile(~S"""
      (defn read [p (Record [name :string age :int])] :string (. p :name))
      (defn main [] (Tuple :string :int (Record []))
        (let [p #{:age 42 :name "Ada"}]
          {(read p) (. p :age) (the (Record []) #{})}))
      """)

    assert apply(name, :main, []) == {"Ada", 42, %{}}
    assert Eval.run(core, :main, []) == {:ok, {"Ada", 42, %{}}}
  end

  test "empty map construction is determined by its explicit context" do
    {name, core} =
      compile("""
      (defn main [] (Map :string :int) (map-put (map-new) "answer" 42))
      """)

    assert apply(name, :main, []) == %{"answer" => 42}
    assert Eval.run(core, :main, []) == {:ok, %{"answer" => 42}}

    assert {:error, [_ | _]} =
             "(defn main [] :int (map-size (map-new)))" |> Parser.parse() |> Elab.module()
  end

  test "missing map lookup normalizes to a catchable badarg" do
    {name, core} = compile(~S"(defn main [] :int (map-get (the (Map :atom :int) #{}) :absent))")
    assert Runtime.run(name, :main, []) == {:crash, :badarg}
    assert Eval.run(core, :main, []) == {:crash, :badarg}
  end

  test "map arguments and anonymous record shapes cannot erase type errors" do
    for source <- [
          ~S"(defn main [] (Map :atom :int) #{:x true})",
          ~S"(defn main [] :int (map-get (the (Map :atom :int) #{}) 1))",
          ~S"(defn main [] (Map :atom :int) (map-put (the (Map :atom :int) #{}) :x true))",
          ~S"(defn main [] (Map :atom :int) (map-merge (the (Map :atom :int) #{}) (the (Map :int :int) #{})))",
          "(defn main [] (Map :atom :int) (map-from-list [1 2]))",
          ~S"(defn main [] :int (. #{:x 1} :missing))",
          ~S"(defn main [] (Record [x :int]) #{:x 1 :extra 2})",
          ~S"(defn main [] (Record [x :int x :int]) #{:x 1})"
        ] do
      assert {:error, [_ | _]} = source |> Parser.parse() |> Elab.module()
    end
  end

  test "independent Lint checks every map entry, operation operand and type" do
    for expression <- [
          [:map, :Int, :String, [1, 2]],
          [:map, :Int, :String, ["wrong", "value"]],
          [:map, :Unknown, :String],
          [:map, :Int, :String, [:malformed]],
          [:prim, :"map-get", [:map, :Int, :String], "wrong"],
          [:prim, :"map-put", [:map, :Int, :String], 1, 2],
          [:prim, :"map-size", 1],
          [:prim, :"map-from-list", [:tuple, 1, 2]]
        ] do
      assert {:error, [_ | _]} = Lint.synth(expression)
    end
  end
end
