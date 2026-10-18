defmodule Vaisto.Elab.ConstraintsTest do
  use ExUnit.Case, async: true
  alias Vaisto.{Compilation, Elab, Parser}
  alias Vaisto.Liquid.{Eval, Lint, Runtime}

  defp compile(source) do
    name = :"NativeConstraints#{System.unique_integer([:positive])}"
    assert {:ok, core} = source |> Parser.parse() |> Elab.module(name: name)
    assert :ok = Lint.check_module(core)
    assert {:ok, ^name, _} = Compilation.compile_liquid(source, name, format_errors: false)

    on_exit(fn ->
      :code.purge(name)
      :code.delete(name)
    end)

    {name, core}
  end

  defp run(source, value) do
    {name, core} = compile(source)
    assert apply(name, :main, []) == value
    assert Eval.run(core, :main, []) == {:ok, value}
  end

  test "explicit Num constraints preserve one implementation for Int and Float" do
    run(
      """
      (defn double [x :a] :a where [(Num a)] (* x 2))
      (defn increment [x :a] :a where [(Num a)] (+ 1 x))
      (tuple (double 21) (double 1.5) (increment 2) (increment 2.5))
      """,
      {42, 3.0, 3, 3.5}
    )
  end

  test "Num and Ord constraints cooperate without mixing concrete numeric types" do
    run(
      """
      (defn absolute [x :a] :a where [(Num a) (Ord a)]
        (if (< x 0) (- x) x))
      (tuple (absolute -42) (absolute -1.5))
      """,
      {42, 1.5}
    )

    assert {:error, [_ | _]} =
             "(defn add [x :a y :a] :a where [(Num a)] (+ x y)) (add 1 2.0)"
             |> Parser.parse()
             |> Elab.module()
  end

  test "recursive constrained functions pass the same dictionary through recursion" do
    run(
      """
      (defn sum [xs (List :a)] :a where [(Num a)]
        (if (empty? xs) 0 (+ (head xs) (sum (tail xs)))))
      (tuple (sum [1 2 3]) (sum [1.0 2.0 3.0]))
      """,
      {6, 6.0}
    )
  end

  test "Show and Eq constraints remain explicit and instantiate at each top-level use" do
    run(
      """
      (defn render [x :a] :string where [(Show a)] (show x))
      (defn same [x :a y :a] :bool where [(Eq a)] (== x y))
      (str (render 42) (render true) (same 1 1) (same false true))
      """,
      "42truetruefalse"
    )

    assert {:error, [_ | _]} =
             "(defn render [x :a] :string where [(Show a)] (show x)) (let [f render] (tuple (f 1) (f true)))"
             |> Parser.parse()
             |> Elab.module()
  end

  test "user classes and derived instances are ordinary constrained function arguments" do
    run(
      """
      (deftype Value (Value :int) deriving [Eq])
      (defn same [x :a y :a] :bool where [(Eq a)] (== x y))
      (same (Value 1) (Value 1))
      """,
      true
    )

    run(
      """
      (defclass C [a] (combine [x :a y :a] :a))
      (instance C :int (combine [x y] (+ x y)))
      (defn twice [x :a] :a where [(C a)] (combine x x))
      (twice 21)
      """,
      42
    )
  end

  test "effects belong to the returned user function, not dictionary construction" do
    source = """
    (extern foreign:tick [] :int)
    (defn render [x :a] :string where [(Show a)]
      (do (foreign:tick) (show x)))
    (render 42)
    """

    {name, core} = compile(source)
    handler = {:script, [{:external, [:foreign, :tick, []], {:ok, 1}}]}
    assert Runtime.run(name, :main, [], handler: handler) == {:ok, "42"}
    assert Eval.run(core, :main, [], handler: handler) == {:ok, "42"}

    invalid =
      String.replace(
        source,
        "(render 42)",
        "(defn pure-map [f (Fn :int :string) xs (List :int)] (List :string) (map f xs)) (pure-map render [1 2])"
      )

    assert {:error, [_ | _]} = invalid |> Parser.parse() |> Elab.module()
  end

  test "compiler-generated dictionary names are not source bindings" do
    run(
      "(defn render [dictionary :a] :string where [(Show a)] (show dictionary)) (render 42)",
      "42"
    )

    assert {:error, [_ | _]} =
             "(defn render [x :a] :string where [(Show a)] (str dictionary))"
             |> Parser.parse()
             |> Elab.module()

    assert {:error, [_ | _]} =
             "(deftype Box (Box v)) (instance Show (Box a) where [(Show a)] (show [x] (str dictionary)))"
             |> Parser.parse()
             |> Elab.module()
  end

  test "unknown, duplicate, out-of-scope and malformed constraints are rejected" do
    for source <- [
          "(defn f [x :a] :a where [(Missing a)] x)",
          "(defn f [x :a] :a where [(Eq a) (Eq a)] x)",
          "(defn f [x :a] :a where [(Eq b)] x)",
          "(defn f [x] :string where [(Show a)] (show x))",
          "(defn f [x :a] :a where [(Eq a b)] x)",
          "(defn f [x :a] :a where [])",
          "(defn f [x :a] :a where x)",
          "(defn f [x :a] :a (* x 2))"
        ],
        do: assert({:error, [_ | _]} = source |> Parser.parse() |> Elab.module())
  end
end
