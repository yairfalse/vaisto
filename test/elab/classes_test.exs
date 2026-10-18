defmodule Vaisto.Elab.ClassesTest do
  use ExUnit.Case, async: true
  alias Vaisto.{Compilation, Elab, Parser}
  alias Vaisto.Liquid.{Eval, Lint}

  defp run(source, expected) do
    name = :"NativeClasses#{System.unique_integer([:positive])}"
    assert {:ok, core} = source |> Parser.parse() |> Elab.module(name: name)
    assert :ok = Lint.check_module(core)
    assert {:ok, ^name, _} = Compilation.compile_liquid(source, name, format_errors: false)

    try do
      assert apply(name, :main, []) == expected
      assert Eval.run(core, :main, []) == {:ok, expected}
    after
      :code.purge(name)
      :code.delete(name)
    end
  end

  test "builtin Eq, default neq, Show and higher-order methods lower to ordinary Core" do
    run("(defn main [] :string (str (eq 1 1) (neq 1 2) (show 3.5)))", "truetrue3.5")
    run("(defn main [] (List :string) (map show [1 2 3]))", ["1", "2", "3"])
  end

  test "user methods and defaults are checked and execute on native BEAM" do
    run(
      """
      (defclass Printable [a]
        (text [x :a] :string)
        (wrapped [x :a] :string (str "[" (text x) "]")))
      (instance Printable :int (text [n] (str "int:" n)))
      (defn main [] :string (wrapped 42))
      """,
      "[int:42]"
    )
  end

  test "an instance is chosen from all argument types, not just the first" do
    run(
      """
      (defclass Pick [a] (pick [ignored :int value :a] :a))
      (instance Pick :string (pick [ignored value] value))
      (defn main [] :string (pick 1 "right"))
      """,
      "right"
    )
  end

  test "nested constrained instances receive explicit dictionaries" do
    run(
      """
      (deftype Maybe (Just v) (Nothing))
      (instance Show (Maybe a) where [(Show a)]
        (show [x] (match x
          [(Just v) (str "Just(" (show v) ")")]
          [(Nothing) "Nothing"])))
      (defn main [] :string (show (Just (Just 42))))
      """,
      "Just(Just(42))"
    )
  end

  test "constrained equality and its default retain their own dictionaries" do
    run(
      """
      (deftype Maybe (Just v) (Nothing))
      (instance Eq (Maybe a) where [(Eq a)]
        (eq [x y] (match (tuple x y)
          [{(Just a) (Just b)} (eq a b)]
          [{(Nothing) (Nothing)} true]
          [_ false])))
      (defn main [] :bool (neq (Just (Just 1)) (Just (Just 2))))
      """,
      true
    )
  end

  test "derived equality supports generic data and enum Show has exact names" do
    run(
      """
      (deftype Maybe (Just v) (Nothing) deriving [Eq])
      (defn main [] :bool (eq (Just (Just 1)) (Just (Just 1))))
      """,
      true
    )

    run(
      """
      (deftype Signal (Go) (Stop) deriving [Eq Show])
      (defn main [] :string (str (show (Go)) (neq (Go) (Stop))))
      """,
      "Gotrue"
    )
  end

  test "derived ground records and recursive sums use checked structural equality" do
    run(
      """
      (deftype Point [x :int y :int] deriving [Eq])
      (deftype Tree (Leaf :int) (Branch :Tree :Tree) deriving [Eq])
      (defn main [] :bool (if (eq (Point 1 2) (Point 1 2))
        (eq (Branch (Leaf 1) (Leaf 2)) (Branch (Leaf 1) (Leaf 2))) false))
      """,
      true
    )
  end

  test "class contracts reject wrong bodies, unknown and duplicate methods and missing instances" do
    for source <- [
          "(defclass C [a] (m [x :a] :int)) (instance C :string (m [x] false))",
          "(defclass C [a] (m [x :a] :int)) (instance C :string)",
          "(defclass C [a] (m [x :a] :int)) (instance C :string (extra [x] 1))",
          "(defclass C [a] (m [x :a] :int)) (instance C :string (m [x] 1) (m [x] 2))",
          "(defclass C [a] (m [x :a] :int)) (instance C :string (m [x y] 1))",
          "(instance Missing :int (m [x] x))",
          "(defn main [] :bool (eq [1] [1]))",
          "(defn f [x :a] :string (show x))",
          "(deftype Box [x (Fn :int :int)] deriving [Eq])",
          "(deftype Box (Boxed v)) (instance Show (Box a) where [(Missing a)] (show [x] \"x\"))",
          "(deftype Box (Boxed v)) (instance Show (Box a) where [(Show b)] (show [x] \"x\"))"
        ] do
      assert {:error, [_ | _]} = source |> Parser.parse() |> Elab.module()
    end
  end

  test "overlapping source instances are errors, but explicit local builtin instances are respected" do
    assert {:error, [_ | _]} =
             """
             (instance Show :int (show [x] "one"))
             (instance Show :int (show [x] "two"))
             """
             |> Parser.parse()
             |> Elab.module()

    run("(instance Show :int (show [x] \"custom\")) (defn main [] :string (show 1))", "custom")
  end

  test "unused builtin dictionaries are not exported" do
    assert {:ok, [:module, _, _ | [[:def, :main, _, _]]]} =
             "(defn main [] :int 1)" |> Parser.parse() |> Elab.module()
  end

  test "cyclic and expanding dictionary requirements fail rather than looping" do
    for constraint <- ["(Show (Box a))", "(Show (Box (Box a)))"] do
      source = """
      (deftype Box (Box v))
      (instance Show (Box a) where [#{constraint}]
        (show [x] "unreachable"))
      (defn main [] :string (show (Box 1)))
      """

      assert {:error, [error | _]} = source |> Parser.parse() |> Elab.module()
      assert error.message =~ "recursive instance resolution"
    end
  end

  test "defaults are checked generically even when no instance uses them" do
    assert {:error, [_ | _]} =
             "(defclass C [a] (m [x :a] :int false))" |> Parser.parse() |> Elab.module()

    run(
      """
      (defclass C [a]
        (text [x :a] :string)
        (wrapped [dictionary :a] :string (text dictionary)))
      (instance C :int (text [x] "ok"))
      (defn main [] :string (wrapped 1))
      """,
      "ok"
    )
  end
end
