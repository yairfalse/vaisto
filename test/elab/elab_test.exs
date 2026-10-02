defmodule Vaisto.ElabTest do
  # The Phase 1a elaborator, from its requirements: liquid-types.md §8 (the
  # rule table, read bidirectionally), §9 (local unification, signatures, no
  # let generalization, lambdas typed from the expected arrow, no :any), Q35,
  # Q36, and the HM defects it is meant to remove (RFC §1.12).
  use ExUnit.Case, async: true

  alias Vaisto.{Elab, Parser}
  alias Vaisto.Liquid.{Eval, Lint}

  # Elaborate, and hold every accepted program to Core Lint (§8.1: one table,
  # two implementations; Lint rejecting the output is an elaborator bug).
  defp elab(source, opts \\ []) do
    case Elab.module(Parser.parse(source), opts) do
      {:ok, core} = ok ->
        assert :ok = Lint.check_module(core), "Core Lint rejects the elaborator's output for:\n#{source}"
        ok

      other ->
        other
    end
  end

  defp run(source) do
    assert {:ok, core} = elab(source)
    Eval.run(core, :main, [])
  end

  defp error(source) do
    assert {:error, [error | _]} = elab(source)
    error
  end

  describe "introductions are checked against the expected type (§8.1)" do
    test "a lambda takes its parameter types from the arrow it is checked against (§9)" do
      assert {:ok, [2, 4, 6]} = run("(defn main [] (List :int) (map (fn [x] (* x 2)) [1 2 3]))")
    end

    test "an empty list takes its element type from the context" do
      assert {:ok, core} = elab("(defn f [] (List :int) [])")
      assert [:def, :f, _, [:fn, [], [:inj, [:List, :Int], :Nil]]] = List.last(core)
    end

    test "a constructor's type arguments come from the expected type" do
      assert {:ok, 5} = run("(deftype Result (Ok v) (Err e))\n(defn ok [n :int] (Result :int :string) (Ok n))\n(defn main [] :int (match (ok 5) [(Ok v) v] [(Err _) 0]))")
    end

    test "an operator given where a function is expected is expanded to one" do
      assert {:ok, 6} = run("(defn main [] :int (fold + 0 [1 2 3]))")
    end
  end

  describe "eliminations synthesize (§8.1)" do
    test "application, selection and match" do
      source = """
      (deftype Point [x :int y :int])
      (deftype Shape (Circle :int) (Rect :Point))
      (defn area [s :Shape] :int (match s [(Circle r) (* r r)] [(Rect p) (* (. p :x) (. p :y))]))
      (defn main [] :int (+ (area (Circle 2)) (area (Rect (Point 3 4)))))
      """

      assert {:ok, 16} = run(source)
    end

    test "a polymorphic definition is instantiated at each use (inst)" do
      assert {:ok, core} = elab("(defn id [x :a] :a x)\n(defn main [] :int (if (id true) (id 1) 0))")
      assert {:ok, 1} = Eval.run(core, :main, [])
      main = List.last(core) |> inspect()
      assert main =~ "[:inst, :id, :Int]" and main =~ "[:inst, :id, [:Bool]]"
    end
  end

  describe "inference is local unification (§9)" do
    test "function arguments are checked after the others, so their types are known" do
      assert {:ok, 7} = run("(defn twice [f (Fn :a :a) x :a] :a (f (f x)))\n(defn main [] :int (twice (fn [n] (+ n 1)) 5))")
    end

    test "an arithmetic operator on open types is chosen when the definition ends" do
      assert {:ok, 7} = run("(defn main [] :int (let [f (fn [a b] (+ a b))] (f 3 4)))")
    end

    test "an arithmetic operator whose type nothing determines is an error, not a guess" do
      assert error("(defn main [] :int (let [f (fn [a b] (+ a b))] 0))").message =~ "cannot tell whether `+` works on Int or Float"
    end

    test "a type nothing constrains cannot change the program, and is chosen as Unit" do
      source = "(deftype Result (Ok v) (Err e))\n(defn main [] :int (let [r (Ok 42)] (match r [(Ok v) v] [(Err _) 0])))"
      assert {:ok, core} = elab(source)
      assert inspect(core) =~ "[:Result, :Int, :Unit]"
      assert {:ok, 42} = Eval.run(core, :main, [])
    end
  end

  describe "signatures (Q35) and no :any (§9)" do
    test "every top-level definition needs a signature" do
      e = error("(defn double [x] (* x 2))")
      assert e.message == "`double` needs a signature"
      assert e.note =~ "parameter `x`"

      assert error("(defn f [x :int] (+ x 1))").note =~ "the result"
    end

    test ":any is no type" do
      assert error("(defn f [x :any] :int 1)").message == "`f` needs a signature"
    end

    test "a lowercase keyword is a type variable of the signature" do
      assert {:ok, 3} = run("(defn first [xs (List :a)] :a (head xs))\n(defn main [] :int (first [3 4]))")
    end

    test "a signature's type variable is rigid in its body: D17 is an error, not a generalization" do
      assert error("(defn double [x :a] :a (* x 2))").message =~ "`*` works on Int or Float, not a"
    end

    test "a suggested signature is verified, and one HM got wrong fails (§13)" do
      double = Vaisto.Liquid.Canonical.parse!("(forall ((t Type)) (-> ((tvar t)) (eff closed) Int))", atoms: :create)
      assert {:error, [_]} = elab("(defn double [x] (* x 2))", suggest: %{double: double})

      int_to_int = [:->, [:Int], [:eff, :closed], :Int]
      assert {:ok, _} = elab("(defn double [x] (* x 2))", suggest: %{double: int_to_int})
    end
  end

  test "let is not generalized (Q36)" do
    assert error("(defn main [] :int (let [id (fn [x] x)] (do (id true) (id 1))))").message == "type mismatch"
  end

  describe "the HM defects the elaborator removes (RFC §1.12)" do
    test "D1: a parameter annotated with a declared type matches values of that type" do
      assert {:ok, 1} = run("(deftype Point [x :int y :int])\n(defn idp [p :Point] :Point p)\n(defn main [] :int (. (idp (Point 1 2)) :x))")

      e = error("(deftype Point [x :int y :int])\n(deftype Other (O :int))\n(defn idp [p :Point] :Point p)\n(defn main [] :Point (idp (O 1)))")
      assert e.note == "expected Point, found Other"
    end

    test "D22: a match ties its scrutinee to its patterns' type" do
      source = "(deftype A (X :int))\n(deftype B (Y :int))\n(defn f [v :A] :int (match v [(X n) n]))\n(defn main [] :int (f (Y 1)))"
      assert error(source).note == "expected A, found B"
    end

    test "== needs a ground type: values of a type variable need an Eq dictionary" do
      assert error("(defn same [x :a y :a] :bool (== x y))").message =~ "`==` needs a type without type variables"
      assert {:ok, _} = elab("(defn same [x :int y :int] :bool (== x y))")
    end
  end

  test "errors point at the source" do
    e = error("(defn f [x :int] :int\n  (+ x \"one\"))")
    assert e.message =~ "`+` works on Int or Float"
    assert e.primary_span.line == 2
  end

  test "a program outside slice 1 says so instead of guessing" do
    assert {:outside, reason} = elab("(process counter 0 :inc (+ state 1))")
    assert reason =~ "process"
    assert {:outside, _} = elab("(defn f [] :string (str 1))")
  end

  test "D23-like misparse: [x :a] is one parameter of type a, not two parameters" do
    assert {:defn, :id, [x: :a], _, :a, _} = Parser.parse("(defn id [x :a] :a x)")
  end
end
