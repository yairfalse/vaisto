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

    test "a type inference cannot determine is an error where it arises, and (the τ e) states it" do
      untyped = "(deftype Result (Ok v) (Err e))\n(defn main [] :int (let [r (Ok 42)] (match r [(Ok v) v] [(Err _) 0])))"
      e = error(untyped)
      assert e.message == "the type arguments of `Ok` cannot be determined"
      assert e.hint =~ "(the"

      typed = "(deftype Result (Ok v) (Err e))\n(defn main [] :int (let [r (the (Result :int :string) (Ok 42))] (match r [(Ok v) v] [(Err _) 0])))"
      assert {:ok, 42} = run(typed)
    end

    test "an ascription may use only its definition's type variables" do
      assert {:ok, _} = elab("(defn f [x :a] :a (the :a x))")
      assert error("(defn f [x :a] :a (the :b x))").message == "the type variable `b` is not in scope here"
    end
  end

  describe "no coercion (§8.1: no subsumption but refinement)" do
    test "arithmetic and comparison take operands of one numeric type" do
      assert error("(defn f [] :float (+ 1 2.5))").note == "expected Int, found Float"
      assert error("(defn f [] :bool (< 2.5 1))").note == "expected Float, found Int"
      assert {:ok, 3.5} = run("(defn main [] :float (+ 1.0 2.5))")
    end

    test "/ divides Floats" do
      assert error("(defn f [] :float (/ 7 2))").note == "expected Float, found Int"
      assert {:ok, 3.5} = run("(defn main [] :float (/ 7.0 2.0))")
    end

    test "whether an operand is checked first or second does not change the verdict" do
      a = "(defn apply-first [fs (List (Fn :a :a)) x :a] :a ((head fs) x))\n(defn main [] :float (apply-first [(fn [y] (+ y 1))] 2.5))"
      b = "(defn apply-first [x :a fs (List (Fn :a :a))] :a ((head fs) x))\n(defn main [] :float (apply-first 2.5 [(fn [y] (+ y 1))]))"
      assert {:error, _} = elab(a)
      assert {:error, _} = elab(b)
    end
  end

  describe "no hidden partiality (liquid-core.md §6.6, §10.8)" do
    test "a match must be exhaustive, and the error names a missing case" do
      e = error("(deftype C (R) (G))\n(defn f [c :C] :int (match c [(R) 1]))")
      assert e.message == "this match is not exhaustive"
      assert e.note == "no clause matches (G)"

      assert error("(defn f [n :int] :int (match n [0 1]))").note == "no clause matches _"
      assert error("(defn f [xs (List :int)] :int (match xs [[] 0]))").note == "no clause matches [_ | _]"
      assert {:ok, _} = elab("(defn f [b :bool] :int (match b [true 1] [false 0]))")
    end

    test "a guarded clause does not count towards exhaustiveness" do
      assert error("(defn f [n :int] :int (match n [0 1]))").message == "this match is not exhaustive"
    end

    test "a let pattern must be irrefutable" do
      e = error("(deftype R (Ok :int) (Err :string))\n(defn f [r :R] :int (let [(Ok v) r] v))")
      assert e.message == "this pattern can fail to match"
      assert {:ok, 3} = run("(deftype P [x :int y :int])\n(defn main [] :int (let [(P a b) (P 1 2)] (+ a b)))")
    end

    test "a guard must be guard-safe" do
      e = error("(defn pos [x :int] :bool (> x 0))\n(defn f [x :int :when (pos x)] :int x)")
      assert e.message == "a guard cannot call a function"
      assert {:ok, _} = elab("(defn f [x :int :when (and (> x 0) (< x 10))] :int x)")
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
      assert error("(defn double [x :a] :a (* x 2))").note == "expected a, found Int"
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
      assert error("(defn same [x :a y :a] :bool (== x y))").message =~ "`==` compares ground data, but this is a"
      assert {:ok, _} = elab("(defn same [x :int y :int] :bool (== x y))")
    end
  end

  test "errors point at the source" do
    e = error("(defn f [x :int] :int\n  (+ x \"one\"))")
    assert e.note == "expected Int, found String"
    assert e.primary_span.line == 2
  end

  test "a program outside slice 1 says so instead of guessing" do
    assert {:outside, reason} = elab("(process counter 0 :inc (+ state 1))")
    assert reason =~ "process"
    assert {:outside, _} = elab("(defn f [] :string (str 1))")
  end

  # Each from a program an adversarial review found accepted with Core that
  # Lint rejects, or meaning something else than the backends.
  describe "review findings" do
    test "== compares ground data, through declared types too" do
      assert error("(deftype F [g (Fn :int :int)])\n(defn same [a :F b :F] :bool (== a b))").message =~ "`==` compares ground data"
    end

    test "a pattern binds each name once: a repeated name is not an equality test" do
      assert error("(defn f [x :int x :int] :int x)").message == "the parameter `x` appears twice"
      assert error("(defn f [p (Tuple :int :int)] :int (match p [{x x} x]))").message == "`x` is bound twice in this pattern"
    end

    test "duplicate definitions, fields and constructors, and built-in type names" do
      assert error("(defn f [] :int 1)\n(defn f [] :int 2)").message == "`f` is defined twice"
      assert error("(deftype R [x :int x :int])").message =~ "names a field twice"
      assert error("(deftype S (A) (A :int))").message =~ "names a constructor twice"
      assert error("(deftype S (A))\n(deftype T (A :int))").message =~ "the constructor `A` is already defined"
      assert error("(deftype List [x :int])").message =~ "built-in type"
    end

    test "_ binds nothing" do
      assert error("(defn f [_ :int] :int _)").message =~ "cannot be used as a value"
    end

    test "a binder may be named like a prelude definition" do
      assert {:ok, _} = elab("(defn main [] :int (let [prelude.map 1] (head (map (fn [x] x) [prelude.map]))))")
    end

    test "the empty tuple pattern is unit" do
      assert {:ok, _} = elab("(defn f [u :unit] :int (match u [{} 1]))")
    end

    test "a catch binder is a name" do
      assert error("(deftype R (Ok v) (Err e))\n(defn f [] :int (try (div 1 0) [catch [:error (Ok x) 1]]))").message =~ "binds the reason to a name"
    end

    test "a lambda parameter may be annotated, as a defn's is (D20)" do
      assert {:ok, 6} = run("(defn main [] :int (let [f (fn [x :int] (* x 2))] (f 3)))")
      assert error("(defn f [] :int ((fn [[a b]] a) [1 2]))").message =~ "a lambda parameter is a name"
    end

    test "a constructor is a function where a function is expected" do
      assert {:ok, _} = elab("(deftype Opt (Some :int) (None))\n(defn f [xs (List :int)] (List :Opt) (map Some xs))")
    end

    test "a parse error is an error, not something outside the fragment" do
      assert {:error, [_]} = elab("(defn f [] :int (match 5))")
    end
  end

  test "D23-like misparse: [x :a] is one parameter of type a, not two parameters" do
    assert {:defn, :id, [x: :a], _, :a, _} = Parser.parse("(defn id [x :a] :a x)")
  end
end
