defmodule Vaisto.Liquid.LintTest do
  # Every expectation here is derived from docs/design/liquid-core.md §10: for
  # each rule, a term it accepts and a term it rejects (RFC §19, M1).
  use ExUnit.Case, async: true

  alias Vaisto.Liquid.{Canonical, Lint}

  @types """
  ((type Point () (record (x Int) (y Int)))
   (type Box (a) (record (item (tvar a))))
   (type Color () (sum (Red) (Green)))
   (type R () (sum (Ok Int) (Err String)))
   (type Shape () (sum (Circle Int) (Rect Int Int))))
  """

  @id "(forall ((a Type)) (-> ((tvar a)) (eff closed) (tvar a)))"
  @get_x "(forall ((r Row)) (-> ((Record (row (x Int) (rvar r)))) (eff closed) Int))"

  defp core(text), do: Canonical.parse!(text, atoms: :create)
  defp opts(bind), do: [types: core(@types), bind: core(bind)]

  defp synth(term, bind \\ "()"), do: Lint.synth(core(term), opts(bind))

  defp accepts(term, type, bind \\ "()") do
    assert synth(term, bind) == {:ok, core(type)}, "expected #{term} to synthesize #{type}"
  end

  defp rejects(term, message, bind \\ "()") do
    assert {:error, [%{message: actual}]} = synth(term, bind), "expected #{term} to be rejected"
    assert actual =~ message
  end

  defp module(items), do: Lint.check_module(core("(module T (core-version 0) #{items})"))

  describe "§10.2 well-formed types" do
    test "type variables must be in scope" do
      accepts("(let ((f #{@id} (fn ((x (tvar a))) x))) 1)", "Int")
      rejects("(fn ((x (tvar a))) x)", "type variable a is not in scope")
    end

    test "named types take their declared number of arguments" do
      accepts("(fn ((b (Box Int))) b)", "(-> ((Box Int)) (eff closed) (Box Int))")
      rejects("(fn ((b (Box))) b)", "Box takes 1 type arguments")
      rejects("(fn ((b (Nope))) b)", "unknown type Nope")
    end

    test "rows have distinct labels and a closed or bound tail" do
      accepts("(fn ((r (Record (row (x Int) closed)))) r)", "(-> ((Record (row (x Int) closed))) (eff closed) (Record (row (x Int) closed)))")
      rejects("(fn ((r (Record (row (x Int) (x Int) closed)))) r)", "has a label twice")
      rejects("(fn ((r (Record (row (x Int) (rvar r))))) r)", "rvar r is not a Row variable in scope")
    end

    test "forall binders have kinds, and their names are distinct" do
      rejects("(let ((f (forall ((a Type) (a Row)) Int) 1)) 1)", "with a name twice")
      rejects("(let ((f (forall ((a Kind)) Int) 1)) 1)", "malformed binder")
    end

    test "refinements are not in version 0" do
      rejects("(fn ((x (refine v Int (prim gt v 0)))) x)", "refinements are not in")
    end
  end

  describe "§10.3 type equality" do
    test "rows are equal whatever the order of their labels" do
      accepts(
        "(app (fn ((r (Record (row (y Int) (x Int) closed)))) 1) (record (Record (row (x Int) (y Int) closed)) (x 1) (y 2)))",
        "Int"
      )
    end

    test "a named record is its branded row" do
      accepts(
        "(app (fn ((r (Record (row (#Point Unit) (x Int) (y Int) closed)))) (select r y)) (record (Point) (x 1) (y 2)))",
        "Int"
      )

      rejects(
        "(app (fn ((r (Record (row (x Int) (y Int) closed)))) (select r y)) (record (Point) (x 1) (y 2)))",
        "expected (Record (row (x Int) (y Int) closed)), found (Point)"
      )
    end

    test "a row variable is instantiated by splicing, so a row-polymorphic function takes a named record (D24)" do
      accepts(
        "(let ((get-x #{@get_x} (fn ((p (Record (row (x Int) (rvar r))))) (select p x)))) " <>
          "(app (inst get-x (row (#Point Unit) (y Int) closed)) (record (Point) (x 1) (y 2))))",
        "Int"
      )
    end

    test "a splice that repeats a label is rejected" do
      rejects(
        "(let ((get-x #{@get_x} (fn ((p (Record (row (x Int) (rvar r))))) (select p x)))) " <>
          "(inst get-x (row (x Int) closed)))",
        "label twice"
      )
    end

    test "a pi type equals the arrow without its parameter names (§3.4)" do
      accepts("(fn ((f (pi ((n Int)) (eff closed) Int))) (app f 1))", "(-> ((-> (Int) (eff closed) Int)) (eff closed) Int)")
      accepts("(app (fn ((f (pi ((n Int)) (eff closed) Int))) 1) (fn ((m Int)) m))", "Int")
    end

    test "forall types are equal up to renaming their binders" do
      accepts("(app (fn ((f #{@id})) 1) (let ((g (forall ((b Type)) (-> ((tvar b)) (eff closed) (tvar b))) (fn ((y (tvar b))) y))) g))", "Int")
    end

    test "substitution does not capture" do
      # k : forall a. forall b. a -> b, instantiated at a := b where an outer b is in
      # scope. The inner binder must be renamed, giving forall c. b -> c.
      k = "((k (forall ((a Type)) (forall ((b Type)) (-> ((tvar a)) (eff closed) (tvar b))))))"

      in_scope_of_b = fn param_type ->
        "(let ((t (forall ((b Type)) (-> ((tvar b)) (eff closed) Int))" <>
          " (fn ((y (tvar b))) (app (fn ((g #{param_type})) 1) (inst k (tvar b))))))" <>
          " 1)"
      end

      accepts(in_scope_of_b.("(forall ((c Type)) (-> ((tvar b)) (eff closed) (tvar c)))"), "Int", k)
      rejects(in_scope_of_b.("(forall ((b Type)) (-> ((tvar b)) (eff closed) (tvar b)))"), "expected", k)
    end

    test "Dyn equals only Dyn" do
      accepts("(up Int 1)", "Dyn")
      rejects("(app (fn ((x Int)) x) (up Int 1))", "expected Int, found Dyn")
    end
  end

  describe "§10.4 synthesis" do
    test "literals and variables" do
      accepts(~S|(tuple 1 1.5 "s" (atom a) (unit))|, "(Tuple Int Float String Atom Unit)")
      accepts("x", "Int", "((x Int))")
      rejects("x", "unbound variable x")
    end

    test "application" do
      accepts("(app (fn ((x Int)) x) 1)", "Int")
      rejects("(app (fn ((x Int)) x) 1 2)", "takes 1 arguments, not 2")
      rejects("(app 1 2)", "cannot apply a value of type Int")
    end

    test "let checks its binding against its annotation" do
      accepts("(let ((x Int 1)) x)", "Int")
      rejects(~S|(let ((x Int "a")) x)|, "expected Int, found String")
    end

    test "letrec puts the whole group in scope" do
      accepts(
        "(letrec ((even? (-> (Int) (eff closed) (Bool)) (fn ((n1 Int)) (if (prim eq n1 0) (inj (Bool) true) (app odd? (prim sub n1 1)))))" <>
          " (odd? (-> (Int) (eff closed) (Bool)) (fn ((n2 Int)) (if (prim eq n2 0) (inj (Bool) false) (app even? (prim sub n2 1))))))" <>
          " (app even? 4))",
        "(Bool)"
      )

      rejects("(letrec ((f (-> () (eff closed) Int) 1)) 1)", "something other than a fn")
    end

    test "inst substitutes as many arguments as there are binders, of the right kinds" do
      accepts("(let ((id #{@id} (fn ((x (tvar a))) x))) (app (inst id String) \"s\"))", "String")
      rejects("(let ((id #{@id} (fn ((x (tvar a))) x))) (inst id Int Int))", "2 arguments to a forall of 1")
      rejects("(let ((id #{@id} (fn ((x (tvar a))) x))) (inst id (row closed)))", "not a type")
      rejects("(inst 1 Int)", "is not a forall")
    end

    test "records give exactly their fields" do
      accepts("(record (Point) (y 2) (x 1))", "(Point)")
      accepts("(record (Box Int) (item 1))", "(Box Int)")
      rejects("(record (Point) (x 1))", "exactly the fields")
      rejects("(record (Box Int) (item \"s\"))", "expected Int, found String")
      rejects("(record (Record (row (x Int) (rvar r))) (x 1))", "rvar r is not a Row variable")
    end

    test "select reads a field of a named record, an anonymous record, or a tuple" do
      accepts("(select (record (Box String) (item \"s\")) item)", "String")
      accepts("(select (record (Record (row (x Int) closed)) (x 1)) x)", "Int")
      accepts("(select (tuple 1 \"a\") 1)", "String")
      rejects("(select (record (Point) (x 1) (y 2)) z)", "has no field z")
      rejects("(select (tuple 1 \"a\") 2)", "has no field 2")
    end

    test "inj builds a named sum with one argument per field" do
      accepts("(inj (Shape) Rect 1 2)", "(Shape)")
      accepts("(inj (List String) Cons \"a\" (inj (List String) Nil))", "(List String)")
      rejects("(inj (Shape) Rect 1)", "takes 2 fields, not 1")
      rejects("(inj (Shape) Triangle)", "not a constructor of Shape")
      rejects("(inj (Point) Point 1 2)", "Point is not a sum type")
    end

    test "if needs a Bool condition and branches of one type" do
      accepts("(if (inj (Bool) true) 1 2)", "Int")
      rejects("(if 1 2 3)", "expected (Bool), found Int")
      rejects(~S|(if (inj (Bool) true) 1 "a")|, "branches have types Int and String")
    end

    test "perform: the operations of version 0" do
      accepts("(perform now)", "Int")
      accepts("(perform external (atom m) (atom f) (inj (List Dyn) Nil))", "Dyn")
      rejects("(perform tick)", "not an operation of Liquid Core version 0")
      rejects("(perform now 1)", "takes 0 operands")
    end

    test "handle-crash: body and handler share a type, the handler sees a Reason" do
      accepts("(handle-crash 1 ((r (Reason)) 2))", "Int")
      accepts("(handle-crash 1 ((r (Reason)) (match r ((inj (Reason) badarith) 0) (_ 1))))", "Int")
      rejects(~S|(handle-crash 1 ((r (Reason)) "s"))|, "branches have types Int and String")
      rejects("(handle-crash 1 ((r Int) 2))", "binds a (Reason)")
    end

    test "up embeds a value without functions into Dyn" do
      accepts("(up (Tuple Int String) (tuple 1 \"a\"))", "Dyn")
      rejects("(up (-> (Int) (eff closed) Int) (fn ((x Int)) x))", "not ground data")
    end

    test "decode is not in version 0" do
      rejects("(decode Int (up Int 1))", "decode is not in")
    end
  end

  describe "§10.5 and §10.6 checking, and terms that never return" do
    test "a crash checks against any type and synthesizes none" do
      assert synth("(perform crash (inj (Reason) badarg))") == :none
      accepts("(if (inj (Bool) true) (perform crash (inj (Reason) badarg)) 3)", "Int")
      accepts("(let ((x Int (perform crash (inj (Reason) badarg)))) x)", "Int")
      rejects("(perform crash 1)", "expected (Reason), found Int")
    end

    test "a term that never returns cannot stand where a type must be synthesized" do
      rejects("(tuple (perform crash (inj (Reason) badarg)))", "never returns, so its type is unknown")
    end

    test "a fn checks against an arrow with the same parameters" do
      accepts("(let ((f (-> (Int) (eff closed) Int) (fn ((x Int)) (perform crash (inj (Reason) badarg))))) 1)", "Int")
      rejects("(let ((f (-> (Int) (eff closed) Int) (fn ((x String)) 1))) 1)", "does not have type")
    end
  end

  describe "§10.7 polymorphic bindings" do
    test "a polymorphic let binds a value" do
      accepts("(let ((nil (forall ((a Type)) (List (tvar a))) (inj (List (tvar a)) Nil))) (inst nil Int))", "(List Int)")
      rejects("(let ((x (forall ((a Type)) (tvar a)) (perform crash (inj (Reason) badarg)))) 1)", "must bind a value")
      rejects("(let ((x (forall ((a Type)) Dyn) (perform external (atom m) (atom f) (inj (List Dyn) Nil)))) 1)", "must bind a value")
    end
  end

  describe "§10.8 exhaustiveness" do
    test "every constructor of a sum must be covered" do
      accepts("(match (inj (R) Ok 1) ((inj (R) Ok n) n) ((inj (R) Err e) 0))", "Int")
      rejects("(match (inj (R) Ok 1) ((inj (R) Ok n) n))", "not exhaustive: (Err _) is not matched")
    end

    test "nested patterns are checked to any depth" do
      list = "(inj (List Int) Nil)"
      accepts("(match #{list} ((inj (List Int) Nil) 0) ((inj (List Int) Cons h t) h))", "Int")
      rejects("(match #{list} ((inj (List Int) Nil) 0) ((inj (List Int) Cons h (inj (List Int) Nil)) h))", "(Cons _ (Cons _ _)) is not matched")
    end

    test "literal patterns never cover an infinite type" do
      accepts("(match 1 (0 (atom zero)) (_ (atom other)))", "Atom")
      rejects("(match 1 (0 (atom zero)) (1 (atom one)))", "not exhaustive: _ is not matched")
    end

    test "tuples, records and Unit have one constructor" do
      accepts("(match (tuple (inj (Bool) true) (unit)) ((tuple (inj (Bool) true) (unit)) 1) ((tuple (inj (Bool) false) _) 2))", "Int")
      rejects("(match (tuple (inj (Bool) true) 1) ((tuple (inj (Bool) true) _) 1))", "(tuple (false) _) is not matched")
      accepts("(match (record (Point) (x 1) (y 2)) ((record (Point) (x a)) a))", "Int")
    end

    test "a guarded clause does not count" do
      rejects("(match (inj (Color) Red) ((inj (Color) Red) 1) (c (when (prim eq 1 1)) 2))", "(Green) is not matched")
    end
  end

  describe "§10.9 patterns" do
    test "patterns type against the scrutinee" do
      rejects(~S|(match 1 ("a" 1) (_ 2))|, ~S|pattern "a" does not match values of Int|)
      rejects("(match (tuple 1 2) ((tuple a) a) (_ 0))", "does not match values of (Tuple Int Int)")
      rejects("(match (inj (R) Ok 1) ((inj (Shape) Circle n) n) (_ 0))", "pattern of (Shape) does not match")
      rejects("(match (inj (R) Ok 1) ((inj (R) Ok) 1) (_ 0))", "has 1 fields, not 0")
      rejects("(match (record (Point) (x 1) (y 2)) ((record (Point) (z a)) a))", "Point has no field z")
    end

    test "a pattern binds each variable once" do
      rejects("(match (tuple 1 2) ((tuple a a) a))", "a is bound more than once")
      accepts("(match (tuple 1 2) ((as p (tuple a b)) p))", "(Tuple Int Int)")
    end
  end

  describe "§6.6 guards" do
    test "guards are guard-safe Bool terms" do
      accepts("(match 1 (n (when (prim gt (prim div 10 n) 1)) n) (_ 0))", "Int")
      rejects("(match 1 (n (when (app (fn ((y Int)) (inj (Bool) true)) n)) n) (_ 0))", "is not guard-safe")
      rejects(~S|(match 1 (n (when (prim eq (prim concat "a" "b") "ab")) n) (_ 0))|, "is not guard-safe")
      rejects("(match 1 (n (when 1) n) (_ 0))", "expected (Bool), found Int")
    end
  end

  describe "§10.10 primitive types" do
    test "fixed operand types" do
      accepts("(prim add 1 2)", "Int")
      accepts("(prim flt 1.0 2.0)", "(Bool)")
      accepts("(prim int-to-float 1)", "Float")
      rejects("(prim add 1 1.5)", "expected Int, found Float")
      rejects("(prim add 1)", "takes 2 operands, not 1")
      rejects("(prim pow 2 3)", "pow is not a primitive")
    end

    test "eq compares two values of one ground data type (D25)" do
      accepts("(prim eq (tuple 1 2) (tuple 3 4))", "(Bool)")
      rejects("(prim eq 1 1.0)", "expected Int, found Float")
      rejects("(prim eq (fn ((x Int)) x) (fn ((y Int)) y))", "not ground data")
    end

    test "list primitives take their element type from the list" do
      accepts("(prim head (inj (List String) Nil))", "String")
      accepts("(prim tail (inj (List String) Nil))", "(List String)")
      rejects("(prim length 1)", "length needs a list")
    end
  end

  describe "§10.11 ground data types" do
    @eq_at_a "(let ((same (forall ((a Type)) (-> ((tvar a) (tvar a)) (eff closed) (Bool))) (fn ((p (tvar a)) (q (tvar a))) (prim eq p q)))) 1)"

    test "equality and up are not available at a type variable" do
      rejects(@eq_at_a, "eq compares values of (tvar a), which is not ground data")
      rejects("(let ((emb (forall ((a Type)) (-> ((tvar a)) (eff closed) Dyn)) (fn ((p (tvar a))) (up (tvar a) p)))) 1)", "up of (tvar a)")
    end

    test "polymorphic equality takes the equality as an argument instead (RFC §4.2)" do
      accepts(
        "(let ((same (forall ((a Type)) (-> ((-> ((tvar a) (tvar a)) (eff closed) (Bool)) (tvar a) (tvar a)) (eff closed) (Bool)))" <>
          " (fn ((eq-a (-> ((tvar a) (tvar a)) (eff closed) (Bool))) (p (tvar a)) (q (tvar a))) (app eq-a p q))))" <>
          " (app (inst same Int) (fn ((m Int) (n Int)) (prim eq m n)) 1 2))",
        "(Bool)"
      )
    end

    test "a type is ground through the declarations of its named types" do
      accepts("(prim eq (inj (List (Point)) Nil) (inj (List (Point)) Nil))", "(Bool)")

      rejects(
        "(prim eq (inj (List (-> (Int) (eff closed) Int)) Nil) (inj (List (-> (Int) (eff closed) Int)) Nil))",
        "not ground data"
      )

      rejects("(prim eq (record (Box (-> () (eff closed) Int)) (item (fn () 1))) (record (Box (-> () (eff closed) Int)) (item (fn () 2))))", "not ground data")
    end

    test "an open row is not ground" do
      rejects(
        "(let ((f (forall ((r Row)) (-> ((Record (row (x Int) (rvar r)))) (eff closed) (Bool))) (fn ((p (Record (row (x Int) (rvar r))))) (prim eq p p)))) 1)",
        "not ground data"
      )
    end

    test "a recursive declaration is ground when its members are" do
      types = core("((type Tree () (sum (Leaf) (Node (Tree) Int (Tree)))) (type FnTree () (sum (FLeaf) (FNode (FnTree) (-> () (eff closed) Int)))))")
      assert Lint.synth(core("(prim eq (inj (Tree) Leaf) (inj (Tree) Leaf))"), types: types) == {:ok, [:Bool]}
      assert {:error, [%{message: m}]} = Lint.synth(core("(prim eq (inj (FnTree) FLeaf) (inj (FnTree) FLeaf))"), types: types)
      assert m =~ "not ground data"
    end
  end

  describe "§10.12 binders" do
    test "a binder may not shadow another" do
      rejects("(let ((x Int 1)) (let ((x Int 2)) x))", "x is bound more than once")
      rejects("(fn ((x Int)) (let ((x Int 2)) x))", "x is bound more than once")
      rejects("(let ((r (Reason) (inj (Reason) badarg))) (handle-crash 1 ((r (Reason)) 2)))", "r is bound more than once")
    end

    test "binders are distinct across a whole term, not only along one path" do
      rejects("(tuple (fn ((x Int)) x) (fn ((x Int)) x))", "x is bound more than once")
      rejects("(match (inj (R) Ok 1) ((inj (R) Ok n) n) ((inj (R) Err n) 0))", "n is bound more than once")
      accepts("(tuple (fn ((x Int)) x) (fn ((y Int)) y))", "(Tuple (-> (Int) (eff closed) Int) (-> (Int) (eff closed) Int))")
    end

    test "a binder may not reuse a name already in scope" do
      rejects("(let ((x Int 1)) x)", "x is bound more than once", "((x Int))")
      assert {:error, [%{message: m, in: :g}]} =
               module("(def f (-> () (eff closed) Int) (fn () 1)) (def g (-> () (eff closed) Int) (fn () (let ((f Int 2)) f)))")

      assert m =~ "f is bound more than once"
    end

    test "_ binds nothing, so it may repeat and cannot be referred to" do
      accepts("(let ((_ Int 1)) (let ((_ Int 2)) 3))", "Int")
      accepts("(fn ((_ Int) (_ String)) 1)", "(-> (Int String) (eff closed) Int)")
      rejects("(let ((_ Int 1)) _)", "unbound variable _")
    end
  end

  describe "§10.13 modules" do
    test "a well-typed module passes" do
      assert module("""
             (type Point () (record (x Int) (y Int)))
             (def main (-> () (eff closed) Int) (fn () (app y-of (record (Point) (x 1) (y 2)))))
             (def y-of (-> ((Point)) (eff closed) Int) (fn ((p (Point))) (select p y)))
             (def id #{@id} (fn ((x (tvar a))) x))
             """) == :ok
    end

    test "every problem is reported, with the definition it is in and its span" do
      assert {:error, problems} =
               module("""
               (def good (-> () (eff closed) Int) (fn () 1))
               (def bad1 (-> () (eff closed) Int) (fn () "s"))
               (def bad2 (-> () (eff closed) Int) (fn () (prim add 1 (meta (span "t.va" 3 9)) 1.0)))
               """)

      assert [%{in: :bad1, message: m1}, %{in: :bad2, message: m2, span: span}] = problems
      assert m1 =~ "expected Int, found String"
      assert m2 =~ "expected Int, found Float"
      assert span == [:span, "t.va", 3, 9]
    end

    test "definitions are fns with distinct names whose types are closed" do
      assert {:error, [%{message: m}]} = module("(def x Int 1)")
      assert m =~ "not a fn"
      assert {:error, [%{message: m}]} = module("(def f (-> () (eff closed) Int) (fn () 1)) (def f (-> () (eff closed) Int) (fn () 2))")
      assert m =~ "defined twice"
      assert {:error, [%{message: m} | _]} = module("(def f (-> ((tvar a)) (eff closed) Int) (fn ((x (tvar a))) 1))")
      assert m =~ "type variable a is not in scope"
    end

    test "declarations: names are fresh, members distinct, field types well formed" do
      assert {:error, [%{message: m}]} = module("(type List () (sum (A)))")
      assert m =~ "reserved or built-in"
      assert {:error, [%{message: m}]} = module("(type Int () (sum (A)))")
      assert m =~ "reserved or built-in"
      assert {:error, [%{message: m}]} = module("(type T2 () (sum (A) (A)))")
      assert m =~ "names a constructor twice"
      assert {:error, [%{message: m}]} = module("(type T2 () (record (x Int) (x Int)))")
      assert m =~ "names a field twice"
      assert {:error, [%{message: m}]} = module("(type T2 () (record (#x Int)))")
      assert m =~ "may not begin with #"
      assert {:error, [%{message: m}]} = module("(type T2 () (record (x (tvar a))))")
      assert m =~ "type variable a is not in scope"
      assert module("(type Tree (a) (sum (Leaf) (Node (Tree (tvar a)) (tvar a) (Tree (tvar a)))))") == :ok
    end

    test "something that is not a version 0 module is rejected" do
      assert {:error, [%{message: m}]} = Lint.check_module(core("(module T (core-version 1))"))
      assert m =~ "not a Liquid Core version 0 module"
    end

    test "a definition whose type is not well formed is not in scope" do
      assert {:error, problems} = module("(def f (-> Int (eff closed) Int) (fn () 1)) (def main (-> () (eff closed) Int) (fn () (app f)))")
      assert Enum.any?(problems, &(&1.in == :f))
      assert Enum.any?(problems, &(&1.in == :main and &1.message =~ "unbound"))
    end

    test "Lint is total: bad declarations are problems, through every entry point" do
      bad = core("((type S () (sum (A) (A Int))))")
      assert {:error, _} = Lint.synth(core("1"), types: bad)
      assert {:error, _} = Lint.check(core("1"), :Int, types: bad)
      assert {:error, _} = module("(type S () (sum (A) (A Int))) (def main (-> () (eff closed) Int) (fn () (match (inj (S) A) ((inj (S) A) 1))))")
    end
  end

  describe "§10.8 every clause is reachable" do
    test "a clause after clauses that match everything it does" do
      rejects("(match 1 (_ 1) (2 2))", "clause 2 of the match can never be chosen")
      rejects("(match (inj (Color) Red) ((inj (Color) Red) 1) ((inj (Color) Green) 2) (_ 3))", "clause 3 of the match can never be chosen")
    end

    test "a guarded clause before does not make a later one unreachable" do
      accepts("(match 1 (n (when (prim gt n 0)) 1) (_ 0))", "Int")
    end

    test "the exhaustiveness oracle for producers of Core" do
      types = [types: core(@types)]
      assert Lint.exhaustive?(core("((inj (Color) Red) (inj (Color) Green))"), core("(Color)"), types)
      refute Lint.exhaustive?(core("((inj (Color) Red))"), core("(Color)"), types)
      refute Lint.exhaustive?([0, 1], :Int, types)
      refute Lint.exhaustive?([:not_a_pattern, [:junk]], [:Nope], types)
    end
  end

  describe "§10.16 effects" do
    test "a function performs only the operations its row allows" do
      assert {:error, [%{message: m}]} = module("(def f (-> () (eff closed) Int) (fn () (perform now)))")
      assert m =~ "now is performed here"
      assert :ok = module("(def f (-> () (eff now closed) Int) (fn () (perform now)))")
    end

    test "a call performs the callee's effects" do
      assert {:error, [%{in: :g, message: m}]} = module("(def f (-> () (eff now closed) Int) (fn () (perform now))) (def g (-> () (eff closed) Int) (fn () (app f)))")
      assert m =~ "now is performed here"
    end

    test "a synthesized fn is pure" do
      rejects("(let ((f (-> () (eff now closed) Int) (fn () (perform now)))) (app (fn () (app f))))", "now is performed here")
    end

    test "crash is exempt until Phase 4, and a term outside every fn may perform anything" do
      assert :ok = module("(def f (-> () (eff closed) Int) (fn () (perform crash (inj (Reason) no_match))))")
      accepts("(perform now)", "Int")
    end
  end
end
