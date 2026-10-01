defmodule Vaisto.Liquid.EvalTest do
  # Every expectation here is derived from docs/design/liquid-core.md, §5 to §8.
  use ExUnit.Case, async: true

  alias Vaisto.Liquid.{Canonical, Eval}
  alias Vaisto.Liquid.Eval.Stuck

  @types """
  ((type Point () (record (x Int) (y Int)))
   (type Color () (sum (Red) (Green)))
   (type R () (sum (Ok Int) (Err String)))
   (type P () (sum (Pair Int Int))))
  """

  defp core(text), do: Canonical.parse!(text, atoms: :create)
  defp ev(text, opts \\ []), do: Eval.eval(core(text), [types: core(@types)] ++ opts)
  defp script(steps), do: [handler: {:script, steps}]

  describe "§5 values and their representation" do
    test "base values" do
      assert ev("42") == {:ok, 42}
      assert ev("1.5") == {:ok, 1.5}
      assert ev(~S|"hi"|) == {:ok, "hi"}
      assert ev("(atom hello)") == {:ok, :hello}
      assert ev("(unit)") == {:ok, nil}
      assert ev(~S|(tuple 1 "a")|) == {:ok, {1, "a"}}
    end

    test "built-in sums keep Erlang's representations" do
      assert ev("(inj (Bool) true)") == {:ok, true}
      assert ev("(inj (Bool) false)") == {:ok, false}
      assert ev("(inj (List Int) Nil)") == {:ok, []}
      assert ev("(inj (List Int) Cons 1 (inj (List Int) Cons 2 (inj (List Int) Nil)))") == {:ok, [1, 2]}
      assert ev("(inj (Reason) badarith)") == {:ok, :badarith}
      assert ev("(inj (Reason) user 7)") == {:ok, {:user, 7}}
      assert ev("(inj (Reason) raised (atom error) 7)") == {:ok, {:raised, :error, 7}}
    end

    test "declared sums are tagged tuples, one element per field" do
      assert ev("(inj (Color) Red)") == {:ok, {:Red}}
      assert ev("(inj (R) Ok 1)") == {:ok, {:Ok, 1}}
      assert ev("(inj (P) Pair 1 2)") == {:ok, {:Pair, 1, 2}}
    end

    test "a named record is a tagged tuple in declaration order; an anonymous record is a map" do
      assert ev("(record (Point) (y 2) (x 1))") == {:ok, {:Point, 1, 2}}
      assert ev("(record (Record (row (x Int) (y Int) closed)) (x 1) (y 2))") == {:ok, %{x: 1, y: 2}}
    end

    test "a function is a closure" do
      assert {:ok, %Eval.Closure{}} = ev("(fn ((x Int)) x)")
    end
  end

  describe "§6.2 order" do
    test "operands are evaluated left to right" do
      steps = [{:tick, [:a], {:ok, 1}}, {:tick, [:b], {:ok, 2}}]
      assert ev("(tuple (perform tick (atom a)) (perform tick (atom b)))", script(steps)) == {:ok, {1, 2}}
    end

    test "the function is evaluated before its arguments" do
      steps = [{:tick, [:f], {:ok, nil}}, {:tick, [:arg], {:ok, 5}}]

      term = """
      (app (let ((_ Unit (perform tick (atom f)))) (fn ((x Int)) x))
           (perform tick (atom arg)))
      """

      assert ev(term, script(steps)) == {:ok, 5}
    end

    test "record fields are evaluated in the order written" do
      steps = [{:tick, [:y], {:ok, 2}}, {:tick, [:x], {:ok, 1}}]
      term = "(record (Point) (y (perform tick (atom y))) (x (perform tick (atom x))))"
      assert ev(term, script(steps)) == {:ok, {:Point, 1, 2}}
    end

    test "nothing to the right of a crash is evaluated" do
      term = "(tuple (perform crash (inj (Reason) badarith)) (perform tick (atom never)))"
      assert ev(term) == {:crash, :badarith}
    end
  end

  describe "§6.3 environments and functions" do
    test "let binds one variable" do
      assert ev("(let ((x Int 1)) (prim add x 2))") == {:ok, 3}
    end

    test "closures capture their environment lexically" do
      term = """
      (let ((k Int 10))
        (let ((f (-> (Int) (eff closed) Int) (fn ((x Int)) (prim add x k))))
          (let ((k Int 0)) (app f 1))))
      """

      assert ev(term) == {:ok, 11}
    end

    test "letrec functions see the whole group" do
      term = """
      (letrec ((even? (-> (Int) (eff closed) (Bool))
                 (fn ((n Int)) (if (prim eq n 0) (inj (Bool) true) (app odd? (prim sub n 1)))))
               (odd? (-> (Int) (eff closed) (Bool))
                 (fn ((n Int)) (if (prim eq n 0) (inj (Bool) false) (app even? (prim sub n 1))))))
        (tuple (app even? 10) (app odd? 7) (app even? 7)))
      """

      assert ev(term) == {:ok, {true, true, false}}
    end

    test "types are erased: inst and up evaluate to the value of their term" do
      id = "(forall ((a Type)) (-> ((tvar a)) (eff closed) (tvar a)))"
      assert ev("(let ((id #{id} (fn ((x (tvar a))) x))) (app (inst id Int) 3))") == {:ok, 3}
      assert ev("(up Int 3)") == {:ok, 3}
    end

    test "the defs of a module form one letrec" do
      module = core("""
      (module Demo (core-version 0)
        (type Point () (record (x Int) (y Int)))
        (def main (-> () (eff closed) Int)
          (fn () (app sum-to 4)))
        (def sum-to (-> (Int) (eff closed) Int)
          (fn ((n Int)) (if (prim eq n 0) 0 (prim add n (app sum-to (prim sub n 1))))))
        (def y-of (-> ((Point)) (eff closed) Int)
          (fn ((p (Point))) (select p y))))
      """)

      assert Eval.run(module, :main, []) == {:ok, 10}
      assert Eval.run(module, :"y-of", [{:Point, 1, 2}]) == {:ok, 2}
    end
  end

  describe "§6.4 data" do
    test "select reads a named record, an anonymous record, or a tuple position" do
      assert ev("(select (record (Point) (x 1) (y 2)) y)") == {:ok, 2}
      assert ev("(select (record (Record (row (x Int) (y Int) closed)) (x 1) (y 2)) x)") == {:ok, 1}
      assert ev(~S|(select (tuple 1 "a") 1)|) == {:ok, "a"}
    end

    test "one row-polymorphic function reads a field of any record (D24)" do
      get_x = "(fn ((r (Record (row (x Int) (rvar r))))) (select r x))"
      assert ev("(app #{get_x} (record (Point) (x 1) (y 2)))") == {:ok, 1}
      assert ev("(app #{get_x} (record (Record (row (x Int) (z Int) closed)) (x 5) (z 0)))") == {:ok, 5}
    end
  end

  describe "§6.5 control" do
    test "if evaluates only the chosen branch" do
      assert ev("(if (inj (Bool) true) 1 (perform crash (inj (Reason) user 0)))") == {:ok, 1}
      assert ev("(if (inj (Bool) false) (perform crash (inj (Reason) user 0)) 2)") == {:ok, 2}
    end

    test "and, written as if, short-circuits (D5)" do
      # (and (!= y 0) (> (div x y) 1)) with x = 10, y = 0
      term = """
      (let ((x Int 10))
        (let ((y Int 0))
          (if (prim ne y 0) (prim gt (prim div x y) 1) (inj (Bool) false))))
      """

      assert ev(term) == {:ok, false}
    end

    test "or, written as if, short-circuits" do
      assert ev("(if (inj (Bool) true) (inj (Bool) true) (prim head (inj (List Int) Nil)))") == {:ok, true}
    end

    test "match chooses the first clause whose pattern matches, with its bindings" do
      term = """
      (match (inj (R) Ok 5)
        ((inj (R) Err e) 0)
        ((inj (R) Ok n) (prim add n 1))
        (_ 99))
      """

      assert ev(term) == {:ok, 6}
    end

    test "no matching clause crashes with no_match" do
      assert ev("(match (inj (R) Err \"e\") ((inj (R) Ok n) n))") == {:crash, :no_match}
    end

    test "literal patterns match exactly equal values" do
      assert ev("(match -0.0 (0.0 (atom zero)) (_ (atom other)))") == {:ok, :other}
      assert ev("(match 0.0 (0.0 (atom zero)) (_ (atom other)))") == {:ok, :zero}
      assert ev(~S|(match "a" ("b" 1) ("a" 2))|) == {:ok, 2}
      assert ev("(match (atom x) ((atom y) 1) ((atom x) 2))") == {:ok, 2}
      assert ev("(match (unit) ((unit) 1))") == {:ok, 1}
    end

    test "structural patterns: tuple, record fields, constructors, as" do
      assert ev("(match (tuple 1 2) ((tuple a b) (prim sub a b)))") == {:ok, -1}
      assert ev("(match (record (Point) (x 1) (y 2)) ((record (Point) (y v)) v))") == {:ok, 2}
      assert ev("(match (inj (P) Pair 3 4) ((inj (P) Pair a b) (prim mul a b)))") == {:ok, 12}
      assert ev("(match (inj (Color) Green) ((inj (Color) Red) 1) ((inj (Color) Green) 2))") == {:ok, 2}
      assert ev("(match (inj (List Int) Cons 1 (inj (List Int) Nil)) ((inj (List Int) Nil) 0) ((inj (List Int) Cons h t) h))") == {:ok, 1}
      assert ev("(match (inj (Bool) false) ((inj (Bool) true) 1) ((inj (Bool) false) 0))") == {:ok, 0}
      assert ev("(match (inj (Reason) badarg) ((inj (Reason) badarith) 1) ((inj (Reason) badarg) 2))") == {:ok, 2}
      assert ev("(match (tuple 1 2) ((as whole (tuple 1 _)) whole))") == {:ok, {1, 2}}
    end
  end

  describe "§6.6 guards" do
    test "a clause is chosen only when its guard gives true" do
      term = "(match 5 (n (when (prim gt n 10)) (atom big)) (n (when (prim gt n 1)) (atom medium)) (_ (atom small)))"
      assert ev(term) == {:ok, :medium}
    end

    test "a guard that crashes counts as false" do
      term = "(match 0 (n (when (prim gt (prim div 10 n) 1)) (atom first)) (_ (atom fallback)))"
      assert ev(term) == {:ok, :fallback}
    end
  end

  describe "§6.7 crashes" do
    test "perform crash crashes with its reason" do
      assert ev("(perform crash (inj (Reason) user 7))") == {:crash, {:user, 7}}
    end

    test "handle-crash binds the reason of a crash in its body" do
      assert ev("(handle-crash (prim head (inj (List Int) Nil)) ((r (Reason)) r))") == {:ok, :badarg}
      assert ev("(handle-crash 1 ((r (Reason)) 2))") == {:ok, 1}
    end

    test "a crash in the handler is not caught by the same handle-crash" do
      term = """
      (handle-crash (perform crash (inj (Reason) user 1))
        ((r (Reason)) (perform crash (inj (Reason) user 2))))
      """

      assert ev(term) == {:crash, {:user, 2}}
      assert ev("(handle-crash #{term} ((r (Reason)) r))") == {:ok, {:user, 2}}
    end
  end

  describe "§7 primitives" do
    test "add, sub, mul and neg are exact" do
      assert ev("(prim mul 4294967296 4294967296)") == {:ok, 18_446_744_073_709_551_616}
      assert ev("(prim sub 1 18446744073709551616)") == {:ok, -18_446_744_073_709_551_615}
      assert ev("(prim add -3 5)") == {:ok, 2}
      assert ev("(prim neg -3)") == {:ok, 3}
    end

    test "div rounds toward zero and rem takes the sign of the dividend, for every sign" do
      for {x, y, q, r} <- [{7, 2, 3, 1}, {-7, 2, -3, -1}, {7, -2, -3, 1}, {-7, -2, 3, -1}, {6, 3, 2, 0}, {-6, 3, -2, 0}, {0, 5, 0, 0}, {1, 7, 0, 1}, {-1, 7, 0, -1}] do
        assert ev("(prim div #{x} #{y})") == {:ok, q}, "div #{x} #{y}"
        assert ev("(prim rem #{x} #{y})") == {:ok, r}, "rem #{x} #{y}"
      end
    end

    test "div and rem on integers beyond 64 bits" do
      # x = 2^100 + 1 = 3q + 2 with q = (2^100 - 1) / 3, so tdiv(x, -3) = -q and trem(x, -3) = 2.
      x = "1267650600228229401496703205377"
      assert ev("(prim div #{x} -3)") == {:ok, -422_550_200_076_076_467_165_567_735_125}
      assert ev("(prim rem #{x} -3)") == {:ok, 2}
    end

    test "div and rem by zero crash with badarith" do
      assert ev("(prim div 1 0)") == {:crash, :badarith}
      assert ev("(prim rem 1 0)") == {:crash, :badarith}
    end

    test "integer comparisons" do
      assert ev("(tuple (prim lt 1 2) (prim le 2 2) (prim gt 1 2) (prim ge 2 3))") == {:ok, {true, true, false, false}}
    end

    test "float arithmetic is IEEE-754, and a non-finite result crashes with badarith" do
      assert ev("(prim fadd 0.1 0.2)") == {:ok, 0.1 + 0.2}
      assert ev("(prim fneg 0.0)") == {:ok, -0.0}
      assert ev("(prim fdiv 1.0 4.0)") == {:ok, 0.25}
      assert ev("(prim fmul 1.0e308 10.0)") == {:crash, :badarith}
      assert ev("(prim fadd 1.0e308 1.0e308)") == {:crash, :badarith}
      assert ev("(prim fsub -1.0e308 1.0e308)") == {:crash, :badarith}
    end

    test "fdiv by 0.0 or -0.0 crashes with badarith" do
      assert ev("(prim fdiv 1.0 0.0)") == {:crash, :badarith}
      assert ev("(prim fdiv 1.0 -0.0)") == {:crash, :badarith}
      assert ev("(prim fdiv 0.0 0.0)") == {:crash, :badarith}
    end

    test "float comparisons: -0.0 is not less than 0.0" do
      assert ev("(tuple (prim flt -0.0 0.0) (prim fle 1.0 1.0) (prim fgt 2.0 1.0) (prim fge 1.0 2.0))") ==
               {:ok, {false, true, true, false}}
    end

    test "int-to-float gives the nearest float, and crashes with badarg out of range" do
      assert ev("(prim int-to-float 3)") == {:ok, 3.0}
      assert ev("(prim int-to-float #{2 ** 2000})") == {:crash, :badarg}
    end

    test "eq and ne are exact equality of representations" do
      assert ev("(prim eq 0.0 -0.0)") == {:ok, false}
      assert ev("(prim eq (tuple 1 2) (tuple 1 2))") == {:ok, true}
      assert ev("(prim ne (inj (R) Ok 1) (inj (R) Ok 2))") == {:ok, true}
      assert ev(~S|(prim eq "a" "a")|) == {:ok, true}
    end

    test "not and concat" do
      assert ev("(prim not (inj (Bool) false))") == {:ok, true}
      assert ev(~S|(prim concat "ab" "cd")|) == {:ok, "abcd"}
    end

    test "list primitives" do
      list = "(inj (List Int) Cons 1 (inj (List Int) Cons 2 (inj (List Int) Cons 3 (inj (List Int) Nil))))"
      assert ev("(prim length #{list})") == {:ok, 3}
      assert ev("(prim length (inj (List Int) Nil))") == {:ok, 0}
      assert ev("(prim empty? (inj (List Int) Nil))") == {:ok, true}
      assert ev("(prim empty? #{list})") == {:ok, false}
      assert ev("(prim head #{list})") == {:ok, 1}
      assert ev("(prim tail #{list})") == {:ok, [2, 3]}
    end

    test "head and tail of Nil crash with badarg" do
      assert ev("(prim head (inj (List Int) Nil))") == {:crash, :badarg}
      assert ev("(prim tail (inj (List Int) Nil))") == {:crash, :badarg}
    end
  end

  describe "§8 effects and handlers" do
    test "the pure handler answers no request" do
      assert_raise Stuck, ~r/pure handler/, fn -> ev("(perform tick (atom a))") end
    end

    test "the scripted handler answers requests in order" do
      steps = [{:now, [], {:ok, 100}}, {:now, [], {:ok, 105}}]
      assert ev("(prim sub (perform now) (perform now))", script(steps)) == {:ok, -5}
    end

    test "a raised outcome makes the perform crash with (raised c v)" do
      steps = [{:external, [:file, :read, []], {:raised, :error, :enoent}}]

      assert ev("(perform external (atom file) (atom read) (inj (List Dyn) Nil))", script(steps)) ==
               {:crash, {:raised, :error, :enoent}}
    end

    test "a request the script does not expect, or a script not used up, is reported" do
      assert_raise Stuck, ~r/does not fit/, fn ->
        ev("(perform now)", script([{:random, [], {:ok, 4}}]))
      end

      assert_raise Stuck, ~r/not used up/, fn ->
        ev("1", script([{:now, [], {:ok, 1}}]))
      end
    end

    test "the same answers in the same order give the same outcome" do
      steps = [{:random, [], {:ok, 4}}, {:random, [], {:ok, 9}}]
      term = "(tuple (perform random) (perform random))"
      assert ev(term, script(steps)) == ev(term, script(steps))
    end
  end

  describe "§6.1 terms that are not well formed" do
    test "stop the evaluator instead of crashing the program" do
      assert_raise Stuck, fn -> ev("(if 1 2 3)") end
      assert_raise Stuck, fn -> ev("unbound") end
      assert_raise Stuck, fn -> ev("(prim add 1 1.0)") end
      assert_raise Stuck, fn -> ev("(app (fn ((x Int)) x) 1 2)") end
      assert_raise Stuck, fn -> ev("(decode Int 1)") end
    end
  end
end
