defmodule Vaisto.Refine.RefineTest do
  # The checking rules of liquid-vaisto-rfc.md §4.4, §10 and §11.3, and the
  # bridge gates of §21.1, each from the requirement it states.
  use ExUnit.Case, async: true

  alias Vaisto.{Compilation, Parser, TypeChecker}
  alias Vaisto.Refine
  alias Vaisto.Refine.{Solver, Surface, VC, VCGen}

  @safe_div "(defn safe-div [x :int y {d :int | (!= d 0)}] :int (div x y))\n"

  defp compile(source, opts \\ []) do
    Compilation.compile(source, :"RefineTest_#{:erlang.unique_integer([:positive])}", Keyword.merge([load: false, format_errors: false], opts))
  end

  defp messages({:error, errors}), do: Enum.map(errors, & &1.message)
  defp messages(other), do: flunk("expected errors, got #{inspect(other)}")

  defp labels({:error, errors}), do: Enum.map(errors, &(&1.primary_span && &1.primary_span.label))

  describe "where obligations arise (§4.4)" do
    @describetag :z3

    test "a call to a refined function is checked in a function with no refinements of its own" do
      assert ["requirement not met"] = messages(compile(@safe_div <> "(defn f [n :int] :int (safe-div 1 n))"))
    end

    test "div, head and tail are checked inside refined functions only" do
      assert {:ok, _, _} = compile("(defn plain [x :int y :int] :int (div x y))")

      result = compile("(defn refined [x :int y :int] {r :int | true} (div x y))")
      assert ["requirement not met"] = messages(result)
      assert ["`div` requires a non-zero divisor"] = labels(result)

      result = compile("(defn first [xs (List :int)] {r :int | true} (head xs))")
      assert ["`head` requires a non-empty list"] = labels(result)
    end

    test "a refined function used as a value must accept every argument" do
      result = compile(@safe_div <> "(defn f [] :int (let [g safe-div] (g 1 2)))")
      assert ["requirement not met"] = messages(result)
      assert [label] = labels(result)
      assert label =~ "used as a value"
    end

    test "calls inside a lambda are checked, with the lambda's parameters unrefined" do
      assert ["requirement not met"] = messages(compile(@safe_div <> "(defn f [n :int] :int (let [g (fn [k] (safe-div 1 k))] n))"))
    end
  end

  describe "facts are scoped to the path that produced them (§4.4, §10)" do
    @describetag :z3

    test "a branch fact discharges an obligation inside its branch only" do
      assert {:ok, _, _} = compile(@safe_div <> "(defn f [n :int] :int (if (!= n 0) (safe-div 1 n) 0))")
      assert ["requirement not met"] = messages(compile(@safe_div <> "(defn f [n :int] :int (if (!= n 0) 0 (safe-div 1 n)))"))
    end

    test "what the second operand of and learns is not known after the and" do
      source = @safe_div <> "(defn f [x :int y :int] :int (do (and (!= y 0) (> (safe-div x y) 1)) (safe-div x y)))"
      assert ["requirement not met"] = messages(compile(source))
    end

    test "or checks its second operand where the first is false" do
      assert {:ok, _, _} = compile(@safe_div <> "(defn f [x :int y :int] :bool (or (== y 0) (> (safe-div x y) 1)))")
    end

    test "a list pattern gives the length facts of §10.3" do
      at = "(defn at [xs (List :int) i {k :int | (and (>= k 0) (< k (len xs)))}] :int (if (== i 0) (head xs) (at (tail xs) (- i 1))))\n"
      assert {:ok, _, _} = compile(at <> "(defn second-or-zero [xs (List :int)] :int (match xs [[] 0] [[h | t] (if (empty? t) h (at xs 1))]))")
      assert ["requirement not met"] = messages(compile(at <> "(defn bad [xs (List :int)] :int (match xs [[] 0] [[h | t] (at xs 1)]))"))
    end

    test "a refined call's result refinement is a fact for its caller" do
      source = "(defn pos [x :int] {r :int | (> r 0)} (if (> x 0) x 1))\n" <> @safe_div <> "(defn f [n :int] :int (safe-div 10 (pos n)))"
      assert {:ok, _, _} = compile(source)
    end

    test "a let-bound value keeps what is known about it" do
      assert {:ok, _, _} = compile(@safe_div <> "(defn f [n :int] :int (let [m (+ (* n n) 1)] (safe-div 10 m)))")
    end
  end

  describe "guards (§10.4)" do
    @describetag :z3

    test "a guard holds in the body, with its definedness: a guard that crashes counts as false" do
      # (> (div 10 n) 1) can only be true where it is defined, so n ≠ 0 in the body.
      assert {:ok, _, _} = compile(@safe_div <> "(defn f [n :int :when (> (div 10 n) 1)] :int (safe-div 1 n))")
    end

    test "a guard that does not imply the requirement does not discharge it" do
      assert ["requirement not met"] = messages(compile(@safe_div <> "(defn f [n :int :when (> n -5)] :int (safe-div 1 n))"))
    end

    test "requirements and guard together must be satisfiable" do
      result = compile("(defn f [n {v :int | (> v 0)} :when (< n 0)] :int n)")
      assert ["the requirements of `f` can never be met"] = messages(result)
    end
  end

  describe "contradictions and vacuity (§11.3)" do
    @describetag :z3

    test "an obligation that holds only because its facts contradict each other is an error" do
      result = compile(@safe_div <> "(defn f [n :int] :int (if (> n 0) (if (< n 0) (safe-div 1 n) 0) 0))")
      assert ["this code can only run under contradictory facts"] = messages(result)
    end

    test "a definition whose requirements can never be met is reported once" do
      result = compile("(defn never [x {v :int | (and (> v 0) (< v 0))}] {r :int | (> r 0)} (div 1 x))")
      assert ["the requirements of `never` can never be met"] = messages(result)
    end
  end

  # C19 (§14.2, §10.3). Clause guards are written in Core here: match and
  # receive have no surface guards yet (D27), and multi-clause functions are
  # typed Any -> Any, so the rule cannot be reached from surface syntax today.
  describe "C19: negating an earlier clause's guard includes its definedness" do
    @describetag :z3

    test "the third clause's requirement n ≠ 0 fails, because at n = 0 both guards crash" do
      module =
        Vaisto.Liquid.Canonical.parse!("""
        (module Main (core-version 0)
          (def safe-div (-> (Int Int) (eff closed) Int) (fn ((x Int) (y Int)) (prim div x y)))
          (def c19 (-> (Int) (eff closed) Int)
            (fn ((n Int))
              (match n
                (a (when (prim gt (prim div 10 a) 1)) 1)
                (b (when (prim lt (prim div 10 b) 2)) 2)
                (_ (app safe-div 1 n))))))
        """, atoms: :create)

      sig = %Surface.Sig{name: :"safe-div", params: [{:x, :int, nil}, {:y, :int, {:d, Parser.parse("(!= d 0)")}}], result: {:int, nil}}
      {:ok, sigs} = VCGen.logic_sigs(%{"safe-div": sig}, %{"safe-div": [:->, [:Int, :Int], [:eff, :closed], :Int]})
      {vcs, []} = VCGen.generate(module, sigs, MapSet.new([:"safe-div", :c19]))

      [call] = for %VC{kind: :precondition, meta: %{def: :c19}} = vc <- vcs, do: vc
      assert [{_, {:invalid, _}}] = Solver.Z3.check([call], [])
    end
  end

  describe "the bridge gates (§21.1)" do
    test "a call outside a defn is refused, since only calls in definitions are checked" do
      assert [message] = messages(compile(@safe_div <> "(safe-div 1 0)"))
      assert message =~ "only calls inside a `defn` are checked"
    end

    test "a definition the checker cannot read is refused when it uses a refined function" do
      assert [message] = messages(compile(@safe_div <> "(defn f [n :int] :string (str (safe-div 1 n)))"))
      assert message =~ "cannot read `f`"
    end

    test "a definition that uses a refined function must pass Core Lint" do
      # HM generalizes x over every type (D17), which Core Lint rejects.
      assert [message] = messages(compile(@safe_div <> "(defn f [x] (do (* x 2) (safe-div 1 1)))"))
      assert message =~ "needs `f` to be well typed"
    end

    test "== on Float is refused where the backends disagree (D25)" do
      assert [message] = messages(compile("(defn f [x :float y {v :int | (> v 0)}] :bool (== x 1.0))"))
      assert message =~ "D25"
    end

    test "a failed square is a build error, never a silent pass (K11)" do
      source = "(defn f [x :int] :int x)"
      {:ok, _, typed} = TypeChecker.check(Parser.parse(source))
      # A signature that puts a Bool refinement on an Int parameter.
      sig = %Surface.Sig{name: :f, params: [{:x, :bool, {:v, true}}], result: {:int, nil}}
      assert {:error, [error]} = Refine.check(typed, %{f: sig}, solver: Solver.Unknown)
      assert error.message =~ "does not commute"
    end
  end

  describe "the solver (§11.2)" do
    test "a program without refinements never starts the solver" do
      assert {:ok, _, _} = compile("(defn f [x :int] :int (div 1 x))", solver: __MODULE__.NoSolver, z3: "/nonexistent/z3")
    end

    test "refined code without z3 does not build, and says why" do
      assert ["refinement checking needs the `z3` solver, which was not found on PATH"] = messages(compile(@safe_div, z3: "/nonexistent/z3"))
    end
  end

  describe "refinements cannot bypass the checker" do
    test "the type checker refuses a refined type on any path that skips the refinement checker" do
      assert {:error, error} = TypeChecker.check(Parser.parse(@safe_div))
      assert error.message == "refinement types are not checked on this path"
    end
  end

  defmodule NoSolver do
    @behaviour Vaisto.Refine.Solver
    def check(_vcs, _opts), do: raise("the solver must not start")
    def identity, do: {"none", "0"}
  end
end
