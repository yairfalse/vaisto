defmodule Vaisto.Liquid.HarnessTest do
  # The differential harness (liquid-vaisto-rfc.md §6.3, liquid-core.md §9).
  # Expectations come from the defects the RFC reproduces (Appendix A) and
  # the ones liquid-core.md §12 adds.
  use ExUnit.Case, async: true

  alias Vaisto.Liquid.Harness

  describe "verdicts" do
    test "agree: the evaluator and both backends give the same outcome" do
      assert %{verdict: :agree, eval: {:ok, 3}} = Harness.run("(defn add [x :int y :int] :int (+ x y))\n(defn main [] :int (add 1 2))")
      assert %{verdict: :agree, eval: {:crash, :no_match}} = Harness.run("(defn f [n :int] :int (match n [0 1]))\n(defn main [] :int (f 5))")
    end

    # P0-4 (D5): and/or short-circuit on both backends (liquid-core.md §4.3)
    test "D5 fixed: and does not run its right operand when the left is false" do
      result = Harness.run("(defn chk [x :int y :int] :bool (and (!= y 0) (> (div x y) 1)))\n(defn main [] :bool (chk 10 0))")
      assert %{verdict: :agree, eval: {:ok, false}} = result
    end

    test "D5 fixed: or does not run its right operand when the left is true" do
      result = Harness.run("(defn chk [x :int y :int] :bool (or (== y 0) (> (div x y) 1)))\n(defn main [] :bool (chk 10 0))")
      assert %{verdict: :agree, eval: {:ok, true}} = result
    end

    test "and and or run their right operand when the left does not decide" do
      assert %{verdict: :agree, eval: {:crash, :badarith}} =
               Harness.run("(defn chk [x :int y :int] :bool (and (== y 0) (> (div x y) 1)))\n(defn main [] :bool (chk 10 0))")

      assert %{verdict: :agree, eval: {:crash, :badarith}} =
               Harness.run("(defn chk [x :int y :int] :bool (or (!= y 0) (> (div x y) 1)))\n(defn main [] :bool (chk 10 0))")
    end

    test "D25: the Elixir backend's == is not exact equality" do
      result = Harness.run("(defn main [] :bool (== 0.0 -0.0))")
      assert %{verdict: {:disagree, [:elixir]}, eval: {:ok, false}} = result
      assert result.beam.elixir == {:ok, true}
    end

    # P0-10 (D11): guarded defn on the Core Erlang backend
    test "D11 fixed: a guarded defn runs on both backends" do
      neg = "(defn neg [x :int :when (< x 0)] :int (- 0 x))\n"
      assert %{verdict: :agree, eval: {:ok, 5}} = Harness.run(neg <> "(defn main [] :int (neg -5))")
      assert %{verdict: :agree, eval: {:crash, :no_match}} = Harness.run(neg <> "(defn main [] :int (neg 5))")
    end

    test "a guard that crashes counts as false (liquid-core.md §6.6)" do
      result = Harness.run("(defn f [x :int y :int :when (> (div x y) 0)] :int x)\n(defn main [] :int (f 1 0))")
      assert %{verdict: :agree, eval: {:crash, :no_match}} = result
    end

    test "or short-circuits inside a guard" do
      result = Harness.run("(defn f [x :int y :int :when (or (== y 0) (> (div x y) 1))] :int 7)\n(defn main [] :int (f 10 0))")
      assert %{verdict: :agree, eval: {:ok, 7}} = result
    end

    # P0-10 (D12): field access on the Elixir backend
    test "D12 fixed: field access runs on both backends" do
      result = Harness.run("(deftype Point [x :int y :int])\n(defn main [] :int (. (Point 1 2) :y))")
      assert %{verdict: :agree, eval: {:ok, 2}} = result
    end

    test "field access reads a record whatever expression produces it" do
      point = "(deftype Point [x :int y :int])\n"
      assert %{verdict: :agree, eval: {:ok, 2}} = Harness.run(point <> "(defn main [] :int (. (do (Point 1 2)) :y))")
      assert %{verdict: :agree, eval: {:ok, 1}} = Harness.run(point <> "(defn main [] :int (let [p (Point 1 2)] (. p :x)))")
    end

    # P0-14 (D15): let, try and receive bindings are lexically scoped
    test "D15 fixed: a let binding does not leak into the enclosing block" do
      result = Harness.run("(defn f [x :int] :int (do (let [x 100] x) x))\n(defn main [] :int (f 1))")
      assert %{verdict: :agree, eval: {:ok, 1}} = result
    end

    test "D15 fixed: HM rejects a use of the outer binding at the leaked type" do
      assert %{verdict: {:hm_rejects, _}} =
               Harness.run("(defn f [x :string] :int (do (let [x 1] x) (+ x 1)))\n(defn main [] :int (f \"hi\"))")
    end

    test "Lint rejects what HM accepted but left ill-typed (D17)" do
      assert %{verdict: {:lint_rejects, [%{in: :double}]}} = Harness.run("(defn double [x] (* x 2))\n(defn main [] (double 21))")
    end

    test "outside the fragment, rejected by HM, or nothing to run" do
      assert %{verdict: {:outside_fragment, "the builtin :str"}} = Harness.run("(defn main [] :string (str 1))")
      assert %{verdict: {:hm_rejects, _}} = Harness.run("(defn main [] :int \"no\")")
      assert %{verdict: :no_main} = Harness.run("(defn f [] :int 1)")
    end

    test "a program that does not end does not end on any path" do
      result = Harness.run("(defn loop [n :int] :int (loop n))\n(defn main [] :int (loop 1))")
      assert %{verdict: :agree, eval: :did_not_end} = result
    end
  end

  describe "the corpus of every program in the test suite (RFC §6.3)" do
    # The disagreements the harness is known to find there, each a defect
    # this repository has recorded. A new disagreement fails this test; so
    # does fixing one of these without removing it here.
    @known []

    test "no program makes the harness raise, and every disagreement is a known defect" do
      # The test suite as it was before Liquid Core: the tests under test/liquid
      # reproduce defects on purpose, and assert on them themselves.
      corpus = Harness.corpus(Path.wildcard("test/**/*_test.exs") -- Path.wildcard("test/liquid/*_test.exs"))
      assert length(corpus) > 100

      results = for {file, source} <- corpus, do: {file, source, Harness.run(source)}
      disagreeing = for {file, source, %{verdict: {:disagree, _}}} <- results, do: {file, source}

      unknown = for {file, source} <- disagreeing, not Enum.any?(@known, fn {_, p} -> String.contains?(source, p) end), do: {file, source}
      assert unknown == [], "disagreements that are not known defects:\n#{inspect(unknown, pretty: true)}"

      for {defect, program} <- @known do
        assert Enum.any?(disagreeing, fn {_, s} -> String.contains?(s, program) end), "#{defect} no longer disagrees: remove it from @known"
      end

      agreeing = Enum.count(results, &match?({_, _, %{verdict: :agree}}, &1))
      assert agreeing >= 80, "only #{agreeing} programs agree"
    end
  end
end
