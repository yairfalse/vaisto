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

    test "D5: the Core Erlang backend does not short-circuit and" do
      result = Harness.run("(defn chk [x :int y :int] :bool (and (!= y 0) (> (div x y) 1)))\n(defn main [] :bool (chk 10 0))")
      assert %{verdict: {:disagree, [:core]}, eval: {:ok, false}} = result
      assert result.beam.core == {:crash, :badarith}
      assert result.beam.elixir == {:ok, false}
    end

    test "D25: the Elixir backend's == is not exact equality" do
      result = Harness.run("(defn main [] :bool (== 0.0 -0.0))")
      assert %{verdict: {:disagree, [:elixir]}, eval: {:ok, false}} = result
      assert result.beam.elixir == {:ok, true}
    end

    test "D11: the Core Erlang backend cannot compile a guarded defn" do
      result = Harness.run("(defn neg [x :int :when (< x 0)] :int (- 0 x))\n(defn main [] :int (neg -5))")
      assert %{verdict: {:disagree, [:core]}, eval: {:ok, 5}} = result
      assert {:compile_error, _} = result.beam.core
    end

    test "D12: the Elixir backend cannot compile field access" do
      result = Harness.run("(deftype Point [x :int y :int])\n(defn main [] :int (. (Point 1 2) :y))")
      assert %{verdict: {:disagree, [:elixir]}, eval: {:ok, 2}} = result
      assert result.beam.core == {:ok, 2}
      assert {:compile_error, _} = result.beam.elixir
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
    @known [
      {"D11", "(defn negate [x :int :when (< x 0)] :int (- 0 x))"}
    ]

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
