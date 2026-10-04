defmodule Vaisto.MultiClauseTest do
  # Multi-clause functions with a signature (owner decision, 2026-10-04: the
  # (Fn ...) type after the name), and guards in match clauses (D27), which they
  # need. Each runs on both backends and through the Phase 1a elaborator.
  use ExUnit.Case, async: true

  alias Vaisto.{Elab, Parser, Runner}
  alias Vaisto.Liquid.Lint

  defp strip(ast), do: ast |> Vaisto.LocationStripper.strip() |> elem(0)

  defp run_both(source) do
    results = for backend <- [:core, :elixir], do: Runner.run(source, backend: backend)
    assert [r, r] = results, "the backends disagree: #{inspect(results)}"
    r
  end

  defp elab(source) do
    case Elab.module(Parser.parse(source)) do
      {:ok, core} = ok ->
        assert :ok = Lint.check_module(core)
        ok

      other ->
        other
    end
  end

  describe "the signature after the name" do
    test "is a single-clause defn whose body matches on its parameter" do
      assert {:defn, :len, [{:"arg 1", {:call, :List, _, _}}], {:match, :"arg 1", [_, _], _}, :int} =
               strip(Parser.parse("(defn len (Fn (List :a) :int) [[] 0] [[h | t] (+ 1 (len t))])"))
    end

    test "its arity says how many patterns each clause has" do
      assert {:defn, :add, [{:"arg 1", :int}, {:"arg 2", :int}], {:match, {:tuple, [:"arg 1", :"arg 2"], _}, [{{:tuple_pattern, [0, :y]}, :y}, _], _}, :int} =
               strip(Parser.parse("(defn add (Fn :int :int :int) [0 y y] [x y (+ x y)])"))
    end

    test "a clause with the wrong number of patterns, a signature with no argument, or a non-clause is an error" do
      assert {:error, e, _} = Parser.parse("(defn add (Fn :int :int :int) [0 1])")
      assert e.message =~ "each clause of `add` has 2 patterns and a body"
      assert {:error, e, _} = Parser.parse("(defn k (Fn :int) [1])")
      assert e.message =~ "takes at least one argument"
      assert {:error, e, _} = Parser.parse("(defn f (Fn :int :int) [0 1] 2)")
      assert e.message =~ "has only clauses"
    end

    test "runs on both backends, with recursion, several arguments and guards" do
      assert {:ok, 120} = run_both("(defn fact (Fn :int :int) [0 1] [n (* n (fact (- n 1)))])\n(defn main [] :int (fact 5))")
      assert {:ok, 10} = run_both("(defn add (Fn :int :int :int) [0 y y] [x y (+ x y)])\n(defn main [] :int (+ (add 0 5) (add 2 3)))")
      assert {:ok, :negative} = run_both("(defn sign (Fn :int :atom) [x :when (< x 0) :negative] [0 :zero] [_ :positive])\n(defn main [] :atom (sign -4))")
    end

    test "its generated parameters capture no name of the program" do
      assert {:ok, 8} = run_both("(defn arg [] :int 7)\n(defn f (Fn :int :int) [x (+ x (arg))])\n(defn main [] :int (f 1))")
    end

    test "gives the elaborator the signature it needs, and its clauses must be exhaustive" do
      assert {:ok, _} = elab("(defn len (Fn (List :a) :int) [[] 0] [[h | t] (+ 1 (len t))])")
      assert {:error, [e]} = elab("(defn f (Fn :int :int) [0 1])")
      assert e.message == "this match is not exhaustive"
    end
  end

  describe "guards in match clauses (D27)" do
    test "choose a clause only when the guard holds, on both backends" do
      classify = "(defn classify [n :int] :atom (match n [x :when (< x 0) :negative] [0 :zero] [_ :positive]))\n"
      assert {:ok, :negative} = run_both(classify <> "(defn main [] :atom (classify -5))")
      assert {:ok, :positive} = run_both(classify <> "(defn main [] :atom (classify 5))")
    end

    test "a guard that crashes counts as false (liquid-core.md §6.6)" do
      assert {:ok, 2} = run_both("(defn f [n :int] :int (match n [x :when (> (div 10 x) 1) 1] [_ 2]))\n(defn main [] :int (f 0))")
    end

    test "a guard is a Bool" do
      assert {:error, _} = Runner.run("(defn f [n :int] :int (match n [x :when x 1] [_ 2]))\n(defn main [] :int (f 0))")
    end

    test "a guarded clause does not count towards exhaustiveness" do
      source = "(deftype C (R) (G))\n(defn f [c :C] :int (match c [(R) :when false 1] [(G) 2]))\n(defn main [] :int (f (G)))"
      assert {:error, _} = Runner.run(source)
      assert {:error, [%{message: "this match is not exhaustive"}]} = elab(source)
    end
  end
end
