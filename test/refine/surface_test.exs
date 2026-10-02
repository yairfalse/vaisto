defmodule Vaisto.Refine.SurfaceTest do
  # Refinement syntax (RFC §4.4, owner decision Q1: braces in the type slot),
  # the parser hazards the RFC names before refinements can land (§4.4: braces
  # in parameter position; D6), D27, and the split into plain program and
  # signatures (§21.1).
  use ExUnit.Case, async: true

  alias Vaisto.{Compilation, Parser}
  alias Vaisto.Refine.Surface

  defp strip(ast), do: Vaisto.LocationStripper.strip(ast) |> elem(0)

  describe "parsing refined types" do
    test "a refined parameter type" do
      assert {:defn, :"safe-div", [{:x, :int}, {:y, {:refine, :d, :int, pred}}], _body, :int} =
               strip(Parser.parse("(defn safe-div [x :int y {d :int | (!= d 0)}] :int (div x y))"))

      assert {:call, :!=, [:d, 0], _} = pred
    end

    test "a refined result type, and a refined list parameter" do
      assert {:defn, :clamp0, [x: :int], {:if, _, 0, :x, _}, {:refine, :r, :int, _}} =
               strip(Parser.parse("(defn clamp0 [x :int] {r :int | (>= r 0)} (if (< x 0) 0 x))"))

      assert {:defn, :at, [{:xs, {:refine, :v, {:call, :List, _, _}, _}}], _, :int} =
               strip(Parser.parse("(defn at [xs {v (List :int) | (> (len v) 0)}] :int (head xs))"))
    end

    test "a guarded defn keeps its guard" do
      assert {:defn, :f, [{:n, {:refine, :v, :int, _}}], 1, :int, {:call, :>, _, _}} =
               strip(Parser.parse("(defn f [n {v :int | (> v 0)} :when (> n 1)] :int 1)"))
    end

    test "braces in a parameter list are only ever a refined type (RFC §4.4 hazard)" do
      # Before refinements, this parsed as a three-parameter function.
      assert {:error, error, _} = (Parser.parse("(defn f [x :int {d :int | (!= d 0)}] :int x)"))
      assert error.message =~ "braces in a parameter list write a refined type"

      assert {:error, error, _} = (Parser.parse("(defn f [x {1 2}] :int x)"))
      assert error.message =~ "a refined type"
    end

    test "a tuple body is still a body" do
      assert {:defn, :f, [], {:tuple_pattern, [1, 2]}, :any} = strip(Parser.parse("(defn f [] {1 2})"))
      assert {:defn, :g, [], {:do, [{:tuple_pattern, [1, 2]}, 3], _}, :any} = strip(Parser.parse("(defn g [] {1 2} 3)"))
    end

    test "D6: only a capitalized head is a return type, so a call stays in the body (P0-5)" do
      assert {:defn, :f, [x: :any], {:do, [{:call, :println, _, _}, :x], _}, :any} =
               strip(Parser.parse("(defn f [x] (println x) x)"))

      assert {:defn, :g, [], {:list, [], _}, {:call, :List, _, _}} = strip(Parser.parse("(defn g [] (List :int) (list))"))
    end

    test "D27: a match clause with :when is refused, not silently ignored" do
      assert {:error, error, _} = Parser.parse("(match n [x :when (> x 0) 1] [_ 0])")
      assert error.message == "guards in match and receive clauses are not supported yet"
    end
  end

  describe "split/1" do
    test "gives HM the base types and the checker the signatures" do
      ast = Parser.parse("(defn safe-div [x :int y {d :int | (!= d 0)}] :int (div x y))\n(defn plain [x :int] :int x)")
      {plain, sigs} = Surface.split(ast)

      assert [{:defn, :"safe-div", [x: :int, y: :int], _, :int, _}, {:defn, :plain, _, _, _, _}] = plain
      assert %{"safe-div": %Surface.Sig{params: [{:x, :int, nil}, {:y, :int, {:d, _}}], result: {:int, nil}}} = sigs
      refute Surface.refined?(plain)
      assert Surface.refined?(ast)
    end

    test "a program without refinements is unchanged" do
      ast = Parser.parse("(defn f [x :int] :int x)")
      assert {^ast, sigs} = Surface.split(ast)
      assert sigs == %{}
    end
  end

  test "entry points that do not check refinements refuse refined code instead of dropping it" do
    source = "(defn f [x {v :int | (> v 0)}] :int x)"
    assert {:error, %Vaisto.Error{message: "refinement types are not checked on this path"}} = Vaisto.check(source)
    assert_raise RuntimeError, ~r/refinement types are not checked/, fn -> Vaisto.compile_string(source) end
    assert {:error, _} = Compilation.compile(source, :SurfaceProbe, load: false, format_errors: false, z3: "/nonexistent/z3")
  end
end
