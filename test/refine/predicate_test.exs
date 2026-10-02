defmodule Vaisto.Refine.PredicateTest do
  # The predicate translator (liquid-vaisto-rfc.md §4.4, §5.2, M6): one test per
  # IR node, from its Vaisto spelling, and the rejections M6 requires.
  use ExUnit.Case, async: true

  alias Vaisto.Parser
  alias Vaisto.Refine.{Predicate, Render}

  @scope %{
    x: {{:var, :x, :int}, :int},
    y: {{:var, :y, :int}, :int},
    b: {{:var, :b, :bool}, :bool},
    xs: {{:var, :xs, :list}, :list}
  }

  defp ir(text), do: Predicate.translate(Parser.parse(text), @scope)

  describe "each IR node, from its Vaisto spelling" do
    test "literals and variables" do
      assert {:ok, {:bool, true}} = ir("true")
      assert {:ok, {:lt, {:int, -3}, {:var, :x, :int}}} = ir("(< -3 x)")
      assert {:ok, {:var, :b, :bool}} = ir("b")
    end

    test "arithmetic" do
      assert {:ok, {:lt, {:arith, :add, [{:var, :x, :int}, {:int, 1}]}, {:var, :y, :int}}} = ir("(< (+ x 1) y)")
      assert {:ok, {:lt, {:arith, :sub, _}, _}} = ir("(< (- x 1) y)")
      assert {:ok, {:le, {:arith, :mul, [{:int, 2}, {:var, :x, :int}]}, _}} = ir("(<= (* 2 x) y)")
      assert {:ok, {:lt, {:arith, :neg, [{:var, :x, :int}]}, _}} = ir("(< (- x) y)")
    end

    test "comparisons: > and >= swap their operands (§4.3)" do
      assert {:ok, {:lt, {:var, :x, :int}, {:var, :y, :int}}} = ir("(< x y)")
      assert {:ok, {:le, {:var, :x, :int}, {:var, :y, :int}}} = ir("(<= x y)")
      assert {:ok, {:lt, {:var, :y, :int}, {:var, :x, :int}}} = ir("(> x y)")
      assert {:ok, {:le, {:var, :y, :int}, {:var, :x, :int}}} = ir("(>= x y)")
    end

    test "equality at any one sort" do
      assert {:ok, {:eq, {:var, :x, :int}, {:int, 0}}} = ir("(== x 0)")
      assert {:ok, {:not, {:eq, {:var, :x, :int}, {:int, 0}}}} = ir("(!= x 0)")
      assert {:ok, {:eq, {:var, :b, :bool}, {:bool, false}}} = ir("(== b false)")
    end

    test "connectives" do
      assert {:ok, {:and, [_, _, _]}} = ir("(and b (> x 0) (< x 9))")
      assert {:ok, {:or, [_, _]}} = ir("(or b (> x 0))")
      assert {:ok, {:not, {:var, :b, :bool}}} = ir("(not b)")
      assert {:ok, {:implies, {:var, :b, :bool}, {:lt, _, _}}} = ir("(=> b (< x y))")
      assert {:ok, {:iff, {:var, :b, :bool}, {:lt, _, _}}} = ir("(iff b (< x y))")
    end

    test "if is ite" do
      assert {:ok, {:lt, {:ite, {:var, :b, :bool}, {:var, :x, :int}, {:var, :y, :int}}, {:int, 9}}} = ir("(< (if b x y) 9)")
    end

    test "len, also spelled length, is the list measure" do
      assert {:ok, {:lt, {:var, :x, :int}, {:len, {:var, :xs, :list}}}} = ir("(< x (len xs))")
      assert {:ok, {:lt, {:var, :x, :int}, {:len, {:var, :xs, :list}}}} = ir("(< x (length xs))")
    end
  end

  describe "rejections: never approximated (§11.2, M6)" do
    test "Float and String are not sorts of the logic (C18)" do
      assert {:error, "refinements on Float are not supported"} = ir("(> x 0.5)")
      assert {:error, "refinements on String are not supported"} = ir("(== x \"s\")")
      assert {:error, "refinements on Float are not supported"} = Predicate.sort_of_base(:float)
    end

    test "functions and partial operators" do
      assert {:error, message} = ir("(> (weird 0) 0)")
      assert message =~ "`weird` cannot be used in a refinement"
      assert {:error, message} = ir("(> (div x y) 0)")
      assert message =~ "partial"
    end

    test "names out of scope and ill-sorted terms" do
      assert {:error, "`z` is not in scope in this refinement"} = ir("(> z 0)")
      assert {:error, _} = ir("(< b 1)")
      assert {:error, _} = ir("(+ x 1)")
    end
  end

  test "a predicate splits into its top-level conjuncts, as written (§13.2)" do
    conjuncts = Predicate.conjuncts(Parser.parse("(and (>= k 0) (and (< k 9) b))"))
    assert ["(>= k 0)", "(< k 9)", "b"] = Enum.map(conjuncts, &Render.surface(&1, %{}))
  end

  test "rendering a predicate back substitutes the caller's names (§13.1)" do
    assert "(< (length xs) (len xs))" = Render.surface(Parser.parse("(< k (len xs))"), %{k: "(length xs)"})
  end
end
