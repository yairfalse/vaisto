defmodule Vaisto.LanguageFeaturesBatch3Test do
  use ExUnit.Case, async: true

  import Vaisto.TestHelpers, only: [check_type: 1]
  alias Vaisto.Parser
  alias Vaisto.Runner

  # ═══════════════════════════════════════════════════════════════════
  # Multi-binding For Comprehension
  # ═══════════════════════════════════════════════════════════════════

  describe "multi-binding for parsing" do
    test "single binding still works" do
      ast = Parser.parse("(for [x xs] (+ x 1))")
      assert {:call, :map, [{:fn, _, _, _}, :xs], _} = ast
    end

    test "two bindings desugar to flat_map/map" do
      ast = Parser.parse("(for [x xs y ys] (+ x y))")
      assert {:call, :flat_map, [{:fn, _, _, _}, :xs], _} = ast
    end

    test "three bindings desugar to nested flat_map/flat_map/map" do
      ast = Parser.parse("(for [a as b bs c cs] (+ a (+ b c)))")
      # Outermost: flat_map over as
      assert {:call, :flat_map, [{:fn, _, inner, _}, :as], _} = ast
      # Middle: flat_map over bs
      assert {:call, :flat_map, [{:fn, _, _, _}, :bs], _} = inner
    end

    test "two bindings with :when" do
      ast = Parser.parse("(for [x xs y ys :when (< x y)] (list x y))")
      # Outermost: flat_map over xs
      assert {:call, :flat_map, [{:fn, _, _, _}, :xs], _} = ast
    end
  end

  describe "multi-binding for type checking" do
    test "two bindings produce list type" do
      code = """
      (let [xs (list 1 2)
            ys (list 10 20)]
        (for [x xs y ys] (+ x y)))
      """
      result = check_type(code)
      assert {:ok, [:List, :Int]} = result
    end

    test "single binding backward compat" do
      code = "(for [x (list 1 2 3)] (+ x 1))"
      result = check_type(code)
      assert {:ok, [:List, :Int]} = result
    end
  end

  describe "multi-binding for e2e" do
    test "two bindings cartesian product" do
      {:ok, mod} = Runner.compile_and_load(
        "(defn main [] (List :int) (for [x (list 1 2) y (list 10 20)] (+ x y)))",
        :ForMulti1
      )
      assert Runner.call(mod, :main) == [11, 21, 12, 22]
    end

    test "two bindings with :when filter" do
      {:ok, mod} = Runner.compile_and_load(
        "(defn main [] (List (List :int)) (for [x (list 1 2 3) y (list 1 2 3) :when (< x y)] (list x y)))",
        :ForMultiWhen
      )
      assert Runner.call(mod, :main) == [[1, 2], [1, 3], [2, 3]]
    end

    test "three bindings" do
      {:ok, mod} = Runner.compile_and_load(
        "(defn main [] (List :int) (for [a (list 1 2) b (list 10 20) c (list 100)] (+ a (+ b c))))",
        :ForMulti3
      )
      assert Runner.call(mod, :main) == [111, 121, 112, 122]
    end

    test "single binding still works e2e" do
      {:ok, mod} = Runner.compile_and_load(
        "(defn main [] (List :int) (for [x (list 1 2 3)] (* x x)))",
        :ForSingle
      )
      assert Runner.call(mod, :main) == [1, 4, 9]
    end
  end

  # ═══════════════════════════════════════════════════════════════════
  # Let Destructuring with Constructor Patterns
  # ═══════════════════════════════════════════════════════════════════

  describe "let destructuring e2e" do
    test "sum type variant destructuring" do
      {:ok, mod} = Runner.compile_and_load("""
      (deftype Result (Ok :int))
      (defn main [] :int
        (let [(Ok v) (Ok 42)] v))
      """, :LetDestrSum)
      assert Runner.call(mod, :main) == 42
    end

    test "record destructuring" do
      {:ok, mod} = Runner.compile_and_load("""
      (deftype Point [x :int y :int])
      (defn main [] :int
        (let [(Point x y) (Point 3 7)] (+ x y)))
      """, :LetDestrRec)
      assert Runner.call(mod, :main) == 10
    end

    test "nested let with constructor pattern" do
      {:ok, mod} = Runner.compile_and_load("""
      (deftype Pair [a :int b :int])
      (defn main [] :int
        (let [p (Pair 5 10)
              (Pair a b) p]
          (+ a b)))
      """, :LetDestrNest)
      assert Runner.call(mod, :main) == 15
    end
  end

  # ═══════════════════════════════════════════════════════════════════
  # Guards in defn
  # ═══════════════════════════════════════════════════════════════════

  describe "guarded defn parsing" do
    test "single-clause with guard produces 7-tuple" do
      ast = Parser.parse("(defn abs [x :int :when (< x 0)] :int (- 0 x))")
      assert {:defn, :abs, _, _, :int, {:call, :<, _, _}, _loc} = ast
    end

    test "single-clause without guard still produces 6-tuple" do
      ast = Parser.parse("(defn id [x :int] :int x)")
      assert {:defn, :id, _, _, :int, %Vaisto.Parser.Loc{}} = ast
    end
  end

  describe "guarded defn type checking" do
    test "guard must return bool" do
      code = "(defn bad [x :int :when (+ x 1)] :int x)"
      result = check_type(code)
      assert {:error, _} = result
    end

    test "valid guard type checks" do
      code = "(defn pos [x :int :when (> x 0)] :int x)"
      result = check_type(code)
      assert {:ok, [:->, [:Int], _, :Int]} = result
    end
  end

  describe "guarded defn e2e" do
    test "single-clause guarded defn" do
      {:ok, mod} = Runner.compile_and_load("""
      (defn negate [x :int :when (< x 0)] :int (- 0 x))
      (defn main [] :int (negate -5))
      """, :GuardSingle)
      assert Runner.call(mod, :main) == 5
    end

    test "guarded defn raises on guard failure" do
      {:ok, mod} = Runner.compile_and_load("""
      (defn negate [x :int :when (< x 0)] :int (- 0 x))
      (defn main [] :int (negate 5))
      """, :GuardFail)
      assert {:crash, :no_match} = Vaisto.Liquid.Runtime.run(mod, :main, [])
    end

    test "multi-clause with guards" do
      {:ok, mod} = Runner.compile_and_load("""
      (defn classify (Fn :int :atom)
        [x :when (< x 0) :negative]
        [x :when (> x 0) :positive]
        [x :zero])
      (defn main [] :atom (classify -3))
      """, :GuardMulti1)
      assert Runner.call(mod, :main) == :negative
    end

    test "multi-clause guards dispatch correctly" do
      {:ok, mod} = Runner.compile_and_load("""
      (defn classify (Fn :int :atom)
        [x :when (< x 0) :negative]
        [x :when (> x 0) :positive]
        [x :zero])
      (defn main [] :atom (classify 0))
      """, :GuardMulti2)
      assert Runner.call(mod, :main) == :zero
    end

    test "multi-clause guards with positive" do
      {:ok, mod} = Runner.compile_and_load("""
      (defn classify (Fn :int :atom)
        [x :when (< x 0) :negative]
        [x :when (> x 0) :positive]
        [x :zero])
      (defn main [] :atom (classify 7))
      """, :GuardMulti3)
      assert Runner.call(mod, :main) == :positive
    end
  end
end
