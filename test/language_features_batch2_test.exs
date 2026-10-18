defmodule Vaisto.LanguageFeaturesBatch2Test do
  use ExUnit.Case, async: true

  import Vaisto.TestHelpers, only: [check_type: 1]
  alias Vaisto.Parser
  alias Vaisto.Runner

  # ═══════════════════════════════════════════════════════════════════
  # String Interpolation
  # ═══════════════════════════════════════════════════════════════════

  describe "string interpolation parsing" do
    test "plain string without interpolation stays as {:string, ...}" do
      assert {:string, "hello"} = Parser.parse(~s|"hello"|)
    end

    test "interpolation desugars to str call" do
      ast = Parser.parse(~S|"hello #{name}"|)
      assert {:call, :str, [{:string, "hello "}, :name], _loc} = ast
    end

    test "escaped interpolation stays as plain string" do
      ast = Parser.parse(~S|"hello \#{name}"|)
      assert {:string, ~S|hello #{name}|} = ast
    end

    test "expression inside interpolation" do
      ast = Parser.parse(~S|"result: #{(+ 1 2)}"|)
      assert {:call, :str, [{:string, "result: "}, {:call, :+, [1, 2], _}], _} = ast
    end

    test "multiple interpolations" do
      ast = Parser.parse(~S|"#{a} and #{b}"|)
      assert {:call, :str, [:a, {:string, " and "}, :b], _} = ast
    end

    test "interpolation at start" do
      ast = Parser.parse(~S|"#{x} world"|)
      assert {:call, :str, [:x, {:string, " world"}], _} = ast
    end

    test "interpolation at end" do
      ast = Parser.parse(~S|"hello #{x}"|)
      assert {:call, :str, [{:string, "hello "}, :x], _} = ast
    end

    test "only interpolation" do
      ast = Parser.parse(~S|"#{x}"|)
      # Single expression wrapped in str for type safety
      assert {:call, :str, [:x], _loc} = ast
    end
  end

  describe "string interpolation type checking" do
    test "interpolated string has type :string" do
      result = check_type(~S|(let [x 42] "value: #{x}")|)
      assert {:ok, :String} = result
    end

    test "expression interpolation type checks" do
      result = check_type(~S|"#{(+ 1 2)}"|)
      assert {:ok, :String} = result
    end
  end

  describe "string interpolation e2e" do
    test "integer interpolation" do
      {:ok, mod} = Runner.compile_and_load(
        ~S|(defn main [] :string (let [x 42] "value: #{x}"))|,
        :InterpInt
      )
      assert Runner.call(mod, :main) == "value: 42"
    end

    test "expression interpolation" do
      {:ok, mod} = Runner.compile_and_load(
        ~S|(defn main [] :string "#{(+ 1 2)}")|,
        :InterpExpr
      )
      assert Runner.call(mod, :main) == "3"
    end

    test "boolean interpolation" do
      {:ok, mod} = Runner.compile_and_load(
        ~S|(defn main [] :string "#{true}")|,
        :InterpBool
      )
      assert Runner.call(mod, :main) == "true"
    end

    test "multiple interpolations" do
      {:ok, mod} = Runner.compile_and_load(
        ~S|(defn main [] :string "#{1} and #{2}")|,
        :InterpMulti
      )
      assert Runner.call(mod, :main) == "1 and 2"
    end

    test "escaped interpolation is literal" do
      {:ok, mod} = Runner.compile_and_load(
        ~S|(defn main [] :string "\#{not interpolated}")|,
        :InterpEscaped
      )
      assert Runner.call(mod, :main) == ~S|#{not interpolated}|
    end
  end

  # ═══════════════════════════════════════════════════════════════════
  # For Comprehension
  # ═══════════════════════════════════════════════════════════════════

  describe "for comprehension parsing" do
    test "basic for desugars to map" do
      ast = Parser.parse("(for [x xs] (* x 2))")
      assert {:call, :map, [{:fn, [:x], {:call, :*, [:x, 2], _}, _}, :xs], _} = ast
    end

    test "for with :when desugars to map + filter" do
      ast = Parser.parse("(for [x xs :when (> x 2)] (* x 2))")
      assert {:call, :map, [_map_fn, {:call, :filter, [_filter_fn, :xs], _}], _} = ast
    end
  end

  describe "for comprehension type checking" do
    test "basic for returns list" do
      result = check_type("(let [xs (list 1 2 3)] (for [x xs] (* x 2)))")
      assert {:ok, [:List, :Int]} = result
    end

    test "for with :when returns list" do
      result = check_type("(let [xs (list 1 2 3 4)] (for [x xs :when (> x 2)] (* x 2)))")
      assert {:ok, [:List, :Int]} = result
    end
  end

  describe "for comprehension e2e" do
    test "basic map" do
      {:ok, mod} = Runner.compile_and_load(
        "(defn main [] (List :int) (for [x (list 1 2 3)] (* x 2)))",
        :ForBasic
      )
      assert Runner.call(mod, :main) == [2, 4, 6]
    end

    test "with :when filter" do
      {:ok, mod} = Runner.compile_and_load(
        "(defn main [] (List :int) (for [x (list 1 2 3 4) :when (> x 2)] (* x 2)))",
        :ForWhen
      )
      assert Runner.call(mod, :main) == [6, 8]
    end
  end

  # ═══════════════════════════════════════════════════════════════════
  # Thread-last (->>)
  # ═══════════════════════════════════════════════════════════════════

  describe "thread-last parsing" do
    test "basic thread-last desugars correctly" do
      ast = Parser.parse("(->> x (f a) (g b))")
      # (->> x (f a) (g b)) → (g b (f a x))
      assert {:call, :g, [:b, {:call, :f, [:a, :x], _}], _} = ast
    end

    test "bare symbol form" do
      ast = Parser.parse("(->> x f g)")
      # (->> x f g) → (g (f x))
      assert {:call, :g, [{:call, :f, [:x], _}], _} = ast
    end

    test "single value returns as-is" do
      ast = Parser.parse("(->> x)")
      assert :x = ast
    end
  end

  describe "thread-last e2e" do
    test "filter then map" do
      {:ok, mod} = Runner.compile_and_load(
        "(defn main [] (List :int) (->> (list 1 2 3 4) (filter (fn [x] (> x 2))) (map (fn [x] (* x 2)))))",
        :ThreadLastFilterMap
      )
      assert Runner.call(mod, :main) == [6, 8]
    end

    test "arithmetic threading" do
      {:ok, mod} = Runner.compile_and_load(
        "(defn main [] :int (->> 5 (+ 1) (* 2)))",
        :ThreadLastArith
      )
      # (->> 5 (+ 1) (* 2)) → (* 2 (+ 1 5)) → (* 2 6) → 12
      assert Runner.call(mod, :main) == 12
    end
  end
end
