defmodule Vaisto.CoreBackendParityTest do
  @moduledoc """
  Tests that verify parity between :elixir and :core backends.
  Each test runs the same code through both backends and compares results.

  This catches:
  - Missing to_core_expr clauses
  - Semantic differences between backends
  - Edge cases in code generation
  """
  use ExUnit.Case, async: true

  alias Vaisto.Runner

  # Helper to run code through both backends and compare
  defp run_both(code, test_name) do
    elixir_result = Runner.run(code, backend: :elixir)
    core_result = Runner.run(code, backend: :core)

    case {elixir_result, core_result} do
      {{:ok, ev}, {:ok, cv}} when ev == cv ->
        {:ok, ev}
      {{:ok, ev}, {:ok, cv}} ->
        {:mismatch, elixir: ev, core: cv}
      {{:ok, _}, {:error, err}} ->
        {:core_failed, err}
      {{:error, err}, {:ok, _}} ->
        {:elixir_failed, err}
      {{:error, e1}, {:error, e2}} ->
        {:both_failed, elixir: e1, core: e2}
    end
  end

  # Helper to compile through both backends
  defp compile_both(code, base_name) do
    elixir_mod = :"#{base_name}_Elixir"
    core_mod = :"#{base_name}_Core"

    elixir_result = Runner.compile_and_load(code, elixir_mod, backend: :elixir)
    core_result = Runner.compile_and_load(code, core_mod, backend: :core)

    {elixir_result, core_result}
  end

  # =============================================================================
  # BASIC EXPRESSIONS
  # =============================================================================

  describe "literals" do
    test "integers" do
      assert {:ok, 42} = run_both("42", :int_lit)
      assert {:ok, -17} = run_both("-17", :neg_int)
      assert {:ok, 0} = run_both("0", :zero)
    end

    test "floats" do
      assert {:ok, 3.14} = run_both("3.14", :float_lit)
      assert {:ok, -2.5} = run_both("-2.5", :neg_float)
      assert {:ok, 0.0} = run_both("0.0", :zero_float)
    end

    test "booleans" do
      assert {:ok, true} = run_both("true", :bool_true)
      assert {:ok, false} = run_both("false", :bool_false)
    end

    test "atoms" do
      assert {:ok, :hello} = run_both(":hello", :atom_lit)
      assert {:ok, :ok} = run_both(":ok", :atom_ok)
      assert {:ok, :error} = run_both(":error", :atom_error)
    end

    test "strings" do
      assert {:ok, "hello"} = run_both("\"hello\"", :string_lit)
      assert {:ok, ""} = run_both("\"\"", :empty_string)
      assert {:ok, "with spaces"} = run_both("\"with spaces\"", :string_spaces)
    end

    test "unit" do
      assert {:ok, nil} = run_both("()", :unit_lit)
    end
  end

  describe "arithmetic" do
    test "integer operations" do
      assert {:ok, 5} = run_both("(+ 2 3)", :add_int)
      assert {:ok, 7} = run_both("(- 10 3)", :sub_int)
      assert {:ok, 24} = run_both("(* 4 6)", :mul_int)
    end

    test "float operations" do
      assert {:ok, 5.5} = run_both("(+ 2.5 3.0)", :add_float)
      assert {:ok, 1.5} = run_both("(- 4.0 2.5)", :sub_float)
      assert {:ok, 6.0} = run_both("(* 2.0 3.0)", :mul_float)
    end

    test "division always returns float" do
      result = run_both("(/ 10 4)", :div_int)
      assert {:ok, 2.5} = result
    end

    test "mixed int/float" do
      assert {:ok, 5.5} = run_both("(+ 2 3.5)", :mixed_add)
      assert {:ok, 6.0} = run_both("(* 2 3.0)", :mixed_mul)
    end

    test "nested arithmetic" do
      assert {:ok, 14} = run_both("(+ (* 2 3) (+ 4 4))", :nested_arith)
      assert {:ok, 20} = run_both("(* (+ 2 3) (- 10 6))", :nested_arith2)
    end

    test "deeply nested" do
      # + only takes 2 args, so nest properly
      assert {:ok, 55} = run_both("(+ (+ (+ (+ 1 2) 3) 4) (+ (+ (+ 5 6) (+ 7 8)) (+ 9 10)))", :deep_nest)
    end
  end

  describe "comparisons" do
    test "integer comparisons" do
      assert {:ok, true} = run_both("(< 1 2)", :lt_true)
      assert {:ok, false} = run_both("(< 2 1)", :lt_false)
      assert {:ok, true} = run_both("(> 5 3)", :gt_true)
      assert {:ok, true} = run_both("(== 5 5)", :eq_true)
      assert {:ok, false} = run_both("(== 5 6)", :eq_false)
      assert {:ok, true} = run_both("(!= 5 6)", :neq_true)
    end

    test "float comparisons" do
      assert {:ok, true} = run_both("(< 1.5 2.5)", :lt_float)
      assert {:ok, true} = run_both("(> 3.0 2.0)", :gt_float)
    end

    test "mixed comparisons" do
      assert {:ok, true} = run_both("(< 1 2.5)", :lt_mixed)
      assert {:ok, true} = run_both("(> 3.0 2)", :gt_mixed)
    end
  end

  describe "conditionals" do
    test "if true branch" do
      assert {:ok, 1} = run_both("(if true 1 2)", :if_true)
    end

    test "if false branch" do
      assert {:ok, 2} = run_both("(if false 1 2)", :if_false)
    end

    test "if with expression condition" do
      assert {:ok, :yes} = run_both("(if (> 5 3) :yes :no)", :if_expr_cond)
      assert {:ok, :no} = run_both("(if (< 5 3) :yes :no)", :if_expr_cond2)
    end

    test "nested if" do
      code = "(if true (if false 1 2) 3)"
      assert {:ok, 2} = run_both(code, :nested_if)
    end

    test "if with different atom branches" do
      assert {:ok, :positive} = run_both("(if (> 5 0) :positive :negative)", :if_atoms)
    end
  end

  describe "let bindings" do
    test "simple let" do
      assert {:ok, 42} = run_both("(let [x 42] x)", :let_simple)
    end

    test "let with expression" do
      assert {:ok, 10} = run_both("(let [x (+ 3 7)] x)", :let_expr)
    end

    test "multiple bindings" do
      assert {:ok, 7} = run_both("(let [x 3 y 4] (+ x y))", :let_multi)
    end

    test "nested lets" do
      assert {:ok, 6} = run_both("(let [x 1] (let [y 2] (let [z 3] (+ x (+ y z)))))", :let_nested)
    end

    test "shadowing" do
      assert {:ok, 20} = run_both("(let [x 10] (let [x 20] x))", :let_shadow)
    end

    test "using outer in inner" do
      # x=10, y=x+5=15, x+y=10+15=25
      assert {:ok, 25} = run_both("(let [x 10] (let [y (+ x 5)] (+ x y)))", :let_outer_inner)
    end
  end

  describe "lists" do
    test "empty list" do
      assert {:ok, []} = run_both("(list)", :empty_list)
    end

    test "integer list" do
      assert {:ok, [1, 2, 3]} = run_both("(list 1 2 3)", :int_list)
    end

    test "atom list" do
      assert {:ok, [:a, :b, :c]} = run_both("(list :a :b :c)", :atom_list)
    end

    test "head" do
      assert {:ok, 1} = run_both("(head (list 1 2 3))", :head_list)
    end

    test "tail" do
      assert {:ok, [2, 3]} = run_both("(tail (list 1 2 3))", :tail_list)
    end

    test "cons" do
      assert {:ok, [0, 1, 2]} = run_both("(cons 0 (list 1 2))", :cons_list)
    end

    test "length" do
      assert {:ok, 3} = run_both("(length (list 1 2 3))", :length_list)
      assert {:ok, 0} = run_both("(length (list))", :length_empty)
    end

    test "empty?" do
      assert {:ok, true} = run_both("(empty? (list))", :empty_true)
      assert {:ok, false} = run_both("(empty? (list 1))", :empty_false)
    end

    test "nested list operations" do
      assert {:ok, 1} = run_both("(head (tail (cons 0 (list 1 2))))", :nested_list_ops)
    end
  end

  describe "functions" do
    test "simple function" do
      code = """
      (defn double [x] (* x 2))
      (defn main [] (double 21))
      """
      assert {:ok, 42} = run_both(code, :simple_fn)
    end

    test "function with type annotation" do
      code = """
      (defn add [a :int b :int] :int (+ a b))
      (defn main [] (add 3 4))
      """
      assert {:ok, 7} = run_both(code, :typed_fn)
    end

    test "recursive function" do
      code = """
      (defn factorial [n]
        (if (== n 0)
          1
          (* n (factorial (- n 1)))))
      (defn main [] (factorial 5))
      """
      assert {:ok, 120} = run_both(code, :recursive_fn)
    end

    test "mutual recursion" do
      code = """
      (defn is-even [n]
        (if (== n 0)
          true
          (is-odd (- n 1))))
      (defn is-odd [n]
        (if (== n 0)
          false
          (is-even (- n 1))))
      (defn main [] (is-even 10))
      """
      assert {:ok, true} = run_both(code, :mutual_rec)
    end

    test "higher-order function" do
      code = """
      (defn apply-twice [f x] (f (f x)))
      (defn inc [n] (+ n 1))
      (defn main [] (apply-twice inc 5))
      """
      assert {:ok, 7} = run_both(code, :higher_order)
    end

    test "function returning function" do
      code = """
      (defn make-adder [n] (fn [x] (+ x n)))
      (defn main []
        (let [add5 (make-adder 5)]
          (add5 10)))
      """
      assert {:ok, 15} = run_both(code, :fn_returning_fn)
    end
  end

  describe "anonymous functions" do
    test "simple lambda" do
      code = """
      (defn main []
        (let [f (fn [x] (* x 2))]
          (f 21)))
      """
      assert {:ok, 42} = run_both(code, :simple_lambda)
    end

    test "lambda with multiple args" do
      code = """
      (defn main []
        (let [f (fn [a b] (+ a b))]
          (f 3 4)))
      """
      assert {:ok, 7} = run_both(code, :multi_arg_lambda)
    end

    test "closure capturing variable" do
      code = """
      (defn main []
        (let [x 10]
          (let [f (fn [y] (+ x y))]
            (f 5))))
      """
      assert {:ok, 15} = run_both(code, :closure)
    end

    test "nested closures" do
      code = """
      (defn main []
        (let [x 1]
          (let [y 2]
            (let [f (fn [z] (+ x (+ y z)))]
              (f 3)))))
      """
      assert {:ok, 6} = run_both(code, :nested_closure)
    end
  end

  describe "match expressions" do
    test "match on integer" do
      code = """
      (defn main []
        (match 2
          [1 :one]
          [2 :two]
          [_ :other]))
      """
      assert {:ok, :two} = run_both(code, :match_int)
    end

    test "match on atom" do
      code = """
      (defn main []
        (match :hello
          [:hello :greeting]
          [:bye :farewell]
          [_ :unknown]))
      """
      assert {:ok, :greeting} = run_both(code, :match_atom)
    end

    test "match with variable binding" do
      code = """
      (defn main []
        (match 42
          [x x]))
      """
      assert {:ok, 42} = run_both(code, :match_var)
    end

    test "match on list" do
      code = """
      (defn main []
        (match (list 1 2 3)
          [[] :empty]
          [[h | t] h]))
      """
      assert {:ok, 1} = run_both(code, :match_list)
    end

    test "match with nested pattern" do
      code = """
      (defn main []
        (match (list 1 2 3)
          [[a | [b | rest]] (+ a b)]))
      """
      assert {:ok, 3} = run_both(code, :match_nested)
    end

    test "match wildcard" do
      code = """
      (defn main []
        (match 999
          [1 :one]
          [_ :other]))
      """
      assert {:ok, :other} = run_both(code, :match_wildcard)
    end
  end

  describe "cond expressions" do
    test "simple cond" do
      code = """
      (defn main []
        (cond
          [(< 1 0) :nope]
          [(> 1 0) :yes]
          [:else :fallback]))
      """
      assert {:ok, :yes} = run_both(code, :cond_simple)
    end

    test "cond falls through" do
      code = """
      (defn main []
        (cond
          [false :a]
          [false :b]
          [:else :c]))
      """
      assert {:ok, :c} = run_both(code, :cond_fallthrough)
    end

    test "cond with expressions" do
      code = """
      (defn main []
        (let [x 5]
          (cond
            [(< x 0) :negative]
            [(== x 0) :zero]
            [:else :positive])))
      """
      assert {:ok, :positive} = run_both(code, :cond_expr)
    end
  end

  describe "do blocks" do
    test "simple do" do
      code = """
      (defn main []
        (do
          1
          2
          3))
      """
      assert {:ok, 3} = run_both(code, :do_simple)
    end

    test "do with side effects simulation" do
      code = """
      (defn main []
        (let [x 1]
          (do
            (+ x 1)
            (+ x 2)
            (+ x 10))))
      """
      assert {:ok, 11} = run_both(code, :do_sequence)
    end
  end

  describe "tuples" do
    test "simple tuple" do
      code = "(tuple :ok 42)"
      assert {:ok, {:ok, 42}} = run_both(code, :tuple_simple)
    end

    test "nested tuple" do
      code = "(tuple :error (tuple :reason \"failed\"))"
      assert {:ok, {:error, {:reason, "failed"}}} = run_both(code, :tuple_nested)
    end

    test "tuple in match" do
      code = """
      (defn main []
        (match (tuple :ok 42)
          [(tuple :ok val) val]
          [(tuple :error _) 0]))
      """
      assert {:ok, 42} = run_both(code, :tuple_match)
    end
  end

  describe "map/filter/fold" do
    test "map over list" do
      code = """
      (defn double [x] (* x 2))
      (defn main [] (map double (list 1 2 3)))
      """
      assert {:ok, [2, 4, 6]} = run_both(code, :map_list)
    end

    test "map with lambda" do
      code = """
      (defn main [] (map (fn [x] (+ x 1)) (list 1 2 3)))
      """
      assert {:ok, [2, 3, 4]} = run_both(code, :map_lambda)
    end

    test "filter list" do
      code = """
      (defn is-positive [x] (> x 0))
      (defn main [] (filter is-positive (list -1 2 -3 4 0)))
      """
      assert {:ok, [2, 4]} = run_both(code, :filter_list)
    end

    test "filter with lambda" do
      code = """
      (defn main [] (filter (fn [x] (> x 2)) (list 1 2 3 4 5)))
      """
      assert {:ok, [3, 4, 5]} = run_both(code, :filter_lambda)
    end

    test "fold left" do
      code = """
      (defn main [] (fold + 0 (list 1 2 3 4 5)))
      """
      assert {:ok, 15} = run_both(code, :fold_sum)
    end

    test "fold with lambda" do
      code = """
      (defn main [] (fold (fn [acc x] (+ acc (* x x))) 0 (list 1 2 3)))
      """
      assert {:ok, 14} = run_both(code, :fold_lambda)
    end

    test "chained operations" do
      code = """
      (defn main []
        (fold + 0
          (map (fn [x] (* x 2))
            (filter (fn [x] (> x 0)) (list -1 2 3 -4 5)))))
      """
      assert {:ok, 20} = run_both(code, :chained_ops)
    end
  end

  describe "algebraic data types" do
    test "simple sum type" do
      code = """
      (deftype Color (Red) (Green) (Blue))
      (defn main [] (Red))
      """
      assert {:ok, {:Red}} = run_both(code, :simple_adt)
    end

    test "sum type with data" do
      code = """
      (deftype Result (Ok value) (Err msg))
      (defn main [] (Ok 42))
      """
      assert {:ok, {:Ok, 42}} = run_both(code, :adt_with_data)
    end

    test "match on sum type" do
      code = """
      (deftype Maybe (Just value) (Nothing))
      (defn unwrap-or [m default]
        (match m
          [(Just v) v]
          [(Nothing) default]))
      (defn main [] (unwrap-or (Just 42) 0))
      """
      assert {:ok, 42} = run_both(code, :match_adt)
    end

    test "product type (record)" do
      code = """
      (deftype Point [x :int y :int])
      (defn main [] (Point 3 4))
      """
      result = run_both(code, :product_type)
      # Records might be represented differently
      case result do
        {:ok, %{x: 3, y: 4}} -> :ok
        {:ok, {3, 4}} -> :ok
        {:ok, {:Point, 3, 4}} -> :ok
        other -> flunk("Unexpected result: #{inspect(other)}")
      end
    end
  end

  describe "string operations" do
    test "string concatenation" do
      code = """
      (defn main [] (str "hello" " " "world"))
      """
      assert {:ok, "hello world"} = run_both(code, :str_concat)
    end

    test "string from int" do
      code = """
      (defn main [] (str "value: " 42))
      """
      assert {:ok, "value: 42"} = run_both(code, :str_int)
    end
  end

  # =============================================================================
  # EDGE CASES AND STRESS TESTS
  # =============================================================================

  describe "edge cases" do
    test "deeply nested function calls" do
      code = """
      (defn id [x] x)
      (defn main [] (id (id (id (id (id 42))))))
      """
      assert {:ok, 42} = run_both(code, :deep_calls)
    end

    test "large list" do
      # Generate a list of 100 numbers
      nums = Enum.join(1..100, " ")
      code = "(fold + 0 (list #{nums}))"
      assert {:ok, 5050} = run_both(code, :large_list)
    end

    test "complex expression" do
      code = """
      (defn fib [n]
        (if (< n 2)
          n
          (+ (fib (- n 1)) (fib (- n 2)))))
      (defn main [] (fib 10))
      """
      assert {:ok, 55} = run_both(code, :fibonacci)
    end

    test "multiple function definitions" do
      code = """
      (defn a [] 1)
      (defn b [] (+ (a) 1))
      (defn c [] (+ (b) 1))
      (defn d [] (+ (c) 1))
      (defn e [] (+ (d) 1))
      (defn main [] (e))
      """
      assert {:ok, 5} = run_both(code, :multi_fns)
    end

    test "variable shadowing in match" do
      code = """
      (defn main []
        (let [x 10]
          (match 5
            [x (+ x 100)])))
      """
      # x in match shadows outer x, so we get 5 + 100 = 105
      assert {:ok, 105} = run_both(code, :shadow_match)
    end
  end

  describe "error handling parity" do
    test "both fail on type error" do
      code = "(+ 1 :atom)"
      {elixir_result, core_result} = compile_both(code, :TypeErr)
      assert {:error, _} = elixir_result
      assert {:error, _} = core_result
    end

    test "both fail on undefined variable" do
      code = "(+ x 1)"
      {elixir_result, core_result} = compile_both(code, :UndefVar)
      assert {:error, _} = elixir_result
      assert {:error, _} = core_result
    end

    test "both fail on undefined function" do
      code = "(nonexistent 1 2 3)"
      {elixir_result, core_result} = compile_both(code, :UndefFn)
      assert {:error, _} = elixir_result
      assert {:error, _} = core_result
    end
  end
end
