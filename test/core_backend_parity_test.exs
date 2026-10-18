defmodule Vaisto.CoreBackendParityTest do
  @moduledoc """
  Native BEAM execution agrees with the independent Core evaluator.
  """
  use ExUnit.Case, async: true

  alias Vaisto.Runner

  # No second compiler backend: compare two independent execution paths.
  defp run_both(code, test_name, steps \\ []) do
    evaluator_result = with {:ok, core} <- Vaisto.Compilation.check_liquid(code), do: Vaisto.Liquid.Eval.run(core, :main, [], handler: {:script, steps})
    name = :"Parity_#{System.unique_integer([:positive])}"
    native_result = with {:ok, module, _} <- Vaisto.Compilation.compile(code, name) do
      try do
        Vaisto.Liquid.Runtime.run(module, :main, [], handler: {:script, steps})
      after
        :code.purge(module)
        :code.delete(module)
      end
    end

    case {evaluator_result, native_result} do
      {{:ok, ev}, {:ok, cv}} when ev == cv ->
        {:ok, ev}
      {{:ok, ev}, {:ok, cv}} ->
        {:mismatch, test_name, evaluator: ev, native: cv}
      {{:ok, _}, {:error, err}} ->
        {:native_failed, test_name, err}
      {{:error, err}, {:ok, _}} ->
        {:evaluator_failed, test_name, err}
      {{:error, e1}, {:error, e2}} ->
        {:both_failed, test_name, evaluator: e1, native: e2}
    end
  end

  # Both the Core checking gate and native compiler must reject invalid code.
  defp compile_both(code, base_name) do
    core_mod = :"#{base_name}_Core"

    elixir_result = Vaisto.Compilation.check_liquid(code)
    core_result = Runner.compile_and_load(code, core_mod)

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
      result = run_both("(/ (int-to-float 10) (int-to-float 4))", :div_int)
      assert {:ok, 2.5} = result
    end

    test "mixed int/float" do
      assert {:ok, 5.5} = run_both("(+ (int-to-float 2) 3.5)", :mixed_add)
      assert {:ok, 6.0} = run_both("(* (int-to-float 2) 3.0)", :mixed_mul)
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
      assert {:ok, true} = run_both("(< (int-to-float 1) 2.5)", :lt_mixed)
      assert {:ok, true} = run_both("(> 3.0 (int-to-float 2))", :gt_mixed)
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
      assert {:ok, []} = run_both("(the (List :int) (list))", :empty_list)
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
      assert {:ok, 0} = run_both("(length (the (List :int) (list)))", :length_empty)
    end

    test "empty?" do
      assert {:ok, true} = run_both("(empty? (the (List :int) (list)))", :empty_true)
      assert {:ok, false} = run_both("(empty? (list 1))", :empty_false)
    end

    test "nested list operations" do
      assert {:ok, 1} = run_both("(head (tail (cons 0 (list 1 2))))", :nested_list_ops)
    end
  end

  describe "functions" do
    test "simple function" do
      code = """
      (defn double [x :int] :int (* x 2))
      (defn main [] :int (double 21))
      """
      assert {:ok, 42} = run_both(code, :simple_fn)
    end

    test "function with type annotation" do
      code = """
      (defn add [a :int b :int] :int (+ a b))
      (defn main [] :int (add 3 4))
      """
      assert {:ok, 7} = run_both(code, :typed_fn)
    end

    test "recursive function" do
      code = """
      (defn factorial [n :int] :int
        (if (== n 0)
          1
          (* n (factorial (- n 1)))))
      (defn main [] :int (factorial 5))
      """
      assert {:ok, 120} = run_both(code, :recursive_fn)
    end

    test "mutual recursion" do
      code = """
      (defn is-even [n :int] :bool
        (if (== n 0)
          true
          (is-odd (- n 1))))
      (defn is-odd [n :int] :bool
        (if (== n 0)
          false
          (is-even (- n 1))))
      (defn main [] :bool (is-even 10))
      """
      assert {:ok, true} = run_both(code, :mutual_rec)
    end

    test "higher-order function" do
      code = """
      (defn apply-twice [f (Fn :int :int) x :int] :int (f (f x)))
      (defn inc [n :int] :int (+ n 1))
      (defn main [] :int (apply-twice inc 5))
      """
      assert {:ok, 7} = run_both(code, :higher_order)
    end

    test "function returning function" do
      code = """
      (defn make-adder [n :int] (Fn :int :int) (fn [x] (+ x n)))
      (defn main [] :int
        (let [add5 (make-adder 5)]
          (add5 10)))
      """
      assert {:ok, 15} = run_both(code, :fn_returning_fn)
    end
  end

  describe "anonymous functions" do
    test "simple lambda" do
      code = """
      (defn main [] :int
        (let [f (fn [x] (* x 2))]
          (f 21)))
      """
      assert {:ok, 42} = run_both(code, :simple_lambda)
    end

    test "lambda with multiple args" do
      code = """
      (defn main [] :int
        (let [f (fn [a b] (+ a b))]
          (f 3 4)))
      """
      assert {:ok, 7} = run_both(code, :multi_arg_lambda)
    end

    test "closure capturing variable" do
      code = """
      (defn main [] :int
        (let [x 10]
          (let [f (fn [y] (+ x y))]
            (f 5))))
      """
      assert {:ok, 15} = run_both(code, :closure)
    end

    test "nested closures" do
      code = """
      (defn main [] :int
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
      (defn main [] :atom
        (match 2
          [1 :one]
          [2 :two]
          [_ :other]))
      """
      assert {:ok, :two} = run_both(code, :match_int)
    end

    test "match on atom" do
      code = """
      (defn main [] :atom
        (match :hello
          [:hello :greeting]
          [:bye :farewell]
          [_ :unknown]))
      """
      assert {:ok, :greeting} = run_both(code, :match_atom)
    end

    test "match with variable binding" do
      code = """
      (defn main [] :int
        (match 42
          [x x]))
      """
      assert {:ok, 42} = run_both(code, :match_var)
    end

    test "match on list" do
      code = """
      (defn main [] :int
        (match (list 1 2 3)
          [[] 0]
          [[h | t] h]))
      """
      assert {:ok, 1} = run_both(code, :match_list)
    end

    test "match with nested pattern" do
      code = """
      (defn main [] :int
        (match (list 1 2 3)
          [[a | [b | rest]] (+ a b)]
          [_ 0]))
      """
      assert {:ok, 3} = run_both(code, :match_nested)
    end

    test "match wildcard" do
      code = """
      (defn main [] :atom
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
      (defn main [] :atom
        (cond
          [(< 1 0) :nope]
          [(> 1 0) :yes]
          [:else :fallback]))
      """
      assert {:ok, :yes} = run_both(code, :cond_simple)
    end

    test "cond falls through" do
      code = """
      (defn main [] :atom
        (cond
          [false :a]
          [false :b]
          [:else :c]))
      """
      assert {:ok, :c} = run_both(code, :cond_fallthrough)
    end

    test "cond with expressions" do
      code = """
      (defn main [] :atom
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
      (defn main [] :int
        (do
          1
          2
          3))
      """
      assert {:ok, 3} = run_both(code, :do_simple)
    end

    test "do with side effects simulation" do
      code = """
      (defn main [] :int
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
      (defn main [] :int
        (match (tuple :ok 42)
          [{:ok val} val]
          [{:error _} 0]
          [_ 0]))
      """
      assert {:ok, 42} = run_both(code, :tuple_match)
    end
  end

  describe "map/filter/fold" do
    test "map over list" do
      code = """
      (defn double [x :int] :int (* x 2))
      (defn main [] (List :int) (map double (list 1 2 3)))
      """
      assert {:ok, [2, 4, 6]} = run_both(code, :map_list)
    end

    test "map with lambda" do
      code = """
      (defn main [] (List :int) (map (fn [x] (+ x 1)) (list 1 2 3)))
      """
      assert {:ok, [2, 3, 4]} = run_both(code, :map_lambda)
    end

    test "filter list" do
      code = """
      (defn is-positive [x :int] :bool (> x 0))
      (defn main [] (List :int) (filter is-positive (list -1 2 -3 4 0)))
      """
      assert {:ok, [2, 4]} = run_both(code, :filter_list)
    end

    test "filter with lambda" do
      code = """
      (defn main [] (List :int) (filter (fn [x] (> x 2)) (list 1 2 3 4 5)))
      """
      assert {:ok, [3, 4, 5]} = run_both(code, :filter_lambda)
    end

    test "fold left" do
      code = """
      (defn main [] :int (fold + 0 (list 1 2 3 4 5)))
      """
      assert {:ok, 15} = run_both(code, :fold_sum)
    end

    test "fold with lambda" do
      code = """
      (defn main [] :int (fold (fn [acc x] (+ acc (* x x))) 0 (list 1 2 3)))
      """
      assert {:ok, 14} = run_both(code, :fold_lambda)
    end

    test "chained operations" do
      code = """
      (defn main [] :int
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
      (defn main [] :Color (Red))
      """
      assert {:ok, {:Red}} = run_both(code, :simple_adt)
    end

    test "sum type with data" do
      code = """
      (deftype Result (Ok value) (Err msg))
      (defn main [] (Result :int :string) (Ok 42))
      """
      assert {:ok, {:Ok, 42}} = run_both(code, :adt_with_data)
    end

    test "match on sum type" do
      code = """
      (deftype Maybe (Just value) (Nothing))
      (defn unwrap-or [m (Maybe :a) default :a] :a
        (match m
          [(Just v) v]
          [(Nothing) default]))
      (defn main [] :int (unwrap-or (Just 42) 0))
      """
      assert {:ok, 42} = run_both(code, :match_adt)
    end

    test "product type (record)" do
      code = """
      (deftype Point [x :int y :int])
      (defn main [] :Point (Point 3 4))
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
      (defn main [] :string (str "hello" " " "world"))
      """
      assert {:ok, "hello world"} = run_both(code, :str_concat)
    end

    test "string from int" do
      code = """
      (defn main [] :string (str "value: " 42))
      """
      assert {:ok, "value: 42"} = run_both(code, :str_int)
    end

    test "str with multiple mixed-type args" do
      code = ~s|(str "x" 42 true)|
      assert {:ok, "x42true"} = run_both(code, :str_multi_mixed)
    end
  end

  # =============================================================================
  # EDGE CASES AND STRESS TESTS
  # =============================================================================

  describe "edge cases" do
    test "deeply nested function calls" do
      code = """
      (defn id [x :int] :int x)
      (defn main [] :int (id (id (id (id (id 42))))))
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
      (defn fib [n :int] :int
        (if (< n 2)
          n
          (+ (fib (- n 1)) (fib (- n 2)))))
      (defn main [] :int (fib 10))
      """
      assert {:ok, 55} = run_both(code, :fibonacci)
    end

    test "multiple function definitions" do
      code = """
      (defn a [] :int 1)
      (defn b [] :int (+ (a) 1))
      (defn c [] :int (+ (b) 1))
      (defn d [] :int (+ (c) 1))
      (defn e [] :int (+ (d) 1))
      (defn main [] :int (e))
      """
      assert {:ok, 5} = run_both(code, :multi_fns)
    end

    test "variable shadowing in match" do
      code = """
      (defn main [] :int
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

  # =============================================================================
  # TRY/CATCH/AFTER
  # =============================================================================

  describe "try/catch" do
    test "catch error" do
      code = "(extern erlang:error [:atom] :atom) (try (erlang:error :boom) [catch [:error e :caught]])"
      assert {:ok, :caught} = run_both(code, "try_catch_error", [{:external, [:erlang, :error, [:boom]], {:raised, :error, :boom}}])
    end

    test "catch throw" do
      code = "(extern erlang:throw [:int] :int) (try (erlang:throw 42) [catch [:throw v (match (decode (Tuple :atom :atom :int) (up :Reason v)) [(Core.Ok {_ _ n}) n] [_ (crash :bad_decode)])]])"
      assert {:ok, 42} = run_both(code, "try_catch_throw", [{:external, [:erlang, :throw, [42]], {:raised, :throw, 42}}])
    end

    test "no exception passes through" do
      assert {:ok, 42} = run_both(~s|(try 42 [catch [:error e 0]])|, "try_no_exception")
    end

    test "try/catch/after" do
      code = "(extern erlang:put [:atom :bool] :dyn) (extern erlang:get [:atom] :bool) (do (erlang:put :after_ran false) (try 42 [catch [:error e 0]] [after (erlang:put :after_ran true)]) (erlang:get :after_ran))"
      steps = [{:external, [:erlang, :put, [:after_ran, false]], {:ok, :undefined}}, {:external, [:erlang, :put, [:after_ran, true]], {:ok, false}}, {:external, [:erlang, :get, [:after_ran]], {:ok, true}}]
      assert {:ok, true} = run_both(code, "try_catch_after", steps)
    end
  end
end
