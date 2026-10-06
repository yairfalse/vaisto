defmodule Vaisto.TypeSystemRedesignTest do
  @moduledoc """
  TDD tests for type system improvements:
  1. Atom types - singleton vs universal
  2. Numeric types - int/float/num hierarchy
  3. Send operators - ! vs !! semantics
  """
  use ExUnit.Case, async: true

  alias Vaisto.Runner

  # =============================================================================
  # 1. ATOM TYPES
  # =============================================================================

  describe "atom types in expressions" do
    # CURRENT: Fails because :yes and :no are different singleton types
    # GOAL: Both branches should unify to :atom
    test "if branches with different atoms should unify" do
      code = "(if true :yes :no)"
      assert {:ok, result} = Runner.run(code)
      assert result in [:yes, :no]
    end

    test "if branches with same atom should work" do
      code = "(if true :same :same)"
      assert {:ok, :same} = Runner.run(code)
    end

    test "match with atom results should unify" do
      code = """
      (match 1
        [1 :one]
        [2 :two]
        [_ :other])
      """
      assert {:ok, :one} = Runner.run(code)
    end

    test "cond with atom results should unify" do
      code = """
      (cond
        [(> 1 2) :nope]
        [(< 1 2) :yes]
        [:else :fallback])
      """
      assert {:ok, :yes} = Runner.run(code)
    end

    test "let binding with atom should work" do
      code = "(let [x :hello] x)"
      assert {:ok, :hello} = Runner.run(code)
    end

    test "function returning atom should work" do
      code = """
      (defn get_status [] :ok)
      (defn main [] (get_status))
      """
      assert {:ok, :ok} = Runner.run(code)
    end

    test "atom in list should work" do
      code = "(list :a :b :c)"
      assert {:ok, [:a, :b, :c]} = Runner.run(code)
    end

    test "mixed atoms in list should unify to atom type" do
      # All elements should have type :atom
      code = "(head (list :first :second :third))"
      assert {:ok, :first} = Runner.run(code)
    end
  end

  describe "atom types in patterns" do
    # Patterns should remain specific (singleton types)
    test "match on specific atom" do
      code = """
      (match :yes
        [:yes 1]
        [:no 0])
      """
      assert {:ok, 1} = Runner.run(code)
    end

    test "match atom extracts value" do
      code = """
      (match :hello
        [x x])
      """
      assert {:ok, :hello} = Runner.run(code)
    end

    test "match rejects wrong atom at runtime" do
      # This should compile but fail at runtime (pattern match error)
      code = """
      (match :wrong
        [:yes 1]
        [:no 0])
      """
      # Should compile...
      assert {:ok, mod} = Runner.compile_and_load(code, :AtomMatchFail)
      # ...but fail at runtime (CaseClauseError since match compiles to case)
      assert_raise CaseClauseError, fn -> Runner.call(mod, :main) end
    end
  end

  # =============================================================================
  # 2. NUMERIC TYPES
  # =============================================================================

  describe "integer arithmetic" do
    test "int + int = int" do
      code = "(+ 1 2)"
      assert {:ok, 3} = Runner.run(code)
    end

    test "int - int = int" do
      code = "(- 10 3)"
      assert {:ok, 7} = Runner.run(code)
    end

    test "int * int = int" do
      code = "(* 6 7)"
      assert {:ok, 42} = Runner.run(code)
    end

    test "int / int = float (division always produces float)" do
      code = "(/ 10 3)"
      {:ok, result} = Runner.run(code)
      assert is_float(result)
      assert_in_delta result, 3.333, 0.01
    end

    test "exact division still returns float" do
      code = "(/ 10 2)"
      {:ok, result} = Runner.run(code)
      assert is_float(result)
      assert result == 5.0
    end
  end

  describe "float arithmetic" do
    # CURRENT: Fails because + only accepts :int
    # GOAL: + should accept :num (int or float)
    test "float + float = float" do
      code = "(+ 1.5 2.5)"
      assert {:ok, 4.0} = Runner.run(code)
    end

    test "float - float = float" do
      code = "(- 5.5 2.0)"
      assert {:ok, 3.5} = Runner.run(code)
    end

    test "float * float = float" do
      code = "(* 2.5 4.0)"
      assert {:ok, 10.0} = Runner.run(code)
    end

    test "float / float = float" do
      code = "(/ 7.5 2.5)"
      assert {:ok, 3.0} = Runner.run(code)
    end
  end

  describe "mixed numeric arithmetic" do
    # CURRENT: Fails
    # GOAL: Mixed should widen to float
    test "int + float = float" do
      code = "(+ 1 2.5)"
      {:ok, result} = Runner.run(code)
      assert is_float(result)
      assert result == 3.5
    end

    test "float + int = float" do
      code = "(+ 2.5 1)"
      {:ok, result} = Runner.run(code)
      assert is_float(result)
      assert result == 3.5
    end

    test "nested mixed arithmetic" do
      code = "(+ (* 2 3.0) (/ 10 4))"
      {:ok, result} = Runner.run(code)
      assert is_float(result)
      assert result == 8.5  # 6.0 + 2.5
    end
  end

  describe "numeric comparisons" do
    test "int comparisons" do
      assert {:ok, true} = Runner.run("(< 1 2)")
      assert {:ok, false} = Runner.run("(> 1 2)")
      assert {:ok, true} = Runner.run("(== 5 5)")
      assert {:ok, true} = Runner.run("(!= 5 6)")
    end

    test "float comparisons" do
      assert {:ok, true} = Runner.run("(< 1.0 2.0)")
      assert {:ok, true} = Runner.run("(> 2.5 1.5)")
    end

    test "mixed comparisons" do
      assert {:ok, true} = Runner.run("(< 1 2.0)")
      assert {:ok, true} = Runner.run("(> 2.5 2)")
      # == is exact equality of representations (liquid-core.md §7, D25)
      assert {:ok, false} = Runner.run("(== 5 5.0)")
      assert {:ok, true} = Runner.run("(== 5.0 5.0)")
    end
  end

  describe "numeric type annotations" do
    test "function with int param accepts int" do
      code = """
      (defn double [x :int] :int (* x 2))
      (defn main [] (double 21))
      """
      assert {:ok, 42} = Runner.run(code)
    end

    test "function with int param rejects float" do
      code = """
      (defn double [x :int] :int (* x 2))
      (defn main [] (double 21.0))
      """
      result = Runner.compile_and_load(code, :IntRejectsFloat)
      assert {:error, %Vaisto.Error{}} = result
    end

    test "function with num param accepts both" do
      code = """
      (defn double [x :num] :num (* x 2))
      (defn main [] (+ (double 21) (double 21.0)))
      """
      assert {:ok, 84.0} = Runner.run(code)
    end
  end

  # =============================================================================
  # 3. SEND OPERATORS
  # =============================================================================

  describe "! (safe send) with typed PIDs" do
    test "valid message to typed PID succeeds" do
      code = """
      (process counter 0
        :inc (+ state 1)
        :get state)

      (defn main []
        (let [pid (spawn counter 0)]
          (! pid :inc)))
      """
      assert {:ok, _} = Runner.compile_and_load(code, :SafeSendValid)
    end

    test "invalid message to typed PID fails at compile time" do
      code = """
      (process counter 0
        :inc (+ state 1))

      (defn main []
        (! (spawn counter 0) :invalid))
      """
      result = Runner.compile_and_load(code, :SafeSendInvalid)
      assert {:error, %Vaisto.Error{note: note}} = result
      assert note =~ "does not accept"
    end

    test "multiple valid messages" do
      code = """
      (process counter 0
        :inc (+ state 1)
        :dec (- state 1)
        :reset 0)

      (defn main []
        (let [pid (spawn counter 0)]
          (do
            (! pid :inc)
            (! pid :dec)
            (! pid :reset))))
      """
      assert {:ok, _} = Runner.compile_and_load(code, :SafeSendMultiple)
    end
  end

  describe "! (safe send) edge cases" do
    test "sending to non-PID fails" do
      code = "(! 42 :msg)"
      result = Runner.compile_and_load(code, :SendToInt)
      assert {:error, %Vaisto.Error{message: msg}} = result
      assert msg =~ "non-pid"
    end

    test "sending to string fails" do
      code = "(! \"not a pid\" :msg)"
      result = Runner.compile_and_load(code, :SendToString)
      assert {:error, %Vaisto.Error{message: msg}} = result
      assert msg =~ "non-pid"
    end

    test "sending to list fails" do
      code = "(! (list 1 2 3) :msg)"
      result = Runner.compile_and_load(code, :SendToList)
      assert {:error, %Vaisto.Error{message: msg}} = result
      assert msg =~ "non-pid"
    end
  end

  describe "!! (unsafe send)" do
    test "allows any message to typed PID" do
      code = """
      (process counter 0
        :inc (+ state 1))

      (defn main []
        (!! (spawn counter 0) :totally_invalid_message))
      """
      # Should compile - message not validated
      assert {:ok, _} = Runner.compile_and_load(code, :UnsafeSendAny)
    end

    test "allows untyped PID parameter" do
      code = """
      (process echo 0
        :ping state)

      (defn send_to [pid]
        (!! pid :anything))

      (defn main []
        (send_to (spawn echo 0)))
      """
      assert {:ok, _} = Runner.compile_and_load(code, :UnsafeSendUntyped)
    end

    test "still rejects obvious non-PIDs" do
      code = "(!! 42 :msg)"
      result = Runner.compile_and_load(code, :UnsafeSendInt)
      assert {:error, %Vaisto.Error{message: msg}} = result
      assert msg =~ "non-pid"
    end

    test "rejects string as PID" do
      code = "(!! \"nope\" :msg)"
      result = Runner.compile_and_load(code, :UnsafeSendString)
      assert {:error, %Vaisto.Error{message: msg}} = result
      assert msg =~ "non-pid"
    end

    test "accepts :any typed parameter" do
      # When a function takes an untyped param, !! should allow it
      code = """
      (defn broadcast [target msg]
        (!! target msg))

      (process server 0
        :ping state)

      (defn main []
        (broadcast (spawn server 0) :ping))
      """
      assert {:ok, _} = Runner.compile_and_load(code, :UnsafeAnyParam)
    end
  end

  describe "! vs !! semantic difference" do
    test "! validates, !! does not - same process" do
      valid_code = """
      (process counter 0
        :inc (+ state 1))

      (defn main []
        (! (spawn counter 0) :inc))
      """
      assert {:ok, _} = Runner.compile_and_load(valid_code, :SafeValid)

      invalid_safe = """
      (process counter 0
        :inc (+ state 1))

      (defn main []
        (! (spawn counter 0) :wrong))
      """
      assert {:error, _} = Runner.compile_and_load(invalid_safe, :SafeInvalid)

      invalid_unsafe = """
      (process counter 0
        :inc (+ state 1))

      (defn main []
        (!! (spawn counter 0) :wrong))
      """
      # !! allows it even though :wrong isn't valid
      assert {:ok, _} = Runner.compile_and_load(invalid_unsafe, :UnsafeAllows)
    end
  end

  # =============================================================================
  # INTEGRATION TESTS
  # =============================================================================

  describe "combined features" do
    test "function returning different atoms based on condition" do
      code = """
      (defn status [x]
        (if (> x 0) :positive :non_positive))

      (defn main []
        (status 5))
      """
      assert {:ok, :positive} = Runner.run(code)
    end

    test "numeric function with atom result" do
      code = """
      (defn sign [x]
        (cond
          [(> x 0) :positive]
          [(< x 0) :negative]
          [:else :zero]))

      (defn main []
        (sign (- 5 10)))
      """
      assert {:ok, :negative} = Runner.run(code)
    end

    test "process with mixed numeric state" do
      code = """
      (process accumulator 0.0
        :add_int (+ state 1)
        :add_float (+ state 0.5))

      (defn main []
        (let [pid (spawn accumulator 0.0)]
          (do
            (! pid :add_int)
            (! pid :add_float))))
      """
      assert {:ok, _} = Runner.compile_and_load(code, :MixedNumericProcess)
    end
  end
end
