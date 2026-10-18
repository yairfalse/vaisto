defmodule Vaisto.REPLTest do
  use ExUnit.Case

  describe "definition accumulation via Runner" do
    # Test the accumulating context behavior through Runner
    # which uses the same compilation approach

    test "can define and call function" do
      source = """
      (defn double [x :int] :int (* x 2))
      (double 21)
      """
      {:ok, mod} = Vaisto.Runner.compile_and_load(source, :REPLTest1)
      assert Vaisto.Runner.call(mod, :main) == 42
    end

    test "can define multiple functions that reference each other" do
      source = """
      (defn double [x :int] :int (* x 2))
      (defn quad [x :int] :int (double (double x)))
      (quad 5)
      """
      {:ok, mod} = Vaisto.Runner.compile_and_load(source, :REPLTest2)
      assert Vaisto.Runner.call(mod, :main) == 20
    end

    test "can define recursive functions" do
      source = """
      (defn fact [n :int] :int
        (if (== n 0)
          1
          (* n (fact (- n 1)))))
      (fact 5)
      """
      {:ok, mod} = Vaisto.Runner.compile_and_load(source, :REPLTest3)
      assert Vaisto.Runner.call(mod, :main) == 120
    end

    test "can define and use records" do
      source = """
      (deftype point [x :int y :int])
      (defn make-point [] :point (point 10 20))
      (match (make-point)
        [(point x y) (+ x y)])
      """
      {:ok, mod} = Vaisto.Runner.compile_and_load(source, :REPLTest4)
      assert Vaisto.Runner.call(mod, :main) == 30
    end

    test "type errors are caught" do
      source = "(+ 1 :atom)"
      result = Vaisto.Runner.compile_and_load(source, :REPLTestBad)
      assert {:error, _} = result
    end
  end

  describe "incremental compilation simulation" do
    # Simulate what the REPL does: accumulate defs and recompile

    test "simulates REPL session" do
      # Step 1: Define double
      defs1 = ["(defn double [x :int] :int (* x 2))"]

      # Step 2: Use double
      source2 = Enum.join(defs1 ++ ["(double 21)"], "\n")
      {:ok, mod2} = Vaisto.Runner.compile_and_load(source2, :REPLSim1)
      assert Vaisto.Runner.call(mod2, :main) == 42

      # Step 3: Define quad using double
      defs3 = defs1 ++ ["(defn quad [x :int] :int (double (double x)))"]
      source3 = Enum.join(defs3 ++ ["(quad 5)"], "\n")
      {:ok, mod3} = Vaisto.Runner.compile_and_load(source3, :REPLSim2)
      assert Vaisto.Runner.call(mod3, :main) == 20

      # Step 4: Define another function
      defs4 = defs3 ++ ["(defn octo [x :int] :int (quad (quad x)))"]
      source4 = Enum.join(defs4 ++ ["(octo 1)"], "\n")
      {:ok, mod4} = Vaisto.Runner.compile_and_load(source4, :REPLSim3)
      # quad(1) = 4, quad(4) = 16, so octo(1) = 16
      assert Vaisto.Runner.call(mod4, :main) == 16
    end

    test "whole programs reject duplicate definitions; the actual REPL handles replacement" do
      # Interactive transactional replacement is exercised by NativeREPLTest.

      source = """
      (defn foo [] :int 1)
      (defn foo [] :int 2)
      (foo)
      """
      assert {:error, error} = Vaisto.Runner.compile_and_load(source, :REPLShadow)
      assert error.message =~ "defined twice"
    end
  end

  describe "explicit polymorphic signatures in REPL context" do
    test "polymorphic identity function" do
      source = """
      (defn id [x :a] :a x)
      (id 42)
      """
      {:ok, mod} = Vaisto.Runner.compile_and_load(source, :REPLPoly1)
      assert Vaisto.Runner.call(mod, :main) == 42
    end

    test "polymorphic function used multiple times" do
      source = """
      (defn id [x :a] :a x)
      (+ (id 1) (id 2))
      """
      {:ok, mod} = Vaisto.Runner.compile_and_load(source, :REPLPoly2)
      assert Vaisto.Runner.call(mod, :main) == 3
    end
  end

end
