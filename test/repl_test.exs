defmodule Vaisto.REPLTest do
  use ExUnit.Case
  alias Vaisto.REPL

  # We can't easily test the interactive loop, but we can test
  # the core functions by accessing the module's internal state

  describe "REPL state management" do
    test "initial state has primitives" do
      state = %REPL{
        env: initial_env(),
        defs: [],
        def_sources: [],
        counter: 0
      }

      assert Map.has_key?(state.env, :+)
      assert Map.has_key?(state.env, :-)
      assert state.defs == []
    end
  end

  describe "definition accumulation via Runner" do
    # Test the accumulating context behavior through Runner
    # which uses the same compilation approach

    test "can define and call function" do
      source = """
      (defn double [x] (* x 2))
      (double 21)
      """
      {:ok, mod} = Vaisto.Runner.compile_and_load(source, :REPLTest1)
      assert Vaisto.Runner.call(mod, :main) == 42
    end

    test "can define multiple functions that reference each other" do
      source = """
      (defn double [x] (* x 2))
      (defn quad [x] (double (double x)))
      (quad 5)
      """
      {:ok, mod} = Vaisto.Runner.compile_and_load(source, :REPLTest2)
      assert Vaisto.Runner.call(mod, :main) == 20
    end

    test "can define recursive functions" do
      source = """
      (defn fact [n]
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
      (defn make-point [] (point 10 20))
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
      defs1 = ["(defn double [x] (* x 2))"]

      # Step 2: Use double
      source2 = Enum.join(defs1 ++ ["(double 21)"], "\n")
      {:ok, mod2} = Vaisto.Runner.compile_and_load(source2, :REPLSim1)
      assert Vaisto.Runner.call(mod2, :main) == 42

      # Step 3: Define quad using double
      defs3 = defs1 ++ ["(defn quad [x] (double (double x)))"]
      source3 = Enum.join(defs3 ++ ["(quad 5)"], "\n")
      {:ok, mod3} = Vaisto.Runner.compile_and_load(source3, :REPLSim2)
      assert Vaisto.Runner.call(mod3, :main) == 20

      # Step 4: Define another function
      defs4 = defs3 ++ ["(defn octo [x] (quad (quad x)))"]
      source4 = Enum.join(defs4 ++ ["(octo 1)"], "\n")
      {:ok, mod4} = Vaisto.Runner.compile_and_load(source4, :REPLSim3)
      # quad(1) = 4, quad(4) = 16, so octo(1) = 16
      assert Vaisto.Runner.call(mod4, :main) == 16
    end

    test "later definitions can shadow earlier ones" do
      # In a real REPL, you could redefine functions
      # Here we just test that the latest definition wins

      source = """
      (defn foo [] 1)
      (defn foo [] 2)
      (foo)
      """
      {:ok, mod} = Vaisto.Runner.compile_and_load(source, :REPLShadow)
      # The second definition should be used
      assert Vaisto.Runner.call(mod, :main) == 2
    end
  end

  describe "type inference in REPL context" do
    test "polymorphic identity function" do
      source = """
      (defn id [x] x)
      (id 42)
      """
      {:ok, mod} = Vaisto.Runner.compile_and_load(source, :REPLPoly1)
      assert Vaisto.Runner.call(mod, :main) == 42
    end

    test "polymorphic function used multiple times" do
      source = """
      (defn id [x] x)
      (+ (id 1) (id 2))
      """
      {:ok, mod} = Vaisto.Runner.compile_and_load(source, :REPLPoly2)
      assert Vaisto.Runner.call(mod, :main) == 3
    end
  end

  # Helper to create initial env
  defp initial_env do
    %{
      :+ => {:fn, [:int, :int], :int},
      :- => {:fn, [:int, :int], :int},
      :* => {:fn, [:int, :int], :int},
      :/ => {:fn, [:int, :int], :int},
      :== => {:fn, [:any, :any], :bool},
      :< => {:fn, [:int, :int], :bool},
      :> => {:fn, [:int, :int], :bool},
      :<= => {:fn, [:int, :int], :bool},
      :>= => {:fn, [:int, :int], :bool},
      :!= => {:fn, [:any, :any], :bool}
    }
  end
end
