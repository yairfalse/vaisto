defmodule Vaisto.EmitterTest do
  use ExUnit.Case
  alias Vaisto.Emitter

  describe "to_elixir/1 literals" do
    test "integers" do
      assert Emitter.to_elixir({:lit, :int, 42}) == 42
    end

    test "floats" do
      assert Emitter.to_elixir({:lit, :float, 3.14}) == 3.14
    end

    test "atoms" do
      assert Emitter.to_elixir({:lit, :atom, :ok}) == :ok
    end

    test "booleans" do
      assert Emitter.to_elixir({:lit, :bool, true}) == true
    end
  end

  describe "to_elixir/1 arithmetic" do
    test "addition" do
      # (+ 1 2) typed
      typed_ast = {:call, :+, [{:lit, :int, 1}, {:lit, :int, 2}], :int}
      elixir_ast = Emitter.to_elixir(typed_ast)

      assert elixir_ast == {:+, [], [1, 2]}
    end

    test "nested arithmetic" do
      # (+ 1 (* 2 3))
      inner = {:call, :*, [{:lit, :int, 2}, {:lit, :int, 3}], :int}
      outer = {:call, :+, [{:lit, :int, 1}, inner], :int}

      elixir_ast = Emitter.to_elixir(outer)

      # Should produce: 1 + (2 * 3) as AST
      assert elixir_ast == {:+, [], [1, {:*, [], [2, 3]}]}
    end
  end

  describe "compile/2" do
    test "compiles simple expression to module" do
      # (+ 1 2) typed
      typed_ast = {:call, :+, [{:lit, :int, 1}, {:lit, :int, 2}], :int}

      assert {:ok, TestMod, _bytecode} = Emitter.compile(typed_ast, TestMod)

      # The module should be loadable and callable
      assert TestMod.main() == 3
    end

    test "compiles nested expression" do
      # (+ 1 (* 2 3)) = 7
      inner = {:call, :*, [{:lit, :int, 2}, {:lit, :int, 3}], :int}
      outer = {:call, :+, [{:lit, :int, 1}, inner], :int}

      assert {:ok, TestMod2, _bytecode} = Emitter.compile(outer, TestMod2)
      assert TestMod2.main() == 7
    end
  end

  describe "end-to-end: parse → type check → emit → run" do
    test "simple arithmetic" do
      code = "(+ 1 2)"

      # Parse
      ast = Vaisto.Parser.parse(code)
      assert {:call, :+, [1, 2]} = ast

      # Type check
      {:ok, _type, typed_ast} = Vaisto.TypeChecker.check(ast)

      # Compile and run
      {:ok, E2E1, _} = Emitter.compile(typed_ast, E2E1)
      assert E2E1.main() == 3
    end

    test "nested arithmetic" do
      code = "(* (+ 1 2) (- 5 1))"

      ast = Vaisto.Parser.parse(code)
      {:ok, _type, typed_ast} = Vaisto.TypeChecker.check(ast)
      {:ok, E2E2, _} = Emitter.compile(typed_ast, E2E2)

      # (1 + 2) * (5 - 1) = 3 * 4 = 12
      assert E2E2.main() == 12
    end
  end
end
