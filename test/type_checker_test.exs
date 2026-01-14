defmodule Vaisto.TypeCheckerTest do
  use ExUnit.Case
  alias Vaisto.TypeChecker

  describe "literals" do
    test "integers have type :int" do
      assert {:ok, :int, _} = TypeChecker.check(42)
    end

    test "floats have type :float" do
      assert {:ok, :float, _} = TypeChecker.check(3.14)
    end
  end

  describe "function calls" do
    test "addition of integers returns :int" do
      ast = {:call, :+, [1, 2]}
      assert {:ok, :int, _} = TypeChecker.check(ast)
    end

    test "addition with wrong types fails" do
      ast = {:call, :+, [1, :atom]}
      assert {:error, _} = TypeChecker.check(ast)
    end

    test "arity mismatch fails" do
      ast = {:call, :+, [1]}
      assert {:error, _} = TypeChecker.check(ast)
    end
  end

  describe "supervision" do
    test "valid strategy passes" do
      ast = {:supervise, :one_for_one, []}
      assert {:ok, :supervisor, _} = TypeChecker.check(ast)
    end

    test "invalid strategy fails" do
      ast = {:supervise, :bogus_strategy, []}
      assert {:error, _} = TypeChecker.check(ast)
    end
  end
end
