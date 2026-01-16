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

  describe "typed PIDs" do
    # Helper to create an env with a counter process defined
    defp env_with_counter do
      # Base primitives + counter process type
      %{
        :+ => {:fn, [:int, :int], :int},
        :counter => {:process, :int, [:increment, :get]}
      }
    end

    test "spawn returns typed PID" do
      env = env_with_counter()
      ast = {:call, :spawn, [:counter, 0]}

      assert {:ok, {:pid, :counter, [:increment, :get]}, _} = TypeChecker.check(ast, env)
    end

    test "send valid message to typed PID succeeds" do
      env = env_with_counter()
      spawn_ast = {:call, :spawn, [:counter, 0]}
      ast = {:call, :"!", [spawn_ast, :increment]}

      assert {:ok, :ok, _} = TypeChecker.check(ast, env)
    end

    test "send invalid message to typed PID fails" do
      env = env_with_counter()
      spawn_ast = {:call, :spawn, [:counter, 0]}
      ast = {:call, :"!", [spawn_ast, :invalid_msg]}

      result = TypeChecker.check(ast, env)
      assert {:error, msg} = result
      assert msg =~ "does not accept message"
      assert msg =~ ":invalid_msg"
    end

    test "full module with valid send compiles" do
      code = """
      (process counter 0
        :increment (+ state 1)
        :get state)
      (! (spawn counter 0) :increment)
      """
      ast = Vaisto.Parser.parse(code)
      assert {:ok, :module, _} = TypeChecker.check(ast)
    end

    test "full module with invalid send fails" do
      code = """
      (process counter 0
        :increment (+ state 1)
        :get state)
      (! (spawn counter 0) :reset)
      """
      ast = Vaisto.Parser.parse(code)
      result = TypeChecker.check(ast)
      assert {:error, msg} = result
      assert msg =~ "does not accept message :reset"
    end
  end
end
