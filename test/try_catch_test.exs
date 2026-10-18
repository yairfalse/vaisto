defmodule Vaisto.TryCatchTest do
  use ExUnit.Case, async: true

  import Vaisto.TestHelpers

  alias Vaisto.Runner

  # ============================================================================
  # Parser Tests
  # ============================================================================

  describe "parser: try/catch" do
    test "basic try/catch parses correctly" do
      ast = parse!("(try 1 [catch [:error e 2]])")
      assert {:try, 1, [{:error, :e, 2}], nil, _loc} = ast
    end

    test "try/after parses correctly" do
      ast = parse!("(try 1 [after 2])")
      assert {:try, 1, [], 2, _loc} = ast
    end

    test "try/catch/after parses correctly" do
      ast = parse!("(try 1 [catch [:error e 2]] [after 3])")
      assert {:try, 1, [{:error, :e, 2}], 3, _loc} = ast
    end

    test "multiple catch clauses" do
      ast = parse!("(try 1 [catch [:error e 2] [:throw v 3]])")
      assert {:try, 1, [{:error, :e, 2}, {:throw, :v, 3}], nil, _loc} = ast
    end

    test "multi-expression body wraps in :do" do
      ast = parse!("(try (+ 1 2) (+ 3 4) [catch [:error e 0]])")
      assert {:try, {:do, _, _}, [{:error, :e, 0}], nil, _loc} = ast
    end

    test "multi-expression handler body wraps in :do" do
      ast = parse!("(try 1 [catch [:error e (+ 1 2) (+ 3 4)]])")
      assert {:try, 1, [{:error, :e, {:do, _, _}}], nil, _loc} = ast
    end

    test "multi-expression after body wraps in :do" do
      ast = parse!("(try 1 [after (+ 1 2) (+ 3 4)])")
      assert {:try, 1, [], {:do, _, _}, _loc} = ast
    end

    test "all three catch classes are valid" do
      ast = parse!("(try 1 [catch [:error e 2] [:throw v 3] [:exit r 4]])")
      assert {:try, _, [{:error, _, _}, {:throw, _, _}, {:exit, _, _}], nil, _loc} = ast
    end
  end

  describe "parser: try error cases" do
    test "empty try" do
      assert {:error, _, _} = parse!("(try)")
    end

    test "try without catch or after" do
      assert {:error, _, _} = parse!("(try 1)")
    end

    test "empty catch block" do
      assert {:error, _, _} = parse!("(try 1 [catch])")
    end

    test "invalid catch class" do
      assert {:error, _, _} = parse!("(try 1 [catch [:foo e 2]])")
    end

    test "catch clause missing body" do
      assert {:error, _, _} = parse!("(try 1 [catch [:error e]])")
    end

    test "empty after block" do
      assert {:error, _, _} = parse!("(try 1 [after])")
    end
  end

  # ============================================================================
  # TypeChecker Tests
  # ============================================================================

  describe "type checker: try/catch" do
    test "body and handler types unify" do
      assert {:ok, :Int} = check_type("(try 1 [catch [:error e 2]])")
    end

    test "body and handler type mismatch" do
      assert {:error, _} = check_type(~s|(try 1 [catch [:error e "oops"]])|)
    end

    test "an ignored exception does not alter the result type" do
      assert {:ok, :Int} = check_type("(try 1 [catch [:error e 42]])")
    end

    test "after type is ignored" do
      assert {:ok, :Int} = check_type(~s|(try 1 [catch [:error e 2]] [after "cleanup"])|)
    end

    test "try-only-after: result type = body type" do
      assert {:ok, :Int} = check_type(~s|(try 42 [after "cleanup"])|)
    end

    test "multiple catch clauses all unify" do
      assert {:ok, :Int} = check_type("(try 1 [catch [:error e 2] [:throw v 3]])")
    end

    test "multiple catch clauses type mismatch" do
      assert {:error, _} = check_type(~s|(try 1 [catch [:error e 2] [:throw v "oops"]])|)
    end
  end

  # ============================================================================
  # End-to-end Tests
  # ============================================================================

  describe "e2e: native try/catch" do
    test "catch error" do
      assert {:ok, :caught} = Runner.run("(extern erlang:error [:atom] :atom) (try (erlang:error :boom) [catch [:error e :caught]])")
    end

    test "catch throw" do
      assert {:ok, 42} = Runner.run("(extern erlang:throw [:int] :int) (try (erlang:throw 42) [catch [:throw v (match (decode (Tuple :atom :atom :int) (up :Reason v)) [(Core.Ok {_ _ n}) n] [_ (crash :bad_decode)])]])")
    end

    test "catch exit" do
      assert {:ok, :exited} = Runner.run("(extern erlang:exit [:atom] :atom) (try (erlang:exit :bye) [catch [:exit r :exited]])")
    end

    test "no exception: body value passes through" do
      assert {:ok, 42} = Runner.run(~s|(try 42 [catch [:error e 0]])|)
    end

    test "after runs on success" do
      code = "(extern erlang:put [:atom :bool] :dyn) (extern erlang:get [:atom] :bool) (extern erlang:error [:atom] :atom) (do (erlang:put :after_ran false) (try 42 [catch [:error e 0]] [after (erlang:put :after_ran true)]) (erlang:get :after_ran))"
      assert {:ok, true} = Runner.run(code)
    end

    test "after runs on exception" do
      code = "(extern erlang:put [:atom :bool] :dyn) (extern erlang:get [:atom] :bool) (extern erlang:error [:atom] :atom) (do (erlang:put :after_ran false) (try (erlang:error :boom) [catch [:error e :caught]] [after (erlang:put :after_ran true)]) (erlang:get :after_ran))"
      assert {:ok, true} = Runner.run(code)
    end

    test "after-only: exception propagates" do
      code = "(extern erlang:put [:atom :bool] :dyn) (extern erlang:get [:atom] :bool) (extern erlang:error [:atom] :bool) (do (erlang:put :after_ran false) (try (try (erlang:error :boom) [after (erlang:put :after_ran true)]) [catch [:error e (erlang:get :after_ran)]]))"
      assert {:ok, true} = Runner.run(code)
    end

    test "nested try/catch" do
      code = "(extern erlang:error [:atom] :atom) (try (try (erlang:error :inner) [catch [:error e :inner_caught]]) [catch [:error e :outer_caught]])"
      assert {:ok, :inner_caught} = Runner.run(code)
    end

    test "try in function body" do
      code = "(extern erlang:div [:int :int] :int) (try (erlang:div 10 0) [catch [:error e 0]])"
      assert {:ok, 0} = Runner.run(code)
    end

    test "catch clause var binding" do
      assert {:ok, 100} = Runner.run("(extern erlang:throw [:int] :int) (try (erlang:throw 99) [catch [:throw v (match (decode (Tuple :atom :atom :int) (up :Reason v)) [(Core.Ok {_ _ n}) (+ n 1)] [_ (crash :bad_decode)])]])")
    end
  end

  describe "e2e: reference evaluator try/catch" do
    test "catch error" do
      assert {:ok, :caught} = run_reference("(extern erlang:error [:atom] :atom) (try (erlang:error :boom) [catch [:error e :caught]])", [{:external, [:erlang, :error, [:boom]], {:raised, :error, :boom}}])
    end

    test "catch throw" do
      assert {:ok, 42} = run_reference("(extern erlang:throw [:int] :int) (try (erlang:throw 42) [catch [:throw v (match (decode (Tuple :atom :atom :int) (up :Reason v)) [(Core.Ok {_ _ n}) n] [_ (crash :bad_decode)])]])", [{:external, [:erlang, :throw, [42]], {:raised, :throw, 42}}])
    end

    test "no exception passes through" do
      assert {:ok, 42} = run_reference(~s|(try 42 [catch [:error e 0]])|)
    end

    test "after runs on success (core)" do
      code = "(extern erlang:put [:atom :bool] :dyn) (extern erlang:get [:atom] :bool) (extern erlang:error [:atom] :atom) (do (erlang:put :after_ran false) (try 42 [catch [:error e 0]] [after (erlang:put :after_ran true)]) (erlang:get :after_ran))"
      assert {:ok, true} = run_reference(code, [{:external, [:erlang, :put, [:after_ran, false]], {:ok, :undefined}}, {:external, [:erlang, :put, [:after_ran, true]], {:ok, false}}, {:external, [:erlang, :get, [:after_ran]], {:ok, true}}])
    end

    test "after runs on exception (core)" do
      code = "(extern erlang:put [:atom :bool] :dyn) (extern erlang:get [:atom] :bool) (extern erlang:error [:atom] :atom) (do (erlang:put :after_ran false) (try (erlang:error :boom) [catch [:error e :caught]] [after (erlang:put :after_ran true)]) (erlang:get :after_ran))"
      assert {:ok, true} = run_reference(code, [{:external, [:erlang, :put, [:after_ran, false]], {:ok, :undefined}}, {:external, [:erlang, :error, [:boom]], {:raised, :error, :boom}}, {:external, [:erlang, :put, [:after_ran, true]], {:ok, false}}, {:external, [:erlang, :get, [:after_ran]], {:ok, true}}])
    end
  end

  defp run_reference(source, steps \\ []) do
    with {:ok, core} <- Vaisto.Compilation.check_liquid(source) do
      Vaisto.Liquid.Eval.run(core, :main, [], handler: {:script, steps})
    end
  end
end
