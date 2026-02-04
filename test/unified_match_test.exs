defmodule Vaisto.UnifiedMatchTest do
  @moduledoc """
  TDD tests for unified tuple/match support.

  Goal: One `match` construct that handles:
  - Vaisto ADT variants: (Ok val), (Err e)
  - Erlang tuples: {:ok val}, {:error reason}
  - Lists: [h | t], []
  - Literals: 1, :foo, "string"

  No more match-tuple. One construct to rule them all.
  """
  use ExUnit.Case
  alias Vaisto.{Parser, TypeChecker, CoreEmitter, Error}

  # Helper to compile and run code, returning the result
  defp run(code, mod_name) do
    ast = Parser.parse(code)
    {:ok, _type, typed} = TypeChecker.check(ast)
    {:ok, mod, binary} = CoreEmitter.compile(typed, mod_name)
    :code.load_binary(mod, ~c"test", binary)
    apply(mod, :main, [])
  end

  describe "unified match: Erlang tuples with brace syntax" do
    @tag :unified_match
    test "matches {:ok val} tuple" do
      code = """
      (match {:ok 42}
        [{:ok v} v]
        [{:error _} 0])
      """
      assert run(code, UnifiedOk1) == 42
    end

    @tag :unified_match
    test "matches {:error reason} tuple" do
      code = """
      (match {:error "failed"}
        [{:ok _} 0]
        [{:error e} 1])
      """
      assert run(code, UnifiedErr1) == 1
    end

    @tag :unified_match
    test "matches 3-element tuple" do
      code = """
      (match {:point 10 20}
        [{:point x y} (+ x y)])
      """
      assert run(code, UnifiedPoint1) == 30
    end

    @tag :unified_match
    test "matches tuple in function" do
      code = """
      (defn unwrap [result]
        (match result
          [{:ok v} v]
          [{:error _} 0]))
      (unwrap {:ok 99})
      """
      assert run(code, UnifiedUnwrap1) == 99
    end

    @tag :unified_match
    test "matches nested tuples" do
      code = """
      (match {:outer {:inner 42}}
        [{:outer {:inner v}} v])
      """
      assert run(code, UnifiedNested1) == 42
    end
  end

  describe "unified match: Vaisto ADTs (existing behavior, must keep working)" do
    @tag :unified_match
    test "matches Ok variant" do
      code = """
      (deftype Result (Ok v) (Err e))
      (match (Ok 42)
        [(Ok v) v]
        [(Err _) 0])
      """
      assert run(code, UnifiedAdtOk1) == 42
    end

    @tag :unified_match
    test "matches Err variant" do
      code = """
      (deftype Result (Ok v) (Err e))
      (match (Err "bad")
        [(Ok _) 0]
        [(Err e) 1])
      """
      assert run(code, UnifiedAdtErr1) == 1
    end

    @tag :unified_match
    test "matches variant with no fields" do
      code = """
      (deftype Option (Some v) (None))
      (match (None)
        [(Some _) 1]
        [(None) 0])
      """
      assert run(code, UnifiedAdtNone1) == 0
    end
  end

  describe "unified match: mixed ADT and Erlang tuples" do
    @tag :unified_match
    test "can use Vaisto Result and return Erlang tuple" do
      code = """
      (deftype Result (Ok v) (Err e))
      (defn to-erlang [r]
        (match r
          [(Ok v) {:ok v}]
          [(Err e) {:error e}]))
      (match (to-erlang (Ok 42))
        [{:ok v} v]
        [{:error _} 0])
      """
      assert run(code, UnifiedMixed1) == 42
    end

    @tag :unified_match
    test "can convert Erlang tuple to Vaisto Result" do
      code = """
      (deftype Result (Ok v) (Err e))
      (defn from-erlang [t]
        (match t
          [{:ok v} (Ok v)]
          [{:error e} (Err e)]))
      (match (from-erlang {:ok 77})
        [(Ok v) v]
        [(Err _) 0])
      """
      assert run(code, UnifiedMixed2) == 77
    end
  end

  describe "unified match: wildcards and literals" do
    @tag :unified_match
    test "wildcard matches any tuple" do
      code = """
      (match {:anything 1 2 3}
        [_ 99])
      """
      assert run(code, UnifiedWild1) == 99
    end

    @tag :unified_match
    test "matches atom literal in tuple" do
      code = """
      (match {:status :ready}
        [{:status :ready} 1]
        [{:status :pending} 2]
        [_ 0])
      """
      assert run(code, UnifiedAtom1) == 1
    end

    @tag :unified_match
    test "matches integer literal in tuple" do
      code = """
      (match {:code 200}
        [{:code 200} :ok]
        [{:code 404} :not-found]
        [_ :unknown])
      """
      # Note: returns atom, checking it compiles and runs
      result = run(code, UnifiedInt1)
      assert result == :ok
    end
  end

  describe "unified match: lists (existing, must keep working)" do
    @tag :unified_match
    test "matches empty list" do
      code = """
      (match []
        [[] 0]
        [[h | t] h])
      """
      assert run(code, UnifiedList1) == 0
    end

    @tag :unified_match
    test "matches cons pattern" do
      code = """
      (match (list 1 2 3)
        [[] 0]
        [[h | t] h])
      """
      assert run(code, UnifiedList2) == 1
    end

    @tag :unified_match
    test "matches list inside tuple" do
      code = """
      (match {:items (list 10 20)}
        [{:items []} 0]
        [{:items [h | _]} h])
      """
      assert run(code, UnifiedListTuple1) == 10
    end
  end

  describe "unified match: exhaustiveness" do
    @tag :unified_match
    test "errors on non-exhaustive result-like tuple match" do
      code = """
      (match {:ok 1}
        [{:ok v} v])
      """
      # Should error: {:ok ...} without {:error ...} pattern
      ast = Parser.parse(code)
      assert {:error, err} = TypeChecker.check(ast)
      text = Error.to_string(err)
      assert text =~ "non-exhaustive" or text =~ "Non-exhaustive"
      assert text =~ ":error"
    end

    @tag :unified_match
    test "allows exhaustive result-like tuple match" do
      code = """
      (match {:ok 42}
        [{:ok v} v]
        [{:error _} 0])
      """
      # Covers both :ok and :error - should compile
      assert run(code, UnifiedExhaust2) == 42
    end

    @tag :unified_match
    test "allows non-result tuple match without catch-all" do
      code = """
      (match {:point 10 20}
        [{:point x y} (+ x y)])
      """
      # :point is not a result-like tag, so no :error required
      assert run(code, UnifiedExhaust3) == 30
    end

    @tag :unified_match
    test "allows result tuple match with catch-all" do
      code = """
      (match {:ok 42}
        [{:ok v} v]
        [_ 0])
      """
      # Has catch-all pattern, so :error not required
      assert run(code, UnifiedExhaust4) == 42
    end
  end

  describe "unified match: type inference" do
    @tag :unified_match
    test "infers tuple type from construction" do
      code = """
      (let [t {:point 1 2}]
        (match t
          [{:point x y} (+ x y)]))
      """
      assert run(code, UnifiedInfer1) == 3
    end

    @tag :unified_match
    test "infers return type from all branches" do
      code = """
      (defn get-val [t]
        (match t
          [{:ok v} v]
          [{:error _} 0]))
      (+ (get-val {:ok 10}) (get-val {:error "x"}))
      """
      assert run(code, UnifiedInfer2) == 10
    end
  end
end
