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
  alias Vaisto.Compilation

  # Helper to compile and run code, returning the result
  defp run(code, mod_name) do
    assert {:ok, mod, _binary} = Compilation.compile(code, mod_name)
    try do
      apply(mod, :main, [])
    after
      :code.purge(mod)
      :code.delete(mod)
    end
  end

  describe "unified match: Erlang tuples with brace syntax" do
    @tag :unified_match
    test "matches {:ok val} tuple" do
      code = """
      (match {:ok 42}
        [{:ok v} v]
        [{:error _} 0]
        [_ 0])
      """
      assert run(code, UnifiedOk1) == 42
    end

    @tag :unified_match
    test "matches {:error reason} tuple" do
      code = """
      (match {:error "failed"}
        [{:ok _} 0]
        [{:error e} 1]
        [_ 0])
      """
      assert run(code, UnifiedErr1) == 1
    end

    @tag :unified_match
    test "matches 3-element tuple" do
      code = """
      (match {:point 10 20}
        [{:point x y} (+ x y)]
        [_ 0])
      """
      assert run(code, UnifiedPoint1) == 30
    end

    @tag :unified_match
    test "matches tuple in function" do
      code = """
      (defn unwrap [result (Tuple :atom :int)] :int
        (match result
          [{:ok v} v]
          [{:error _} 0]
        [_ 0]))
      (unwrap {:ok 99})
      """
      assert run(code, UnifiedUnwrap1) == 99
    end

    @tag :unified_match
    test "matches nested tuples" do
      code = """
      (match {:outer {:inner 42}}
        [{:outer {:inner v}} v]
        [_ 0])
      """
      assert run(code, UnifiedNested1) == 42
    end
  end

  describe "unified match: Vaisto ADTs (existing behavior, must keep working)" do
    @tag :unified_match
    test "matches Ok variant" do
      code = """
      (deftype Result (Ok :int) (Err :string))
      (match (Ok 42)
        [(Ok v) v]
        [(Err _) 0])
      """
      assert run(code, UnifiedAdtOk1) == 42
    end

    @tag :unified_match
    test "matches Err variant" do
      code = """
      (deftype Result (Ok :int) (Err :string))
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
      (match (the (Option :int) (None))
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
      (deftype Result (Ok :int) (Err :string))
      (defn to-erlang [r :Result] (Tuple :atom :int)
        (match r
          [(Ok v) {:ok v}]
          [(Err e) {:error 0}]))
      (match (to-erlang (Ok 42))
        [{:ok v} v]
        [{:error _} 0]
        [_ 0])
      """
      assert run(code, UnifiedMixed1) == 42
    end

    @tag :unified_match
    test "can convert Erlang tuple to Vaisto Result" do
      code = """
      (deftype Result (Ok :int) (Err :string))
      (defn from-erlang [t (Tuple :atom :int)] :Result
        (match t
          [{:ok v} (Ok v)]
          [{:error e} (Err (str e))]
          [_ (crash :bad_decode)]))
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
        [{:items [h | _]} h]
        [_ 0])
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
      assert {:error, [error]} = Compilation.check_liquid(code, :PartialTuple, format_errors: false)
      assert error.message == "this match is not exhaustive"
    end

    @tag :unified_match
    test "allows exhaustive result-like tuple match" do
      code = """
      (match {:ok 42}
        [{:ok v} v]
        [{:error _} 0]
        [_ 0])
      """
      # Covers both :ok and :error - should compile
      assert run(code, UnifiedExhaust2) == 42
    end

    @tag :unified_match
    test "raw tuple exhaustiveness does not depend on result-like tag names" do
      code = """
      (match {:point 10 20}
        [{:point x y} (+ x y)]
        [_ 0])
      """
      # The wildcard covers the other Atom tags; no result-tag heuristic.
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
          [{:point x y} (+ x y)]
        [_ 0]))
      """
      assert run(code, UnifiedInfer1) == 3
    end

    @tag :unified_match
    test "infers return type from all branches" do
      code = """
      (defn get-val [t (Tuple :atom :int)] :int
        (match t
          [{:ok v} v]
          [{:error _} 0]
        [_ 0]))
      (+ (get-val {:ok 10}) (get-val {:error 0}))
      """
      assert run(code, UnifiedInfer2) == 10
    end
  end
end
