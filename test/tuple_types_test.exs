defmodule Vaisto.TupleTypesTest do
  @moduledoc "Tests for tuple type inference (Phase 1 of type system sweep)"
  use ExUnit.Case, async: true

  import Vaisto.TestHelpers

  describe "tuple type inference" do
    test "simple tuple infers element types" do
      assert_type "(tuple 1 2)", [:Tuple, :Int, :Int]
    end

    test "mixed-type tuple" do
      assert_type ~s|(tuple 1 "hello")|, [:Tuple, :Int, :String]
    end

    test "empty tuple" do
      assert_type "(tuple)", :Unit
    end

    test "single-element tuple" do
      assert_type "(tuple true)", [:Tuple, [:Bool]]
    end

    test "nested tuples" do
      assert_type "(tuple 1 (tuple 2 3))", [:Tuple, :Int, [:Tuple, :Int, :Int]]
    end

    test "tuple with list element" do
      assert_type "(tuple (list 1 2) true)", [:Tuple, [:List, :Int], [:Bool]]
    end

    test "shorthand tuple syntax" do
      assert_type "{:ok 42}", [:Tuple, :Atom, :Int]
    end
  end

  describe "tuple pattern matching" do
    test "extracts element types from tuple pattern" do
      code = """
      (let [(tuple a b) (tuple 1 "hello")]
        a)
      """
      assert_type code, :Int
    end

    test "extracts second element type" do
      code = """
      (let [(tuple a b) (tuple 1 "hello")]
        b)
      """
      assert_type code, :String
    end

    test "match expression with tuple scrutinee" do
      code = """
      (match (tuple :ok 42)
        [(tuple :ok val) val]
        [(tuple :error _) 0]
        [_ 0])
      """
      assert_type code, :Int
    end

    test "nested tuple pattern matching" do
      code = """
      (let [(tuple a (tuple b c)) (tuple 1 (tuple 2 3))]
        (+ a (+ b c)))
      """
      assert_type code, :Int
    end
  end

  describe "Tuple annotation syntax" do
    test "parses (Tuple :int :string)" do
      code = """
      (defn make-pair [x :int y :string] (Tuple :int :string) (tuple x y))
      """
      assert {:ok, _} = check_type(code)
    end

    test "parses nested Tuple annotation" do
      code = """
      (defn nested [] (Tuple :int (Tuple :bool :string))
        (tuple 42 (tuple true "hi")))
      """
      assert {:ok, _} = check_type(code)
    end
  end

  describe "tuple unification" do
    test "size mismatch produces error" do
      # Two branches return different-size tuples
      code = """
      (if true (tuple 1 2) (tuple 1 2 3))
      """
      assert {:error, _} = check_type(code)
    end

    test "element type mismatch produces error" do
      # Two branches return tuples with incompatible element types
      code = """
      (if true (tuple 1 "a") (tuple "b" 2))
      """
      assert {:error, _} = check_type(code)
    end

    test "compatible tuples unify" do
      code = """
      (if true (tuple 1 2) (tuple 3 4))
      """
      assert_type code, [:Tuple, :Int, :Int]
    end
  end

  describe "tuple type in functions" do
    test "function returning tuple" do
      code = """
      (defn pair [x :int y :string] (Tuple :int :string) (tuple x y))
      """
      assert {:ok, [:->, [:Int, :String], [:eff, :closed], [:Tuple, :Int, :String]]} = check_type(code)
    end

    test "function taking and returning tuple via inference" do
      code = """
      (defn first-of-pair [p (Tuple :a :b)] :a
        (match p
          [(tuple a _) a]))
      """
      assert {:ok, _} = check_type(code)
    end
  end

  describe "backend parity" do
    test "simple tuple compiles and runs" do
      assert {:ok, {1, "hello"}} = compile_and_run("""
      (defn main [] (Tuple :int :string) (tuple 1 "hello"))
      """)
    end

    test "tuple match compiles and runs" do
      assert {:ok, 42} = compile_and_run("""
      (defn main [] :int
        (match (tuple :ok 42)
          [(tuple :ok val) val]
          [(tuple :error _) 0]
          [_ 0]))
      """)
    end

    test "nested tuple compiles and runs" do
      assert {:ok, {1, {2, 3}}} = compile_and_run("""
      (defn main [] (Tuple :int (Tuple :int :int)) (tuple 1 (tuple 2 3)))
      """)
    end
  end
end
