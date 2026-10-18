defmodule Vaisto.BooleanArithmeticTest do
  use ExUnit.Case, async: true

  import Vaisto.TestHelpers, only: [check_type: 1]
  alias Vaisto.Runner

  # ── Type checking: boolean operators ──────────────────────────────

  describe "type checking boolean operators" do
    test "and returns bool" do
      result = check_type("(and true false)")
      assert {:ok, [:Bool]} = result
    end

    test "or returns bool" do
      result = check_type("(or true false)")
      assert {:ok, [:Bool]} = result
    end

    test "not returns bool" do
      result = check_type("(not true)")
      assert {:ok, [:Bool]} = result
    end

    test "and rejects non-boolean args" do
      result = check_type("(and 1 2)")
      assert {:error, _} = result
    end

    test "or rejects non-boolean args" do
      result = check_type("(or 1 2)")
      assert {:error, _} = result
    end

    test "not rejects non-boolean arg" do
      result = check_type("(not 42)")
      assert {:error, _} = result
    end
  end

  # ── Type checking: integer division ───────────────────────────────

  describe "type checking integer division" do
    test "div returns int" do
      result = check_type("(div 10 3)")
      assert {:ok, :Int} = result
    end

    test "rem returns int" do
      result = check_type("(rem 10 3)")
      assert {:ok, :Int} = result
    end

    test "div rejects float args" do
      result = check_type("(div 1.0 2)")
      assert {:error, _} = result
    end

    test "rem rejects float args" do
      result = check_type("(rem 1.0 2)")
      assert {:error, _} = result
    end
  end

  # ── End-to-end execution ──────────────────────────────────────────

  describe "e2e boolean operators" do
    test "and true true" do
      {:ok, mod} = Runner.compile_and_load("(defn main [] :bool (and true true))", :AndTT)
      assert Runner.call(mod, :main) == true
    end

    test "and true false" do
      {:ok, mod} = Runner.compile_and_load("(defn main [] :bool (and true false))", :AndTF)
      assert Runner.call(mod, :main) == false
    end

    test "or false true" do
      {:ok, mod} = Runner.compile_and_load("(defn main [] :bool (or false true))", :OrFT)
      assert Runner.call(mod, :main) == true
    end

    test "or false false" do
      {:ok, mod} = Runner.compile_and_load("(defn main [] :bool (or false false))", :OrFF)
      assert Runner.call(mod, :main) == false
    end

    test "not false" do
      {:ok, mod} = Runner.compile_and_load("(defn main [] :bool (not false))", :NotF)
      assert Runner.call(mod, :main) == true
    end

    test "not true" do
      {:ok, mod} = Runner.compile_and_load("(defn main [] :bool (not true))", :NotT)
      assert Runner.call(mod, :main) == false
    end
  end

  describe "e2e integer division" do
    test "div 7 2" do
      {:ok, mod} = Runner.compile_and_load("(defn main [] :int (div 7 2))", :Div72)
      assert Runner.call(mod, :main) == 3
    end

    test "rem 7 2" do
      {:ok, mod} = Runner.compile_and_load("(defn main [] :int (rem 7 2))", :Rem72)
      assert Runner.call(mod, :main) == 1
    end

    test "div 10 3" do
      {:ok, mod} = Runner.compile_and_load("(defn main [] :int (div 10 3))", :Div103)
      assert Runner.call(mod, :main) == 3
    end

    test "rem 10 3" do
      {:ok, mod} = Runner.compile_and_load("(defn main [] :int (rem 10 3))", :Rem103)
      assert Runner.call(mod, :main) == 1
    end
  end
end
