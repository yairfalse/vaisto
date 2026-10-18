defmodule Vaisto.LanguageCompletionsTest do
  use ExUnit.Case, async: true

  import Vaisto.TestHelpers, only: [check_type: 1]
  alias Vaisto.Runner

  # ── Type checking: andalso / orelse ────────────────────────────────

  describe "type checking andalso/orelse" do
    test "andalso returns bool" do
      result = check_type("(andalso true false)")
      assert {:ok, [:Bool]} = result
    end

    test "orelse returns bool" do
      result = check_type("(orelse true false)")
      assert {:ok, [:Bool]} = result
    end

    test "andalso rejects non-boolean args" do
      result = check_type("(andalso 1 2)")
      assert {:error, _} = result
    end

    test "orelse rejects non-boolean args" do
      result = check_type("(orelse 1 2)")
      assert {:error, _} = result
    end
  end

  # ── Type checking: string ++ ───────────────────────────────────────

  describe "type checking ++" do
    test "++ returns string" do
      result = check_type(~s|(++ "a" "b")|)
      assert {:ok, :String} = result
    end

    test "++ rejects non-string args" do
      result = check_type(~s|(++ 1 "b")|)
      assert {:error, _} = result
    end
  end

  # ── Type checking: unary negation ──────────────────────────────────

  describe "type checking unary -" do
    test "negate int returns int" do
      result = check_type("(- 5)")
      assert {:ok, :Int} = result
    end

    test "negate float returns float" do
      result = check_type("(- 3.14)")
      assert {:ok, :Float} = result
    end

    test "negate rejects non-numeric" do
      result = check_type(~s|(- "hello")|)
      assert {:error, _} = result
    end
  end

  # ── End-to-end: andalso / orelse ───────────────────────────────────

  describe "e2e andalso" do
    test "andalso true true" do
      {:ok, mod} = Runner.compile_and_load("(defn main [] :bool (andalso true true))", :AndalsoTT)
      assert Runner.call(mod, :main) == true
    end

    test "andalso false true" do
      {:ok, mod} = Runner.compile_and_load("(defn main [] :bool (andalso false true))", :AndalsoFT)
      assert Runner.call(mod, :main) == false
    end

    test "andalso true false" do
      {:ok, mod} = Runner.compile_and_load("(defn main [] :bool (andalso true false))", :AndalsoTF)
      assert Runner.call(mod, :main) == false
    end

    test "orelse false true" do
      {:ok, mod} = Runner.compile_and_load("(defn main [] :bool (orelse false true))", :OrelseFT)
      assert Runner.call(mod, :main) == true
    end

    test "orelse true false" do
      {:ok, mod} = Runner.compile_and_load("(defn main [] :bool (orelse true false))", :OrelseTF)
      assert Runner.call(mod, :main) == true
    end

    test "orelse false false" do
      {:ok, mod} = Runner.compile_and_load("(defn main [] :bool (orelse false false))", :OrelseFF)
      assert Runner.call(mod, :main) == false
    end

    test "andalso short-circuits (does not evaluate second arg on false)" do
      # (/ 1 0) would crash if evaluated, but andalso should short-circuit
      {:ok, mod} = Runner.compile_and_load("(defn main [] :bool (andalso false (> (div 1 0) 0)))", :AndalsoSC)
      assert Runner.call(mod, :main) == false
    end

    test "orelse short-circuits (does not evaluate second arg on true)" do
      {:ok, mod} = Runner.compile_and_load("(defn main [] :bool (orelse true (> (div 1 0) 0)))", :OrelseSC)
      assert Runner.call(mod, :main) == true
    end
  end

  # ── End-to-end: string ++ ──────────────────────────────────────────

  describe "e2e ++" do
    test "concatenates two strings" do
      {:ok, mod} = Runner.compile_and_load(~s|(defn main [] :string (++ "hello" " world"))|, :ConcatHW)
      assert Runner.call(mod, :main) == "hello world"
    end

    test "concatenates empty strings" do
      {:ok, mod} = Runner.compile_and_load(~s|(defn main [] :string (++ "" "ok"))|, :ConcatEmpty)
      assert Runner.call(mod, :main) == "ok"
    end
  end

  # ── End-to-end: unary negation ─────────────────────────────────────

  describe "e2e unary -" do
    test "negate integer" do
      {:ok, mod} = Runner.compile_and_load("(defn main [] :int (- 5))", :NegInt)
      assert Runner.call(mod, :main) == -5
    end

    test "negate float" do
      {:ok, mod} = Runner.compile_and_load("(defn main [] :float (- 3.14))", :NegFloat)
      assert Runner.call(mod, :main) == -3.14
    end

    test "double negation" do
      {:ok, mod} = Runner.compile_and_load("(defn main [] :int (- (- 42)))", :NegNeg)
      assert Runner.call(mod, :main) == 42
    end
  end
end
