defmodule Vaisto.LanguageFeaturesBatch4Test do
  use ExUnit.Case, async: true

  alias Vaisto.TypeChecker
  alias Vaisto.Parser
  alias Vaisto.Runner

  # ═══════════════════════════════════════════════════════════════════
  # Nested Constructor Patterns
  # ═══════════════════════════════════════════════════════════════════

  describe "nested constructor patterns - parsing" do
    test "nested variant+record parses" do
      ast = Parser.parse("(match x [(Ok (Pair a b)) body])")
      assert {:match, _, _, _} = ast
    end

    test "double-nested variant parses" do
      ast = Parser.parse("(match x [(Some (Ok v)) v])")
      assert {:match, _, _, _} = ast
    end
  end

  describe "nested constructor patterns - type checking" do
    test "nested variant fields get correct types" do
      code = """
      (deftype Pair [x :int y :int])
      (deftype Wrap (W Pair))
      (defn unwrap [w :Wrap] :int
        (match w
          [(W (Pair a b)) (+ a b)]))
      """
      forms = Parser.parse(code)
      assert {:ok, _types, _typed} = TypeChecker.check(forms)
    end

    test "nested sum inside sum gets correct types" do
      code = """
      (deftype Inner (A :int) (B :string))
      (deftype Outer (Wrap Inner))
      (defn extract [o :Outer] :int
        (match o
          [(Wrap (A n)) n]
          [(Wrap (B _)) 0]))
      """
      forms = Parser.parse(code)
      assert {:ok, _types, _typed} = TypeChecker.check(forms)
    end
  end

  describe "nested constructor patterns - e2e" do
    test "nested record inside variant" do
      code = """
      (deftype Pair [x :int y :int])
      (deftype Wrap (W Pair))
      (defn main []
        (match (W (Pair 3 4))
          [(W (Pair a b)) (+ a b)]))
      """
      {:ok, mod} = Runner.compile_and_load(code, :NestedPat1)
      assert mod.main() == 7
    end

    test "double-nested variant" do
      code = """
      (deftype Result (Ok :int) (Err :string))
      (deftype Maybe (Some Result) (None))
      (defn main []
        (match (Some (Ok 42))
          [(Some (Ok v)) v]
          [_ 0]))
      """
      {:ok, mod} = Runner.compile_and_load(code, :NestedPat2)
      assert mod.main() == 42
    end

    test "nested constructor in defn_multi" do
      code = """
      (deftype Result (Ok :int) (Err :string))
      (defn unwrap
        [(Ok v) v]
        [(Err _) 0])
      (defn main [] (unwrap (Ok 99)))
      """
      {:ok, mod} = Runner.compile_and_load(code, :NestedPat3)
      assert mod.main() == 99
    end

    test "deep nested constructor in defn_multi" do
      code = """
      (deftype Result (Ok :int) (Err :string))
      (deftype Maybe (Some Result) (None))
      (defn deep
        [(Some (Ok v)) v]
        [_ 0])
      (defn main [] (deep (Some (Ok 77))))
      """
      {:ok, mod} = Runner.compile_and_load(code, :NestedPat4)
      assert mod.main() == 77
    end
  end

  # ═══════════════════════════════════════════════════════════════════
  # As-Patterns
  # ═══════════════════════════════════════════════════════════════════

  describe "as-patterns - parsing" do
    test "as-pattern parses as call to @" do
      ast = Parser.parse("(match x [(r @ (Ok v)) r])")
      assert {:match, _, _, _} = ast
    end
  end

  describe "as-patterns - type checking" do
    test "as-pattern binds both variable and inner bindings" do
      code = """
      (deftype Result (Ok :int) (Err :string))
      (defn extract [r :Result] :int
        (match r
          [(x @ (Ok v)) v]
          [(x @ (Err _)) 0]))
      """
      forms = Parser.parse(code)
      assert {:ok, _types, _typed} = TypeChecker.check(forms)
    end
  end

  describe "as-patterns - e2e" do
    test "as-pattern returns inner binding" do
      code = """
      (deftype Result (Ok :int) (Err :string))
      (defn main []
        (match (Ok 42)
          [(r @ (Ok v)) v]
          [(r @ (Err _)) 0]))
      """
      {:ok, mod} = Runner.compile_and_load(code, :AsPat1)
      assert mod.main() == 42
    end

    test "as-pattern returns whole value" do
      code = """
      (deftype Result (Ok :int) (Err :string))
      (defn main []
        (match (Ok 42)
          [(r @ (Ok _)) r]
          [(r @ (Err _)) r]))
      """
      {:ok, mod} = Runner.compile_and_load(code, :AsPat2)
      assert mod.main() == {:Ok, 42}
    end

    test "as-pattern with wildcard inner" do
      code = """
      (deftype Result (Ok :int) (Err :string))
      (defn main []
        (match (Ok 42)
          [(r @ (Ok _)) r]
          [_ (Ok 0)]))
      """
      {:ok, mod} = Runner.compile_and_load(code, :AsPat3)
      assert mod.main() == {:Ok, 42}
    end

    test "as-pattern in defn_multi" do
      code = """
      (deftype Result (Ok :int) (Err :string))
      (defn identity
        [(r @ (Ok v)) r]
        [(r @ (Err e)) r])
      (defn main [] (identity (Ok 99)))
      """
      {:ok, mod} = Runner.compile_and_load(code, :AsPat4)
      assert mod.main() == {:Ok, 99}
    end
  end

  # ═══════════════════════════════════════════════════════════════════
  # Multi-Error Recovery
  # ═══════════════════════════════════════════════════════════════════

  describe "multi-error recovery" do
    test "multi-clause function with errors in multiple clauses reports all errors" do
      code = """
      (defn bad
        [x (+ x "hello")]
        [y (+ y "world")])
      """
      ast = Parser.parse(code)
      result = TypeChecker.check(ast)
      assert {:error, errors} = result
      assert is_list(errors)
      assert length(errors) == 2
    end

    test "single error in one clause still works" do
      code = """
      (defn bad
        [x (+ x "hello")]
        [y y])
      """
      ast = Parser.parse(code)
      result = TypeChecker.check(ast)
      assert {:error, _} = result
    end

    test "module with multiple bad multi-clause functions reports errors from all" do
      code = """
      (ns BadMod)
      (defn bad1
        [x (+ x "hello")]
        [y (+ y "world")])
      (defn bad2
        [a (+ a "foo")]
        [b (+ b "bar")])
      """
      forms = Parser.parse(code)
      result = TypeChecker.check(forms)
      assert {:error, errors} = result
      assert is_list(errors)
      # Should have errors from both functions (2 each = 4 total)
      assert length(errors) >= 4
    end
  end

  # ═══════════════════════════════════════════════════════════════════
  # Backward Compatibility
  # ═══════════════════════════════════════════════════════════════════

  describe "backward compatibility" do
    test "single-level match patterns still work" do
      code = """
      (deftype Result (Ok :int) (Err :string))
      (defn main []
        (match (Ok 42)
          [(Ok v) v]
          [(Err _) 0]))
      """
      {:ok, mod} = Runner.compile_and_load(code, :BackCompat1)
      assert mod.main() == 42
    end

    test "multi-clause functions still work" do
      code = """
      (defn fact
        [0 1]
        [n (* n (fact (- n 1)))])
      (defn main [] (fact 5))
      """
      {:ok, mod} = Runner.compile_and_load(code, :BackCompat2)
      assert mod.main() == 120
    end

    test "list pattern matching still works" do
      code = """
      (defn head
        [[h | _] h]
        [[] 0])
      (defn main [] (head (list 10 20 30)))
      """
      {:ok, mod} = Runner.compile_and_load(code, :BackCompat3)
      assert mod.main() == 10
    end
  end
end
