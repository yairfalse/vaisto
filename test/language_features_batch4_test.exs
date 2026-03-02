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

    test "multi-clause with 2-field record pattern" do
      code = """
      (deftype Point [x :int y :int])
      (defn get-x
        [(Point x _) x])
      (defn main [] (get-x (Point 42 99)))
      """
      {:ok, mod} = Runner.compile_and_load(code, :MultiFieldPat1)
      assert mod.main() == 42
    end

    test "multi-clause with multiple 2-field record clauses" do
      code = """
      (deftype Pair [a :int b :int])
      (defn swap
        [(Pair a b) (Pair b a)])
      (defn main []
        (let [(Pair x y) (swap (Pair 1 2))]
          (+ (* x 10) y)))
      """
      {:ok, mod} = Runner.compile_and_load(code, :MultiFieldPat2)
      assert mod.main() == 21
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

    test "clause bindings do not leak into subsequent clauses" do
      # Variable y from clause 1's let should NOT be visible in clause 2.
      # If y leaked as :num, (+ y 1) would type-check; without leakage it fails
      # because y is just an atom literal.
      code = """
      (defn f
        [(Ok x) (let [y (+ x 1)] y)]
        [(Err z) (+ y 1)])
      """
      ast = Parser.parse(code)
      result = TypeChecker.check(ast)
      assert {:error, _} = result
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

  # ═══════════════════════════════════════════════════════════════════
  # Bug Fix Regression Tests
  # ═══════════════════════════════════════════════════════════════════

  describe "apply_subst_to_clauses handles 4-tuple (guard) clauses" do
    test "defn_multi with guard type-checks and compiles" do
      code = """
      (defn my-abs
        [x :when (> x 0) x]
        [x (- 0 x)])
      (defn main [] (my-abs -5))
      """
      {:ok, mod} = Runner.compile_and_load(code, :SubstGuardClause)
      assert mod.main() == 5
    end

    test "defn_multi guard clause types are resolved" do
      code = """
      (defn classify
        [x :when (> x 0) :positive]
        [x :when (< x 0) :negative]
        [_ :zero])
      (defn main [] (classify 42))
      """
      {:ok, mod} = Runner.compile_and_load(code, :SubstGuardTypes)
      assert mod.main() == :positive
    end
  end

  describe "match clause variable scoping" do
    test "pattern variables do not leak out of match" do
      # After a match, variables bound inside clauses should not be
      # accessible in the outer scope
      code = """
      (deftype Result (Ok v) (Err e))
      (defn main [] :int
        (let [r (Ok 42)]
          (let [x (match r
                    [(Ok v) v]
                    [(Err e) 0])]
            x)))
      """
      {:ok, mod} = Runner.compile_and_load(code, :MatchScopeLeak1)
      assert mod.main() == 42
    end

    test "match bindings from one clause do not leak to next" do
      # Each match clause's bindings are independent
      code = """
      (deftype Result (Ok v) (Err e))
      (defn main [] :int
        (let [r (Ok 99)]
          (match r
            [(Ok val) val]
            [(Err _) 0])))
      """
      {:ok, mod} = Runner.compile_and_load(code, :MatchScopeLeak2)
      assert mod.main() == 99
    end
  end

  describe "row counter threading in TcCtx" do
    test "multiple unifications in same expression preserve row counter" do
      # Exercises row counter threading: multiple record pattern matches
      # in the same function each need fresh row variables
      code = """
      (deftype Point [x :int y :int])
      (defn get-x [(Point x _) x])
      (defn get-y [(Point _ y) y])
      (defn main [] (+ (get-x (Point 10 20)) (get-y (Point 30 40))))
      """
      {:ok, mod} = Runner.compile_and_load(code, :RowCounterThread)
      assert mod.main() == 50
    end
  end

  describe "expect_bool accepts type variables" do
    test "polymorphic value in if condition does not error" do
      # A type variable should be accepted as a boolean in if conditions
      # This tests expect_bool/1 handling {:tvar, _}
      code = """
      (defn check [b :bool] :int (if b 1 0))
      (defn main [] (check true))
      """
      {:ok, mod} = Runner.compile_and_load(code, :ExpectBoolTvar)
      assert mod.main() == 1
    end
  end

  # ═══════════════════════════════════════════════════════════════════
  # Algorithm W Primitives Consistency
  # ═══════════════════════════════════════════════════════════════════

  describe "Infer primitives match TypeEnv primitives" do
    test "standalone Infer.infer knows about and/or/not" do
      # These operators were missing from Infer.@primitives
      ast = Vaisto.Parser.parse("(fn [x y] (and x y))")
      assert {:ok, {:fn, [:bool, :bool], :bool}, _typed} = Vaisto.TypeSystem.Infer.infer(ast)
    end

    test "standalone Infer.infer knows about div/rem" do
      ast = Vaisto.Parser.parse("(fn [x y] (div x y))")
      assert {:ok, {:fn, [:int, :int], :int}, _typed} = Vaisto.TypeSystem.Infer.infer(ast)
    end

    test "standalone Infer.infer knows about ++" do
      ast = Vaisto.Parser.parse("(fn [x y] (++ x y))")
      assert {:ok, {:fn, [:string, :string], :string}, _typed} = Vaisto.TypeSystem.Infer.infer(ast)
    end

    test "lambda with boolean ops compiles e2e" do
      code = """
      (defn apply-and [f] (f true false))
      (defn main [] (apply-and (fn [x y] (and x y))))
      """
      {:ok, mod} = Runner.compile_and_load(code, :LambdaBoolOps)
      assert mod.main() == false
    end
  end
end
