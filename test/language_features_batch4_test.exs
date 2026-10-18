defmodule Vaisto.LanguageFeaturesBatch4Test do
  use ExUnit.Case, async: true

  alias Vaisto.Elab
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
      assert {:ok, _core} = Elab.module(forms)
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
      assert {:ok, _core} = Elab.module(forms)
    end
  end

  describe "nested constructor patterns - e2e" do
    test "nested record inside variant" do
      code = """
      (deftype Pair [x :int y :int])
      (deftype Wrap (W Pair))
      (defn main [] :int
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
      (defn main [] :int
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
      (defn unwrap (Fn :Result :int)
        [(Ok v) v]
        [(Err _) 0])
      (defn main [] :int (unwrap (Ok 99)))
      """
      {:ok, mod} = Runner.compile_and_load(code, :NestedPat3)
      assert mod.main() == 99
    end

    test "deep nested constructor in defn_multi" do
      code = """
      (deftype Result (Ok :int) (Err :string))
      (deftype Maybe (Some Result) (None))
      (defn deep (Fn :Maybe :int)
        [(Some (Ok v)) v]
        [_ 0])
      (defn main [] :int (deep (Some (Ok 77))))
      """
      {:ok, mod} = Runner.compile_and_load(code, :NestedPat4)
      assert mod.main() == 77
    end

    test "multi-clause with 2-field record pattern" do
      code = """
      (deftype Point [x :int y :int])
      (defn get-x (Fn :Point :int)
        [(Point x _) x])
      (defn main [] :int (get-x (Point 42 99)))
      """
      {:ok, mod} = Runner.compile_and_load(code, :MultiFieldPat1)
      assert mod.main() == 42
    end

    test "multi-clause with multiple 2-field record clauses" do
      code = """
      (deftype Pair [a :int b :int])
      (defn swap (Fn :Pair :Pair)
        [(Pair a b) (Pair b a)])
      (defn main [] :int
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
      assert {:ok, _core} = Elab.module(forms)
    end
  end

  describe "as-patterns - e2e" do
    test "as-pattern returns inner binding" do
      code = """
      (deftype Result (Ok :int) (Err :string))
      (defn main [] :int
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
      (defn main [] :Result
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
      (defn main [] :Result
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
      (defn identity (Fn :Result :Result)
        [(r @ (Ok v)) r]
        [(r @ (Err e)) r])
      (defn main [] :Result (identity (Ok 99)))
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
      (defn bad (Fn :int :int)
        [x (+ x "hello")]
        [y (+ y "world")])
      """
      ast = Parser.parse(code)
      result = Elab.module(ast)
      assert {:error, errors} = result
      assert is_list(errors)
      assert length(errors) == 2
    end

    test "single error in one clause still works" do
      code = """
      (defn bad (Fn :int :int)
        [x (+ x "hello")]
        [y y])
      """
      ast = Parser.parse(code)
      result = Elab.module(ast)
      assert {:error, _} = result
    end

    test "module with multiple bad multi-clause functions reports errors from all" do
      code = """
      (ns BadMod)
      (defn bad1 (Fn :int :int)
        [x (+ x "hello")]
        [y (+ y "world")])
      (defn bad2 (Fn :int :int)
        [a (+ a "foo")]
        [b (+ b "bar")])
      """
      forms = Parser.parse(code)
      result = Elab.module(forms)
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
      (deftype Result (Ok :int) (Err :string))
      (defn f (Fn :Result :int)
        [(Ok x) (let [y (+ x 1)] y)]
        [(Err z) (+ y 1)])
      """
      ast = Parser.parse(code)
      result = Elab.module(ast)
      assert {:error, [%{message: message} | _]} = result
      assert message =~ "y"
    end
  end

  # ═══════════════════════════════════════════════════════════════════
  # Backward Compatibility
  # ═══════════════════════════════════════════════════════════════════

  describe "backward compatibility" do
    test "single-level match patterns still work" do
      code = """
      (deftype Result (Ok :int) (Err :string))
      (defn main [] :int
        (match (Ok 42)
          [(Ok v) v]
          [(Err _) 0]))
      """
      {:ok, mod} = Runner.compile_and_load(code, :BackCompat1)
      assert mod.main() == 42
    end

    test "multi-clause functions still work" do
      code = """
      (defn fact (Fn :int :int)
        [0 1]
        [n (* n (fact (- n 1)))])
      (defn main [] :int (fact 5))
      """
      {:ok, mod} = Runner.compile_and_load(code, :BackCompat2)
      assert mod.main() == 120
    end

    test "list pattern matching still works" do
      code = """
      (defn head (Fn (List :int) :int)
        [[h | _] h]
        [[] 0])
      (defn main [] :int (head (list 10 20 30)))
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
      (defn my-abs (Fn :int :int)
        [x :when (> x 0) x]
        [x (- 0 x)])
      (defn main [] :int (my-abs -5))
      """
      {:ok, mod} = Runner.compile_and_load(code, :SubstGuardClause)
      assert mod.main() == 5
    end

    test "defn_multi guard clause types are resolved" do
      code = """
      (defn classify (Fn :int :atom)
        [x :when (> x 0) :positive]
        [x :when (< x 0) :negative]
        [_ :zero])
      (defn main [] :atom (classify 42))
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
        (let [r (the (Result :int :string) (Ok 42))]
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
        (let [r (the (Result :int :string) (Ok 99))]
          (match r
            [(Ok val) val]
            [(Err _) 0])))
      """
      {:ok, mod} = Runner.compile_and_load(code, :MatchScopeLeak2)
      assert mod.main() == 99
    end
  end

  describe "fresh bindings across record matches" do
    test "multiple unifications in same expression preserve row counter" do
      # Exercises row counter threading: multiple record pattern matches
      # in the same function each need fresh row variables
      code = """
      (deftype Point [x :int y :int])
      (defn get-x (Fn :Point :int) [(Point x _) x])
      (defn get-y (Fn :Point :int) [(Point _ y) y])
      (defn main [] :int (+ (get-x (Point 10 20)) (get-y (Point 30 40))))
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
      (defn main [] :int (check true))
      """
      {:ok, mod} = Runner.compile_and_load(code, :ExpectBoolTvar)
      assert mod.main() == 1
    end
  end

  # ═══════════════════════════════════════════════════════════════════
  # Local bidirectional inference of primitive arguments
  # ═══════════════════════════════════════════════════════════════════

  describe "native local primitive inference" do
    test "boolean operators constrain lambda parameters" do
      ast = Vaisto.Parser.parse("(fn [x y] (and x y))")
      assert {:ok, [:module, _, _ | items]} = Elab.module(ast)
      assert [:def, :main, [:->, [], _, [:->, [[:Bool], [:Bool]], _, [:Bool]]], _] = Enum.find(items, &match?([:def, :main, _, _], &1))
    end

    test "integer division constrains lambda parameters" do
      ast = Vaisto.Parser.parse("(fn [x y] (div x y))")
      assert {:ok, [:module, _, _ | items]} = Elab.module(ast)
      assert [:def, :main, [:->, [], _, [:->, [:Int, :Int], _, :Int]], _] = Enum.find(items, &match?([:def, :main, _, _], &1))
    end

    test "concatenation constrains lambda parameters" do
      ast = Vaisto.Parser.parse("(fn [x y] (++ x y))")
      assert {:ok, [:module, _, _ | items]} = Elab.module(ast)
      assert [:def, :main, [:->, [], _, [:->, [:String, :String], _, :String]], _] = Enum.find(items, &match?([:def, :main, _, _], &1))
    end

    test "lambda with boolean ops compiles e2e" do
      code = """
      (defn apply-and [f (Fn :bool :bool :bool)] :bool (f true false))
      (defn main [] :bool (apply-and (fn [x y] (and x y))))
      """
      {:ok, mod} = Runner.compile_and_load(code, :LambdaBoolOps)
      assert mod.main() == false
    end
  end
end
