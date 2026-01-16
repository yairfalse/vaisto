defmodule Vaisto.EmitterTest do
  use ExUnit.Case
  alias Vaisto.Emitter

  describe "to_elixir/1 literals" do
    test "integers" do
      assert Emitter.to_elixir({:lit, :int, 42}) == 42
    end

    test "floats" do
      assert Emitter.to_elixir({:lit, :float, 3.14}) == 3.14
    end

    test "atoms" do
      assert Emitter.to_elixir({:lit, :atom, :ok}) == :ok
    end

    test "booleans" do
      assert Emitter.to_elixir({:lit, :bool, true}) == true
    end
  end

  describe "to_elixir/1 arithmetic" do
    test "addition" do
      # (+ 1 2) typed
      typed_ast = {:call, :+, [{:lit, :int, 1}, {:lit, :int, 2}], :int}
      elixir_ast = Emitter.to_elixir(typed_ast)

      assert elixir_ast == {:+, [], [1, 2]}
    end

    test "nested arithmetic" do
      # (+ 1 (* 2 3))
      inner = {:call, :*, [{:lit, :int, 2}, {:lit, :int, 3}], :int}
      outer = {:call, :+, [{:lit, :int, 1}, inner], :int}

      elixir_ast = Emitter.to_elixir(outer)

      # Should produce: 1 + (2 * 3) as AST
      assert elixir_ast == {:+, [], [1, {:*, [], [2, 3]}]}
    end
  end

  describe "compile/2" do
    test "compiles simple expression to module" do
      # (+ 1 2) typed
      typed_ast = {:call, :+, [{:lit, :int, 1}, {:lit, :int, 2}], :int}

      assert {:ok, TestMod, _bytecode} = Emitter.compile(typed_ast, TestMod)

      # The module should be loadable and callable
      assert TestMod.main() == 3
    end

    test "compiles nested expression" do
      # (+ 1 (* 2 3)) = 7
      inner = {:call, :*, [{:lit, :int, 2}, {:lit, :int, 3}], :int}
      outer = {:call, :+, [{:lit, :int, 1}, inner], :int}

      assert {:ok, TestMod2, _bytecode} = Emitter.compile(outer, TestMod2)
      assert TestMod2.main() == 7
    end
  end

  describe "end-to-end: parse → type check → emit → run" do
    test "simple arithmetic" do
      code = "(+ 1 2)"

      # Parse
      ast = Vaisto.Parser.parse(code)
      assert {:call, :+, [1, 2]} = ast

      # Type check
      {:ok, _type, typed_ast} = Vaisto.TypeChecker.check(ast)

      # Compile and run
      {:ok, E2E1, _} = Emitter.compile(typed_ast, E2E1)
      assert E2E1.main() == 3
    end

    test "nested arithmetic" do
      code = "(* (+ 1 2) (- 5 1))"

      ast = Vaisto.Parser.parse(code)
      {:ok, _type, typed_ast} = Vaisto.TypeChecker.check(ast)
      {:ok, E2E2, _} = Emitter.compile(typed_ast, E2E2)

      # (1 + 2) * (5 - 1) = 3 * 4 = 12
      assert E2E2.main() == 12
    end

    test "let bindings" do
      code = "(let [x 1 y 2] (+ x y))"

      ast = Vaisto.Parser.parse(code)
      {:ok, :int, typed_ast} = Vaisto.TypeChecker.check(ast)
      {:ok, LetE2E, _} = Emitter.compile(typed_ast, LetE2E)

      assert LetE2E.main() == 3
    end

    test "nested let bindings" do
      code = "(let [a 10] (let [b (+ a 5)] (* b 2)))"

      ast = Vaisto.Parser.parse(code)
      {:ok, :int, typed_ast} = Vaisto.TypeChecker.check(ast)
      {:ok, NestedLetE2E, _} = Emitter.compile(typed_ast, NestedLetE2E)

      # (10 + 5) * 2 = 30
      assert NestedLetE2E.main() == 30
    end

    test "record construction and pattern matching" do
      code = """
      (deftype point x y)
      (match (point 1 2) [(point a b) (+ a b)])
      """

      ast = Vaisto.Parser.parse(code)
      {:ok, :module, {:module, [_deftype, match_expr]}} = Vaisto.TypeChecker.check(ast)
      {:ok, RecordMatchE2E, _} = Emitter.compile(match_expr, RecordMatchE2E)

      # 1 + 2 = 3
      assert RecordMatchE2E.main() == 3
    end

    test "nested record pattern matching" do
      code = """
      (deftype pair a b)
      (match (pair 10 20)
        [(pair x y) (* x y)])
      """

      ast = Vaisto.Parser.parse(code)
      {:ok, :module, {:module, [_deftype, match_expr]}} = Vaisto.TypeChecker.check(ast)
      {:ok, NestedRecordE2E, _} = Emitter.compile(match_expr, NestedRecordE2E)

      # 10 * 20 = 200
      assert NestedRecordE2E.main() == 200
    end

    test "user-defined function" do
      code = """
      (defn add [x y] (+ x y))
      (add 3 4)
      """

      ast = Vaisto.Parser.parse(code)
      {:ok, :module, typed_ast} = Vaisto.TypeChecker.check(ast)
      {:ok, DefnE2E, _} = Emitter.compile(typed_ast, DefnE2E)

      assert DefnE2E.main() == 7
    end

    test "multiple user-defined functions" do
      code = """
      (defn double [x] (* x 2))
      (defn add-one [x] (+ x 1))
      (double (add-one 5))
      """

      ast = Vaisto.Parser.parse(code)
      {:ok, :module, typed_ast} = Vaisto.TypeChecker.check(ast)
      {:ok, MultiFnE2E, _} = Emitter.compile(typed_ast, MultiFnE2E)

      # (5 + 1) * 2 = 12
      assert MultiFnE2E.main() == 12
    end

    test "function calling another function" do
      code = """
      (defn square [x] (* x x))
      (defn sum-of-squares [a b] (+ (square a) (square b)))
      (sum-of-squares 3 4)
      """

      ast = Vaisto.Parser.parse(code)
      {:ok, :module, typed_ast} = Vaisto.TypeChecker.check(ast)
      {:ok, FnCallFnE2E, _} = Emitter.compile(typed_ast, FnCallFnE2E)

      # 3^2 + 4^2 = 9 + 16 = 25
      assert FnCallFnE2E.main() == 25
    end

    test "if expression - true branch" do
      code = "(if (> 5 3) 100 200)"

      ast = Vaisto.Parser.parse(code)
      {:ok, :int, typed_ast} = Vaisto.TypeChecker.check(ast)
      {:ok, IfTrueE2E, _} = Emitter.compile(typed_ast, IfTrueE2E)

      assert IfTrueE2E.main() == 100
    end

    test "if expression - false branch" do
      code = "(if (< 5 3) 100 200)"

      ast = Vaisto.Parser.parse(code)
      {:ok, :int, typed_ast} = Vaisto.TypeChecker.check(ast)
      {:ok, IfFalseE2E, _} = Emitter.compile(typed_ast, IfFalseE2E)

      assert IfFalseE2E.main() == 200
    end

    test "if with nested expressions" do
      code = "(if (== 1 1) (+ 10 20) (* 5 5))"

      ast = Vaisto.Parser.parse(code)
      {:ok, :int, typed_ast} = Vaisto.TypeChecker.check(ast)
      {:ok, IfNestedE2E, _} = Emitter.compile(typed_ast, IfNestedE2E)

      assert IfNestedE2E.main() == 30
    end

    test "if inside function" do
      code = """
      (defn my-abs [x] (if (< x 0) (- 0 x) x))
      (my-abs -5)
      """

      ast = Vaisto.Parser.parse(code)
      {:ok, :module, typed_ast} = Vaisto.TypeChecker.check(ast)
      {:ok, MyAbsE2E, _} = Emitter.compile(typed_ast, MyAbsE2E)

      assert MyAbsE2E.main() == 5
    end

    test "recursive factorial" do
      code = """
      (defn factorial [n]
        (if (== n 0)
          1
          (* n (factorial (- n 1)))))
      (factorial 5)
      """

      ast = Vaisto.Parser.parse(code)
      {:ok, :module, typed_ast} = Vaisto.TypeChecker.check(ast)
      {:ok, FactorialE2E, _} = Emitter.compile(typed_ast, FactorialE2E)

      # 5! = 120
      assert FactorialE2E.main() == 120
    end

    test "recursive fibonacci" do
      code = """
      (defn fib [n]
        (if (< n 2)
          n
          (+ (fib (- n 1)) (fib (- n 2)))))
      (fib 10)
      """

      ast = Vaisto.Parser.parse(code)
      {:ok, :module, typed_ast} = Vaisto.TypeChecker.check(ast)
      {:ok, FibE2E, _} = Emitter.compile(typed_ast, FibE2E)

      # fib(10) = 55
      assert FibE2E.main() == 55
    end

    test "mutual recursion" do
      code = """
      (defn is-even [n] (if (== n 0) true (is-odd (- n 1))))
      (defn is-odd [n] (if (== n 0) false (is-even (- n 1))))
      (is-even 10)
      """

      ast = Vaisto.Parser.parse(code)
      {:ok, :module, typed_ast} = Vaisto.TypeChecker.check(ast)
      {:ok, MutualE2E, _} = Emitter.compile(typed_ast, MutualE2E)

      assert MutualE2E.main() == true
    end

    test "string literal" do
      code = "\"hello world\""

      ast = Vaisto.Parser.parse(code)
      {:ok, :string, typed_ast} = Vaisto.TypeChecker.check(ast)
      {:ok, StringE2E, _} = Emitter.compile(typed_ast, StringE2E)

      assert StringE2E.main() == "hello world"
    end

    test "string with escape sequences" do
      code = "\"hello\\nworld\""

      ast = Vaisto.Parser.parse(code)
      {:ok, :string, typed_ast} = Vaisto.TypeChecker.check(ast)
      {:ok, EscapeE2E, _} = Emitter.compile(typed_ast, EscapeE2E)

      assert EscapeE2E.main() == "hello\nworld"
    end

    test "list literal" do
      code = "(list 1 2 3)"

      ast = Vaisto.Parser.parse(code)
      {:ok, {:list, :int}, typed_ast} = Vaisto.TypeChecker.check(ast)
      {:ok, ListE2E, _} = Emitter.compile(typed_ast, ListE2E)

      assert ListE2E.main() == [1, 2, 3]
    end

    test "empty list" do
      code = "(list)"

      ast = Vaisto.Parser.parse(code)
      {:ok, {:list, :any}, typed_ast} = Vaisto.TypeChecker.check(ast)
      {:ok, EmptyListE2E, _} = Emitter.compile(typed_ast, EmptyListE2E)

      assert EmptyListE2E.main() == []
    end

    test "nested list" do
      code = "(list (list 1 2) (list 3 4))"

      ast = Vaisto.Parser.parse(code)
      {:ok, {:list, {:list, :int}}, typed_ast} = Vaisto.TypeChecker.check(ast)
      {:ok, NestedListE2E, _} = Emitter.compile(typed_ast, NestedListE2E)

      assert NestedListE2E.main() == [[1, 2], [3, 4]]
    end

    test "list of strings" do
      code = "(list \"a\" \"b\" \"c\")"

      ast = Vaisto.Parser.parse(code)
      {:ok, {:list, :string}, typed_ast} = Vaisto.TypeChecker.check(ast)
      {:ok, StringListE2E, _} = Emitter.compile(typed_ast, StringListE2E)

      assert StringListE2E.main() == ["a", "b", "c"]
    end

    test "head of list" do
      code = "(head (list 1 2 3))"

      ast = Vaisto.Parser.parse(code)
      {:ok, :int, typed_ast} = Vaisto.TypeChecker.check(ast)
      {:ok, HeadE2E, _} = Emitter.compile(typed_ast, HeadE2E)

      assert HeadE2E.main() == 1
    end

    test "tail of list" do
      code = "(tail (list 1 2 3))"

      ast = Vaisto.Parser.parse(code)
      {:ok, {:list, :int}, typed_ast} = Vaisto.TypeChecker.check(ast)
      {:ok, TailE2E, _} = Emitter.compile(typed_ast, TailE2E)

      assert TailE2E.main() == [2, 3]
    end

    test "cons element to list" do
      code = "(cons 0 (list 1 2 3))"

      ast = Vaisto.Parser.parse(code)
      {:ok, {:list, :int}, typed_ast} = Vaisto.TypeChecker.check(ast)
      {:ok, ConsE2E, _} = Emitter.compile(typed_ast, ConsE2E)

      assert ConsE2E.main() == [0, 1, 2, 3]
    end

    test "cons to empty list" do
      code = "(cons 1 (list))"

      ast = Vaisto.Parser.parse(code)
      {:ok, {:list, :int}, typed_ast} = Vaisto.TypeChecker.check(ast)
      {:ok, ConsEmptyE2E, _} = Emitter.compile(typed_ast, ConsEmptyE2E)

      assert ConsEmptyE2E.main() == [1]
    end

    test "empty? on empty list" do
      code = "(empty? (list))"

      ast = Vaisto.Parser.parse(code)
      {:ok, :bool, typed_ast} = Vaisto.TypeChecker.check(ast)
      {:ok, EmptyTrueE2E, _} = Emitter.compile(typed_ast, EmptyTrueE2E)

      assert EmptyTrueE2E.main() == true
    end

    test "empty? on non-empty list" do
      code = "(empty? (list 1 2))"

      ast = Vaisto.Parser.parse(code)
      {:ok, :bool, typed_ast} = Vaisto.TypeChecker.check(ast)
      {:ok, EmptyFalseE2E, _} = Emitter.compile(typed_ast, EmptyFalseE2E)

      assert EmptyFalseE2E.main() == false
    end

    test "length of list" do
      code = "(length (list 1 2 3 4 5))"

      ast = Vaisto.Parser.parse(code)
      {:ok, :int, typed_ast} = Vaisto.TypeChecker.check(ast)
      {:ok, LengthE2E, _} = Emitter.compile(typed_ast, LengthE2E)

      assert LengthE2E.main() == 5
    end

    test "chained list operations" do
      code = "(head (tail (tail (list 1 2 3 4))))"

      ast = Vaisto.Parser.parse(code)
      {:ok, :int, typed_ast} = Vaisto.TypeChecker.check(ast)
      {:ok, ChainedE2E, _} = Emitter.compile(typed_ast, ChainedE2E)

      # tail [1,2,3,4] = [2,3,4], tail [2,3,4] = [3,4], head [3,4] = 3
      assert ChainedE2E.main() == 3
    end

    test "recursive sum with list operations" do
      code = """
      (defn sum [xs]
        (if (empty? xs)
          0
          (+ (head xs) (sum (tail xs)))))
      (sum (list 1 2 3 4 5))
      """

      ast = Vaisto.Parser.parse(code)
      {:ok, :module, typed_ast} = Vaisto.TypeChecker.check(ast)
      {:ok, RecSumE2E, _} = Emitter.compile(typed_ast, RecSumE2E)

      assert RecSumE2E.main() == 15
    end

    test "recursive list reverse" do
      code = """
      (defn reverse-helper [xs acc]
        (if (empty? xs)
          acc
          (reverse-helper (tail xs) (cons (head xs) acc))))
      (defn reverse [xs]
        (reverse-helper xs (list)))
      (reverse (list 1 2 3 4))
      """

      ast = Vaisto.Parser.parse(code)
      {:ok, :module, typed_ast} = Vaisto.TypeChecker.check(ast)
      {:ok, ReverseE2E, _} = Emitter.compile(typed_ast, ReverseE2E)

      assert ReverseE2E.main() == [4, 3, 2, 1]
    end

    test "map with user-defined function" do
      code = """
      (defn double [x] (* x 2))
      (map double (list 1 2 3 4 5))
      """

      ast = Vaisto.Parser.parse(code)
      {:ok, :module, typed_ast} = Vaisto.TypeChecker.check(ast)
      {:ok, MapE2E, _} = Emitter.compile(typed_ast, MapE2E)

      assert MapE2E.main() == [2, 4, 6, 8, 10]
    end

    test "filter with user-defined predicate" do
      code = """
      (defn positive? [x] (> x 0))
      (filter positive? (list -2 -1 0 1 2 3))
      """

      ast = Vaisto.Parser.parse(code)
      {:ok, :module, typed_ast} = Vaisto.TypeChecker.check(ast)
      {:ok, FilterE2E, _} = Emitter.compile(typed_ast, FilterE2E)

      assert FilterE2E.main() == [1, 2, 3]
    end

    test "fold with user-defined function" do
      code = """
      (defn add [acc x] (+ acc x))
      (fold add 0 (list 1 2 3 4 5))
      """

      ast = Vaisto.Parser.parse(code)
      {:ok, :module, typed_ast} = Vaisto.TypeChecker.check(ast)
      {:ok, FoldE2E, _} = Emitter.compile(typed_ast, FoldE2E)

      assert FoldE2E.main() == 15
    end

    test "chained map and filter" do
      code = """
      (defn double [x] (* x 2))
      (defn big? [x] (> x 5))
      (filter big? (map double (list 1 2 3 4 5)))
      """

      ast = Vaisto.Parser.parse(code)
      {:ok, :module, typed_ast} = Vaisto.TypeChecker.check(ast)
      {:ok, ChainMapFilterE2E, _} = Emitter.compile(typed_ast, ChainMapFilterE2E)

      # [1,2,3,4,5] -> [2,4,6,8,10] -> [6,8,10]
      assert ChainMapFilterE2E.main() == [6, 8, 10]
    end

    test "fold to build list (reverse)" do
      code = """
      (defn prepend [acc x] (cons x acc))
      (fold prepend (list) (list 1 2 3 4))
      """

      ast = Vaisto.Parser.parse(code)
      {:ok, :module, typed_ast} = Vaisto.TypeChecker.check(ast)
      {:ok, FoldReverseE2E, _} = Emitter.compile(typed_ast, FoldReverseE2E)

      assert FoldReverseE2E.main() == [4, 3, 2, 1]
    end

    test "fold to compute product" do
      code = """
      (defn multiply [acc x] (* acc x))
      (fold multiply 1 (list 1 2 3 4 5))
      """

      ast = Vaisto.Parser.parse(code)
      {:ok, :module, typed_ast} = Vaisto.TypeChecker.check(ast)
      {:ok, FoldProductE2E, _} = Emitter.compile(typed_ast, FoldProductE2E)

      # 1*2*3*4*5 = 120
      assert FoldProductE2E.main() == 120
    end

    test "typed record fields with bracket syntax" do
      code = """
      (deftype point [x :int y :int])
      (match (point 3 4) [(point a b) (+ a b)])
      """

      ast = Vaisto.Parser.parse(code)
      {:ok, :module, {:module, [_deftype, match_expr]}} = Vaisto.TypeChecker.check(ast)
      {:ok, TypedRecordE2E, _} = Emitter.compile(match_expr, TypedRecordE2E)

      # 3 + 4 = 7
      assert TypedRecordE2E.main() == 7
    end

    test "typed record with function using pattern match" do
      code = """
      (deftype vec2 [x :int y :int])
      (defn magnitude-squared [v]
        (match v [(vec2 x y) (+ (* x x) (* y y))]))
      (magnitude-squared (vec2 3 4))
      """

      ast = Vaisto.Parser.parse(code)
      {:ok, :module, typed_ast} = Vaisto.TypeChecker.check(ast)
      {:ok, Vec2E2E, _} = Emitter.compile(typed_ast, Vec2E2E)

      # 3^2 + 4^2 = 9 + 16 = 25
      assert Vec2E2E.main() == 25
    end

    # --- Multi-clause function tests ---

    test "multi-clause function with list patterns" do
      code = """
      (defn len
        [[] 0]
        [[h | t] (+ 1 (len t))])
      (len (list 1 2 3 4 5))
      """

      ast = Vaisto.Parser.parse(code)
      {:ok, :module, typed_ast} = Vaisto.TypeChecker.check(ast)
      {:ok, LenE2E, _} = Emitter.compile(typed_ast, LenE2E)

      assert LenE2E.main() == 5
    end

    test "multi-clause sum function" do
      code = """
      (defn sum
        [[] 0]
        [[h | t] (+ h (sum t))])
      (sum (list 1 2 3 4 5))
      """

      ast = Vaisto.Parser.parse(code)
      {:ok, :module, typed_ast} = Vaisto.TypeChecker.check(ast)
      {:ok, SumMultiE2E, _} = Emitter.compile(typed_ast, SumMultiE2E)

      # 1 + 2 + 3 + 4 + 5 = 15
      assert SumMultiE2E.main() == 15
    end

    test "multi-clause last element function" do
      code = """
      (defn last
        [[x] x]
        [[h | t] (last t)])
      (last (list 1 2 3 4 5))
      """

      ast = Vaisto.Parser.parse(code)
      {:ok, :module, typed_ast} = Vaisto.TypeChecker.check(ast)
      {:ok, LastE2E, _} = Emitter.compile(typed_ast, LastE2E)

      assert LastE2E.main() == 5
    end

    # --- Anonymous function tests ---

    test "anonymous function with map" do
      code = """
      (map (fn [x] (* x 2)) (list 1 2 3 4 5))
      """

      ast = Vaisto.Parser.parse(code)
      {:ok, {:list, :int}, typed_ast} = Vaisto.TypeChecker.check(ast)
      {:ok, AnonMapE2E, _} = Emitter.compile(typed_ast, AnonMapE2E)

      assert AnonMapE2E.main() == [2, 4, 6, 8, 10]
    end

    test "anonymous function with filter" do
      code = """
      (filter (fn [x] (> x 2)) (list 1 2 3 4 5))
      """

      ast = Vaisto.Parser.parse(code)
      {:ok, {:list, :int}, typed_ast} = Vaisto.TypeChecker.check(ast)
      {:ok, AnonFilterE2E, _} = Emitter.compile(typed_ast, AnonFilterE2E)

      assert AnonFilterE2E.main() == [3, 4, 5]
    end

    test "anonymous function with fold" do
      code = """
      (fold (fn [acc x] (+ acc x)) 0 (list 1 2 3 4 5))
      """

      ast = Vaisto.Parser.parse(code)
      {:ok, :int, typed_ast} = Vaisto.TypeChecker.check(ast)
      {:ok, AnonFoldE2E, _} = Emitter.compile(typed_ast, AnonFoldE2E)

      assert AnonFoldE2E.main() == 15
    end

    test "chained anonymous functions" do
      code = """
      (filter (fn [x] (> x 5)) (map (fn [x] (* x 2)) (list 1 2 3 4 5)))
      """

      ast = Vaisto.Parser.parse(code)
      {:ok, {:list, :int}, typed_ast} = Vaisto.TypeChecker.check(ast)
      {:ok, ChainAnonE2E, _} = Emitter.compile(typed_ast, ChainAnonE2E)

      # [1,2,3,4,5] -> [2,4,6,8,10] -> [6,8,10]
      assert ChainAnonE2E.main() == [6, 8, 10]
    end

    # --- Extern function tests ---

    test "extern erlang:hd" do
      code = """
      (extern erlang:hd [:any] :any)
      (erlang:hd (list 1 2 3))
      """

      ast = Vaisto.Parser.parse(code)
      {:ok, :module, typed_ast} = Vaisto.TypeChecker.check(ast)
      {:ok, ExternHdE2E, _} = Emitter.compile(typed_ast, ExternHdE2E)

      assert ExternHdE2E.main() == 1
    end

    test "extern erlang:tl" do
      code = """
      (extern erlang:tl [:any] :any)
      (erlang:tl (list 1 2 3))
      """

      ast = Vaisto.Parser.parse(code)
      {:ok, :module, typed_ast} = Vaisto.TypeChecker.check(ast)
      {:ok, ExternTlE2E, _} = Emitter.compile(typed_ast, ExternTlE2E)

      assert ExternTlE2E.main() == [2, 3]
    end

    test "extern erlang:length" do
      code = """
      (extern erlang:length [:any] :int)
      (erlang:length (list 1 2 3 4 5))
      """

      ast = Vaisto.Parser.parse(code)
      {:ok, :module, typed_ast} = Vaisto.TypeChecker.check(ast)
      {:ok, ExternLengthE2E, _} = Emitter.compile(typed_ast, ExternLengthE2E)

      assert ExternLengthE2E.main() == 5
    end

    test "chained extern calls" do
      code = """
      (extern erlang:hd [:any] :any)
      (extern erlang:tl [:any] :any)
      (erlang:hd (erlang:tl (erlang:tl (list 1 2 3 4))))
      """

      ast = Vaisto.Parser.parse(code)
      {:ok, :module, typed_ast} = Vaisto.TypeChecker.check(ast)
      {:ok, ChainedExternE2E, _} = Emitter.compile(typed_ast, ChainedExternE2E)

      # tl [1,2,3,4] = [2,3,4], tl [2,3,4] = [3,4], hd [3,4] = 3
      assert ChainedExternE2E.main() == 3
    end

    test "extern with user function" do
      code = """
      (extern erlang:abs [:int] :int)
      (defn my-abs [x] (erlang:abs x))
      (my-abs -42)
      """

      ast = Vaisto.Parser.parse(code)
      {:ok, :module, typed_ast} = Vaisto.TypeChecker.check(ast)
      {:ok, ExternInFnE2E, _} = Emitter.compile(typed_ast, ExternInFnE2E)

      assert ExternInFnE2E.main() == 42
    end

    # --- Receive tests ---

    test "receive parses and type checks" do
      code = "(receive [:ping 1] [:pong 2])"

      ast = Vaisto.Parser.parse(code)
      assert {:receive, [ping_clause, pong_clause]} = ast
      # Parser now wraps atoms in {:atom, value} to distinguish from variables
      assert {{:atom, :ping}, 1} = ping_clause
      assert {{:atom, :pong}, 2} = pong_clause

      {:ok, :int, typed_ast} = Vaisto.TypeChecker.check(ast)
      assert {:receive, typed_clauses, :int} = typed_ast
      assert length(typed_clauses) == 2
    end

    test "receive with variable binding" do
      # Use a simple variable pattern (receives any message and binds to x)
      code = "(receive [x x])"

      ast = Vaisto.Parser.parse(code)
      {:ok, :any, typed_ast} = Vaisto.TypeChecker.check(ast)

      assert {:receive, [{_pattern, _body, :any}], :any} = typed_ast
    end

    test "receive emits to elixir receive expression" do
      code = "(receive [:ping 42])"

      ast = Vaisto.Parser.parse(code)
      {:ok, :int, typed_ast} = Vaisto.TypeChecker.check(ast)
      elixir_ast = Emitter.to_elixir(typed_ast)

      # Should produce: receive do :ping -> 42 end
      assert {:receive, [], [[do: _clauses]]} = elixir_ast
    end

    test "receive end-to-end with message in mailbox" do
      # Test receive by manually sending a message before calling main
      # This demonstrates that the receive expression compiles correctly
      code = "(receive [:ping 42] [:pong 99])"

      ast = Vaisto.Parser.parse(code)
      {:ok, :int, typed_ast} = Vaisto.TypeChecker.check(ast)
      {:ok, ReceiveE2E, _} = Emitter.compile(typed_ast, ReceiveE2E)

      # Send :ping to self before calling main which blocks on receive
      send(self(), :ping)
      assert ReceiveE2E.main() == 42

      # Test the other pattern too
      send(self(), :pong)
      assert ReceiveE2E.main() == 99
    end
  end
end
