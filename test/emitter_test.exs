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
  end
end
