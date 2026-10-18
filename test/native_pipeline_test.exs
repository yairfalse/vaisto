defmodule Vaisto.NativePipelineTest do
  use ExUnit.Case
  alias Vaisto.Compilation

  describe "end-to-end: parse → type check → emit → run" do
    test "simple arithmetic" do
      code = "(+ 1 2)"

      # Parse - now includes location
      ast = Vaisto.Parser.parse(code)
      assert {:call, :+, [1, 2], %Vaisto.Parser.Loc{}} = ast

      # Type check

      # Compile and run
      {:ok, E2E1, _} = Compilation.compile(code, E2E1)
      assert apply(E2E1, :main, []) == 3
    end

    test "nested arithmetic" do
      code = "(* (+ 1 2) (- 5 1))"

      {:ok, E2E2, _} = Compilation.compile(code, E2E2)

      # (1 + 2) * (5 - 1) = 3 * 4 = 12
      assert apply(E2E2, :main, []) == 12
    end

    test "let bindings" do
      code = "(let [x 1 y 2] (+ x y))"

      {:ok, LetE2E, _} = Compilation.compile(code, LetE2E)

      assert apply(LetE2E, :main, []) == 3
    end

    test "nested let bindings" do
      code = "(let [a 10] (let [b (+ a 5)] (* b 2)))"

      {:ok, NestedLetE2E, _} = Compilation.compile(code, NestedLetE2E)

      # (10 + 5) * 2 = 30
      assert apply(NestedLetE2E, :main, []) == 30
    end

    test "record construction and pattern matching" do
      code = """
      (deftype point [x :int y :int])
      (match (point 1 2) [(point a b) (+ a b)])
      """

      {:ok, RecordMatchE2E, _} = Compilation.compile(code, RecordMatchE2E)

      # 1 + 2 = 3
      assert apply(RecordMatchE2E, :main, []) == 3
    end

    test "nested record pattern matching" do
      code = """
      (deftype pair [a :int b :int])
      (match (pair 10 20)
        [(pair x y) (* x y)])
      """

      {:ok, NestedRecordE2E, _} = Compilation.compile(code, NestedRecordE2E)

      # 10 * 20 = 200
      assert apply(NestedRecordE2E, :main, []) == 200
    end

    test "user-defined function" do
      code = """
      (defn add [x :int y :int] :int (+ x y))
      (add 3 4)
      """

      {:ok, DefnE2E, _} = Compilation.compile(code, DefnE2E)

      assert apply(DefnE2E, :main, []) == 7
    end

    test "multiple user-defined functions" do
      code = """
      (defn double [x :int] :int (* x 2))
      (defn add-one [x :int] :int (+ x 1))
      (double (add-one 5))
      """

      {:ok, MultiFnE2E, _} = Compilation.compile(code, MultiFnE2E)

      # (5 + 1) * 2 = 12
      assert apply(MultiFnE2E, :main, []) == 12
    end

    test "function calling another function" do
      code = """
      (defn square [x :int] :int (* x x))
      (defn sum-of-squares [a :int b :int] :int (+ (square a) (square b)))
      (sum-of-squares 3 4)
      """

      {:ok, FnCallFnE2E, _} = Compilation.compile(code, FnCallFnE2E)

      # 3^2 + 4^2 = 9 + 16 = 25
      assert apply(FnCallFnE2E, :main, []) == 25
    end

    test "if expression - true branch" do
      code = "(if (> 5 3) 100 200)"

      {:ok, IfTrueE2E, _} = Compilation.compile(code, IfTrueE2E)

      assert apply(IfTrueE2E, :main, []) == 100
    end

    test "if expression - false branch" do
      code = "(if (< 5 3) 100 200)"

      {:ok, IfFalseE2E, _} = Compilation.compile(code, IfFalseE2E)

      assert apply(IfFalseE2E, :main, []) == 200
    end

    test "if with nested expressions" do
      code = "(if (== 1 1) (+ 10 20) (* 5 5))"

      {:ok, IfNestedE2E, _} = Compilation.compile(code, IfNestedE2E)

      assert apply(IfNestedE2E, :main, []) == 30
    end

    test "if inside function" do
      code = """
      (defn my-abs [x :int] :int (if (< x 0) (- 0 x) x))
      (my-abs -5)
      """

      {:ok, MyAbsE2E, _} = Compilation.compile(code, MyAbsE2E)

      assert apply(MyAbsE2E, :main, []) == 5
    end

    test "recursive factorial" do
      code = """
      (defn factorial [n :int] :int
        (if (== n 0)
          1
          (* n (factorial (- n 1)))))
      (factorial 5)
      """

      {:ok, FactorialE2E, _} = Compilation.compile(code, FactorialE2E)

      # 5! = 120
      assert apply(FactorialE2E, :main, []) == 120
    end

    test "recursive fibonacci" do
      code = """
      (defn fib [n :int] :int
        (if (< n 2)
          n
          (+ (fib (- n 1)) (fib (- n 2)))))
      (fib 10)
      """

      {:ok, FibE2E, _} = Compilation.compile(code, FibE2E)

      # fib(10) = 55
      assert apply(FibE2E, :main, []) == 55
    end

    test "mutual recursion" do
      code = """
      (defn is-even [n :int] :bool (if (== n 0) true (is-odd (- n 1))))
      (defn is-odd [n :int] :bool (if (== n 0) false (is-even (- n 1))))
      (is-even 10)
      """

      {:ok, MutualE2E, _} = Compilation.compile(code, MutualE2E)

      assert apply(MutualE2E, :main, []) == true
    end

    test "string literal" do
      code = "\"hello world\""

      {:ok, StringE2E, _} = Compilation.compile(code, StringE2E)

      assert apply(StringE2E, :main, []) == "hello world"
    end

    test "string with escape sequences" do
      code = "\"hello\\nworld\""

      {:ok, EscapeE2E, _} = Compilation.compile(code, EscapeE2E)

      assert apply(EscapeE2E, :main, []) == "hello\nworld"
    end

    test "list literal" do
      code = "(list 1 2 3)"

      {:ok, ListE2E, _} = Compilation.compile(code, ListE2E)

      assert apply(ListE2E, :main, []) == [1, 2, 3]
    end

    test "empty list" do
      code = "(the (List :int) (list))"

      {:ok, EmptyListE2E, _} = Compilation.compile(code, EmptyListE2E)

      assert apply(EmptyListE2E, :main, []) == []
    end

    test "nested list" do
      code = "(list (list 1 2) (list 3 4))"

      {:ok, NestedListE2E, _} = Compilation.compile(code, NestedListE2E)

      assert apply(NestedListE2E, :main, []) == [[1, 2], [3, 4]]
    end

    test "list of strings" do
      code = "(list \"a\" \"b\" \"c\")"

      {:ok, StringListE2E, _} = Compilation.compile(code, StringListE2E)

      assert apply(StringListE2E, :main, []) == ["a", "b", "c"]
    end

    test "head of list" do
      code = "(head (list 1 2 3))"

      {:ok, HeadE2E, _} = Compilation.compile(code, HeadE2E)

      assert apply(HeadE2E, :main, []) == 1
    end

    test "tail of list" do
      code = "(tail (list 1 2 3))"

      {:ok, TailE2E, _} = Compilation.compile(code, TailE2E)

      assert apply(TailE2E, :main, []) == [2, 3]
    end

    test "cons element to list" do
      code = "(cons 0 (list 1 2 3))"

      {:ok, ConsE2E, _} = Compilation.compile(code, ConsE2E)

      assert apply(ConsE2E, :main, []) == [0, 1, 2, 3]
    end

    test "cons to empty list" do
      code = "(cons 1 (list))"

      {:ok, ConsEmptyE2E, _} = Compilation.compile(code, ConsEmptyE2E)

      assert apply(ConsEmptyE2E, :main, []) == [1]
    end

    test "empty? on empty list" do
      code = "(empty? (the (List :int) (list)))"

      {:ok, EmptyTrueE2E, _} = Compilation.compile(code, EmptyTrueE2E)

      assert apply(EmptyTrueE2E, :main, []) == true
    end

    test "empty? on non-empty list" do
      code = "(empty? (list 1 2))"

      {:ok, EmptyFalseE2E, _} = Compilation.compile(code, EmptyFalseE2E)

      assert apply(EmptyFalseE2E, :main, []) == false
    end

    test "length of list" do
      code = "(length (list 1 2 3 4 5))"

      {:ok, LengthE2E, _} = Compilation.compile(code, LengthE2E)

      assert apply(LengthE2E, :main, []) == 5
    end

    test "chained list operations" do
      code = "(head (tail (tail (list 1 2 3 4))))"

      {:ok, ChainedE2E, _} = Compilation.compile(code, ChainedE2E)

      # tail [1,2,3,4] = [2,3,4], tail [2,3,4] = [3,4], head [3,4] = 3
      assert apply(ChainedE2E, :main, []) == 3
    end

    test "recursive sum with list operations" do
      code = """
      (defn sum [xs (List :int)] :int
        (if (empty? xs)
          0
          (+ (head xs) (sum (tail xs)))))
      (sum (list 1 2 3 4 5))
      """

      {:ok, RecSumE2E, _} = Compilation.compile(code, RecSumE2E)

      assert apply(RecSumE2E, :main, []) == 15
    end

    test "recursive list reverse" do
      code = """
      (defn reverse-helper [xs (List :a) acc (List :a)] (List :a)
        (if (empty? xs)
          acc
          (reverse-helper (tail xs) (cons (head xs) acc))))
      (defn reverse [xs (List :a)] (List :a)
        (reverse-helper xs (list)))
      (reverse (list 1 2 3 4))
      """

      {:ok, ReverseE2E, _} = Compilation.compile(code, ReverseE2E)

      assert apply(ReverseE2E, :main, []) == [4, 3, 2, 1]
    end

    test "map with user-defined function" do
      code = """
      (defn double [x :int] :int (* x 2))
      (map double (list 1 2 3 4 5))
      """

      {:ok, MapE2E, _} = Compilation.compile(code, MapE2E)

      assert apply(MapE2E, :main, []) == [2, 4, 6, 8, 10]
    end

    test "filter with user-defined predicate" do
      code = """
      (defn positive? [x :int] :bool (> x 0))
      (filter positive? (list -2 -1 0 1 2 3))
      """

      {:ok, FilterE2E, _} = Compilation.compile(code, FilterE2E)

      assert apply(FilterE2E, :main, []) == [1, 2, 3]
    end

    test "fold with user-defined function" do
      code = """
      (defn add [acc :int x :int] :int (+ acc x))
      (fold add 0 (list 1 2 3 4 5))
      """

      {:ok, FoldE2E, _} = Compilation.compile(code, FoldE2E)

      assert apply(FoldE2E, :main, []) == 15
    end

    test "chained map and filter" do
      code = """
      (defn double [x :int] :int (* x 2))
      (defn big? [x :int] :bool (> x 5))
      (filter big? (map double (list 1 2 3 4 5)))
      """

      {:ok, ChainMapFilterE2E, _} = Compilation.compile(code, ChainMapFilterE2E)

      # [1,2,3,4,5] -> [2,4,6,8,10] -> [6,8,10]
      assert apply(ChainMapFilterE2E, :main, []) == [6, 8, 10]
    end

    test "fold to build list (reverse)" do
      code = """
      (defn prepend [acc (List :int) x :int] (List :int) (cons x acc))
      (fold prepend (list) (list 1 2 3 4))
      """

      {:ok, FoldReverseE2E, _} = Compilation.compile(code, FoldReverseE2E)

      assert apply(FoldReverseE2E, :main, []) == [4, 3, 2, 1]
    end

    test "fold to compute product" do
      code = """
      (defn multiply [acc :int x :int] :int (* acc x))
      (fold multiply 1 (list 1 2 3 4 5))
      """

      {:ok, FoldProductE2E, _} = Compilation.compile(code, FoldProductE2E)

      # 1*2*3*4*5 = 120
      assert apply(FoldProductE2E, :main, []) == 120
    end

    test "typed record fields with bracket syntax" do
      code = """
      (deftype point [x :int y :int])
      (match (point 3 4) [(point a b) (+ a b)])
      """

      {:ok, TypedRecordE2E, _} = Compilation.compile(code, TypedRecordE2E)

      # 3 + 4 = 7
      assert apply(TypedRecordE2E, :main, []) == 7
    end

    test "typed record with function using pattern match" do
      code = """
      (deftype vec2 [x :int y :int])
      (defn magnitude-squared [v :vec2] :int
        (match v [(vec2 x y) (+ (* x x) (* y y))]))
      (magnitude-squared (vec2 3 4))
      """

      {:ok, Vec2E2E, _} = Compilation.compile(code, Vec2E2E)

      # 3^2 + 4^2 = 9 + 16 = 25
      assert apply(Vec2E2E, :main, []) == 25
    end

    # --- Multi-clause function tests ---

    test "multi-clause function with list patterns" do
      code = """
      (defn len (Fn (List :a) :int)
        [[] 0]
        [[h | t] (+ 1 (len t))])
      (len (list 1 2 3 4 5))
      """

      {:ok, LenE2E, _} = Compilation.compile(code, LenE2E)

      assert apply(LenE2E, :main, []) == 5
    end

    test "multi-clause sum function" do
      code = """
      (defn sum (Fn (List :int) :int)
        [[] 0]
        [[h | t] (+ h (sum t))])
      (sum (list 1 2 3 4 5))
      """

      {:ok, SumMultiE2E, _} = Compilation.compile(code, SumMultiE2E)

      # 1 + 2 + 3 + 4 + 5 = 15
      assert apply(SumMultiE2E, :main, []) == 15
    end

    test "multi-clause last element function" do
      code = """
      (defn last (Fn (List :int) :int)
        [[] (crash :no_match)]
        [[x] x]
        [[h | t] (last t)])
      (last (list 1 2 3 4 5))
      """

      {:ok, LastE2E, _} = Compilation.compile(code, LastE2E)

      assert apply(LastE2E, :main, []) == 5
    end

    # --- Anonymous function tests ---

    test "anonymous function with map" do
      code = """
      (map (fn [x] (* x 2)) (list 1 2 3 4 5))
      """

      {:ok, AnonMapE2E, _} = Compilation.compile(code, AnonMapE2E)

      assert apply(AnonMapE2E, :main, []) == [2, 4, 6, 8, 10]
    end

    test "anonymous function with filter" do
      code = """
      (filter (fn [x] (> x 2)) (list 1 2 3 4 5))
      """

      {:ok, AnonFilterE2E, _} = Compilation.compile(code, AnonFilterE2E)

      assert apply(AnonFilterE2E, :main, []) == [3, 4, 5]
    end

    test "anonymous function with fold" do
      code = """
      (fold (fn [acc x] (+ acc x)) 0 (list 1 2 3 4 5))
      """

      {:ok, AnonFoldE2E, _} = Compilation.compile(code, AnonFoldE2E)

      assert apply(AnonFoldE2E, :main, []) == 15
    end

    test "chained anonymous functions" do
      code = """
      (filter (fn [x] (> x 5)) (map (fn [x] (* x 2)) (list 1 2 3 4 5)))
      """

      {:ok, ChainAnonE2E, _} = Compilation.compile(code, ChainAnonE2E)

      # [1,2,3,4,5] -> [2,4,6,8,10] -> [6,8,10]
      assert apply(ChainAnonE2E, :main, []) == [6, 8, 10]
    end

    # --- Extern function tests ---

    test "extern erlang:hd" do
      code = """
      (extern erlang:hd [(List :int)] :int)
      (erlang:hd (list 1 2 3))
      """

      {:ok, ExternHdE2E, _} = Compilation.compile(code, ExternHdE2E)

      assert apply(ExternHdE2E, :main, []) == 1
    end

    test "extern erlang:tl" do
      code = """
      (extern erlang:tl [(List :int)] (List :int))
      (erlang:tl (list 1 2 3))
      """

      {:ok, ExternTlE2E, _} = Compilation.compile(code, ExternTlE2E)

      assert apply(ExternTlE2E, :main, []) == [2, 3]
    end

    test "extern erlang:length" do
      code = """
      (extern erlang:length [(List :int)] :int)
      (erlang:length (list 1 2 3 4 5))
      """

      {:ok, ExternLengthE2E, _} = Compilation.compile(code, ExternLengthE2E)

      assert apply(ExternLengthE2E, :main, []) == 5
    end

    test "chained extern calls" do
      code = """
      (extern erlang:hd [(List :int)] :int)
      (extern erlang:tl [(List :int)] (List :int))
      (erlang:hd (erlang:tl (erlang:tl (list 1 2 3 4))))
      """

      {:ok, ChainedExternE2E, _} = Compilation.compile(code, ChainedExternE2E)

      # tl [1,2,3,4] = [2,3,4], tl [2,3,4] = [3,4], hd [3,4] = 3
      assert apply(ChainedExternE2E, :main, []) == 3
    end

    test "extern with user function" do
      code = """
      (extern erlang:abs [:int] :int)
      (defn my-abs [x :int] :int (erlang:abs x))
      (my-abs -42)
      """

      {:ok, ExternInFnE2E, _} = Compilation.compile(code, ExternInFnE2E)

      assert apply(ExternInFnE2E, :main, []) == 42
    end

    # --- Receive tests ---

    test "receive parses and type checks" do
      code = "(receive [:ping 1] [:pong 2])"

      ast = Vaisto.Parser.parse(code)
      # AST now includes location
      assert {:receive, [ping_clause, pong_clause], %Vaisto.Parser.Loc{}} = ast
      # Parser now wraps atoms in {:atom, value} to distinguish from variables
      assert {{:atom, :ping}, 1} = ping_clause
      assert {{:atom, :pong}, 2} = pong_clause

      assert {:ok, core} = Compilation.check_liquid(code)
      assert Vaisto.Liquid.Canonical.render(core) =~ "(receive"
    end

    test "receive with variable binding" do
      # Use a simple variable pattern (receives any message and binds to x)
      code = "(receive [x x])"

      assert {:ok, _} = Compilation.check_liquid("(defn main [] :dyn (receive [x x]))")
    end

    test "receive lowers to Erlang receive forms" do
      code = "(receive [:ping 42])"

      assert {:ok, core} = Compilation.check_liquid(code)
      assert {:ok, forms} = Vaisto.Liquid.Lower.to_forms(core, NativeReceiveForms)
      assert inspect(forms) =~ ":receive"
    end

    test "receive end-to-end with message in mailbox" do
      # Test receive by manually sending a message before calling main
      # This demonstrates that the receive expression compiles correctly
      code = "(receive [:ping 42] [:pong 99])"

      {:ok, ReceiveE2E, _} = Compilation.compile(code, ReceiveE2E)

      # Send :ping to self before calling main which blocks on receive
      send(self(), :ping)
      assert apply(ReceiveE2E, :main, []) == 42

      # Test the other pattern too
      send(self(), :pong)
      assert apply(ReceiveE2E, :main, []) == 99
    end
  end
end
