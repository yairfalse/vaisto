defmodule Vaisto.MultiClauseTest do
  # Multi-clause functions with a signature (owner decision, 2026-10-04: the
  # (Fn ...) type after the name), and guards in match clauses (D27), which they
  # need. Each runs on both backends and through the Phase 1a elaborator.
  use ExUnit.Case, async: true

  alias Vaisto.{Elab, Parser, Runner}
  alias Vaisto.Liquid.{Harness, Lint}

  defp strip(ast), do: ast |> Vaisto.LocationStripper.strip() |> elem(0)

  defp run_both(source) do
    results = for backend <- [:core, :elixir], do: Runner.run(source, backend: backend)
    assert [r, r] = results, "the backends disagree: #{inspect(results)}"
    r
  end

  defp elab(source) do
    case Elab.module(Parser.parse(source)) do
      {:ok, core} = ok ->
        assert :ok = Lint.check_module(core)
        ok

      other ->
        other
    end
  end

  # The first error's message, from parsing or type checking.
  defp error(source) do
    assert {:error, e} = Runner.run(source)
    e = if is_list(e), do: hd(e), else: e
    e.message
  end

  describe "the signature after the name" do
    test "is a single-clause defn whose body matches on its parameter" do
      assert {:defn, :len, [{:"arg 1", {:call, :List, _, _}}], {:match, :"arg 1", [_, _], _}, :int} =
               strip(Parser.parse("(defn len (Fn (List :a) :int) [[] 0] [[h | t] (+ 1 (len t))])"))
    end

    test "its arity says how many patterns each clause has" do
      assert {:defn, :add, [{:"arg 1", :int}, {:"arg 2", :int}], {:match, {:tuple, [:"arg 1", :"arg 2"], _}, [{{:tuple_pattern, [0, :y]}, :y}, _], _}, :int} =
               strip(Parser.parse("(defn add (Fn :int :int :int) [0 y y] [x y (+ x y)])"))
    end

    test "a clause is its patterns and one body, so a wrong count is an error and not a longer body" do
      assert {:error, e, _} = Parser.parse("(defn add (Fn :int :int :int) [0 1])")
      assert e.message =~ "this clause has 2 items, but the signature takes 2 arguments"
      assert {:error, e, _} = Parser.parse("(defn pick (Fn :atom :atom) [a b b])")
      assert e.message =~ "this clause has 3 items, but the signature takes 1 argument"
      assert {:ok, :ok} = run_both("(defn f (Fn :int :atom) [x (do (+ x 1) :ok)])\n(defn main [] :atom (f 1))")
    end

    test "after the patterns, :when begins a guard, which needs a body" do
      assert {:error, e, _} = Parser.parse("(defn big? (Fn :int :bool) [x :when (> x 100)])")
      assert e.message =~ "a guarded clause is written [pattern :when guard body]"
      assert {:error, e, _} = Parser.parse("(defn f (Fn :int :atom) [x :when])")
      assert e.message =~ "a guarded clause"
      assert {:ok, 1} = run_both("(defn f (Fn :atom :int) [:when 1] [_ 0])\n(defn main [] :int (f :when))")
    end

    test "a signature lists types, and a definition is named by a plain name" do
      for {source, message} <- [
            {"(defn f (Fn (List 5) :int) [x 1])", "`5` is not a type"},
            {"(defn f (Fn (List {v :int | (> v 0)}) :int) [x 1])", "a refined type is a whole argument or result type"},
            {"(defn)", "a definition names a function"},
            {"(defn f)", "a definition has parameters and a body"},
            {"(defn k (Fn :int) [1])", "names at least one argument type and the result type"},
            {"(defn f (Fn :int :int))", "is followed by the function's clauses"},
            {"(defn f (Fn :int :int) [0 1] 2)", "has only clauses"},
            {"(defn f (Fn 5 :int) [x x])", "`5` is not a type"},
            {"(defn f (Fn x :int) [y 1])", "`x` is not a type"},
            {"(defn [x] (Fn :int :int) [1 1])", "a definition is named by a plain name"},
            {"(defn \"f\" (Fn :int :int) [1 1])", "a definition is named by a plain name"}
          ] do
        assert {:error, e, _} = Parser.parse(source), source
        assert e.message =~ message, source
      end
    end

    test "names declared types, plain and applied, as single-clause annotations do (P0-1)" do
      c = "(deftype C (R :int) (G :int))\n"
      assert {:ok, -4} = run_both(c <> "(defn f (Fn C :int) [(R n) n] [(G n) (- 0 n)])\n(defn main [] :int (f (G 4)))")
      maybe = "(deftype Maybe (Just v) (Nothing))\n"
      assert {:ok, 4} = run_both(maybe <> "(defn f (Fn (Maybe :int) :int) [(Just n) n] [(Nothing) 0])\n(defn main [] :int (f (Just 4)))")
      pt = "(deftype Pt [x :int y :int])\n"
      assert {:ok, 1} = run_both(pt <> "(defn f (Fn Pt :int) [p :when (> (. p :x) 0) 1] [_ 0])\n(defn main [] :int (f (Pt 1 2)))")
      assert %{verdict: :agree} = Harness.run(c <> "(defn f (Fn C :int) [(R n) n] [(G n) (- 0 n)])\n(defn main [] :int (f (G 4)))")
    end

    test "runs on both backends, with recursion, several arguments and guards" do
      assert {:ok, 120} = run_both("(defn fact (Fn :int :int) [0 1] [n (* n (fact (- n 1)))])\n(defn main [] :int (fact 5))")
      assert {:ok, 10} = run_both("(defn add (Fn :int :int :int) [0 y y] [x y (+ x y)])\n(defn main [] :int (+ (add 0 5) (add 2 3)))")
      assert {:ok, :negative} = run_both("(defn sign (Fn :int :atom) [x :when (< x 0) :negative] [0 :zero] [_ :positive])\n(defn main [] :atom (sign -4))")
    end

    test "matching several arguments is not matching a result tuple" do
      assert {:ok, 0} = run_both("(defn f (Fn :atom :int :int) [:ok n n] [:other _ 0])\n(defn main [] :int (f :other 4))")
      assert {:ok, 0} = run_both("(defn f (Fn :atom :int :int) [:ok n n] [_ _ 0])\n(defn main [] :int (f :error 4))")
    end

    test "its clauses must cover every combination of finite arguments (Maranget usefulness)" do
      assert error("(defn f (Fn :bool :bool :int) [true true 1] [false false 0])\n(defn main [] :int (f true true))") == "non-exhaustive pattern match"
      assert {:error, %{hint: "no clause matches the arguments true false"}} = Vaisto.TypeChecker.check(Parser.parse("(defn f (Fn :bool :bool :int) [true true 1] [false false 0])"))
      assert {:ok, 2} = run_both("(defn f (Fn :bool :bool :int) [true _ 1] [false false 0] [_ true 2])\n(defn main [] :int (f false true))")
      maybe = "(deftype Maybe (Just :a) (Nothing))\n(deftype Res (Ok :int) (Err :string))\n"
      assert error(maybe <> "(defn f [m (Maybe Res)] :int (match m [(Just (Ok v)) v] [(Nothing) 0]))\n(defn main [] :int 1)") == "non-exhaustive pattern match"
    end

    test "a parameter that holds a function is applied, even when a top-level function has its name" do
      assert {:ok, 20} = run_both("(defn inc [x :int] :int (+ x 1))\n(defn ap (Fn (Fn :int :int) :int) [inc (inc 10)])\n(defn dbl [x :int] :int (* x 2))\n(defn main [] :int (ap dbl))")
      assert {:ok, 8} = run_both("(defn ap (Fn (Fn :int :int) :int) [f (f 4)])\n(defn dbl [x :int] :int (* x 2))\n(defn main [] :int (ap dbl))")
    end

    test "a refinement's binder may have any name, even a node's tag" do
      assert {:ok, _, _} = Vaisto.Compilation.compile("(defn f (Fn {call :int | (> call 0)} {v :int | (> (+ v 1) call)} :int) [x y (+ x y)])\n(defn main [] :int (f 1 5))", :BinderNamedCall)
    end

    test "dependent refinements name earlier arguments by their binders" do
      sig = "(defn f (Fn {a :int | (> a 0)} {b :int | (> b a)} :int) [x y (- y x)])\n"
      assert {:ok, _, _} = Vaisto.Compilation.compile(sig <> "(defn main [] :int (f 3 5))", :DependentOk)
      assert {:error, message} = Vaisto.Compilation.compile(sig <> "(defn main [] :int (f 3 1))", :DependentBad)
      assert message =~ "`f` requires (> 1 3) for its argument 2"
    end

    test "its generated parameters capture no name of the program" do
      assert {:ok, 8} = run_both("(defn arg [] :int 7)\n(defn f (Fn :int :int) [x (+ x (arg))])\n(defn main [] :int (f 1))")
    end

    test "gives the elaborator the signature it needs, and its clauses must be exhaustive" do
      assert {:ok, _} = elab("(defn len (Fn (List :a) :int) [[] 0] [[h | t] (+ 1 (len t))])")
      assert {:error, [e]} = elab("(defn f (Fn :int :int) [0 1])")
      assert e.message == "this match is not exhaustive"
    end
  end

  describe "patterns (liquid-core.md §10.9)" do
    test "a pattern binds each name once" do
      assert error("(defn same (Fn :int :int :atom) [x x :same] [_ _ :diff])\n(defn main [] :atom (same 2 2))") ==
               "`x` is bound twice in one pattern"

      assert error("(defn main [] (List :int) (map (fn [p] (match p [(tuple x x) 1] [_ 0])) [(tuple 1 1)]))") ==
               "`x` is bound twice in one pattern"
    end

    test "a pattern's shape is that of what it matches" do
      assert error("(defn f [c :bool] :int (match c [(tuple a b) 1]))\n(defn main [] :int 1)") == "type mismatch"
      assert error("(defn f (Fn :float :int) [1 1] [_ 0])\n(defn main [] :int 1)") == "type mismatch"
      assert error("(deftype A (X) (Y))\n(deftype B (Z))\n(defn f [a A] :int (match a [(Z) 1] [_ 0]))\n(defn main [] :int 1)") == "type mismatch"
      assert error("(deftype Maybe (Just :a) (Nothing))\n(defn f [m (Maybe :string)] :int (match m [(Just 1) 1] [_ 0]))\n(defn main [] :int 1)") == "type mismatch"
      # Against a value whose type is not known, a match may mix shapes, as Erlang interop does.
      assert {:ok, 1} = run_both("(defn f [t] :int (match t [:ok 0] [{:ok v} 1] [_ 2]))\n(defn main [] :int (f {:ok 5}))")
    end

    test "nil is a literal in a pattern, not a name" do
      assert {:ok, :other} = run_both("(defn f [a :atom] :atom (match a [nil :was-nil] [_ :other]))\n(defn main [] :atom (f :foo))")
      assert error("(defn f [x :int :when nil] :int 1)\n(defn main [] :int (f 3))") == "type mismatch"
    end

    test "a let pattern binds each name once" do
      assert error("(defn main [] :int (let [(tuple a a) (tuple 1 2)] a))") == "`a` is bound twice in one pattern"
    end

    test "list and as-patterns in a lambda match what they say" do
      assert {:ok, [1, 0]} = run_both("(defn main [] (List :int) (map (fn [t] (match t [[1.5] 1] [_ 0])) [[1.5] [2.5]]))")
      assert {:ok, [3, 0]} = run_both("(defn main [] (List :int) (map (fn [t] (match t [[a b] (+ a b)] [_ 0])) [[1 2] [3]]))")
      assert {:ok, [1, 0]} = run_both("(deftype R (Ok :int) (Err :string))\n(defn main [] (List :int) (map (fn [r] (match r [(x @ (Ok v)) v] [_ 0])) [(Ok 1) (Err \"e\")]))")
    end

    test "a constrained instance takes each type parameter's dictionary from its own constructor" do
      e = "(deftype E (L a) (R b))\n(instance Show (E a b) where [(Show a) (Show b)] (show [e] (match e [(L x) (show x)] [(R y) (show y)])))\n(defn g [e (E :int :string)] :string (show e))\n"
      assert {:ok, "hi"} = run_both(e <> "(defn main [] :string (g (R \"hi\")))")
      assert {:ok, "5"} = run_both(e <> "(defn main [] :string (g (L 5)))")
    end

    test "a let-bound empty list is a list of anything, at each use" do
      assert {:ok, 2} = run_both("(defn main [] :int (let [e []] (+ (length (cons 1 e)) (length (cons \"a\" e)))))")
    end

    test "functions cannot be compared, nor values that hold them" do
      assert error("(defn g [] :int 1)\n(defn main [] :bool (== g g))") == "type mismatch"
      assert error("(defn g [x :int] :int x)\n(defn main [] :bool (== (tuple g 1) (tuple g 1)))") == "type mismatch"
    end

    test "nil in a pattern is checked like any literal" do
      assert error("(deftype Maybe (Just v) (Nothing))\n(defn f [m (Maybe :bool)] :int (match m [(Just nil) 1] [(Just true) 2] [(Nothing) 0]))\n(defn main [] :int 1)") == "type mismatch"
    end

    test "the unsigned form's tuple patterns bind their names" do
      assert {:ok, [5, 0]} = run_both("(defn f\n [(tuple 1 x) x]\n [(tuple _ _) 0])\n(defn main [] (List :int) [(f (tuple 1 5)) (f (tuple 2 5))])")
    end

    test "cons onto a list of Int takes only an Int" do
      assert error("(defn main [] :int (div (head (cons 1.5 [1 2])) 1))") == "cons type mismatch"
      assert {:ok, [2]} = run_both("(defn main [] (List :int) (cons 2 []))")
    end

    test "a local shadows a builtin or a definition inside a lambda too" do
      assert {:ok, 15} = run_both("(defn main [] :int (let [+ (fn [a b] (* a b))] (let [g (fn [y] (+ y 3))] (g 5))))")
      assert {:ok, 500} = run_both("(defn k [x :int] :int (+ x 1))\n(defn main [] :int (let [k (fn [x] (* x 100))] (let [g (fn [y] (k y))] (g 5))))")
      assert {:ok, 6} = run_both("(defn mul [a :int b :int] :int (* a b))\n(defn main [] :int (let [f (fn [+] (+ 2 3))] (f mul)))")
      assert error("(defn main [] :int (let [head 5] (head [1])))") == "type mismatch"
    end

    test "a match in a lambda must cover its scrutinee, and its patterns must fit it" do
      assert error("(defn main [] (List :int) (map (fn [n] (match (> n 0) [true 1])) [1 -1]))") == "non-exhaustive pattern match"
      maybe = "(deftype Maybe (Just a) (Nothing))\n"
      assert error(maybe <> "(defn main [] (List :int) (map (fn [m] (match m [(Just x) x])) [(Just 1)]))") == "non-exhaustive pattern match"
      assert error(maybe <> "(defn main [] (List :int) (map (fn [n] (match (+ n 1) [(Just x) x] [_ 0])) [1 2]))") == "type mismatch"
    end

    test "the built-in Eq is exact equality on both backends" do
      assert {:ok, [false, true]} = run_both("(defn main [] (List :bool) [(eq 0.0 -0.0) (neq 0.0 -0.0)])")
    end

    test "a definition whose result is an empty list is a list of anything" do
      assert {:ok, 2} = run_both("(defn empty [] :any [])\n(defn main [] :int (+ (length (cons 1 (empty))) (length (cons \"a\" (empty)))))")
    end

    test "a literal pattern has the type of what it matches" do
      assert {:ok, 1} = run_both("(defn f (Fn :float :int) [1.5 1] [_ 0])\n(defn main [] :int (f 1.5))")
      assert {:ok, 1} = run_both("(defn f (Fn :string :int) [\"a\" 1] [_ 0])\n(defn main [] :int (f \"a\"))")
      assert {:ok, [1, 0]} = run_both("(defn main [] (List :int) (map (fn [s] (match s [\"a\" 1] [_ 0])) [\"a\" \"b\"]))")
      assert error("(defn f (Fn :int :int) [\"a\" 1] [_ 0])\n(defn main [] :int (f 1))") == "type mismatch"
    end
  end

  describe "guards in match clauses (D27)" do
    test "choose a clause only when the guard holds, on both backends" do
      classify = "(defn classify [n :int] :atom (match n [x :when (< x 0) :negative] [0 :zero] [_ :positive]))\n"
      assert {:ok, :negative} = run_both(classify <> "(defn main [] :atom (classify -5))")
      assert {:ok, :positive} = run_both(classify <> "(defn main [] :atom (classify 5))")
    end

    test "a guard that crashes counts as false, wherever in the guard it crashes (liquid-core.md §6.6)" do
      for guard <- [
            "(> (div 10 x) 1)",
            "(or (> x 5) (> (div 10 x) 1))",
            "(and (== x 0) (> (div 10 x) 1))",
            "(if (== x 0) (> (div 10 x) 1) false)"
          ] do
        assert {:ok, 2} = run_both("(defn f [n :int] :int (match n [x :when #{guard} 1] [_ 2]))\n(defn main [] :int (f 0))"), guard
        assert {:ok, 2} = run_both("(defn f (Fn :int :int) [x :when #{guard} 1] [_ 2])\n(defn main [] :int (f 0))"), guard
      end

      assert %{verdict: :agree, eval: {:crash, :no_match}} =
               Harness.run("(defn f [x :int :when (and (== x 0) (> (div 10 x) 1))] :int 1)\n(defn main [] :int (f 0))")
    end

    test "an if in a guard runs only where the guard reaches it, as and and or short-circuit" do
      lambda = "(defn main [] (List :int) (map (fn [p] (match p [(tuple y b) :when (or (== y 0) (if (> (div 10 y) 1) b false)) 1] [_ 0])) [(tuple 0 true)]))"
      assert %{verdict: :agree, eval: {:ok, [1]}} = Vaisto.Elab.Shadow.run(lambda) |> Map.take([:verdict, :eval])
      assert {:ok, 1} = run_both("(defn f (Fn :int :int) [x :when (or (== x 0) (if (> (div 10 x) 1) true false)) 1] [_ 0])\n(defn main [] :int (f 0))")
    end

    test "a guard with many ifs compiles in time linear in its size" do
      sum = Enum.reduce(1..40, "0", fn i, acc -> "(+ #{acc} (if (> x #{i}) 1 0))" end)
      assert {:ok, 1} = run_both("(defn f (Fn :int :int) [x :when (> #{sum} 3) 1] [_ 0])\n(defn main [] :int (f 10))")
    end

    test "if in a guard means the same on both backends, whatever its type" do
      assert {:ok, 1} = run_both("(defn f (Fn :int :int) [x :when (if (> x 0) true false) 1] [_ 0])\n(defn main [] :int (f 3))")
      assert {:ok, 1} = run_both("(defn f (Fn :int :int) [x :when (> (if (> x 0) x (- 0 x)) 2) 1] [_ 0])\n(defn main [] :int (f -3))")
      assert {:ok, 1} = run_both("(defn f (Fn :int :int) [x :when (== (+ (if (> x 0) 1 2) (if (> x 5) 10 20)) 21) 1] [_ 0])\n(defn main [] :int (f 3))")
      assert {:ok, 0} = run_both("(defn f (Fn :int :int) [x :when (> (if (== x 0) 0 (div 10 x)) 1) 1] [_ 0])\n(defn main [] :int (f 0))")
      assert {:ok, 1} = run_both("(defn f [x :int :when (if (> x 0) true false)] :int 1)\n(defn main [] :int (f 3))")
    end

    test "a guard is a Bool, and false and nil are guards like any other" do
      assert error("(defn f [n :int] :int (match n [x :when x 1] [_ 2]))\n(defn main [] :int (f 0))") == "type mismatch"
      assert error("(defn f [n :int] :int (match n [_ :when nil 1] [_ 2]))\n(defn main [] :int (f 0))") == "type mismatch"
      assert {:ok, 2} = run_both("(defn f [n :int] :int (match n [_ :when false 1] [_ 2]))\n(defn main [] :int (f 0))")
    end

    test "a guarded clause does not count towards exhaustiveness (liquid-core.md §10.8)" do
      source = "(deftype C (R) (G))\n(defn f [c :C] :int (match c [(R) :when false 1] [(G) 2]))\n(defn main [] :int (f (G)))"
      assert error(source) =~ "non-exhaustive"
      assert {:error, [%{message: "this match is not exhaustive"}]} = elab(source)
      assert error("(defn f [b :bool] :int (match b [true :when (== 1 2) 1] [false 2]))\n(defn main [] :int (f true))") =~ "non-exhaustive"
    end

    test "a guard in a lambda is checked like any other" do
      assert {:ok, [1, 20, 30]} = run_both("(defn main [] (List :int) (map (fn [n] (match n [x :when (> x 1) (* x 10)] [y y])) [1 2 3]))")
      assert error("(defn main [] :int (head (map (fn [n] (match n [x :when (> x 0) (head x)] [_ 0])) [1 2])))") == "expected a list"
    end
  end

  describe "guards are guard-safe (liquid-core.md §6.6)" do
    test "variables, literals, primitives, field access, tuples, lists, constructors and if are allowed" do
      assert {:ok, 3} = run_both("(defn f (Fn (List :int) :int) [xs :when (and (not (empty? xs)) (== (head xs) 1)) (length xs)] [_ 0])\n(defn main [] :int (f [1 2 3]))")
      assert {:ok, 1} = run_both("(deftype C (R :int) (G))\n(defn f (Fn :int :int) [n :when (== (R n) (R 3)) 1] [_ 0])\n(defn main [] :int (f 3))")
    end

    test "a brace tuple and a single-expression do are allowed" do
      assert {:ok, 1} = run_both("(defn f (Fn :int :int) [x :when (== {x 1} {1 1}) 1] [_ 0])\n(defn main [] :int (f 1))")
      assert {:ok, 1} = run_both("(defn f (Fn :int :int) [x :when (do (> x 0)) 1] [_ 0])\n(defn main [] :int (f 1))")
    end

    test "a function value, a function returning a sum, and a local named like a primitive are not" do
      for {source, what} <- [
            {"(defn f [div (Fn :int :int :int) n :int] :int (match n [x :when (> (div x 1) 5) 1] [_ 0]))", "a call of `div`"},
            {"(def k 5)\n(defn f (Fn :int :int) [x :when (> x k) 1] [_ 0])", "the top-level value `k`"},
            {"(defn g [] :int 1)\n(defn f (Fn :int :int) [n :when (== g g) 1] [_ 0])", "the function `g` as a value"},
            {"(deftype C (R :int) (G))\n(defn mk [x :int] C (R x))\n(defn f (Fn :int :int) [n :when (== (mk n) (R 3)) 1] [_ 0])", "a call of `mk`"},
            {"(defn f [ys (List :int)] :int (let [head (fn [xs] 7)] (match ys [y :when (== (head y) 7) 1] [_ 0])))", "a call of `head`"}
          ] do
        assert error(source <> "\n(defn main [] :int 1)") == "a guard may not contain #{what}", source
      end
    end

    test "calls, lets, concatenation and record construction are not" do
      for {source, what} <- [
            {"(defn pos? [x :int] :bool (> x 0))\n(defn f (Fn :int :int) [x :when (pos? x) 1] [_ 0])", "a call of `pos?`"},
            {"(defn f (Fn :int :int) [x :when (let [y 1] (> x y)) 1] [_ 0])", "a let"},
            {"(defn f (Fn :string :int) [s :when (== (++ s \"a\") \"ba\") 1] [_ 0])", "concatenation"},
            {"(deftype Pt [x :int y :int])\n(defn f (Fn :int :int) [n :when (> n (. (Pt 1 2) :x)) 1] [_ 0])", "the construction of the record `Pt`"},
            {"(defn pos? [x :int] :bool (> x 0))\n(defn f [x :int :when (pos? x)] :int 1)", "a call of `pos?`"},
            {"(defn pos? [x :int] :bool (> x 0))\n(defn main [] (List :int) (map (fn [n] (match n [x :when (pos? x) 1] [_ 0])) [1 2]))", "a call of `pos?`"}
          ] do
        assert error(source) == "a guard may not contain #{what}", source
      end
    end
  end

  describe "the unsigned multi-clause form" do
    test "rejects a guarded clause without a body, and a clause without one" do
      assert {:error, e, _} = Parser.parse("(defn big? [x :when (> x 100)] [_ false])")
      assert e.message =~ "a guarded clause is written [pattern :when guard body]"
      assert {:error, e, _} = Parser.parse("(defn f [x 1] [y])")
      assert e.message =~ "each clause is [pattern body]"
    end
  end

  describe "a guard after the parameter vector" do
    test "is an error that says where the guard goes" do
      for source <- ["(defn f [n :int] :int :when (> n 0) 1)", "(defn f [n :int] :when (> n 0))", "(defn f [n :int] :int :when (> n 0))"] do
        assert {:error, e, _} = Parser.parse(source)
        assert e.message =~ "a guard goes inside the parameter vector", source
      end

      # A body that is the keyword :when, and :when later in a body, are no guard.
      assert {:ok, :when} = run_both("(defn f [] :atom :when)\n(defn main [] :atom (f))")
      assert {:ok, :b} = run_both("(defn f [x] (+ x 1) :when :a :b)\n(defn main [] :atom (f 1))")
    end

    test "a parameter vector with a guard may be followed by a list body" do
      assert {:ok, [2, 2]} = run_both("(defn wrap [x :int :when (> x 0)] [x x])\n(defn main [] (List :int) (wrap 2))")
    end

    test "a definition without a body is an error" do
      for source <- ["(defn f [])", "(defn f [x])"] do
        assert {:error, e, _} = Parser.parse(source)
        assert e.message =~ "defn requires a body expression", source
      end
    end
  end
end
