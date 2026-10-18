defmodule Vaisto.NativeCheckerTest do
  use ExUnit.Case, async: true
  import Vaisto.TestHelpers, only: [check_type: 1]
  alias Vaisto.{Compilation, Elab, Parser}

  # Written source contracts, not HM's private typed-AST representation.
  for {source, expected} <- [
        {"42", :Int},
        {"3.14", :Float},
        {"true", [:Bool]},
        {~s|"hello"|, :String},
        {":hello", :Atom},
        {"(tuple)", :Unit},
        {"(+ 1 2)", :Int},
        {"(+ 1.0 2.0)", :Float},
        {"(/ 4.0 2.0)", :Float},
        {"(div 7 2)", :Int},
        {"(< 1 2)", [:Bool]},
        {"(== 1 2)", [:Bool]},
        {"(!= 1 2)", [:Bool]},
        {"(fn [x] (not x))", [:->, [[:Bool]], [:eff, :closed], [:Bool]]},
        {~s|(fn [x] (++ "hi " x))|, [:->, [:String], [:eff, :closed], :String]},
        {"(fn [x] (+ x 1))", [:->, [:Int], [:eff, :closed], :Int]},
        {"(the (Fn :int :int :int) (fn [x y] (+ x y)))",
         [:->, [:Int, :Int], [:eff, :closed], :Int]},
        {"(defn id [x :a] :a x) (id 42)", :Int},
        {"(defn id [x :a] :a x) (tuple (id 42) (id true))", [:Tuple, :Int, [:Bool]]},
        {"(defn id [x :a] :a x) (map id [1 2])", [:List, :Int]},
        {"(defn pair [x :a y :b] (Tuple :a :b) (tuple x y)) (pair 1 true)",
         [:Tuple, :Int, [:Bool]]},
        {"(defn twice [x :a] :a where [(Num a)] (+ x x)) (twice 21)", :Int},
        {"(defn bigger [x :a y :a] :bool where [(Ord a)] (> x y)) (bigger 2.0 1.0)", [:Bool]},
        {"(if true :yes :no)", :Atom},
        {"(list 1 2)", [:List, :Int]},
        {"(the (List :int) [])", [:List, :Int]},
        {"(tuple 1 true)", [:Tuple, :Int, [:Bool]]},
        {"(deftype Point [x :int y :int]) (Point 1 2)", [:Point]},
        {"(deftype Point [x :int y :int]) (. (Point 1 2) :x)", :Int},
        {"(deftype Point [x :int y :int]) (let [(Point x y) (Point 1 2)] (+ x y))", :Int},
        {"(deftype Opt (Some :int) (None)) (match (Some 1) [(Some x) x] [(None) 0])", :Int},
        {"(let [{x y} {1 true}] y)", [:Bool]},
        {"(match [1] [[] 0] [[h | t] h])", :Int},
        {"(defn len (Fn (List :a) :int) [[] 0] [[h | t] (+ 1 (len t))]) (len [true])", :Int},
        {"(defn wrap (Fn :a (List :a)) [x (list x)]) (tuple (wrap 1) (wrap true))",
         [:Tuple, [:List, :Int], [:List, [:Bool]]]},
        {"(defn abs (Fn :int :int) [n :when (>= n 0) n] [n (- 0 n)]) (abs -1)", :Int},
        {"(extern erlang:abs [:int] :int) (erlang:abs -1)", :Int},
        {"(try 1 [catch [:error e 0]])", :Int},
        {~s|(defn f [x :string] :string (do (let [x 1] x) x)) (f "outer")|, :String},
        {"(let [x 1] (do (let [x true] x) x))", :Int}
      ] do
    test "native type: #{source}" do
      assert {:ok, unquote(Macro.escape(expected))} = check_type(unquote(source))
    end
  end

  for source <- [
        "(+ 1 :atom)",
        "(+ 1)",
        "(no-such-function 1)",
        "missing",
        "(+ 1 2.0)",
        "(== 1 1.0)",
        "(if true 1 1.0)",
        "(if 1 2 3)",
        ~s|(list 1 "hello")|,
        "(deftype Point [x :int]) (Point true)",
        "(defn f [x] x)",
        "(defn f [x x])",
        "(defn f [x :any] :int x)",
        "(defn f [x :num] :num x)",
        "(defn f [x :a] :a (+ x 1))",
        ~s|(defn f [] :int "no")|,
        "(defn f [x :int] :int x) (f true)",
        "(defn f [x :int] :int x) (f 1 2)",
        "(fn [x y] (+ x y))",
        "(fn [x] x)",
        "(let [id (fn [x] x)] (tuple (id 1) (id true)))",
        "(fn [x] (x x))",
        "(defn f [x :dyn] :int x)",
        "(match 2 [1 1])",
        "(let [[h | t] [1]] h)",
        "(match 1 [true 0] [_ 1])",
        "(defn id [x :int] :int x) (defn f [x :int :when (> (id x) 0)] :int x)",
        "(defn f [x :int :when (+ x 1)] :int x)",
        "(defn f [x :a] :a where [(Num a)] (+ x x)) (f true)",
        "(Unknown/call 1)",
        "(!! 42 :ping)",
        "(extern foreign:value [] (Fn :int :int))",
        ~s|(defn f [x :string] :int (do (let [x 1] x) (+ x 1)))|,
        "(defn f [] :int (do (try 1 [catch [:error e 0]]) e))",
        "(defn f [] :int (do (receive [v 0]) v))"
      ] do
    test "native rejection: #{source}" do
      assert {:error, [_ | _]} =
               Compilation.check_liquid(unquote(source), :Rejected, format_errors: false)
    end
  end

  test "errors retain the innermost source location and filename" do
    source = "(+ 1\n  (+ 2 :bad))"

    assert {:error, [error]} =
             Compilation.check_liquid(source, :Located, file: "nested.va", format_errors: false)

    assert error.message == "type mismatch"
    assert error.file == "nested.va"
    assert error.primary_span.line == 2
    assert error.primary_span.col == 3
  end

  test "formatted errors include source and filename" do
    source = "(+ 1 :bad)"
    assert {:error, rendered} = Compilation.check_liquid(source, :Located, file: "sample.va")
    assert rendered =~ "sample.va"
    assert rendered =~ source
    assert rendered =~ "type mismatch"
  end

  test "independent definitions and clauses report all errors with real locations" do
    source = "(defn a [] :int true)\n(defn b (Fn :int :int) [x false] [y true])"
    assert {:error, errors} = source |> Parser.parse() |> Elab.module()
    assert length(errors) == 3
    assert Enum.map(errors, & &1.primary_span.line) == [1, 2, 2]
  end
end
