defmodule Vaisto.LSP.ASTAnalyzerTest do
  use ExUnit.Case
  alias Vaisto.LSP.ASTAnalyzer
  alias Vaisto.Parser
  alias Vaisto.Parser.Loc

  describe "extract_definitions/1" do
    test "extracts function definitions" do
      ast = Parser.parse("(defn add [x y] (+ x y))")
      defs = ASTAnalyzer.extract_definitions(ast)

      assert [%{name: :add, kind: :function, arity: 2}] = defs
    end

    test "extracts function with type annotation" do
      ast = Parser.parse("(defn add [x :int y :int] :int (+ x y))")
      defs = ASTAnalyzer.extract_definitions(ast)

      assert [%{name: :add, kind: :function, arity: 2}] = defs
    end

    test "extracts sum type and constructors" do
      ast = Parser.parse("(deftype Result (Ok v) (Err e))")
      defs = ASTAnalyzer.extract_definitions(ast)

      names = Enum.map(defs, & &1.name)
      kinds = Enum.map(defs, & &1.kind)

      assert :Result in names
      assert :Ok in names
      assert :Err in names
      assert :type in kinds
      assert :constructor in kinds
    end

    test "extracts value definitions" do
      ast = Parser.parse("(def pi 3.14)")
      defs = ASTAnalyzer.extract_definitions(ast)

      assert [%{name: :pi, kind: :variable}] = defs
    end

    test "extracts process definitions" do
      ast = Parser.parse("(process counter 0 :inc (+ state 1))")
      defs = ASTAnalyzer.extract_definitions(ast)

      assert [%{name: :counter, kind: :process}] = defs
    end

    test "extracts extern declarations" do
      ast = Parser.parse("(extern erlang:hd [(List :any)] :any)")
      defs = ASTAnalyzer.extract_definitions(ast)

      assert [%{name: :hd, kind: :extern, arity: 1}] = defs
    end

    test "extracts multiple definitions" do
      code = """
      (defn add [a b] (+ a b))
      (defn sub [a b] (- a b))
      (def zero 0)
      """
      ast = Parser.parse(code)
      defs = ASTAnalyzer.extract_definitions(ast)

      names = Enum.map(defs, & &1.name)
      assert :add in names
      assert :sub in names
      assert :zero in names
    end
  end

  describe "find_definition_at/2" do
    test "finds function definition" do
      code = """
      (defn helper [] :int 42)
      (defn main [] :int (helper))
      """
      ast = Parser.parse(code)

      assert {:ok, loc} = ASTAnalyzer.find_definition_at(ast, :helper)
      assert loc.line == 1
    end

    test "finds type definition" do
      code = "(deftype Point [x :int y :int])"
      ast = Parser.parse(code)

      assert {:ok, loc} = ASTAnalyzer.find_definition_at(ast, :Point)
      assert loc.line == 1
    end

    test "finds constructor definition" do
      code = "(deftype Result (Ok v) (Err e))"
      ast = Parser.parse(code)

      assert {:ok, _loc} = ASTAnalyzer.find_definition_at(ast, :Ok)
      assert {:ok, _loc} = ASTAnalyzer.find_definition_at(ast, :Err)
    end

    test "finds process definition" do
      code = "(process counter 0 :inc (+ state 1))"
      ast = Parser.parse(code)

      assert {:ok, loc} = ASTAnalyzer.find_definition_at(ast, :counter)
      assert loc.line == 1
    end

    test "returns not_found for undefined symbol" do
      ast = Parser.parse("(defn foo [] 42)")

      assert :not_found = ASTAnalyzer.find_definition_at(ast, :undefined)
    end
  end

  describe "walk/3" do
    test "walks all nodes" do
      ast = Parser.parse("(+ 1 2)")

      count = ASTAnalyzer.walk(ast, 0, fn _node, acc -> {:cont, acc + 1} end)

      # Should visit multiple nodes
      assert count > 0
    end

    test "halts early when requested" do
      ast = Parser.parse("(+ 1 2)")

      # Numbers in AST are bare integers, not tuples
      # Walk finds an integer and halts
      result = ASTAnalyzer.walk(ast, nil, fn
        n, _acc when is_integer(n) -> {:halt, {:found, n}}
        _node, acc -> {:cont, acc}
      end)

      assert {:found, n} = result
      assert n in [1, 2]  # Either 1 or 2 is found first (depends on traversal order)
    end
  end

  describe "find_var_type/2" do
    test "finds variable type in typed AST" do
      # Simulate a typed AST with variable references
      typed_ast = {:let, [{:x, {:lit, :int, 42}, :int}], {:var, :x, :int}, :int}

      assert {:ok, :int} = ASTAnalyzer.find_var_type(typed_ast, :x)
    end

    test "returns not_found for undefined variable" do
      typed_ast = {:let, [{:x, {:lit, :int, 42}, :int}], {:var, :x, :int}, :int}

      assert :not_found = ASTAnalyzer.find_var_type(typed_ast, :y)
    end
  end
end
