defmodule Vaisto.LSP.HoverTest do
  use ExUnit.Case
  alias Vaisto.LSP.Hover

  # ============================================================================
  # Go to Definition Tests
  # ============================================================================

  describe "get_definition/4" do
    test "finds function definition from call site" do
      source = """
      (defn add [a :int b :int] :int
        (+ a b))

      (add 1 2)
      """
      # Hover on 'add' in the call (line 4, col 2)
      assert {:ok, loc} = Hover.get_definition(source, 4, 2)
      assert loc.line == 1
      assert loc.col == 7  # 'add' starts at col 7 in defn
    end

    test "finds function definition from within another function" do
      source = """
      (defn helper [] :int 42)

      (defn main [] :int
        (helper))
      """
      # Hover on 'helper' in main (line 4)
      assert {:ok, loc} = Hover.get_definition(source, 4, 4)
      assert loc.line == 1
    end

    test "finds type definition from constructor call" do
      source = """
      (deftype Result (Ok v) (Error e))

      (Ok 42)
      """
      # Hover on 'Ok' (line 3)
      assert {:ok, loc} = Hover.get_definition(source, 3, 2)
      assert loc.line == 1
    end

    test "finds process definition from spawn" do
      source = """
      (process counter 0
        :increment (+ state 1))

      (spawn counter 0)
      """
      # Hover on 'counter' in spawn (line 4)
      assert {:ok, loc} = Hover.get_definition(source, 4, 8)
      assert loc.line == 1
    end

    test "finds variable definition in let binding" do
      source = "(let [x 42] (+ x 1))"
      # Hover on 'x' in the body (col 16)
      assert {:ok, loc} = Hover.get_definition(source, 1, 16)
      # x is defined at col 7
      assert loc.col == 7
    end

    test "finds parameter definition in function" do
      source = "(defn double [n :int] :int (+ n n))"
      # Hover on first 'n' in body (around col 31)
      assert {:ok, loc} = Hover.get_definition(source, 1, 31)
      # n is defined in params at col 15
      assert loc.col == 15
    end

    test "returns not_found for undefined symbol" do
      source = "(+ x 1)"
      assert :not_found = Hover.get_definition(source, 1, 4)
    end

    test "returns not_found for builtin operators" do
      source = "(+ 1 2)"
      # '+' is a builtin, no definition location
      assert :not_found = Hover.get_definition(source, 1, 2)
    end

    test "returns not_found for literals" do
      source = "(+ 42 1)"
      assert :not_found = Hover.get_definition(source, 1, 4)
    end

    test "returns not_found for whitespace" do
      source = "(+ 1 2)"
      assert :not_found = Hover.get_definition(source, 1, 3)
    end

    test "works across multiple definitions" do
      source = """
      (defn foo [] :int 1)
      (defn bar [] :int 2)
      (defn baz [] :int (+ (foo) (bar)))
      """
      # Find 'foo' in baz
      assert {:ok, loc} = Hover.get_definition(source, 3, 23)
      assert loc.line == 1

      # Find 'bar' in baz
      assert {:ok, loc} = Hover.get_definition(source, 3, 30)
      assert loc.line == 2
    end
  end

  describe "token_at/3" do
    test "finds identifier in simple expression" do
      source = "(+ x 1)"
      assert {:ok, "x", %{line: 1, col: 4, length: 1}} = Hover.token_at(source, 1, 4)
    end

    test "finds function name at call position" do
      source = "(add 1 2)"
      assert {:ok, "add", %{line: 1, col: 2, length: 3}} = Hover.token_at(source, 1, 2)
      assert {:ok, "add", %{line: 1, col: 2, length: 3}} = Hover.token_at(source, 1, 3)
      assert {:ok, "add", %{line: 1, col: 2, length: 3}} = Hover.token_at(source, 1, 4)
    end

    test "finds operator" do
      source = "(+ 1 2)"
      assert {:ok, "+", %{line: 1, col: 2, length: 1}} = Hover.token_at(source, 1, 2)
    end

    test "finds number literal" do
      source = "(+ 42 2)"
      assert {:ok, "42", %{line: 1, col: 4, length: 2}} = Hover.token_at(source, 1, 4)
      assert {:ok, "42", %{line: 1, col: 4, length: 2}} = Hover.token_at(source, 1, 5)
    end

    test "finds keyword/atom" do
      source = "(process counter 0 :increment body)"
      assert {:ok, ":increment", %{line: 1, col: 20, length: 10}} = Hover.token_at(source, 1, 20)
    end

    test "finds string literal" do
      source = ~s[(print "hello world")]
      assert {:ok, "\"hello world\"", _} = Hover.token_at(source, 1, 10)
    end

    test "handles multiline source" do
      source = "(let [x 1]\n  (+ x 2))"
      # Line 2, col 6 should be 'x'
      assert {:ok, "x", %{line: 2, col: 6, length: 1}} = Hover.token_at(source, 2, 6)
    end

    test "returns not_found for whitespace" do
      source = "(+ 1 2)"
      assert :not_found = Hover.token_at(source, 1, 1)  # on opening paren
      assert :not_found = Hover.token_at(source, 1, 3)  # space after +
    end

    test "returns not_found for parentheses" do
      source = "(+ 1 2)"
      assert :not_found = Hover.token_at(source, 1, 1)  # (
      assert :not_found = Hover.token_at(source, 1, 7)  # )
    end

    test "returns not_found for out of bounds" do
      source = "(+ 1 2)"
      assert :not_found = Hover.token_at(source, 99, 1)
      assert :not_found = Hover.token_at(source, 1, 99)
    end

    test "handles defn definition" do
      source = "(defn add [a b] (+ a b))"
      assert {:ok, "defn", _} = Hover.token_at(source, 1, 2)
      assert {:ok, "add", %{line: 1, col: 7, length: 3}} = Hover.token_at(source, 1, 7)
      assert {:ok, "a", _} = Hover.token_at(source, 1, 12)
      assert {:ok, "b", _} = Hover.token_at(source, 1, 14)
    end

    test "handles brackets" do
      source = "[a b c]"
      assert :not_found = Hover.token_at(source, 1, 1)  # [
      assert {:ok, "a", _} = Hover.token_at(source, 1, 2)
      assert :not_found = Hover.token_at(source, 1, 7)  # ]
    end
  end

  describe "build_env/1" do
    test "extracts function definition types" do
      # Typed AST for: (defn add [a :int b :int] :int (+ a b))
      # Actual typed AST structure from type checker: {:defn, name, params, body, {:fn, types, ret}}
      typed_ast = {:defn, :add, [:a, :b], {:call, :+, [{:var, :a, :int}, {:var, :b, :int}], :int}, {:fn, [:int, :int], :int}}

      env = Hover.build_env([typed_ast])

      assert %{type: {:fn, [:int, :int], :int}} = env[:add]
    end

    test "extracts function definition with location (test format)" do
      # Test format with location: {:defn, name, params, body, ret_type, loc}
      loc = %Vaisto.Parser.Loc{line: 1, col: 1}
      typed_ast = {:defn, :add, [{:a, :int}, {:b, :int}], {:call, :+, [:a, :b], :int}, :int, loc}

      env = Hover.build_env([typed_ast])

      assert %{type: {:fn, [:int, :int], :int}, loc: ^loc} = env[:add]
    end

    test "extracts value definition types" do
      # Actual typed AST: {:defval, name, typed_expr, type}
      typed_ast = {:defval, :pi, {:lit, :float, 3.14}, :float}

      env = Hover.build_env([typed_ast])

      assert %{type: :float} = env[:pi]
    end

    test "extracts process definitions" do
      loc = %Vaisto.Parser.Loc{line: 1, col: 1}
      typed_ast = {:process, :counter, {:lit, :int, 0}, [{:increment, {:call, :+, [:state, 1], :int}}], loc}

      env = Hover.build_env([typed_ast])

      assert %{type: {:process, :any, [:increment]}} = env[:counter]
    end

    test "extracts type definitions (sum type)" do
      loc = %Vaisto.Parser.Loc{line: 1, col: 1}
      typed_ast = {:deftype, :Result, {:sum, [{:Ok, [:v]}, {:Error, [:e]}]}, loc}

      env = Hover.build_env([typed_ast])

      # Type name is in env
      assert %{type: {:sum, :Result, [{:Ok, [:v]}, {:Error, [:e]}]}} = env[:Result]
      # Constructors are also in env
      assert env[:Ok] != nil
      assert env[:Error] != nil
    end

    test "extracts type definitions (product type)" do
      loc = %Vaisto.Parser.Loc{line: 1, col: 1}
      typed_ast = {:deftype, :Point, {:product, [{:x, :int}, {:y, :int}]}, loc}

      env = Hover.build_env([typed_ast])

      assert %{type: {:record, :Point, [{:x, :int}, {:y, :int}]}} = env[:Point]
    end

    test "extracts extern declarations" do
      loc = %Vaisto.Parser.Loc{line: 1, col: 1}
      typed_ast = {:extern, :erlang, :hd, [:list], :any, loc}

      env = Hover.build_env([typed_ast])

      assert %{type: {:fn, [:list], :any}} = env[:hd]
    end

    test "handles module with multiple definitions" do
      loc = %Vaisto.Parser.Loc{line: 1, col: 1}
      typed_asts = [
        {:defn, :add, [{:a, :int}, {:b, :int}], {:call, :+, [:a, :b], :int}, :int, loc},
        {:defn, :sub, [{:a, :int}, {:b, :int}], {:call, :-, [:a, :b], :int}, :int, loc},
        {:defval, :zero, {:lit, :int, 0}, loc}
      ]

      env = Hover.build_env(typed_asts)

      assert env[:add] != nil
      assert env[:sub] != nil
      assert env[:zero] != nil
    end
  end

  describe "get_hover/4" do
    test "returns type for variable in let binding" do
      source = "(let [x 42] x)"
      # Hover on the 'x' at the end (position 13)
      assert {:ok, hover} = Hover.get_hover(source, 1, 13)
      assert hover.contents =~ "Int"
    end

    test "returns type for function definition" do
      source = "(defn add [a :int b :int] :int (+ a b))"
      # Hover on 'add'
      assert {:ok, hover} = Hover.get_hover(source, 1, 7)
      assert hover.contents =~ "Int"
      assert hover.contents =~ "->"
    end

    test "returns type for function call" do
      source = "(defn add [a :int b :int] :int (+ a b))\n(add 1 2)"
      # Hover on 'add' in the call
      assert {:ok, hover} = Hover.get_hover(source, 2, 2)
      assert hover.contents =~ "(Int, Int) -> Int"
    end

    test "returns type for builtin operator" do
      source = "(+ 1 2)"
      assert {:ok, hover} = Hover.get_hover(source, 1, 2)
      assert hover.contents =~ "Int"
      assert hover.contents =~ "->"
    end

    test "returns type for integer literal" do
      source = "(+ 42 1)"
      assert {:ok, hover} = Hover.get_hover(source, 1, 4)
      assert hover.contents =~ "Int"
    end

    test "returns type for string literal" do
      source = ~s["hello"]
      assert {:ok, hover} = Hover.get_hover(source, 1, 3)
      assert hover.contents =~ "String"
    end

    test "returns type for atom literal" do
      source = ":ok"
      assert {:ok, hover} = Hover.get_hover(source, 1, 1)
      assert hover.contents =~ ":ok"
    end

    test "returns not_found for undefined variable" do
      source = "(+ x 1)"  # x is not defined
      assert :not_found = Hover.get_hover(source, 1, 4)
    end

    test "returns not_found for whitespace" do
      source = "(+ 1 2)"
      assert :not_found = Hover.get_hover(source, 1, 3)
    end

    test "handles process definition" do
      source = """
      (process counter 0
        :increment (+ state 1)
        :get state)
      """
      # Hover on 'counter'
      assert {:ok, hover} = Hover.get_hover(source, 1, 10)
      assert hover.contents =~ "Process" or hover.contents =~ "counter"
    end

    test "handles type constructor" do
      source = """
      (deftype Result (Ok v) (Error e))
      (Ok 42)
      """
      # Hover on 'Ok' in the call
      assert {:ok, hover} = Hover.get_hover(source, 2, 2)
      assert hover.contents =~ "Result" or hover.contents =~ "Ok"
    end

    test "handles parameter in function body" do
      source = "(defn double [x :int] :int (+ x x))"
      # Hover on first 'x' in body (position ~29)
      assert {:ok, hover} = Hover.get_hover(source, 1, 29)
      assert hover.contents =~ "Int"
    end
  end

  describe "format_hover/3" do
    test "formats simple type" do
      result = Hover.format_hover("x", :int)
      assert result =~ "Int"
      assert result =~ "```vaisto"
    end

    test "formats function type" do
      result = Hover.format_hover("add", {:fn, [:int, :int], :int})
      assert result =~ "(Int, Int) -> Int"
    end

    test "formats list type" do
      result = Hover.format_hover("xs", {:list, :int})
      assert result =~ "List(Int)"
    end

    test "formats process type" do
      result = Hover.format_hover("counter", {:process, :int, [:increment, :get]})
      assert result =~ "Process"
    end

    test "formats with definition location" do
      loc = %Vaisto.Parser.Loc{line: 5, col: 1, file: "test.va"}
      result = Hover.format_hover("add", {:fn, [:int], :int}, loc: loc)
      assert result =~ "test.va:5"
    end

    test "formats with definition location (no file)" do
      loc = %Vaisto.Parser.Loc{line: 5, col: 1, file: nil}
      result = Hover.format_hover("add", {:fn, [:int], :int}, loc: loc)
      assert result =~ "line 5"
    end

    test "formats with kind annotation" do
      result = Hover.format_hover("add", {:fn, [:int], :int}, kind: :function)
      assert result =~ "function"
    end
  end
end
