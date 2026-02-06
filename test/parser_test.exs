defmodule Vaisto.ParserTest do
  use ExUnit.Case
  alias Vaisto.Parser

  describe "character literals" do
    test "parses named chars" do
      assert 10 = Parser.parse(~s|\\newline|)
      assert 32 = Parser.parse(~s|\\space|)
    end

    test "parses simple chars" do
      assert 97 = Parser.parse(~s|\\a|)
      assert 40 = Parser.parse(~s|\\(|)
    end
  end

  describe "basic expressions" do
    test "parses integers" do
      assert Parser.parse("42") == 42
    end

    test "parses simple call" do
      # Calls now include location as last element
      assert {:call, :+, [1, 2], %Vaisto.Parser.Loc{line: 1, col: 1}} = Parser.parse("(+ 1 2)")
    end

    test "parses nested calls" do
      result = Parser.parse("(+ 1 (* 2 3))")
      assert {:call, :+, [1, inner], %Vaisto.Parser.Loc{line: 1, col: 1}} = result
      assert {:call, :*, [2, 3], %Vaisto.Parser.Loc{line: 1, col: 6}} = inner
    end

    test "parses atoms with colon" do
      # Atoms are now wrapped in {:atom, value} to distinguish from variables
      assert Parser.parse(":ok") == {:atom, :ok}
    end
  end

  describe "special forms" do
    test "parses cond expression" do
      code = """
      (cond
        [(> x 0) :positive]
        [:else :zero])
      """
      # cond transforms to nested if
      # (if (> x 0) :positive :zero)
      result = Parser.parse(code)
      assert {:if, {:call, :>, [:x, 0], _}, {:atom, :positive}, {:atom, :zero}, _} = result
    end

    test "parses multi-clause cond" do
      code = """
      (cond
        [(> x 0) :positive]
        [(< x 0) :negative]
        [:else :zero])
      """
      # (if (> x 0) :positive (if (< x 0) :negative :zero))
      result = Parser.parse(code)
      assert {:if, _, {:atom, :positive}, {:if, _, {:atom, :negative}, {:atom, :zero}, _}, _} = result
    end

    test "cond requires else" do
      code = "(cond [(> x 0) :pos])"
      assert {:error, "cond requires an :else clause as the last argument", _} = Parser.parse(code)
    end

    test "parses process definition" do
      code = "(process counter 0 :increment (+ state 1))"
      result = Parser.parse(code)

      # Process now includes location
      assert {:process, :counter, 0, handlers, %Vaisto.Parser.Loc{}} = result
      # Atom handler names are now wrapped, handler body has location
      assert [{{:atom, :increment}, {:call, :+, [:state, 1], %Vaisto.Parser.Loc{}}}] = handlers
    end

    test "parses supervision tree" do
      code = "(supervise :one_for_one (counter 0))"
      result = Parser.parse(code)

      # Supervise now includes location
      assert {:supervise, {:atom, :one_for_one}, children, %Vaisto.Parser.Loc{}} = result
      assert [{:call, :counter, [0], %Vaisto.Parser.Loc{}}] = children
    end
  end

  describe "string escape sequences" do
    test "parses simple string" do
      assert {:string, "hello"} = Parser.parse(~s|"hello"|)
    end

    test "parses newline escape" do
      assert {:string, "a\nb"} = Parser.parse(~s|"a\\nb"|)
    end

    test "parses tab escape" do
      assert {:string, "a\tb"} = Parser.parse(~s|"a\\tb"|)
    end

    test "parses escaped quote" do
      assert {:string, ~s|say "hi"|} = Parser.parse(~s|"say \\"hi\\""|)
    end

    test "parses escaped backslash" do
      # \\n in source should become literal backslash + n, not newline
      assert {:string, "a\\nb"} = Parser.parse(~s|"a\\\\nb"|)
    end

    test "parses escaped backslash followed by escape" do
      # \\\n in source should become backslash + newline
      assert {:string, "a\\\nb"} = Parser.parse(~s|"a\\\\\\nb"|)
    end

    test "parses multiple escapes" do
      assert {:string, "line1\nline2\tindented"} = Parser.parse(~s|"line1\\nline2\\tindented"|)
    end
  end

  describe "edge cases" do
    test "returns nil for empty source" do
      assert Parser.parse("") == nil
    end

    test "skips leading whitespace and adjusts column location" do
      # Whitespace is skipped, so location starts at column 3
      assert {:call, :+, [1, 2], %Vaisto.Parser.Loc{line: 1, col: 3}} = Parser.parse("  (+ 1 2)  ")
    end

    test "parses multi-line expressions with correct start location" do
      code = """
      (+ 1
         2)
      """
      assert {:call, :+, [1, 2], %Vaisto.Parser.Loc{line: 1, col: 1}} = Parser.parse(code)
    end
  end

  describe "error messages with locations" do
    test "unclosed parenthesis reports location" do
      code = "(+ 1 2"
      error = assert_raise RuntimeError, fn -> Parser.parse(code) end
      assert error.message =~ "Unclosed parenthesis at line 1, column 1"
    end

    test "unclosed parenthesis on line 2 reports correct line" do
      code = """
      (+ 1 2)
      (defn foo [x]
      """
      error = assert_raise RuntimeError, fn -> Parser.parse(code) end
      assert error.message =~ "line 2"
    end

    test "unclosed bracket reports location" do
      code = "(let [x 1 body)"
      error = assert_raise RuntimeError, fn -> Parser.parse(code) end
      assert error.message =~ "Unclosed bracket at line 1, column 6"
    end

    test "unterminated string reports location" do
      code = ~s|(print "hello)|
      error = assert_raise RuntimeError, fn -> Parser.parse(code) end
      assert error.message =~ "Unterminated string"
      assert error.message =~ "line 1"
    end

    test "nested unclosed paren reports outermost unclosed location" do
      # In "(if true (+ 1 2)", the inner paren IS closed by the final )
      # It's the outer paren at column 1 that's unclosed
      code = "(if true (+ 1 2)"
      error = assert_raise RuntimeError, fn -> Parser.parse(code) end
      assert error.message =~ "line 1, column 1"
    end

    test "truly unclosed inner paren reports correct location" do
      # "(if true (+ 1 2" - both parens unclosed, inner one detected first
      code = "(if true (+ 1 2"
      error = assert_raise RuntimeError, fn -> Parser.parse(code) end
      # Inner paren at column 10 is the first to hit end of input
      assert error.message =~ "line 1, column 10"
    end
  end
end
