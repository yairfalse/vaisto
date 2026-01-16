defmodule Vaisto.ParserTest do
  use ExUnit.Case
  alias Vaisto.Parser

  describe "basic expressions" do
    test "parses integers" do
      assert Parser.parse("42") == 42
    end

    test "parses simple call" do
      assert Parser.parse("(+ 1 2)") == {:call, :+, [1, 2]}
    end

    test "parses nested calls" do
      assert Parser.parse("(+ 1 (* 2 3))") == {:call, :+, [1, {:call, :*, [2, 3]}]}
    end

    test "parses atoms with colon" do
      assert Parser.parse(":ok") == :ok
    end
  end

  describe "special forms" do
    test "parses process definition" do
      code = "(process counter 0 :increment (+ state 1))"
      result = Parser.parse(code)
      
      assert {:process, :counter, 0, handlers} = result
      assert [{:increment, {:call, :+, [:state, 1]}}] = handlers
    end

    test "parses supervision tree" do
      code = "(supervise :one_for_one (counter 0))"
      result = Parser.parse(code)
      
      assert {:supervise, :one_for_one, children} = result
      assert [{:call, :counter, [0]}] = children
    end
  end

  describe "edge cases" do
    test "handles empty input" do
      assert Parser.parse("") == nil
    end

    test "handles whitespace" do
      assert Parser.parse("  (+ 1 2)  ") == {:call, :+, [1, 2]}
    end

    test "handles newlines" do
      code = """
      (+ 1
         2)
      """
      assert Parser.parse(code) == {:call, :+, [1, 2]}
    end
  end
end
