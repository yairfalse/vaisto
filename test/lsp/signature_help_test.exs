defmodule Vaisto.LSP.SignatureHelpTest do
  use ExUnit.Case
  alias Vaisto.LSP.SignatureHelp

  describe "get_signature_help/3" do
    test "returns nil outside of a function call" do
      text = "hello"
      assert nil == SignatureHelp.get_signature_help(text, 1, 6)
    end

    test "returns signature for builtin + operator" do
      text = "(+ 1 "
      result = SignatureHelp.get_signature_help(text, 1, 6)

      assert result != nil
      assert result["activeSignature"] == 0
      assert [sig | _] = result["signatures"]
      assert sig["label"] =~ "+"
    end

    test "returns signature for map function" do
      text = "(map f"
      result = SignatureHelp.get_signature_help(text, 1, 7)

      assert result != nil
      assert [sig | _] = result["signatures"]
      assert sig["label"] =~ "map"
      assert length(sig["parameters"]) == 2
    end

    test "tracks active parameter index" do
      # First argument position
      text = "(+ "
      result1 = SignatureHelp.get_signature_help(text, 1, 4)
      assert result1["activeParameter"] == 0

      # Second argument position
      text = "(+ 1 "
      result2 = SignatureHelp.get_signature_help(text, 1, 6)
      assert result2["activeParameter"] == 1
    end

    test "returns signature for if special form" do
      text = "(if true"
      result = SignatureHelp.get_signature_help(text, 1, 9)

      assert result != nil
      assert [sig | _] = result["signatures"]
      assert sig["label"] =~ "if"
      assert length(sig["parameters"]) == 3  # cond, then, else
    end

    test "returns signature for let special form" do
      text = "(let [x"
      result = SignatureHelp.get_signature_help(text, 1, 8)

      assert result != nil
      assert [sig | _] = result["signatures"]
      assert sig["label"] =~ "let"
    end

    test "returns signature for fold with multiple params" do
      text = "(fold f init"
      result = SignatureHelp.get_signature_help(text, 1, 13)

      assert result != nil
      assert result["activeParameter"] == 2  # Third parameter (xs)
      assert [sig | _] = result["signatures"]
      assert length(sig["parameters"]) == 3
    end

    test "handles nested expressions" do
      # Inside inner call (head)
      text = "(+ 1 (head xs"
      result = SignatureHelp.get_signature_help(text, 1, 14)

      assert result != nil
      assert [sig | _] = result["signatures"]
      assert sig["label"] =~ "head"
    end

    test "returns nil for closed expression" do
      text = "(+ 1 2)"
      result = SignatureHelp.get_signature_help(text, 1, 8)

      assert nil == result
    end

    test "returns nil for user-defined function with incomplete source" do
      # Incomplete source can't be parsed, so user function lookup fails
      # This is expected behavior - only complete source can provide user signatures
      text = "(defn add [x :int y :int] :int (+ x y))\n(add "
      result = SignatureHelp.get_signature_help(text, 2, 6)

      # Returns nil because parse fails on incomplete code
      assert nil == result
    end

    test "returns builtin signature even in incomplete user function" do
      # Test that builtins still work inside function bodies
      text = "(defn add [x :int y :int] :int (+ "
      result = SignatureHelp.get_signature_help(text, 1, 36)

      # Should return the + signature since it's a builtin
      assert result != nil
      assert [sig | _] = result["signatures"]
      assert sig["label"] =~ "+"
    end

    test "signature has markdown documentation" do
      text = "(map "
      result = SignatureHelp.get_signature_help(text, 1, 6)

      assert [sig | _] = result["signatures"]
      assert sig["documentation"]["kind"] == "markdown"
      assert is_binary(sig["documentation"]["value"])
    end

    test "parameters have position-based labels" do
      text = "(+ "
      result = SignatureHelp.get_signature_help(text, 1, 4)

      assert [sig | _] = result["signatures"]
      assert [param | _] = sig["parameters"]
      # Label should be [start, end] positions for highlighting
      assert is_list(param["label"])
      assert length(param["label"]) == 2
    end
  end

  describe "find_enclosing_call/1" do
    test "finds function name after opening paren" do
      assert {:ok, "+", 0} = SignatureHelp.find_enclosing_call("(+ ")
      assert {:ok, "map", 0} = SignatureHelp.find_enclosing_call("(map ")
    end

    test "counts arguments correctly" do
      assert {:ok, "+", 1} = SignatureHelp.find_enclosing_call("(+ 1 ")
      assert {:ok, "+", 2} = SignatureHelp.find_enclosing_call("(+ 1 2 ")
    end

    test "handles nested parens" do
      assert {:ok, "head", 0} = SignatureHelp.find_enclosing_call("(+ 1 (head ")
    end

    test "returns not_found outside function call" do
      assert :not_found = SignatureHelp.find_enclosing_call("hello")
      assert :not_found = SignatureHelp.find_enclosing_call("(+ 1 2)")
    end
  end
end
