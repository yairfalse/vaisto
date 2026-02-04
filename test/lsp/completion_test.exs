defmodule Vaisto.LSP.CompletionTest do
  use ExUnit.Case
  alias Vaisto.LSP.Completion

  describe "get_completions/4" do
    test "returns keyword completions after opening paren" do
      text = "("
      completions = Completion.get_completions(text, 1, 2, "test.va")

      labels = Enum.map(completions, & &1["label"])
      assert "defn" in labels
      assert "let" in labels
      assert "if" in labels
      assert "match" in labels
    end

    test "returns builtin function completions after opening paren" do
      text = "("
      completions = Completion.get_completions(text, 1, 2, "test.va")

      labels = Enum.map(completions, & &1["label"])
      assert "+" in labels
      assert "head" in labels
      assert "map" in labels
      assert "filter" in labels
    end

    test "returns type completions after colon" do
      text = "(defn foo [x :"
      completions = Completion.get_completions(text, 1, 15, "test.va")

      labels = Enum.map(completions, & &1["label"])
      assert ":int" in labels
      assert ":float" in labels
      assert ":string" in labels
      assert ":bool" in labels
    end

    test "filters completions by partial prefix" do
      text = "(def"
      completions = Completion.get_completions(text, 1, 5, "test.va")

      labels = Enum.map(completions, & &1["label"])
      assert "defn" in labels
      assert "deftype" in labels
      assert "defval" in labels
      # Should not include unrelated items
      refute "if" in labels
    end

    test "includes user-defined functions" do
      # Use complete, parseable code
      text = "(defn my-helper [x :int] :int (+ x 1))"
      completions = Completion.get_completions(text, 1, 39, "test.va")

      labels = Enum.map(completions, & &1["label"])
      assert "my-helper" in labels
    end

    test "includes user-defined types" do
      # Use complete, parseable code
      text = "(deftype Point [x :int y :int])"
      completions = Completion.get_completions(text, 1, 32, "test.va")

      labels = Enum.map(completions, & &1["label"])
      assert "Point" in labels
    end

    test "includes sum type constructors" do
      # Use complete, parseable code
      text = "(deftype Result (Ok v) (Err e))"
      completions = Completion.get_completions(text, 1, 32, "test.va")

      labels = Enum.map(completions, & &1["label"])
      assert "Result" in labels
      assert "Ok" in labels
      assert "Err" in labels
    end

    test "completion items have correct structure" do
      text = "("
      completions = Completion.get_completions(text, 1, 2, "test.va")

      defn_completion = Enum.find(completions, & &1["label"] == "defn")
      assert defn_completion["kind"] == 14  # keyword
      assert defn_completion["detail"] != nil
      assert defn_completion["insertText"] == "defn"
    end

    test "operators have operator kind" do
      text = "("
      completions = Completion.get_completions(text, 1, 2, "test.va")

      plus_completion = Enum.find(completions, & &1["label"] == "+")
      assert plus_completion["kind"] == 24  # operator
    end

    test "functions have function kind" do
      text = "("
      completions = Completion.get_completions(text, 1, 2, "test.va")

      map_completion = Enum.find(completions, & &1["label"] == "map")
      assert map_completion["kind"] == 3  # function
    end

    test "types have type kind" do
      text = "(defn foo [x :"
      completions = Completion.get_completions(text, 1, 15, "test.va")

      int_completion = Enum.find(completions, & &1["label"] == ":int")
      assert int_completion["kind"] == 22  # type
    end

    test "handles incomplete input gracefully" do
      text = "(defn incomplete [x"
      # Should not crash
      completions = Completion.get_completions(text, 1, 20, "test.va")
      assert is_list(completions)
      # May return empty list or filtered results for incomplete code
    end

    test "includes extern declarations" do
      # Use complete, parseable code
      text = "(extern erlang:hd [(List :any)] :any)"
      completions = Completion.get_completions(text, 1, 38, "test.va")

      labels = Enum.map(completions, & &1["label"])
      assert "hd" in labels
    end

    test "includes process definitions" do
      # Use complete, parseable code
      text = "(process counter 0 :inc (+ state 1))"
      completions = Completion.get_completions(text, 1, 37, "test.va")

      labels = Enum.map(completions, & &1["label"])
      assert "counter" in labels
    end
  end

  describe "context detection" do
    test "after paren suggests keywords and functions" do
      # Test with just opening paren
      text = "("
      completions = Completion.get_completions(text, 1, 2, "test.va")

      # Should include fold, filter, etc.
      labels = Enum.map(completions, & &1["label"])
      assert "fold" in labels
      assert "filter" in labels
    end

    test "filtering works with partial word after paren" do
      text = "(fo"
      completions = Completion.get_completions(text, 1, 4, "test.va")

      labels = Enum.map(completions, & &1["label"])
      # Should include fold, starts with "fo"
      assert "fold" in labels
    end

    test "general context includes all completion types" do
      text = "de"
      completions = Completion.get_completions(text, 1, 3, "test.va")

      labels = Enum.map(completions, & &1["label"])
      assert "defn" in labels
    end
  end
end
