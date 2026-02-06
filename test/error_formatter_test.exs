defmodule Vaisto.ErrorFormatterTest do
  use ExUnit.Case
  alias Vaisto.ErrorFormatter

  describe "format/2" do
    test "formats basic error with source context" do
      error = %{
        message: "type mismatch: expected Int, found Atom",
        file: "test.va",
        line: 1,
        col: 6,
        span_length: 5,
        hint: nil
      }
      source = "(+ 1 :atom)"

      result = ErrorFormatter.format(error, source)

      assert result =~ "error"
      assert result =~ "type mismatch"
      assert result =~ "test.va:1:6"
      assert result =~ "(+ 1 :atom)"
      assert result =~ "^^^^^"
    end

    test "formats error without file" do
      error = %{
        message: "undefined variable `x`",
        file: nil,
        line: 2,
        col: 1,
        span_length: 1,
        hint: nil
      }
      source = "(+ 1 2)\nx"

      result = ErrorFormatter.format(error, source)

      assert result =~ "2:1"
      # Should not have "filename:" prefix before location
      refute result =~ ~r/\w+\.va:\d+:\d+/
    end

    test "includes hint when provided" do
      error = %{
        message: "unknown function `foo`",
        file: "test.va",
        line: 1,
        col: 2,
        span_length: 3,
        hint: "did you mean `for`?"
      }
      source = "(foo 1 2)"

      result = ErrorFormatter.format(error, source)

      assert result =~ "hint"
      assert result =~ "did you mean `for`?"
    end

    test "formats error on correct line in multiline source with span indicator" do
      error = %{
        message: "type error",
        file: "test.va",
        line: 3,
        col: 4,
        span_length: 3,
        hint: nil
      }
      source = "(defn add [x y]\n  (+ x\n     :bad))"

      result = ErrorFormatter.format(error, source)

      assert result =~ "test.va:3:4"
      assert result =~ ":bad"
      assert result =~ "^^^"
    end
  end

  describe "parse_legacy_error/1" do
    test "parses file:line:col: message format" do
      error = "test.va:3:6: Type mismatch at argument 2"

      result = ErrorFormatter.parse_legacy_error(error)

      assert result.file == "test.va"
      assert result.line == 3
      assert result.col == 6
      assert result.message == "Type mismatch at argument 2"
    end

    test "parses line:col: message format" do
      error = "1:5: Unknown function: foo"

      result = ErrorFormatter.parse_legacy_error(error)

      assert result.file == nil
      assert result.line == 1
      assert result.col == 5
      assert result.message == "Unknown function: foo"
    end

    test "returns nil for unparseable errors" do
      assert ErrorFormatter.parse_legacy_error("some random error") == nil
    end

    test "estimates span length from message with atom" do
      error = "1:1: does not accept message :invalid_msg"

      result = ErrorFormatter.parse_legacy_error(error)

      # Should detect :invalid_msg and set span_length accordingly
      assert result.span_length == 12  # ":invalid_msg" length
    end
  end

  describe "format_all/2" do
    test "formats multiple errors" do
      errors = [
        %{message: "error 1", file: "a.va", line: 1, col: 1, span_length: 1, hint: nil},
        %{message: "error 2", file: "a.va", line: 2, col: 1, span_length: 1, hint: nil}
      ]
      source = "line1\nline2"

      result = ErrorFormatter.format_all(errors, source)

      assert result =~ "error 1"
      assert result =~ "error 2"
      assert result =~ "a.va:1:1"
      assert result =~ "a.va:2:1"
    end
  end
end
