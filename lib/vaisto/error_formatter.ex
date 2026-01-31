defmodule Vaisto.ErrorFormatter do
  @moduledoc """
  Formats compiler errors in a Rust-style diagnostic format.

  Produces human-friendly error messages like:

      error[E001]: type mismatch
        --> test.va:3:6
         |
       2 |   (let [x 42]
       3 |     (+ x "hello"))
         |          ^^^^^^^ expected `Int`, found `String`
         |
         = note: the `+` operator requires numeric arguments
         = help: convert the string to a number
  """

  alias Vaisto.Error

  @type legacy_error :: %{
    message: String.t(),
    file: String.t() | nil,
    line: pos_integer(),
    col: pos_integer(),
    span_length: pos_integer(),
    hint: String.t() | nil
  }

  @doc """
  Formats a structured Vaisto.Error with source context.
  """
  @spec format(Error.t() | legacy_error(), String.t()) :: String.t()
  def format(%Error{} = error, source) do
    lines = String.split(source, "\n")

    [
      format_header(error),
      format_location(error),
      format_source_context(error, lines),
      format_note(error),
      format_hint(error)
    ]
    |> List.flatten()
    |> Enum.reject(&is_nil/1)
    |> Enum.join("\n")
  end

  # Legacy map format for backwards compatibility
  def format(%{message: _, line: _, col: _} = error, source) do
    format_legacy(error, source)
  end

  # Plain string error - try to parse or return as-is
  def format(error, source) when is_binary(error) do
    case parse_legacy_error(error) do
      nil -> error
      parsed -> format_legacy(parsed, source)
    end
  end

  @doc """
  Formats multiple errors with source context.
  """
  @spec format_all([Error.t() | legacy_error()], String.t()) :: String.t()
  def format_all(errors, source) do
    errors
    |> Enum.map(&format(&1, source))
    |> Enum.join("\n\n")
  end

  @doc """
  Parses a legacy error string like "file:line:col: message" into structured format.
  Returns nil if the string doesn't match the expected format.
  """
  @spec parse_legacy_error(String.t()) :: legacy_error() | nil
  def parse_legacy_error(error_string) do
    cond do
      # Format: "file:line:col: message"
      match = Regex.run(~r/^([^:]+):(\d+):(\d+):\s*(.+)$/, error_string) ->
        [_, file, line, col, message] = match
        %{
          message: message,
          file: file,
          line: String.to_integer(line),
          col: String.to_integer(col),
          span_length: estimate_span_length(message),
          hint: nil
        }

      # Format: "line:col: message"
      match = Regex.run(~r/^(\d+):(\d+):\s*(.+)$/, error_string) ->
        [_, line, col, message] = match
        %{
          message: message,
          file: nil,
          line: String.to_integer(line),
          col: String.to_integer(col),
          span_length: estimate_span_length(message),
          hint: nil
        }

      true ->
        nil
    end
  end

  # ============================================================================
  # Structured Error Formatting
  # ============================================================================

  # Format: "error: type mismatch"
  defp format_header(%Error{message: message}) do
    IO.ANSI.format([
      :red, :bright, "error",
      :reset, ": ", message
    ])
    |> IO.iodata_to_binary()
  end

  # Format: "at file:line:col" or just "at line:col"
  defp format_location(%Error{primary_span: nil}), do: nil
  defp format_location(%Error{file: file, primary_span: span}) do
    location = case file do
      nil -> "line #{span.line}"
      f -> "#{f}:#{span.line}"
    end

    IO.ANSI.format([:faint, "  at ", location, :reset])
    |> IO.iodata_to_binary()
  end

  # Format source context - just the code line and pointer (no line numbers)
  defp format_source_context(%Error{primary_span: nil}, _lines), do: nil
  defp format_source_context(%Error{primary_span: span} = error, lines) do
    line_content = Enum.at(lines, span.line - 1, "")

    # The error line (just the code, indented)
    error_line = "    " <> line_content

    # Primary pointer with label
    primary_pointer = format_pointer(span, line_content, error)

    [error_line, primary_pointer]
  end

  # Simplified pointer format (no gutter)
  defp format_pointer(span, line_content, error) do
    # Calculate visual offset (handle tabs)
    prefix = String.slice(line_content, 0, max(span.col - 1, 0))
    visual_offset = visual_length(prefix)

    # Build pointer with carets
    pointer_len = Map.get(span, :length, 1)
    pointer = String.duplicate("^", max(pointer_len, 1))
    # 4 spaces for indent + offset to match code position
    spacing = String.duplicate(" ", 4 + visual_offset)

    # Build the label
    label = build_label(span, error, :primary)

    IO.ANSI.format([
      spacing,
      :red, :bright, pointer,
      :reset, :red, label,
      :reset
    ])
    |> IO.iodata_to_binary()
  end

  defp build_label(span, error, :primary) do
    cond do
      span[:label] -> " #{span.label}"
      error.expected && error.actual ->
        " expected `#{Error.format_type(error.expected)}`, found `#{Error.format_type(error.actual)}`"
      true -> ""
    end
  end
  defp build_label(%{label: label}, _error, :secondary) when is_binary(label), do: " #{label}"
  defp build_label(_span, _error, _type), do: ""

  # Format note section
  defp format_note(%Error{note: nil}), do: nil
  defp format_note(%Error{note: note}) do
    IO.ANSI.format([:faint, "  note: ", note, :reset])
    |> IO.iodata_to_binary()
  end

  # Format hint/help section
  defp format_hint(%Error{hint: nil}), do: nil
  defp format_hint(%Error{hint: hint}) do
    IO.ANSI.format([:green, "  help: ", :reset, hint])
    |> IO.iodata_to_binary()
  end

  # ============================================================================
  # Legacy Format Support (for backwards compatibility)
  # ============================================================================

  defp format_legacy(error, source) do
    lines = String.split(source, "\n")
    line_content = Enum.at(lines, error.line - 1, "")

    [
      format_legacy_header(error),
      format_legacy_location(error),
      format_legacy_source_line(error, line_content),
      format_legacy_pointer(error, line_content),
      format_legacy_hint(error)
    ]
    |> Enum.reject(&is_nil/1)
    |> Enum.join("\n")
  end

  defp format_legacy_header(error) do
    IO.ANSI.format([:red, :bright, "error", :reset, ": ", error.message])
    |> IO.iodata_to_binary()
  end

  defp format_legacy_location(error) do
    location = case error.file do
      nil -> "#{error.line}:#{error.col}"
      file -> "#{file}:#{error.line}:#{error.col}"
    end

    IO.ANSI.format([:blue, :bright, "  --> ", :reset, location])
    |> IO.iodata_to_binary()
  end

  defp format_legacy_source_line(error, line_content) do
    line_num = Integer.to_string(error.line)
    padding = String.duplicate(" ", String.length(line_num))

    [
      IO.ANSI.format([:blue, :bright, "#{padding} |", :reset]) |> IO.iodata_to_binary(),
      IO.ANSI.format([:blue, :bright, "#{line_num} |", :reset, " ", line_content])
      |> IO.iodata_to_binary()
    ]
    |> Enum.join("\n")
  end

  defp format_legacy_pointer(error, line_content) do
    line_num = Integer.to_string(error.line)
    gutter_width = String.length(line_num)

    prefix = String.slice(line_content, 0, error.col - 1)
    visual_offset = visual_length(prefix)

    span = Map.get(error, :span_length, 1)
    pointer = String.duplicate("^", max(span, 1))
    spacing = String.duplicate(" ", visual_offset)

    IO.ANSI.format([
      :blue, :bright, String.duplicate(" ", gutter_width), " | ",
      :reset, spacing,
      :red, :bright, pointer, :reset
    ])
    |> IO.iodata_to_binary()
  end

  defp format_legacy_hint(%{hint: nil}), do: nil
  defp format_legacy_hint(%{hint: hint}) do
    IO.ANSI.format([:blue, :bright, "  = ", :cyan, "hint", :reset, ": ", hint])
    |> IO.iodata_to_binary()
  end

  # ============================================================================
  # Helpers
  # ============================================================================

  # Calculate visual length accounting for tabs
  defp visual_length(str) do
    str
    |> String.graphemes()
    |> Enum.reduce(0, fn
      "\t", acc -> acc + 4 - rem(acc, 4)  # Tab stops every 4 chars
      _, acc -> acc + 1
    end)
  end

  # Try to estimate span length from error message
  defp estimate_span_length(message) do
    cond do
      message =~ ~r/argument \d+/ -> 4
      match = Regex.run(~r/:(\w+)/, message) ->
        [_, atom] = match
        String.length(atom) + 1
      true -> 1
    end
  end
end
