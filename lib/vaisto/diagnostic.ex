defmodule Vaisto.Diagnostic do
  @moduledoc """
  Structured diagnostic information for compiler errors and warnings.

  Captures rich error context including:
  - Error message
  - Location (file, line, column)
  - Span length for underlining
  - Optional hint for fixes
  - Severity level
  """

  defstruct [
    :message,
    :file,
    :line,
    :col,
    :span_length,
    :hint,
    severity: :error
  ]

  @type t :: %__MODULE__{
    message: String.t(),
    file: String.t() | nil,
    line: pos_integer(),
    col: pos_integer(),
    span_length: pos_integer(),
    hint: String.t() | nil,
    severity: :error | :warning | :info
  }

  @doc """
  Creates a new diagnostic from a location struct and message.
  """
  @spec new(Vaisto.Parser.Loc.t(), String.t(), keyword()) :: t()
  def new(%Vaisto.Parser.Loc{} = loc, message, opts \\ []) do
    %__MODULE__{
      message: message,
      file: loc.file,
      line: loc.line,
      col: loc.col,
      span_length: Keyword.get(opts, :span_length, 1),
      hint: Keyword.get(opts, :hint),
      severity: Keyword.get(opts, :severity, :error)
    }
  end

  @doc """
  Creates a diagnostic from line/col integers.
  """
  @spec from_position(pos_integer(), pos_integer(), String.t(), keyword()) :: t()
  def from_position(line, col, message, opts \\ []) do
    %__MODULE__{
      message: message,
      file: Keyword.get(opts, :file),
      line: line,
      col: col,
      span_length: Keyword.get(opts, :span_length, 1),
      hint: Keyword.get(opts, :hint),
      severity: Keyword.get(opts, :severity, :error)
    }
  end

  @doc """
  Converts a diagnostic to the map format expected by ErrorFormatter.
  """
  @spec to_error_map(t()) :: map()
  def to_error_map(%__MODULE__{} = diag) do
    %{
      message: diag.message,
      file: diag.file,
      line: diag.line,
      col: diag.col,
      span_length: diag.span_length,
      hint: diag.hint
    }
  end

  @doc """
  Formats a diagnostic with source context.
  """
  @spec format(t(), String.t()) :: String.t()
  def format(%__MODULE__{} = diag, source) do
    Vaisto.ErrorFormatter.format(to_error_map(diag), source)
  end

  @doc """
  Creates a type mismatch diagnostic with helpful context.
  """
  @spec type_mismatch(Vaisto.Parser.Loc.t(), expected :: term(), got :: term(), keyword()) :: t()
  def type_mismatch(loc, expected, got, opts \\ []) do
    message = "type mismatch: expected #{format_type(expected)}, found #{format_type(got)}"
    hint = Keyword.get(opts, :hint)
    new(loc, message, span_length: Keyword.get(opts, :span_length, 1), hint: hint)
  end

  @doc """
  Creates an unknown function diagnostic.
  """
  @spec unknown_function(Vaisto.Parser.Loc.t(), atom(), keyword()) :: t()
  def unknown_function(loc, name, opts \\ []) do
    message = "unknown function `#{name}`"
    hint = Keyword.get(opts, :hint, "did you mean to define this function?")
    new(loc, message, span_length: String.length(to_string(name)), hint: hint)
  end

  @doc """
  Creates an arity mismatch diagnostic.
  """
  @spec arity_mismatch(Vaisto.Parser.Loc.t(), atom(), expected :: non_neg_integer(), got :: non_neg_integer()) :: t()
  def arity_mismatch(loc, func_name, expected, got) do
    message = "function `#{func_name}` expects #{expected} argument(s), found #{got}"
    new(loc, message, span_length: String.length(to_string(func_name)))
  end

  @doc """
  Creates an undefined variable diagnostic.
  """
  @spec undefined_variable(Vaisto.Parser.Loc.t(), atom()) :: t()
  def undefined_variable(loc, name) do
    message = "undefined variable `#{name}`"
    new(loc, message, span_length: String.length(to_string(name)), hint: "did you mean to define this variable?")
  end

  @doc """
  Creates an invalid message diagnostic for typed PIDs.
  """
  @spec invalid_message(Vaisto.Parser.Loc.t(), process_name :: atom(), message :: atom(), valid :: [atom()]) :: t()
  def invalid_message(loc, process_name, message, valid_messages) do
    valid_str = valid_messages |> Enum.map(&":#{&1}") |> Enum.join(", ")
    msg = "process `#{process_name}` does not accept message `:#{message}`"
    hint = "valid messages are: #{valid_str}"
    new(loc, msg, span_length: String.length(to_string(message)) + 1, hint: hint)
  end

  # Delegate to TypeFormatter for consistent type display
  defp format_type(type), do: Vaisto.TypeFormatter.format(type)
end
