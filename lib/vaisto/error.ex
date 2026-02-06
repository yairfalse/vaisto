defmodule Vaisto.Error do
  @moduledoc """
  Structured compiler error with diagnostics support.

  Errors contain rich information for formatted display:
  - Primary span showing the main error location with label
  - Secondary spans for related code locations
  - Expected/actual types for type errors
  - Hints and notes for additional context

  ## Example

      %Vaisto.Error{
        message: "type mismatch",
        primary_span: %{line: 3, col: 8, length: 7, label: "expected `Int`"},
        expected: :int,
        actual: :string,
        hint: "numeric operations require numeric arguments"
      }
  """

  @type span :: %{
    line: pos_integer(),
    col: pos_integer(),
    length: pos_integer(),
    label: String.t() | nil
  }

  @type t :: %__MODULE__{
    message: String.t(),
    file: String.t() | nil,
    primary_span: span() | nil,
    secondary_spans: [span()],
    expected: term(),
    actual: term(),
    hint: String.t() | nil,
    note: String.t() | nil
  }

  defstruct [
    :message,
    :file,
    :primary_span,
    :expected,
    :actual,
    :hint,
    :note,
    secondary_spans: []
  ]

  @doc """
  Create a new error with the given message and options.
  """
  @spec new(String.t(), keyword()) :: t()
  def new(message, opts \\ []) do
    %__MODULE__{
      message: message,
      file: Keyword.get(opts, :file),
      primary_span: Keyword.get(opts, :span),
      secondary_spans: Keyword.get(opts, :secondary_spans, []),
      expected: Keyword.get(opts, :expected),
      actual: Keyword.get(opts, :actual),
      hint: Keyword.get(opts, :hint),
      note: Keyword.get(opts, :note)
    }
  end

  @doc """
  Create a span from a Loc struct and optional length.
  """
  @spec span_from_loc(Vaisto.Parser.Loc.t() | nil, pos_integer(), String.t() | nil) ::
          span() | nil
  def span_from_loc(loc, length \\ 1, label \\ nil)
  def span_from_loc(nil, _length, _label), do: nil
  def span_from_loc(%Vaisto.Parser.Loc{} = loc, length, label) do
    %{
      line: loc.line,
      col: loc.col,
      length: length,
      label: label
    }
  end

  @doc """
  Add a secondary span to an error.
  """
  @spec add_span(t(), span()) :: t()
  def add_span(%__MODULE__{} = error, span) do
    %{error | secondary_spans: error.secondary_spans ++ [span]}
  end

  @doc """
  Format a type for display in error messages.

  Delegates to `Vaisto.TypeFormatter.format/1` for consistent formatting
  across the codebase.
  """
  defdelegate format_type(type), to: Vaisto.TypeFormatter, as: :format

  @doc """
  Create an Error from a plain string message.

  Useful for normalizing legacy error formats.

  ## Examples

      iex> Vaisto.Error.from_string("something went wrong")
      %Vaisto.Error{message: "something went wrong"}
  """
  @spec from_string(String.t()) :: t()
  def from_string(message) when is_binary(message) do
    %__MODULE__{message: message}
  end

  @doc """
  Wrap an error tuple, normalizing to structured format.

  Handles all common error formats:
  - `{:error, %Error{}}` - already structured, returns as-is
  - `{:error, [list]}` - returns first error normalized
  - `{:error, "string"}` - converts to structured

  ## Examples

      iex> Vaisto.Error.wrap({:error, "bad input"})
      {:error, %Vaisto.Error{message: "bad input"}}

      iex> Vaisto.Error.wrap({:error, %Vaisto.Error{message: "typed"}})
      {:error, %Vaisto.Error{message: "typed"}}
  """
  @spec wrap(term()) :: {:error, t()} | {:ok, term()}
  def wrap({:error, %__MODULE__{} = error}), do: {:error, error}
  def wrap({:error, [first | _]}), do: wrap({:error, first})
  def wrap({:error, []}), do: {:error, from_string("unknown error")}
  def wrap({:error, msg}) when is_binary(msg), do: {:error, from_string(msg)}
  def wrap({:ok, _} = success), do: success
  def wrap(other), do: {:error, from_string(inspect(other))}

  @doc """
  Normalize an error value to a structured Error.

  Unlike `wrap/1`, this takes just the error value (not the tuple).

  ## Examples

      iex> Vaisto.Error.normalize("string error")
      %Vaisto.Error{message: "string error"}

      iex> Vaisto.Error.normalize(%Vaisto.Error{message: "typed"})
      %Vaisto.Error{message: "typed"}
  """
  @spec normalize(term()) :: t()
  def normalize(%__MODULE__{} = error), do: error
  def normalize(msg) when is_binary(msg), do: from_string(msg)
  def normalize(other), do: from_string(inspect(other))

  @doc """
  Check if a value is an Error struct.
  """
  @spec error?(term()) :: boolean()
  def error?(%__MODULE__{}), do: true
  def error?(_), do: false

  @doc """
  Extract the message from any error format.

  ## Examples

      iex> Vaisto.Error.message(%Vaisto.Error{message: "foo"})
      "foo"

      iex> Vaisto.Error.message("plain string")
      "plain string"
  """
  @spec message(t() | String.t()) :: String.t()
  def message(%__MODULE__{message: msg}), do: msg
  def message(msg) when is_binary(msg), do: msg
  def message(other), do: inspect(other)

  @doc """
  Convert an error to a full text representation including all fields.

  This is useful for pattern matching in tests.
  """
  @spec to_string(t()) :: String.t()
  def to_string(%__MODULE__{} = error) do
    parts = [error.message]
    parts = if error.note, do: parts ++ [error.note], else: parts
    parts = if error.hint, do: parts ++ [error.hint], else: parts
    Enum.join(parts, " ")
  end

  # Allow string pattern matching on Error structs
  defimpl String.Chars do
    def to_string(error), do: Vaisto.Error.to_string(error)
  end
end

