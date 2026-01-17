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
  def add_span(%__MODULE__{} = error, span) do
    %{error | secondary_spans: error.secondary_spans ++ [span]}
  end

  @doc """
  Format a type for display in error messages.
  """
  def format_type(:int), do: "Int"
  def format_type(:float), do: "Float"
  def format_type(:bool), do: "Bool"
  def format_type(:string), do: "String"
  def format_type(:atom), do: "Atom"
  def format_type(:any), do: "Any"
  def format_type(:unit), do: "()"
  def format_type({:list, elem}), do: "List(#{format_type(elem)})"
  def format_type({:fn, args, ret}) do
    arg_str = args |> Enum.map(&format_type/1) |> Enum.join(", ")
    "(#{arg_str}) -> #{format_type(ret)}"
  end
  def format_type({:pid, name, _}), do: "Pid(#{name})"
  def format_type({:process, state, _}), do: "Process(#{format_type(state)})"
  def format_type({:sum, name, _}), do: "#{name}"
  def format_type({:record, name, _}), do: "#{name}"
  def format_type({:atom, a}), do: ":#{a}"
  def format_type(other) when is_atom(other), do: "#{other}"
  def format_type(other), do: inspect(other)
end
