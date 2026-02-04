defmodule Vaisto.LSP.Position do
  @moduledoc """
  Value object for LSP position handling.

  LSP uses 0-indexed line and character positions, while Vaisto internally
  uses 1-indexed positions (line and col). This module provides a single
  source of truth for coordinate conversions between these systems.

  ## Usage

      # Convert from Vaisto Loc to LSP Position
      lsp_pos = Position.from_loc(%Loc{line: 1, col: 5})
      # => %Position{line: 0, character: 4}

      # Convert from LSP Position to Vaisto Loc
      loc = Position.to_loc(%Position{line: 0, character: 4})
      # => %Loc{line: 1, col: 5}

      # Create an LSP Range from a Vaisto location and length
      range = Position.to_lsp_range(%Loc{line: 1, col: 5}, 3)
      # => %{"start" => %{"line" => 0, "character" => 4},
      #      "end" => %{"line" => 0, "character" => 7}}
  """

  alias Vaisto.Parser.Loc

  defstruct [:line, :character]

  @type t :: %__MODULE__{
    line: non_neg_integer(),
    character: non_neg_integer()
  }

  @doc """
  Create an LSP Position from a Vaisto Loc.

  Converts 1-indexed Vaisto coordinates to 0-indexed LSP coordinates.

  ## Examples

      iex> Vaisto.LSP.Position.from_loc(%Vaisto.Parser.Loc{line: 1, col: 1})
      %Vaisto.LSP.Position{line: 0, character: 0}

      iex> Vaisto.LSP.Position.from_loc(%Vaisto.Parser.Loc{line: 5, col: 10})
      %Vaisto.LSP.Position{line: 4, character: 9}
  """
  @spec from_loc(Loc.t()) :: t()
  def from_loc(%Loc{line: line, col: col}) do
    %__MODULE__{line: line - 1, character: col - 1}
  end

  @doc """
  Create a Vaisto Loc from an LSP Position.

  Converts 0-indexed LSP coordinates to 1-indexed Vaisto coordinates.

  ## Examples

      iex> Vaisto.LSP.Position.to_loc(%Vaisto.LSP.Position{line: 0, character: 0})
      %Vaisto.Parser.Loc{line: 1, col: 1}

      iex> Vaisto.LSP.Position.to_loc(%Vaisto.LSP.Position{line: 4, character: 9})
      %Vaisto.Parser.Loc{line: 5, col: 10}
  """
  @spec to_loc(t()) :: Loc.t()
  def to_loc(%__MODULE__{line: line, character: char}) do
    %Loc{line: line + 1, col: char + 1}
  end

  @doc """
  Create an LSP Position from line and character integers (already 0-indexed).

  Use this when constructing positions from LSP request parameters.
  """
  @spec new(non_neg_integer(), non_neg_integer()) :: t()
  def new(line, character) do
    %__MODULE__{line: line, character: character}
  end

  @doc """
  Convert 0-indexed LSP coordinates to 1-indexed Vaisto coordinates.

  This is a convenience function for converting LSP request parameters directly.

  ## Examples

      iex> Vaisto.LSP.Position.lsp_to_vaisto(0, 4)
      {1, 5}
  """
  @spec lsp_to_vaisto(non_neg_integer(), non_neg_integer()) :: {pos_integer(), pos_integer()}
  def lsp_to_vaisto(line, character) do
    {line + 1, character + 1}
  end

  @doc """
  Convert 1-indexed Vaisto coordinates to 0-indexed LSP coordinates.

  ## Examples

      iex> Vaisto.LSP.Position.vaisto_to_lsp(1, 5)
      {0, 4}
  """
  @spec vaisto_to_lsp(pos_integer(), pos_integer()) :: {non_neg_integer(), non_neg_integer()}
  def vaisto_to_lsp(line, col) do
    {line - 1, col - 1}
  end

  @doc """
  Create an LSP Range map from a Vaisto location and span length.

  This is commonly used for hover ranges, diagnostics, and document symbols.

  ## Examples

      iex> loc = %Vaisto.Parser.Loc{line: 1, col: 5}
      iex> Vaisto.LSP.Position.to_lsp_range(loc, 3)
      %{"start" => %{"line" => 0, "character" => 4},
        "end" => %{"line" => 0, "character" => 7}}
  """
  @spec to_lsp_range(Loc.t(), non_neg_integer()) :: map()
  def to_lsp_range(%Loc{line: line, col: col}, length) do
    %{
      "start" => %{"line" => line - 1, "character" => col - 1},
      "end" => %{"line" => line - 1, "character" => col - 1 + length}
    }
  end

  @doc """
  Create an LSP Range map from Vaisto line/col/length values.

  Convenience function for when you have separate values instead of a Loc struct.
  """
  @spec to_lsp_range(pos_integer(), pos_integer(), non_neg_integer()) :: map()
  def to_lsp_range(line, col, length) when is_integer(line) and is_integer(col) do
    %{
      "start" => %{"line" => line - 1, "character" => col - 1},
      "end" => %{"line" => line - 1, "character" => col - 1 + length}
    }
  end

  @doc """
  Create an LSP Range spanning a single line from start to end column.
  """
  @spec line_range(pos_integer(), pos_integer(), pos_integer()) :: map()
  def line_range(line, start_col, end_col) do
    %{
      "start" => %{"line" => line - 1, "character" => start_col - 1},
      "end" => %{"line" => line - 1, "character" => end_col - 1}
    }
  end

  @doc """
  Create an LSP Position map from a Vaisto Loc.

  Returns the map format expected by LSP responses.
  """
  @spec to_lsp_position(Loc.t()) :: map()
  def to_lsp_position(%Loc{line: line, col: col}) do
    %{"line" => line - 1, "character" => col - 1}
  end

  @doc """
  Create an LSP Position map from Vaisto line/col values.
  """
  @spec to_lsp_position(pos_integer(), pos_integer()) :: map()
  def to_lsp_position(line, col) when is_integer(line) and is_integer(col) do
    %{"line" => line - 1, "character" => col - 1}
  end

  @doc """
  Convert the Position struct to an LSP-format map.
  """
  @spec to_map(t()) :: map()
  def to_map(%__MODULE__{line: line, character: char}) do
    %{"line" => line, "character" => char}
  end

  @doc """
  Offset a position by a number of characters (same line).
  """
  @spec offset(t(), integer()) :: t()
  def offset(%__MODULE__{line: line, character: char}, delta) do
    %__MODULE__{line: line, character: char + delta}
  end
end
