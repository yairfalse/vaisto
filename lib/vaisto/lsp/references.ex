defmodule Vaisto.LSP.References do
  @moduledoc """
  LSP references and rename support for Vaisto.

  Finds all occurrences of a symbol (function, variable, type) across
  documents and supports safe renaming with workspace edits.
  """

  alias Vaisto.LSP.Position

  # ============================================================================
  # Public API
  # ============================================================================

  @doc """
  Find all references to the symbol at the given position.
  Returns a list of LSP Location objects.
  """
  def find_references(text, line, col, _uri, documents) do
    case get_symbol_at(text, line, col) do
      {:ok, symbol, _range} ->
        # Search in all documents
        documents
        |> Enum.flat_map(fn {doc_uri, doc_text} ->
          find_symbol_occurrences(doc_text, symbol, doc_uri)
        end)

      :not_found ->
        []
    end
  end

  @doc """
  Get the symbol at the given position.
  Returns {:ok, symbol_name, range} or :not_found.
  """
  def get_symbol_at(text, line, col) do
    lines = String.split(text, "\n")
    current_line = Enum.at(lines, line - 1, "")

    # Find word boundaries around cursor
    case find_word_at(current_line, col) do
      {:ok, word, start_col, end_col} ->
        range = Position.line_range(line, start_col, end_col)
        {:ok, word, range}

      :not_found ->
        :not_found
    end
  end

  @doc """
  Prepare a rename operation.
  Returns {:ok, workspace_edit} or {:error, reason}.
  """
  def prepare_rename(text, line, col, _uri, documents, new_name) do
    case get_symbol_at(text, line, col) do
      {:ok, symbol, _range} ->
        # Validate new name
        if valid_identifier?(new_name) do
          # Find all occurrences and create edits
          edits_by_uri = documents
          |> Enum.map(fn {doc_uri, doc_text} ->
            edits = find_symbol_occurrences(doc_text, symbol, doc_uri)
            |> Enum.map(fn loc ->
              %{
                "range" => loc["range"],
                "newText" => new_name
              }
            end)
            {doc_uri, edits}
          end)
          |> Enum.reject(fn {_uri, edits} -> edits == [] end)
          |> Enum.into(%{})

          workspace_edit = %{
            "changes" => edits_by_uri
          }

          {:ok, workspace_edit}
        else
          {:error, "Invalid identifier: #{new_name}"}
        end

      :not_found ->
        {:error, "No symbol at position"}
    end
  end

  # ============================================================================
  # Symbol Search
  # ============================================================================

  # Text-based search provides accurate character-level positions
  # for all symbol occurrences including variable references
  defp find_symbol_occurrences(text, symbol, uri) do
    lines = String.split(text, "\n")
    symbol_str = to_string(symbol)

    # Use regex to find word boundaries
    pattern = ~r/(?<![a-zA-Z0-9_])#{Regex.escape(symbol_str)}(?![a-zA-Z0-9_])/

    lines
    |> Enum.with_index()
    |> Enum.flat_map(fn {line_text, line_idx} ->
      Regex.scan(pattern, line_text, return: :index)
      |> Enum.map(fn [{start, len}] ->
        %{
          "uri" => uri,
          "range" => %{
            "start" => %{"line" => line_idx, "character" => start},
            "end" => %{"line" => line_idx, "character" => start + len}
          }
        }
      end)
    end)
  end

  # ============================================================================
  # Helpers
  # ============================================================================

  defp find_word_at(line, col) do
    chars = String.graphemes(line)
    col_idx = col - 1  # Convert to 0-indexed

    # Check if cursor is on a valid identifier character
    char_at = Enum.at(chars, col_idx, "")
    if not identifier_char?(char_at) do
      :not_found
    else
      # Find word start
      start_idx = find_word_start(chars, col_idx)
      # Find word end
      end_idx = find_word_end(chars, col_idx)

      word = chars
             |> Enum.slice(start_idx..end_idx)
             |> Enum.join()

      if word == "" do
        :not_found
      else
        {:ok, word, start_idx + 1, end_idx + 2}  # Convert back to 1-indexed, end is exclusive
      end
    end
  end

  defp find_word_start(chars, idx) do
    if idx <= 0 do
      0
    else
      prev_char = Enum.at(chars, idx - 1, "")
      if identifier_char?(prev_char) do
        find_word_start(chars, idx - 1)
      else
        idx
      end
    end
  end

  defp find_word_end(chars, idx) do
    if idx >= length(chars) - 1 do
      idx
    else
      next_char = Enum.at(chars, idx + 1, "")
      if identifier_char?(next_char) do
        find_word_end(chars, idx + 1)
      else
        idx
      end
    end
  end

  defp identifier_char?(char) when is_binary(char) do
    char =~ ~r/[a-zA-Z0-9_!?\-+*\/<>=]/
  end
  defp identifier_char?(_), do: false

  defp valid_identifier?(name) do
    # Must start with letter or allowed symbol, followed by valid chars
    name =~ ~r/^[a-zA-Z_!?\-+*\/<>=][a-zA-Z0-9_!?\-+*\/<>=]*$/
  end
end
