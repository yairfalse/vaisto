defmodule Vaisto.LSP.References do
  @moduledoc """
  LSP references and rename support for Vaisto.

  Finds all occurrences of a symbol (function, variable, type) across
  documents and supports safe renaming with workspace edits.
  """

  alias Vaisto.Parser

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
        range = %{
          "start" => %{"line" => line - 1, "character" => start_col - 1},
          "end" => %{"line" => line - 1, "character" => end_col - 1}
        }
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

  defp find_symbol_occurrences(text, symbol, uri) do
    try do
      ast = Parser.parse(text)
      forms = if is_list(ast), do: ast, else: [ast]

      # Walk AST and collect all occurrences
      forms
      |> Enum.flat_map(&find_in_form(&1, symbol, uri))
    rescue
      _ ->
        # If parsing fails, fall back to text search
        find_symbol_textually(text, symbol, uri)
    end
  end

  # Walk a form and find all occurrences of symbol
  defp find_in_form(form, symbol, uri) do
    symbol_atom = if is_binary(symbol), do: String.to_atom(symbol), else: symbol

    collect_locations(form, symbol_atom, uri)
  end

  defp collect_locations(form, target, uri) do
    do_collect(form, target, uri, [])
    |> Enum.reverse()
  end

  # Function definition - check name
  defp do_collect({:defn, name, params, body, loc}, target, uri, acc) do
    acc = if name == target do
      [make_location(uri, loc, name) | acc]
    else
      acc
    end
    # Also check params and body
    acc = Enum.reduce(params, acc, &do_collect(&1, target, uri, &2))
    do_collect(body, target, uri, acc)
  end

  defp do_collect({:defn, name, params, body, _ret, loc}, target, uri, acc) do
    acc = if name == target do
      [make_location(uri, loc, name) | acc]
    else
      acc
    end
    acc = Enum.reduce(params, acc, &do_collect(&1, target, uri, &2))
    do_collect(body, target, uri, acc)
  end

  # Type definition
  defp do_collect({:deftype, name, def, loc}, target, uri, acc) do
    acc = if name == target do
      [make_location(uri, loc, name) | acc]
    else
      acc
    end
    do_collect(def, target, uri, acc)
  end

  # Process definition
  defp do_collect({:process, name, init, handlers, loc}, target, uri, acc) do
    acc = if name == target do
      [make_location(uri, loc, name) | acc]
    else
      acc
    end
    acc = do_collect(init, target, uri, acc)
    Enum.reduce(handlers, acc, &do_collect(&1, target, uri, &2))
  end

  # Function call
  defp do_collect({:call, name, args, loc}, target, uri, acc) when is_atom(name) do
    acc = if name == target do
      [make_location(uri, loc, name) | acc]
    else
      acc
    end
    Enum.reduce(args, acc, &do_collect(&1, target, uri, &2))
  end

  # Variable reference
  defp do_collect({:var, name, _type}, target, _uri, acc) when is_atom(name) do
    # Variables don't have location info in typed AST, skip for now
    if name == target do
      acc  # Would need to track location
    else
      acc
    end
  end

  # Let binding
  defp do_collect({:let, bindings, body, _type}, target, uri, acc) do
    acc = Enum.reduce(bindings, acc, fn
      {{:var, name, _}, expr, _}, a ->
        a = if name == target, do: a, else: a  # TODO: add location
        do_collect(expr, target, uri, a)
      _, a -> a
    end)
    do_collect(body, target, uri, acc)
  end

  # If expression
  defp do_collect({:if, cond, then_br, else_br, _type}, target, uri, acc) do
    acc = do_collect(cond, target, uri, acc)
    acc = do_collect(then_br, target, uri, acc)
    do_collect(else_br, target, uri, acc)
  end

  # Match expression
  defp do_collect({:match, expr, clauses, _type}, target, uri, acc) do
    acc = do_collect(expr, target, uri, acc)
    Enum.reduce(clauses, acc, fn {pattern, body}, a ->
      a = do_collect(pattern, target, uri, a)
      do_collect(body, target, uri, a)
    end)
  end

  # Do block
  defp do_collect({:do, exprs, _type}, target, uri, acc) do
    Enum.reduce(exprs, acc, &do_collect(&1, target, uri, &2))
  end

  # List
  defp do_collect({:list, elements, _type}, target, uri, acc) do
    Enum.reduce(elements, acc, &do_collect(&1, target, uri, &2))
  end

  # Tuple
  defp do_collect({:tuple, elements, _type}, target, uri, acc) do
    Enum.reduce(elements, acc, &do_collect(&1, target, uri, &2))
  end

  # Anonymous function
  defp do_collect({:fn, _params, body, _type}, target, uri, acc) do
    do_collect(body, target, uri, acc)
  end

  # Sum type variants
  defp do_collect({:sum, variants}, target, _uri, acc) do
    Enum.reduce(variants, acc, fn {ctor_name, _args}, a ->
      if ctor_name == target do
        a  # Would need location info
      else
        a
      end
    end)
  end

  # Literals - no symbols to find
  defp do_collect({:lit, _, _}, _target, _uri, acc), do: acc

  # Lists and other collections in raw AST
  defp do_collect(list, target, uri, acc) when is_list(list) do
    Enum.reduce(list, acc, &do_collect(&1, target, uri, &2))
  end

  # Tuples in raw AST
  defp do_collect(tuple, target, uri, acc) when is_tuple(tuple) do
    tuple
    |> Tuple.to_list()
    |> Enum.reduce(acc, &do_collect(&1, target, uri, &2))
  end

  # Atoms, integers, etc - no children
  defp do_collect(_other, _target, _uri, acc), do: acc

  # ============================================================================
  # Text-based Fallback
  # ============================================================================

  defp find_symbol_textually(text, symbol, uri) do
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

  defp make_location(uri, %Parser.Loc{} = loc, name) do
    name_len = name |> to_string() |> String.length()
    %{
      "uri" => uri,
      "range" => %{
        "start" => %{"line" => loc.line - 1, "character" => loc.col - 1},
        "end" => %{"line" => loc.line - 1, "character" => loc.col - 1 + name_len}
      }
    }
  end

  defp make_location(uri, _loc, _name) do
    # Fallback for missing location info
    %{
      "uri" => uri,
      "range" => %{
        "start" => %{"line" => 0, "character" => 0},
        "end" => %{"line" => 0, "character" => 0}
      }
    }
  end
end
