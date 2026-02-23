defmodule Vaisto.LSP.InlayHints do
  @moduledoc """
  Generates LSP inlay hints from Vaisto source code.

  Shows inferred types for:
  - Let bindings: (let [x 42] ...) → x: Int
  - Unannotated function parameters: (defn f [x y] ...) → x: Int, y: Int
  """

  alias Vaisto.{Parser, TypeChecker, TypeFormatter}
  alias Vaisto.LSP.Position

  @doc """
  Collect inlay hints for the given source text.
  Returns a list of LSP InlayHint objects.
  """
  def collect(text, file \\ nil) do
    with {:ok, raw_ast} <- safe_parse(text, file),
         {:ok, _type, typed_ast} <- safe_check(raw_ast) do
      lines = String.split(text, ~r/\r\n|\r|\n/)
      forms = if is_list(typed_ast), do: typed_ast, else: [typed_ast]
      raw_forms = if is_list(raw_ast), do: raw_ast, else: [raw_ast]

      Enum.zip(raw_forms, forms)
      |> Enum.flat_map(fn {raw, typed} -> collect_hints(raw, typed, lines) end)
    else
      _ -> []
    end
  end

  defp safe_parse(text, file) do
    try do
      {:ok, Parser.parse(text, file: file)}
    rescue
      _ -> :error
    end
  end

  defp safe_check(ast) do
    try do
      TypeChecker.check(ast)
    rescue
      _ -> :error
    end
  end

  # ============================================================================
  # Hint collection
  # ============================================================================

  # defn without return type annotation
  defp collect_hints(
    {:defn, name, params, _body, %Parser.Loc{} = loc},
    {:defn, _tname, _tparams, tbody, {:fn, param_types, _ret}},
    lines
  ) do
    param_hints(name, params, param_types, loc, lines) ++
      collect_body_hints(tbody, lines)
  end

  # defn with return type annotation
  defp collect_hints(
    {:defn, name, params, _body, _ret_ann, %Parser.Loc{} = loc},
    {:defn, _tname, _tparams, tbody, {:fn, param_types, _ret}},
    lines
  ) do
    param_hints(name, params, param_types, loc, lines) ++
      collect_body_hints(tbody, lines)
  end

  # Top-level let
  defp collect_hints(
    {:let, raw_bindings, _body_raw, %Parser.Loc{} = loc},
    {:let, typed_bindings, body_typed, _type},
    lines
  ) do
    let_hints(raw_bindings, typed_bindings, loc, lines) ++
      collect_body_hints(body_typed, lines)
  end

  defp collect_hints(_, _, _lines), do: []

  # Walk typed AST body for nested let bindings
  defp collect_body_hints({:let, typed_bindings, body, _type}, lines) do
    # For nested lets in typed AST, we only have typed bindings (no raw positions)
    # We use name scanning to find positions
    nested = typed_bindings
    |> Enum.flat_map(fn
      {name, _value, type} when is_atom(name) ->
        type_str = TypeFormatter.format(type)
        find_let_binding_hints(name, type_str, lines)
      _ -> []
    end)
    nested ++ collect_body_hints(body, lines)
  end

  defp collect_body_hints({:do, exprs, _type}, lines) when is_list(exprs) do
    Enum.flat_map(exprs, &collect_body_hints(&1, lines))
  end

  defp collect_body_hints({:if, _cond, then_br, else_br, _type}, lines) do
    collect_body_hints(then_br, lines) ++ collect_body_hints(else_br, lines)
  end

  defp collect_body_hints(_, _lines), do: []

  # ============================================================================
  # Parameter hints — unannotated params only
  # ============================================================================

  defp param_hints(_name, params, param_types, loc, lines) when is_list(params) do
    # Find the parameter list in source by locating "[" after defn name on the same line
    line_text = Enum.at(lines, loc.line - 1, "")

    params
    |> Enum.zip(param_types)
    |> Enum.flat_map(fn
      # Annotated param like {name, :int} — skip
      {{_name, _ann}, _type} -> []
      # Unannotated param — show hint
      {name, type} when is_atom(name) ->
        name_str = Atom.to_string(name)
        type_str = TypeFormatter.format(type)
        # Find param position in the line
        case find_token_in_line(line_text, name_str, loc.line) do
          nil -> []
          {line_num, col} ->
            [make_hint(type_str, line_num, col + String.length(name_str))]
        end
      _ -> []
    end)
  end
  defp param_hints(_, _, _, _, _), do: []

  # ============================================================================
  # Let binding hints
  # ============================================================================

  defp let_hints(raw_bindings, typed_bindings, loc, lines) when is_list(typed_bindings) do
    line_text = Enum.at(lines, loc.line - 1, "")

    typed_bindings
    |> Enum.flat_map(fn
      {name, _value, type} when is_atom(name) ->
        # Check if the raw binding has an annotation
        has_annotation = Keyword.keyword?(raw_bindings) and
          not is_nil(Keyword.get(raw_bindings, name))

        if has_annotation do
          # Let bindings in raw AST are keyword-style [name: value]
          # They never have type annotations, so always show hint
          name_str = Atom.to_string(name)
          type_str = TypeFormatter.format(type)
          case find_token_in_line(line_text, name_str, loc.line) do
            nil ->
              # Try scanning all lines for multiline let
              find_let_binding_hints(name, type_str, lines)
            {line_num, col} ->
              [make_hint(type_str, line_num, col + String.length(name_str))]
          end
        else
          []
        end
      _ -> []
    end)
  end
  defp let_hints(_, _, _, _), do: []

  # Scan source lines for a let-binding variable name
  defp find_let_binding_hints(name, type_str, lines) do
    name_str = Atom.to_string(name)
    lines
    |> Enum.with_index(1)
    |> Enum.flat_map(fn {line_text, line_num} ->
      # Look for the name inside a let binding context (after "[")
      if String.contains?(line_text, name_str) do
        case find_token_in_line(line_text, name_str, line_num) do
          nil -> []
          {ln, col} -> [make_hint(type_str, ln, col + String.length(name_str))]
        end
      else
        []
      end
    end)
    |> Enum.take(1)  # Only first occurrence
  end

  # ============================================================================
  # Position helpers
  # ============================================================================

  # Find a token in a line, ensuring it's a whole word (not part of a larger name)
  defp find_token_in_line(line_text, token, line_num) do
    case :binary.match(line_text, token) do
      {pos, len} ->
        before_ok = pos == 0 or not word_char?(String.at(line_text, pos - 1))
        after_ok = (pos + len) >= String.length(line_text) or
          not word_char?(String.at(line_text, pos + len))

        if before_ok and after_ok do
          {line_num, pos}  # 1-based line, 0-based col
        else
          # Try searching further in the string
          rest = String.slice(line_text, (pos + len)..-1//1)
          case find_token_in_line(rest, token, line_num) do
            nil -> nil
            {ln, col} -> {ln, col + pos + len}
          end
        end
      :nomatch -> nil
    end
  end

  defp word_char?(nil), do: false
  defp word_char?(ch), do: Regex.match?(~r/[a-zA-Z0-9_\-]/, ch)

  defp make_hint(type_str, line, col) do
    {lsp_line, lsp_col} = Position.vaisto_to_lsp(line, col)
    %{
      "position" => %{"line" => lsp_line, "character" => lsp_col},
      "label" => ": #{type_str}",
      "kind" => 1,  # Type
      "paddingLeft" => false,
      "paddingRight" => true
    }
  end
end
