defmodule Vaisto.LSP.Handler do
  @moduledoc """
  Handles LSP requests and notifications.

  Dispatches to appropriate handlers based on method name.
  """

  alias Vaisto.LSP.{Protocol, Hover}
  alias Vaisto.{Parser, TypeChecker}

  @doc """
  Handle an incoming LSP request/notification.
  Returns {response_or_nil, new_state}.
  """
  def handle(%{method: method, id: id, params: params}, state) do
    case method do
      "initialize" ->
        handle_initialize(id, params, state)

      "initialized" ->
        # Client acknowledged initialization
        {nil, state}

      "shutdown" ->
        {Protocol.response(id, nil), state}

      "exit" ->
        System.halt(0)
        {nil, state}

      "textDocument/didOpen" ->
        handle_did_open(params, state)

      "textDocument/didChange" ->
        handle_did_change(params, state)

      "textDocument/didSave" ->
        handle_did_save(params, state)

      "textDocument/didClose" ->
        handle_did_close(params, state)

      "textDocument/hover" ->
        handle_hover(id, params, state)

      "textDocument/definition" ->
        handle_definition(id, params, state)

      "textDocument/documentSymbol" ->
        handle_document_symbol(id, params, state)

      _ ->
        # Unknown method - return method not found for requests, ignore notifications
        if id do
          {Protocol.error_response(id, -32601, "Method not found: #{method}"), state}
        else
          {nil, state}
        end
    end
  end

  # ============================================================================
  # Lifecycle
  # ============================================================================

  defp handle_initialize(id, params, state) do
    workspace_root = get_in(params, ["rootUri"]) || get_in(params, ["rootPath"])

    capabilities = %{
      "textDocumentSync" => %{
        "openClose" => true,
        "change" => 1,  # Full sync
        "save" => %{"includeText" => true}
      },
      "hoverProvider" => true,
      "definitionProvider" => true,
      "documentSymbolProvider" => true
    }

    result = %{
      "capabilities" => capabilities,
      "serverInfo" => %{
        "name" => "vaisto-lsp",
        "version" => "0.1.0"
      }
    }

    new_state = %{state | workspace_root: workspace_root}
    {Protocol.response(id, result), new_state}
  end

  # ============================================================================
  # Document Sync
  # ============================================================================

  defp handle_did_open(%{"textDocument" => doc}, state) do
    uri = doc["uri"]
    text = doc["text"]

    new_docs = Map.put(state.documents, uri, text)
    new_state = %{state | documents: new_docs}

    # Validate and publish diagnostics
    {diagnostics_notification, new_state} = validate_document(uri, text, new_state)

    {diagnostics_notification, new_state}
  end

  defp handle_did_change(%{"textDocument" => doc, "contentChanges" => changes}, state) do
    uri = doc["uri"]
    # Full sync - take the last change
    text = List.last(changes)["text"]

    new_docs = Map.put(state.documents, uri, text)
    new_state = %{state | documents: new_docs}

    # Validate and publish diagnostics
    {diagnostics_notification, new_state} = validate_document(uri, text, new_state)

    {diagnostics_notification, new_state}
  end

  defp handle_did_save(%{"textDocument" => doc}, state) do
    uri = doc["uri"]
    text = state.documents[uri]

    if text do
      validate_document(uri, text, state)
    else
      {nil, state}
    end
  end

  defp handle_did_close(%{"textDocument" => doc}, state) do
    uri = doc["uri"]
    new_docs = Map.delete(state.documents, uri)
    new_diagnostics = Map.delete(state.diagnostics, uri)

    # Clear diagnostics
    notification = Protocol.publish_diagnostics(uri, [])

    {notification, %{state | documents: new_docs, diagnostics: new_diagnostics}}
  end

  # ============================================================================
  # Validation
  # ============================================================================

  defp validate_document(uri, text, state) do
    file = uri_to_path(uri)

    diagnostics = case parse_and_check(text, file) do
      {:ok, _type, _ast} ->
        []

      {:error, error} when is_binary(error) ->
        [Protocol.diagnostic(error, text)]

      {:error, %Vaisto.Error{} = error} ->
        [Protocol.diagnostic(error, text)]
    end

    new_diagnostics = Map.put(state.diagnostics, uri, diagnostics)
    new_state = %{state | diagnostics: new_diagnostics}

    notification = Protocol.publish_diagnostics(uri, diagnostics)
    {notification, new_state}
  end

  defp parse_and_check(text, file) do
    try do
      ast = Parser.parse(text, file: file)
      TypeChecker.check(ast)
    rescue
      e ->
        {:error, Exception.message(e)}
    end
  end

  # ============================================================================
  # Hover
  # ============================================================================

  defp handle_hover(id, %{"textDocument" => doc, "position" => pos}, state) do
    uri = doc["uri"]
    text = state.documents[uri]

    if text do
      line = pos["line"] + 1  # LSP is 0-indexed
      col = pos["character"] + 1
      file = uri_to_path(uri)

      result = case Hover.get_hover(text, line, col, file) do
        {:ok, hover} ->
          %{
            "contents" => %{
              "kind" => "markdown",
              "value" => hover.contents
            },
            "range" => %{
              "start" => %{"line" => hover.range.line - 1, "character" => hover.range.col - 1},
              "end" => %{"line" => hover.range.line - 1, "character" => hover.range.col - 1 + hover.range.length}
            }
          }

        :not_found ->
          nil
      end

      {Protocol.response(id, result), state}
    else
      {Protocol.response(id, nil), state}
    end
  end

  # ============================================================================
  # Go to Definition
  # ============================================================================

  defp handle_definition(id, %{"textDocument" => doc, "position" => pos}, state) do
    uri = doc["uri"]
    text = state.documents[uri]

    if text do
      line = pos["line"] + 1
      col = pos["character"] + 1
      file = uri_to_path(uri)

      result = case Hover.get_definition(text, line, col, file) do
        {:ok, loc} ->
          %{
            "uri" => uri,
            "range" => %{
              "start" => %{"line" => loc.line - 1, "character" => loc.col - 1},
              "end" => %{"line" => loc.line - 1, "character" => loc.col - 1}
            }
          }

        :not_found ->
          nil
      end

      {Protocol.response(id, result), state}
    else
      {Protocol.response(id, nil), state}
    end
  end

  # ============================================================================
  # Document Symbols
  # ============================================================================

  defp handle_document_symbol(id, %{"textDocument" => doc}, state) do
    uri = doc["uri"]
    text = state.documents[uri]

    if text do
      symbols = extract_symbols(text, uri)
      {Protocol.response(id, symbols), state}
    else
      {Protocol.response(id, []), state}
    end
  end

  defp extract_symbols(text, _uri) do
    try do
      ast = Parser.parse(text)
      forms = if is_list(ast), do: ast, else: [ast]

      forms
      |> Enum.filter(&is_top_level_form/1)
      |> Enum.map(&symbol_from_form/1)
      |> Enum.reject(&is_nil/1)
    rescue
      _ -> []
    end
  end

  defp is_top_level_form({:defn, _, _, _, _, _}), do: true
  defp is_top_level_form({:defn, _, _, _, _}), do: true
  defp is_top_level_form({:defn_multi, _, _, _, _}), do: true
  defp is_top_level_form({:deftype, _, _, _}), do: true
  defp is_top_level_form({:process, _, _, _, _}), do: true
  defp is_top_level_form({:extern, _, _, _, _, _}), do: true
  defp is_top_level_form(_), do: false

  defp symbol_from_form({:defn, name, _params, _body, _ret, loc}) do
    make_symbol(name, 12, loc)  # 12 = Function
  end
  defp symbol_from_form({:defn, name, _params, _body, loc}) do
    make_symbol(name, 12, loc)
  end
  defp symbol_from_form({:defn_multi, name, _arity, _clauses, loc}) do
    make_symbol(name, 12, loc)
  end
  defp symbol_from_form({:deftype, name, _def, loc}) do
    make_symbol(name, 5, loc)  # 5 = Class (type)
  end
  defp symbol_from_form({:process, name, _init, _handlers, loc}) do
    make_symbol(name, 9, loc)  # 9 = Constructor
  end
  defp symbol_from_form({:extern, _mod, func, _args, _ret, loc}) do
    make_symbol(func, 12, loc)
  end
  defp symbol_from_form(_), do: nil

  defp make_symbol(name, kind, %Parser.Loc{} = loc) do
    line = loc.line - 1
    %{
      "name" => to_string(name),
      "kind" => kind,
      "range" => %{
        "start" => %{"line" => line, "character" => 0},
        "end" => %{"line" => line, "character" => 0}
      },
      "selectionRange" => %{
        "start" => %{"line" => line, "character" => loc.col - 1},
        "end" => %{"line" => line, "character" => loc.col - 1 + String.length(to_string(name))}
      }
    }
  end

  # ============================================================================
  # Helpers
  # ============================================================================

  defp uri_to_path("file://" <> path), do: URI.decode(path)
  defp uri_to_path(path), do: path
end
