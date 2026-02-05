defmodule Vaisto.LSP.Protocol do
  @moduledoc """
  JSON-RPC encoding/decoding for Language Server Protocol.

  Uses Elixir's built-in JSON (Jason-compatible API via :json in OTP 27+,
  or falls back to simple implementation for older versions).
  """

  # ============================================================================
  # Decoding
  # ============================================================================

  @doc """
  Decode a JSON-RPC message from a string.
  """
  def decode(content) do
    case json_decode(content) do
      {:ok, %{"jsonrpc" => "2.0"} = msg} ->
        {:ok, normalize_message(msg)}
      {:ok, _} ->
        {:error, :invalid_jsonrpc}
      {:error, _} = error ->
        error
    end
  end

  defp normalize_message(msg) do
    %{
      id: Map.get(msg, "id"),
      method: Map.get(msg, "method"),
      params: Map.get(msg, "params", %{})
    }
  end

  # ============================================================================
  # Encoding
  # ============================================================================

  @doc """
  Encode a response to JSON string.
  """
  def encode(%{id: id, result: result}) do
    json_encode(%{
      "jsonrpc" => "2.0",
      "id" => id,
      "result" => result
    })
  end

  def encode(%{id: id, error: error}) do
    json_encode(%{
      "jsonrpc" => "2.0",
      "id" => id,
      "error" => error
    })
  end

  def encode(%{method: method, params: params}) do
    json_encode(%{
      "jsonrpc" => "2.0",
      "method" => method,
      "params" => params
    })
  end

  # ============================================================================
  # LSP Message Builders
  # ============================================================================

  @doc "Build a successful response"
  def response(id, result) do
    %{id: id, result: result}
  end

  @doc "Build an error response"
  def error_response(id, code, message) do
    %{id: id, error: %{"code" => code, "message" => message}}
  end

  @doc "Build a notification (no id)"
  def notification(method, params) do
    %{method: method, params: params}
  end

  @doc "Build a publishDiagnostics notification"
  def publish_diagnostics(uri, diagnostics) do
    notification("textDocument/publishDiagnostics", %{
      "uri" => uri,
      "diagnostics" => diagnostics
    })
  end

  @doc "Build an LSP Diagnostic from Vaisto error"
  def diagnostic(error, _source) when is_binary(error) do
    # Parse legacy string errors
    case Vaisto.ErrorFormatter.parse_legacy_error(error) do
      nil ->
        %{
          "range" => range(1, 1, 1, 1),
          "severity" => 1,  # Error
          "source" => "vaisto",
          "message" => error
        }
      parsed ->
        line = parsed.line - 1  # LSP is 0-indexed
        col = parsed.col - 1
        %{
          "range" => range(line, col, line, col + parsed.span_length),
          "severity" => 1,
          "source" => "vaisto",
          "message" => parsed.message
        }
    end
  end

  def diagnostic(%Vaisto.Error{} = error, _source) do
    span = error.primary_span || %{line: 1, col: 1, length: 1}
    line = span.line - 1  # LSP is 0-indexed
    col = span.col - 1
    length = Map.get(span, :length, 1)

    message = build_message(error)

    %{
      "range" => range(line, col, line, col + length),
      "severity" => 1,  # Error
      "source" => "vaisto",
      "message" => message
    }
  end

  defp build_message(%Vaisto.Error{} = error) do
    msg = error.message

    msg = if error.expected && error.actual do
      "#{msg}: expected `#{Vaisto.Error.format_type(error.expected)}`, found `#{Vaisto.Error.format_type(error.actual)}`"
    else
      msg
    end

    msg = if error.note, do: "#{msg}\n#{error.note}", else: msg
    msg = if error.hint, do: "#{msg}\nhint: #{error.hint}", else: msg

    msg
  end

  defp range(start_line, start_col, end_line, end_col) do
    %{
      "start" => %{"line" => start_line, "character" => start_col},
      "end" => %{"line" => end_line, "character" => end_col}
    }
  end

  # ============================================================================
  # JSON helpers (simple implementation for portability)
  # ============================================================================

  defp json_decode(string) do
    try do
      # Try to use Jason if available, otherwise use Code.eval_string
      # (This is a simple approach - in production, use Jason)
      if Code.ensure_loaded?(Jason) do
        Jason.decode(string)
      else
        # Simple JSON parsing via Erlang
        case :json.decode(string) do
          {:ok, value, _rest} -> {:ok, value}
          value when is_map(value) -> {:ok, value}
          value when is_list(value) -> {:ok, value}
          other -> {:ok, other}
        end
      end
    rescue
      e -> {:error, e}
    end
  end

  defp json_encode(term) do
    if Code.ensure_loaded?(Jason) do
      Jason.encode!(term)
    else
      # Use Erlang's built-in JSON encoder (OTP 27+)
      :json.encode(term) |> IO.iodata_to_binary()
    end
  end
end
