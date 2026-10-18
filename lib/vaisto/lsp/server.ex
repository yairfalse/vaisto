defmodule Vaisto.LSP.Server do
  @moduledoc """
  Language Server Protocol implementation for Vaisto.

  Provides IDE features:
  - Diagnostics (type errors, parse errors)
  - Hover (type information)
  - Go to definition
  - Document symbols (outline)

  ## Running the Server

      # Start on stdio (default)
      Vaisto.LSP.Server.start()

      # Or via CLI
      vaistoc lsp
  """

  use GenServer

  alias Vaisto.LSP.{Protocol, Handler}

  defstruct [
    :input,
    :output,
    documents: %{},      # uri => source text
    diagnostics: %{},    # uri => [diagnostic]
    workspace_root: nil
  ]

  # ============================================================================
  # Client API
  # ============================================================================

  def start(opts \\ []) do
    input = Keyword.get(opts, :input, :stdio)
    output = Keyword.get(opts, :output, :stdio)
    GenServer.start(__MODULE__, {input, output}, name: __MODULE__)
  end

  def stop do
    GenServer.stop(__MODULE__)
  end

  # ============================================================================
  # GenServer Callbacks
  # ============================================================================

  @impl true
  def init({input, output}) do
    state = %__MODULE__{input: input, output: output}

    # Start reading from input
    if input == :stdio do
      # LSP Content-Length counts UTF-8 bytes, not decoded characters.
      :ok = :io.setopts(:standard_io, encoding: :latin1)
      server = self()
      spawn_link(fn -> read_loop(server) end)
    end

    {:ok, state}
  end

  @impl true
  def handle_cast({:message, content}, state) do
    case Protocol.decode(content) do
      {:ok, request} ->
        {response, new_state} = Handler.handle(request, state)
        if response do
          send_response(response, state)
        end
        {:noreply, new_state}

      {:error, reason} ->
        IO.puts(:stderr, "Failed to decode message: #{inspect(reason)}")
        {:noreply, state}
    end
  end

  @impl true
  def handle_info({:stdin, data}, state) do
    # Handle incoming data from stdin
    GenServer.cast(self(), {:message, data})
    {:noreply, state}
  end

  # ============================================================================
  # Internal Functions
  # ============================================================================

  defp read_loop(server) do
    case read_message() do
      {:ok, content} ->
        GenServer.cast(server, {:message, content})
        read_loop(server)

      :eof ->
        :ok

      {:error, reason} ->
        IO.puts(:stderr, "Read error: #{inspect(reason)}")
    end
  end

  # Read LSP message from stdio
  # Format: Content-Length: <length>\r\n\r\n<content>
  defp read_message do
    with {:ok, header} <- read_header(),
         {:ok, length} <- parse_content_length(header),
         {:ok, content} <- read_content(length) do
      {:ok, content}
    end
  end

  defp read_header(acc \\ "") do
    case IO.read(:stdio, :line) do
      :eof -> :eof
      {:error, reason} -> {:error, reason}
      line ->
        # Check for empty line (signals end of headers)
        trimmed = String.trim(line)
        if trimmed == "" and acc != "" do
          # Got blank line after headers - we're done
          {:ok, acc}
        else
          read_header(acc <> line)
        end
    end
  end

  defp parse_content_length(header) do
    case Regex.run(~r/Content-Length:\s*(\d+)/i, header) do
      [_, length] -> {:ok, String.to_integer(length)}
      nil -> {:error, :no_content_length}
    end
  end

  defp read_content(length) do
    case IO.binread(:stdio, length) do
      :eof -> :eof
      {:error, reason} -> {:error, reason}
      content when is_binary(content) and byte_size(content) == length -> {:ok, content}
      content when is_binary(content) -> {:error, :truncated_message}
    end
  end

  defp send_response(response, state) do
    json = Protocol.encode(response)
    header = "Content-Length: #{byte_size(json)}\r\n\r\n"

    case state.output do
      :stdio ->
        IO.binwrite(:stdio, header <> json)
        # Flush is important for LSP clients to receive the response
        :ok
      other ->
        send(other, {:lsp_response, header <> json})
    end
  end
end
