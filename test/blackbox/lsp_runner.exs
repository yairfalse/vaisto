# LSP Black-Box Test Runner
# Tests the Vaisto LSP server at the JSON-RPC protocol boundary.
# Talks directly to the GenServer; native_cli_test.exs also checks real stdio.

defmodule LSPRunner do
  @green "\e[0;32m"
  @red "\e[0;31m"
  @yellow "\e[0;33m"
  @bold "\e[1m"
  @reset "\e[0m"

  def run do
    dataset = load_dataset()
    IO.puts("#{@bold}Vaisto LSP Black-Box Tests#{@reset}")
    IO.puts("#{@bold}#{String.duplicate("─", 56)}#{@reset}")
    IO.puts("Test cases: #{length(dataset)}\n")

    results = Enum.map(dataset, fn tc ->
      run_test_case(tc)
    end)

    pass = Enum.count(results, &(&1 == :pass))
    fail = Enum.count(results, &(&1 == :fail))
    bug = Enum.count(results, &(&1 == :bug))

    IO.puts("\n#{@bold}#{String.duplicate("─", 56)}#{@reset}")
    IO.puts("  #{@green}PASS:#{@reset} #{pass}    #{@red}FAIL:#{@reset} #{fail}    #{@red}BUGS:#{@reset} #{bug}    TOTAL: #{length(results)}")
    IO.puts("#{@bold}#{String.duplicate("─", 56)}#{@reset}")

    if fail > 0 or bug > 0, do: System.halt(1)
  end

  defp load_dataset do
    path = Path.join(__DIR__, "lsp_dataset.json")
    data = File.read!(path) |> Jason.decode!()
    data["test_cases"]
  end

  defp run_test_case(tc) do
    id = tc["id"]
    name = tc["name"]

    try do
      # Start a fresh server for each test
      test_pid = self()
      {:ok, server} = Vaisto.LSP.Server.start(input: :test, output: test_pid)
      Process.unlink(server)

      # Build messages
      messages = build_messages(tc)

      # Send messages and collect responses
      start_time = System.monotonic_time(:millisecond)
      responses = send_and_collect(server, messages)
      elapsed = System.monotonic_time(:millisecond) - start_time

      # Verify expectations
      result = verify(tc, responses, elapsed)

      # Cleanup
      try do
        GenServer.stop(server, :normal, 500)
      catch
        :exit, _ -> :ok
      end

      status = if result == :pass, do: "#{@green}PASS#{@reset}", else: "#{@red}FAIL#{@reset}"
      IO.puts("  #{status}  #{id} #{name}#{if result != :pass, do: " — #{result}", else: ""}")

      if result == :pass, do: :pass, else: :fail
    rescue
      e ->
        msg = Exception.message(e)
        IO.puts("  #{@red}BUG #{@reset}  #{id} #{name} — CRASH: #{String.slice(msg, 0, 80)}")
        :bug
    catch
      :exit, reason ->
        IO.puts("  #{@red}BUG #{@reset}  #{id} #{name} — EXIT: #{inspect(reason) |> String.slice(0, 80)}")
        :bug
    end
  end

  defp build_messages(tc) do
    cond do
      tc["messages"] ->
        Enum.map(tc["messages"], &build_json_rpc/1)

      tc["messages_generate"] ->
        gen = tc["messages_generate"]
        prefix = Enum.map(gen["prefix"], &build_json_rpc/1)
        template = gen["template"]
        repeat = gen["repeat"]

        generated = for i <- 2..(repeat + 1) do
          # Replace INDEX in the template
          json = Jason.encode!(template)
          |> String.replace("INDEX", to_string(i))
          |> Jason.decode!()
          build_json_rpc(json)
        end

        prefix ++ generated

      true ->
        []
    end
  end

  defp build_json_rpc(msg) do
    text = msg["text"]

    text = cond do
      is_binary(text) and String.starts_with?(text, "GENERATE_REPEATED:") ->
        [_, rest] = String.split(text, "GENERATE_REPEATED:", parts: 2)
        [pattern, count_str] = String.split(String.trim(rest), " :", parts: 2)
        count = String.to_integer(String.trim(count_str))
        String.duplicate(pattern, count)
      true ->
        text
    end

    # Rebuild with potentially modified text
    msg = if text != msg["text"] do
      put_in(msg, ["params", "textDocument", "text"], text)
    else
      msg
    end

    base = %{"jsonrpc" => "2.0", "method" => msg["method"], "params" => msg["params"] || %{}}
    if msg["id"], do: Map.put(base, "id", msg["id"]), else: base
  end

  defp send_and_collect(server, messages) do
    responses = []

    Enum.reduce(messages, responses, fn msg, acc ->
      json = Jason.encode!(msg)
      GenServer.cast(server, {:message, json})

      # Small delay to let GenServer process
      Process.sleep(50)

      # Collect any responses
      acc ++ drain_responses()
    end)
  end

  defp drain_responses do
    receive do
      {:lsp_response, data} ->
        # Parse the JSON-RPC response from the Content-Length framed data
        case parse_lsp_response(data) do
          {:ok, parsed} -> [parsed | drain_responses()]
          _ -> drain_responses()
        end
    after
      100 -> []
    end
  end

  defp parse_lsp_response(data) do
    # Strip Content-Length header
    case String.split(data, "\r\n\r\n", parts: 2) do
      [_header, json] -> Jason.decode(json)
      _ -> Jason.decode(data)
    end
  end

  defp verify(tc, responses, elapsed) do
    cond do
      # Check for crashes
      tc["expect_no_crash"] && responses == :crashed ->
        "server crashed"

      # Check max response time
      tc["max_response_ms"] && elapsed > tc["max_response_ms"] ->
        "too slow: #{elapsed}ms > #{tc["max_response_ms"]}ms"

      # Check specific response ID
      tc["expect_response_id"] ->
        verify_response(tc, responses)

      # Check notification
      tc["expect_notification"] ->
        verify_notification(tc, responses)

      # No specific expectation beyond not crashing
      tc["expect_no_crash"] ->
        :pass

      true ->
        :pass
    end
  end

  defp verify_response(tc, responses) do
    target_id = tc["expect_response_id"]
    resp = Enum.find(responses, fn r -> r["id"] == target_id end)

    cond do
      resp == nil ->
        "no response for id #{target_id}"

      tc["expect_error_code"] ->
        error = resp["error"]
        if error && error["code"] == tc["expect_error_code"] do
          :pass
        else
          "expected error #{tc["expect_error_code"]}, got #{inspect(error)}"
        end

      tc["expect_result"] == nil && Map.has_key?(tc, "expect_result") ->
        if resp["result"] == nil, do: :pass, else: "expected null result"

      tc["expect_result_keys"] ->
        result = resp["result"]
        if result && Enum.all?(tc["expect_result_keys"], &Map.has_key?(result, &1)) do
          :pass
        else
          missing = Enum.reject(tc["expect_result_keys"], &Map.has_key?(result || %{}, &1))
          "missing keys: #{inspect(missing)}"
        end

      tc["expect_capabilities"] ->
        caps = get_in(resp, ["result", "capabilities"])
        if caps && Enum.all?(tc["expect_capabilities"], &Map.has_key?(caps, &1)) do
          :pass
        else
          missing = Enum.reject(tc["expect_capabilities"], &Map.has_key?(caps || %{}, &1))
          "missing capabilities: #{inspect(missing)}"
        end

      tc["expect_capability"] ->
        caps = get_in(resp, ["result", "capabilities"])
        if caps && Map.has_key?(caps, tc["expect_capability"]) do
          :pass
        else
          "missing capability: #{tc["expect_capability"]}"
        end

      tc["expect_result_not_null"] ->
        if resp["result"] != nil, do: :pass, else: "result was null"

      tc["expect_result_is_list"] ->
        result = resp["result"]
        cond do
          not is_list(result) -> "expected list, got #{inspect(result)}"
          tc["expect_result_min_length"] && length(result) < tc["expect_result_min_length"] ->
            "expected >= #{tc["expect_result_min_length"]} items, got #{length(result)}"
          tc["expect_result_length"] != nil && length(result) != tc["expect_result_length"] ->
            "expected #{tc["expect_result_length"]} items, got #{length(result)}"
          true -> :pass
        end

      true ->
        # Just check we got a response
        :pass
    end
  end

  defp verify_notification(tc, responses) do
    method = tc["expect_notification"]

    # Use the LAST matching notification (most recent state after all messages)
    matching = Enum.filter(responses, fn r ->
      r["method"] == method || get_in(r, ["params", "diagnostics"]) != nil
    end)

    # Filter by URI if specified
    matching = if tc["expect_notification_uri"] do
      uri = tc["expect_notification_uri"]
      Enum.filter(matching, fn r -> get_in(r, ["params", "uri"]) == uri end)
    else
      matching
    end

    notif = List.last(matching)

    if notif do
      verify_diagnostics(tc, notif)
    else
      "no #{method} notification received"
    end
  end

  defp verify_diagnostics(tc, notif) do
    diagnostics = get_in(notif, ["params", "diagnostics"]) || []

    cond do
      tc["expect_diagnostics_count"] != nil && length(diagnostics) != tc["expect_diagnostics_count"] ->
        "expected #{tc["expect_diagnostics_count"]} diagnostics, got #{length(diagnostics)}"

      tc["expect_diagnostics_min"] && length(diagnostics) < tc["expect_diagnostics_min"] ->
        "expected >= #{tc["expect_diagnostics_min"]} diagnostics, got #{length(diagnostics)}"

      tc["expect_diagnostic_source"] ->
        if Enum.all?(diagnostics, &(&1["source"] == tc["expect_diagnostic_source"])) do
          :pass
        else
          "some diagnostics missing source '#{tc["expect_diagnostic_source"]}'"
        end

      tc["expect_valid_ranges"] ->
        valid = Enum.all?(diagnostics, fn d ->
          range = d["range"]
          range && range["start"]["line"] >= 0 && range["start"]["character"] >= 0
        end)
        if valid, do: :pass, else: "invalid range in diagnostics"

      true ->
        :pass
    end
  end
end

LSPRunner.run()
