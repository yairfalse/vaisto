defmodule Vaisto.NativeCLITest do
  use ExUnit.Case, async: true

  defp cli(args, cwd) do
    paths =
      for module <- [Vaisto.CLI, Jason, Toml],
          do: module |> :code.which() |> List.to_string() |> Path.dirname()

    path_args = Enum.flat_map(paths, &["-pa", &1])

    System.cmd(
      System.find_executable("elixir"),
      path_args ++ ["-e", "Vaisto.CLI.main(System.argv())", "--" | args],
      cd: cwd,
      stderr_to_stdout: true
    )
  end

  setup do
    dir = Path.join(System.tmp_dir!(), "vaisto-native-cli-#{System.unique_integer([:positive])}")
    File.mkdir_p!(dir)
    on_exit(fn -> File.rm_rf!(dir) end)
    %{dir: dir}
  end

  test "the real CLI evaluates native code and reports normalized crashes", %{dir: dir} do
    assert {"42\n", 0} = cli(["--eval", "(+ 40 2)"], dir)
    assert {output, 1} = cli(["--eval", "(div 1 0)"], dir)
    assert output =~ "runtime error"
    assert output =~ "badarith"
    refute output =~ "** (throw)"
  end

  test "unsigned code and removed backend flags fail, not fall back", %{dir: dir} do
    assert {output, 1} = cli(["--eval", "(defn f [x] (+ x 1)) (f 41)"], dir)
    assert output =~ "needs a signature"
    assert {output, 1} = cli(["--backend", "elixir", "--eval", "42"], dir)
    assert output =~ "unknown option: --backend"
  end

  test "file compilation writes real BEAM and preserves output on rejection", %{dir: dir} do
    source = Path.join(dir, "main.va")
    output = Path.join(dir, "main.beam")
    File.write!(source, "(defn main [] :int 42)")
    assert {_, 0} = cli([source, "-o", output], dir)
    assert {:ok, <<"FOR1", _::binary>> = beam} = File.read(output)
    assert {:ok, {_, _}} = :beam_lib.chunks(beam, [:exports])
    File.write!(source, "(+ 1 true)")
    assert {_, 1} = cli([source, "-o", output], dir)
    assert File.read!(output) == beam
  end

  test "init and manifest build produce native BEAM and canonical interfaces", %{dir: dir} do
    assert {_, 0} = cli(["init", "demo-lib"], dir)
    project = Path.join(dir, "demo-lib")
    assert {output, 0} = cli(["build"], project)
    assert output =~ "Built 1 module"
    beams = Path.wildcard(Path.join(project, "**/*.beam"))
    interfaces = Path.wildcard(Path.join(project, "**/*.vsi"))
    assert length(beams) == 1
    assert length(interfaces) == 1
    assert {:ok, bytes} = File.read(hd(interfaces))
    assert {:ok, tree} = Vaisto.Liquid.Canonical.decode(bytes, atoms: :create)
    assert {:ok, _} = Vaisto.Interface.validate_core(tree)
  end

  test "real stdio uses byte lengths for Unicode documents and preserves the following frames" do
    paths =
      for module <- [Vaisto.CLI, Jason, Toml],
          do: module |> :code.which() |> List.to_string() |> Path.dirname()

    args = Enum.flat_map(paths, &["-pa", &1]) ++ ["-e", "Vaisto.CLI.main([\"lsp\"])"]

    port =
      Port.open({:spawn_executable, System.find_executable("elixir")}, [
        :binary,
        :exit_status,
        args: args
      ])

    uri = "file:///tmp/vaisto-native-stdio.va"

    messages = [
      %{jsonrpc: "2.0", id: 1, method: "initialize", params: %{}},
      %{
        jsonrpc: "2.0",
        method: "textDocument/didOpen",
        params: %{
          textDocument: %{
            uri: uri,
            version: 1,
            text: "\"λ🙂\"\n(defn add [x :int y :int] :int (+ x y))"
          }
        }
      },
      %{
        jsonrpc: "2.0",
        id: 2,
        method: "textDocument/hover",
        params: %{textDocument: %{uri: uri}, position: %{line: 1, character: 7}}
      },
      %{jsonrpc: "2.0", id: 3, method: "shutdown", params: %{}},
      %{jsonrpc: "2.0", method: "exit", params: %{}}
    ]

    try do
      bytes =
        Enum.map_join(messages, fn message ->
          json = Jason.encode!(message)
          "Content-Length: #{byte_size(json)}\r\n\r\n" <> json
        end)

      assert Port.command(port, bytes)
      responses = port_output(port, "") |> decode_frames([])
      assert Enum.any?(responses, &(&1["id"] == 1))
      assert Enum.any?(responses, &(&1["id"] == 2 and inspect(&1["result"]) =~ "Int"))
      assert Enum.any?(responses, &(&1["id"] == 3 and &1["result"] == nil))

      assert Enum.any?(
               responses,
               &(&1["method"] == "textDocument/publishDiagnostics" and
                   &1["params"]["diagnostics"] == [])
             )
    after
      # A failed transport test must not leave its own CLI waiting on stdin.
      if info = Port.info(port, :os_pid),
        do:
          System.cmd("kill", ["-TERM", Integer.to_string(elem(info, 1))], stderr_to_stdout: true)

      if Port.info(port), do: Port.close(port)
    end
  end

  defp port_output(port, bytes) do
    receive do
      {^port, {:data, more}} ->
        port_output(port, bytes <> more)

      {^port, {:exit_status, status}} ->
        assert status == 0, bytes
        bytes
    after
      5_000 -> flunk("CLI stdio timed out: #{inspect(bytes)}")
    end
  end

  defp decode_frames("", responses), do: Enum.reverse(responses)

  defp decode_frames(bytes, responses) do
    assert {ending, 4} = :binary.match(bytes, "\r\n\r\n"),
           "non-protocol stdout: #{inspect(bytes)}"

    header = binary_part(bytes, 0, ending)
    assert [_, length] = Regex.run(~r/^Content-Length: (\d+)$/, header)
    length = String.to_integer(length)
    payload = binary_part(bytes, ending + 4, length)
    rest = binary_part(bytes, ending + 4 + length, byte_size(bytes) - ending - 4 - length)
    decode_frames(rest, [Jason.decode!(payload) | responses])
  end
end
