defmodule Vaisto.Std.NativeIOJsonTest do
  use ExUnit.Case, async: true
  import ExUnit.CaptureIO
  alias Vaisto.{Compilation, Elab, Parser}
  alias Vaisto.Liquid.Lint

  defp compile(file, suffix) do
    source = File.read!("std/prelude.va") <> "\n" <> File.read!(file) <> "\n" <> suffix
    name = :"NativeIOJson#{System.unique_integer([:positive])}"
    assert {:ok, core} = source |> Parser.parse() |> Elab.module(name: name)
    assert :ok = Lint.check_module(core)
    assert {:ok, ^name, _} = Compilation.compile(source, name, format_errors: false)

    on_exit(fn ->
      :code.purge(name)
      :code.delete(name)
    end)

    name
  end

  test "real JSON library checks encoding results and uses proper keyword options" do
    name =
      compile("std/Json.va", ~S"""
      (defn main [] :bool
        (match (decode "{\"answer\":42}")
          [(Ok raw) (match (decode (Map :string :int) raw)
            [(Core.Ok value) (== (map-get value "answer") 42)]
            [(Core.Err _) false])]
          [(Err _) false]))
      """)

    assert apply(name, :main, [])
    assert {:Ok, json} = apply(name, :encode, [%{"answer" => 42}])
    assert Jason.decode!(json) == %{"answer" => 42}
    assert {:Ok, pretty} = apply(name, :"encode-pretty", [%{"answer" => 42}])
    assert pretty =~ "\n"
    assert Jason.decode!(pretty) == %{"answer" => 42}
    assert apply(name, :"decode-atoms", [json]) == {:Ok, %{answer: 42}}
    assert {:Err, %Jason.DecodeError{}} = apply(name, :decode, ["not json"])
  end

  test "real IO library preserves a debugged value's type and checks file results" do
    name = compile("std/IO.va", "(defn main [] :int (debug \"answer\" 42))")

    output =
      capture_io(fn ->
        assert apply(name, :main, []) == 42
        assert apply(name, :printf, ["~p~n", [42]]) == :ok
      end)

    assert output == "answer: 42\n42\n"
    path = Path.join(System.tmp_dir!(), "native-io-#{System.unique_integer([:positive])}")
    on_exit(fn -> File.rm(path) end)
    assert apply(name, :"write-file", [path, "typed"]) == {:Ok, :ok}
    assert apply(name, :"read-file", [path]) == {:Ok, "typed"}
    assert apply(name, :"read-file", [path <> ".absent"]) == {:Err, :enoent}
  end
end
