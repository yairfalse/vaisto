defmodule Vaisto.Std.NativeBinaryTest do
  use ExUnit.Case, async: true
  alias Vaisto.{Compilation, Elab, Parser}
  alias Vaisto.Liquid.Lint

  test "real Binary library generates and loads BEAM and round-trips bytes" do
    source = File.read!("std/prelude.va") <> "\n" <> File.read!("std/Binary.va")
    name = :"NativeBinary#{System.unique_integer([:positive])}"
    generated = :"GeneratedBinary#{System.unique_integer([:positive])}"

    path =
      Path.join(
        System.tmp_dir!(),
        "vaisto-native-binary-#{System.unique_integer([:positive])}.beam"
      )

    on_exit(fn ->
      for mod <- [name, generated] do
        :code.purge(mod)
        :code.delete(mod)
      end

      File.rm(path)
    end)

    assert {:ok, core} = source |> Parser.parse() |> Elab.module(name: name)
    assert :ok = Lint.check_module(core)
    assert {:ok, ^name, _} = Compilation.compile_liquid(source, name, format_errors: false)
    assert apply(name, :"from-bytes", [[0, 255, 128]]) == <<0, 255, 128>>
    assert apply(name, :"to-bytes", [<<0, 255, 128>>]) == [0, 255, 128]
    assert apply(name, :concat, [[<<0, 255>>, <<128>>]]) == <<0, 255, 128>>
    assert apply(name, :slice, [<<0, 255, 128>>, 1, 2]) == <<255, 128>>
    assert apply(name, :"byte-at", [<<0, 255, 128>>, 2]) == 128
    term = %{answer: {:ok, [1, 2, 3]}}
    assert apply(name, :deserialize, [apply(name, :serialize, [term])]) == term

    forms = [
      {:attribute, 1, :module, generated},
      {:attribute, 2, :export, [main: 0]},
      {:function, 3, :main, 0, [{:clause, 3, [], [], [{:integer, 3, 42}]}]}
    ]

    assert {:Ok, %{module: ^generated, bytecode: beam}} = apply(name, :"compile-forms", [forms])
    assert is_binary(beam)
    assert {:Ok, ^generated} = apply(name, :"load-module", [generated, "native.beam", beam])
    assert apply(generated, :main, []) == 42
    assert apply(name, :"write-beam", [path, beam]) == {:Ok, :ok}
    assert apply(name, :"read-beam", [path]) == {:Ok, beam}
    assert {:Err, :enoent} = apply(name, :"read-beam", [path <> ".absent"])
    assert {:Err, _} = apply(name, :"compile-forms", [[]])
    assert {:Err, _} = apply(name, :"load-module", [generated, "bad.beam", "not a beam"])
  end
end
