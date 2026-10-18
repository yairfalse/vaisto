defmodule Vaisto.Std.NativeFileTest do
  use ExUnit.Case, async: true
  alias Vaisto.{Compilation, Elab, Parser}
  alias Vaisto.Liquid.Lint

  test "real File library checks foreign results and converts Unicode filenames" do
    source = File.read!("std/prelude.va") <> "\n" <> File.read!("std/File.va")
    name = :"NativeFile#{System.unique_integer([:positive])}"
    dir = Path.join(System.tmp_dir!(), "native-file-#{System.unique_integer([:positive])}")

    on_exit(fn ->
      :code.purge(name)
      :code.delete(name)
      File.rm_rf!(dir)
    end)

    assert {:ok, core} = source |> Parser.parse() |> Elab.module(name: name)
    assert :ok = Lint.check_module(core)
    assert {:ok, ^name, _} = Compilation.compile(source, name, format_errors: false)
    nested = Path.join(dir, "nested")
    assert apply(name, :"mkdir-p", [nested]) == {:Ok, :ok}
    assert apply(name, :"mkdir-p", [nested]) == {:Ok, :ok}
    path = Path.join(nested, "λ.va")
    assert apply(name, :"write-file", [path, "one\ntwo"]) == {:Ok, :ok}
    assert apply(name, :"read-file", [path]) == {:Ok, "one\ntwo"}
    assert apply(name, :"read-lines", [path]) == {:Ok, ["one", "two"]}
    assert apply(name, :"list-dir", [nested]) == {:Ok, ["λ.va"]}
    assert apply(name, :cwd, []) == {:Ok, File.cwd!()}
    assert apply(name, :"read-file", [path <> ".absent"]) == {:Err, :enoent}
    assert apply(name, :delete, [path]) == {:Ok, :ok}
  end
end
