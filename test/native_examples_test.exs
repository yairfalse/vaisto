defmodule Vaisto.NativeExamplesTest do
  use ExUnit.Case
  import ExUnit.CaptureIO
  alias Vaisto.{Build, Compilation}

  setup_all do
    dir =
      Path.join(System.tmp_dir!(), "vaisto-native-examples-#{System.unique_integer([:positive])}")

    File.mkdir_p!(dir)
    on_exit(fn -> File.rm_rf!(dir) end)
    %{dir: dir, prelude: File.read!("std/prelude.va")}
  end

  test "standalone examples check, execute, and preserve the intentionally invalid send", %{
    prelude: prelude
  } do
    for {file, expected} <- [{"math", 21}, {"maybe", 42}, {"tree", 42}] do
      assert {:ok, ^expected} =
               file |> then(&File.read!("examples/#{&1}.va")) |> Compilation.run(prelude: prelude)
    end

    for file <- ["counter", "typed_pid", "typed_pid_valid"] do
      assert {:ok, _} =
               Compilation.check_liquid(File.read!("examples/#{file}.va"), :Example,
                 prelude: prelude
               )
    end

    assert {:error, errors} =
             Compilation.check_liquid(File.read!("examples/typed_pid_invalid.va"), :InvalidSend,
               prelude: prelude,
               format_errors: false
             )

    assert Enum.any?(errors, &String.contains?(&1.message, "does not accept"))
  end

  test "the complete standard library builds canonical interfaces used by the file examples", %{
    dir: dir,
    prelude: prelude
  } do
    assert {:ok, libraries} = Build.build("std", output_dir: dir, prelude: prelude)
    assert length(libraries) == length(Path.wildcard("std/*.va")) - 1

    for result <- libraries do
      assert {:module, _} =
               :code.load_binary(
                 result.module,
                 String.to_charlist(result.beam),
                 File.read!(result.beam)
               )

      on_exit(fn ->
        :code.purge(result.module)
        :code.delete(result.module)
      end)
    end

    for {file, label} <- [{"LineCount", "Lines:"}, {"WordCount", "Words:"}] do
      path = "examples/#{file}.va"

      assert {:ok, result} =
               Build.Compiler.compile(path, %{},
                 output_dir: dir,
                 prelude: prelude,
                 search_paths: [dir],
                 source_roots: [{"examples/", ""}]
               )

      assert {:module, _} =
               :code.load_binary(
                 result.module,
                 String.to_charlist(result.beam),
                 File.read!(result.beam)
               )

      on_exit(fn ->
        :code.purge(result.module)
        :code.delete(result.module)
      end)

      output =
        capture_io(fn ->
          assert {:ok, _} = Vaisto.Liquid.Runtime.run(result.module, :main, [], handler: :native)
        end)

      assert output =~ label
    end
  end

  test "the module example builds and executes checked canonical imports", %{
    dir: dir,
    prelude: prelude
  } do
    assert {:ok, results} =
             Build.build("examples/modules",
               output_dir: dir,
               prelude: prelude,
               source_roots: [{"examples/modules/", ""}]
             )

    assert length(results) == 3

    for result <- results do
      assert {:module, _} =
               :code.load_binary(
                 result.module,
                 String.to_charlist(result.beam),
                 File.read!(result.beam)
               )

      on_exit(fn ->
        :code.purge(result.module)
        :code.delete(result.module)
      end)
    end

    main = Enum.find(results, &(&1.module == Main)).module
    assert {:ok, {:Ok, 4.0}} = Vaisto.Liquid.Runtime.run(main, :main, [], handler: :native)
  end
end
