defmodule Vaisto.Build.IntegrationTest do
  use ExUnit.Case
  alias Vaisto.Build.Compiler
  alias Vaisto.Build.DependencyResolver

  @moduletag :build_integration

  setup do
    # Create a temp directory for tests
    tmp_dir = System.tmp_dir!()
    test_dir = Path.join(tmp_dir, "vaisto_build_test_#{:rand.uniform(100000)}")
    src_dir = Path.join(test_dir, "src")
    build_dir = Path.join(test_dir, "build")
    File.mkdir_p!(src_dir)
    File.mkdir_p!(build_dir)

    on_exit(fn -> File.rm_rf!(test_dir) end)

    {:ok, test_dir: test_dir, src_dir: src_dir, build_dir: build_dir}
  end

  describe "single file compilation" do
    test "compiles simple file", %{src_dir: src_dir, build_dir: build_dir} do
      source = """
      (ns Simple)
      (defn add [x :int y :int] :int (+ x y))
      """
      path = Path.join(src_dir, "Simple.va")
      File.write!(path, source)

      assert {:ok, result} = Compiler.compile(path, %{}, output_dir: build_dir)
      assert result.module == :"Elixir.Simple"
      assert File.exists?(Path.join(build_dir, "Elixir.Simple.beam"))
    end

    test "compiles file with type annotations", %{src_dir: src_dir, build_dir: build_dir} do
      source = """
      (ns TypedModule)
      (defn double [x :int] :int (* x 2))
      (defn main [] :int (double 21))
      """
      path = Path.join(src_dir, "TypedModule.va")
      File.write!(path, source)

      assert {:ok, result} = Compiler.compile(path, %{}, output_dir: build_dir)
      assert result.module == :"Elixir.TypedModule"
    end

    test "compiles file with ADTs", %{src_dir: src_dir, build_dir: build_dir} do
      source = """
      (ns WithADT)
      (deftype Result (Ok v) (Err e))
      (defn wrap [x] (Ok x))
      """
      path = Path.join(src_dir, "WithADT.va")
      File.write!(path, source)

      assert {:ok, result} = Compiler.compile(path, %{}, output_dir: build_dir)
      assert result.module == :"Elixir.WithADT"
    end

    test "returns error for type errors", %{src_dir: src_dir, build_dir: build_dir} do
      source = """
      (ns BadTypes)
      (defn add [x :int y :int] :int (+ x "not an int"))
      """
      path = Path.join(src_dir, "BadTypes.va")
      File.write!(path, source)

      assert {:error, _reason} = Compiler.compile(path, %{}, output_dir: build_dir)
    end

    test "raises error for parse errors", %{src_dir: src_dir, build_dir: build_dir} do
      source = "(defn incomplete [x"
      path = Path.join(src_dir, "BadParse.va")
      File.write!(path, source)

      # Parser raises RuntimeError on parse failure
      assert_raise RuntimeError, fn ->
        Compiler.compile(path, %{}, output_dir: build_dir)
      end
    end
  end

  describe "multi-file builds" do
    test "compiles files in dependency order", %{src_dir: src_dir} do
      # Create module A (no deps)
      File.write!(Path.join(src_dir, "A.va"), """
      (ns A)
      (defn base [] :int 1)
      """)

      # Create module B (depends on A)
      File.write!(Path.join(src_dir, "B.va"), """
      (ns B)
      (import A)
      (defn double-base [] :int (* 2 (A/base)))
      """)

      # Create module C (depends on B)
      File.write!(Path.join(src_dir, "C.va"), """
      (ns C)
      (import B)
      (defn main [] :int (B/double-base))
      """)

      # Build dependency graph
      files = [
        Path.join(src_dir, "A.va"),
        Path.join(src_dir, "B.va"),
        Path.join(src_dir, "C.va")
      ]

      graph = build_dependency_graph(files)

      assert {:ok, order} = DependencyResolver.topological_sort(graph)
      modules = Enum.map(order, & &1.module)

      # A should come before B, B should come before C
      a_idx = Enum.find_index(modules, &(&1 == :"Elixir.A"))
      b_idx = Enum.find_index(modules, &(&1 == :"Elixir.B"))
      c_idx = Enum.find_index(modules, &(&1 == :"Elixir.C"))

      assert a_idx < b_idx
      assert b_idx < c_idx
    end

    test "detects circular dependencies" do
      # Test with a synthetic graph rather than parsing files
      # since parsing may fail on incomplete code
      graph = %{
        :"Elixir.Circular1" => %{
          file: "src/Circular1.va",
          imports: [{:"Elixir.Circular2", nil}]
        },
        :"Elixir.Circular2" => %{
          file: "src/Circular2.va",
          imports: [{:"Elixir.Circular1", nil}]
        }
      }

      assert {:error, :circular_dependency} = DependencyResolver.topological_sort(graph)
    end
  end

  describe "module naming" do
    test "infers module name from namespace", %{src_dir: src_dir, build_dir: build_dir} do
      source = """
      (ns MyModule)
      (defn test [] :int 42)
      """
      # File name should match namespace
      path = Path.join(src_dir, "MyModule.va")
      File.write!(path, source)

      assert {:ok, result} = Compiler.compile(path, %{}, output_dir: build_dir)
      assert result.module == :"Elixir.MyModule"
    end

    test "uses file name when no namespace", %{src_dir: src_dir, build_dir: build_dir} do
      source = """
      (defn test [] :int 42)
      """
      path = Path.join(src_dir, "NoNs.va")
      File.write!(path, source)

      assert {:ok, result} = Compiler.compile(path, %{}, output_dir: build_dir)
      # Should use file name as module name
      assert result.module == :"Elixir.NoNs"
    end
  end

  describe "prelude support" do
    test "compiles with prelude", %{src_dir: src_dir, build_dir: build_dir} do
      prelude = File.read!("std/prelude.va")

      source = """
      (ns WithPrelude)
      (defn wrap-ok [x] (Ok x))
      (defn main [] (wrap-ok 42))
      """
      path = Path.join(src_dir, "WithPrelude.va")
      File.write!(path, source)

      assert {:ok, result} = Compiler.compile(path, %{}, output_dir: build_dir, prelude: prelude)
      assert result.module == :"Elixir.WithPrelude"
    end
  end

  # Helper to build a dependency graph from source files
  defp build_dependency_graph(files) do
    Enum.reduce(files, %{}, fn file, acc ->
      source = File.read!(file)
      ast = Vaisto.Parser.parse(source, file: file)

      {module_name, imports} = extract_module_info(ast, file)

      Map.put(acc, module_name, %{
        file: file,
        imports: Enum.map(imports, &{&1, nil})
      })
    end)
  end

  defp extract_module_info(ast, file) when is_list(ast) do
    ns = Enum.find_value(ast, fn
      {:ns, name, _loc} -> name
      {:ns, name} -> name
      _ -> nil
    end)

    module_name = if ns do
      :"Elixir.#{ns}"
    else
      basename = Path.basename(file, ".va")
      :"Elixir.#{basename}"
    end

    imports = Enum.flat_map(ast, fn
      {:import, mod, _loc} -> [:"Elixir.#{mod}"]
      {:import, mod} -> [:"Elixir.#{mod}"]
      _ -> []
    end)

    {module_name, imports}
  end

  defp extract_module_info(ast, file) do
    extract_module_info([ast], file)
  end
end
