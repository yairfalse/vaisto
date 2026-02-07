defmodule Vaisto.Package.IntegrationTest do
  use ExUnit.Case

  alias Vaisto.Package.{Manifest, Namespace}

  @tmp_base "test/tmp/pkg_integration"

  # Embed prelude for builds
  @prelude File.read!(Path.join(__DIR__, "../../std/prelude.va"))

  setup do
    dir = Path.join(@tmp_base, "#{System.unique_integer([:positive])}")
    File.mkdir_p!(dir)
    on_exit(fn -> File.rm_rf!(@tmp_base) end)
    {:ok, dir: dir}
  end

  describe "init + build workflow" do
    test "init creates runnable project structure", %{dir: dir} do
      project_dir = Path.join(dir, "my-app")

      # Simulate init
      prefix = Namespace.to_prefix("my-app")
      assert prefix == "MyApp"

      src_dir = Path.join(project_dir, "src")
      File.mkdir_p!(src_dir)
      File.write!(Path.join(project_dir, "vaisto.toml"), Manifest.generate("my-app"))

      File.write!(Path.join(src_dir, "MyApp.va"), """
      (ns MyApp)

      (defn hello [] :string
        "hello from my-app")
      """)

      # Verify manifest loads
      assert {:ok, manifest} = Manifest.load(project_dir)
      assert manifest.name == "my-app"

      # Build the project
      assert {:ok, results} = Vaisto.Build.build_project(project_dir, prelude: @prelude)
      assert length(results) == 1
      assert hd(results).module == :"Elixir.MyApp"
    end
  end

  describe "path dependencies" do
    test "project can import from path dependency", %{dir: dir} do
      # Create the dependency: string-utils
      dep_dir = Path.join(dir, "string-utils")
      dep_src = Path.join(dep_dir, "src")
      File.mkdir_p!(dep_src)

      File.write!(Path.join(dep_dir, "vaisto.toml"), Manifest.generate("string-utils"))

      File.write!(Path.join(dep_src, "StringUtils.va"), """
      (ns StringUtils)

      (defn greet [name :string] :string
        name)
      """)

      # Create the main project
      project_dir = Path.join(dir, "my-app")
      project_src = Path.join(project_dir, "src")
      File.mkdir_p!(project_src)

      File.write!(Path.join(project_dir, "vaisto.toml"), """
      [package]
      name = "my-app"

      [dependencies]
      string-utils = { path = "../string-utils" }
      """)

      File.write!(Path.join(project_src, "MyApp.va"), """
      (ns MyApp)

      (import StringUtils)

      (defn main [] :string
        (StringUtils/greet "world"))
      """)

      # Build should compile both packages
      assert {:ok, results} = Vaisto.Build.build_project(project_dir, prelude: @prelude)
      modules = Enum.map(results, & &1.module)
      assert :"Elixir.StringUtils" in modules
      assert :"Elixir.MyApp" in modules
    end

    test "dep without manifest uses directory name for namespace", %{dir: dir} do
      # Create a dependency without vaisto.toml (just source files)
      dep_dir = Path.join(dir, "utils")
      dep_src = Path.join(dep_dir, "src")
      File.mkdir_p!(dep_src)

      File.write!(Path.join(dep_src, "Utils.va"), """
      (ns Utils)

      (defn id [x :int] :int x)
      """)

      # Create main project
      project_dir = Path.join(dir, "my-app")
      project_src = Path.join(project_dir, "src")
      File.mkdir_p!(project_src)

      File.write!(Path.join(project_dir, "vaisto.toml"), """
      [package]
      name = "my-app"

      [dependencies]
      utils = { path = "../utils" }
      """)

      File.write!(Path.join(project_src, "MyApp.va"), """
      (ns MyApp)

      (import Utils)

      (defn main [] :int
        (Utils/id 42))
      """)

      assert {:ok, results} = Vaisto.Build.build_project(project_dir, prelude: @prelude)
      assert length(results) == 2
    end
  end

  describe "backward compatibility" do
    test "build/2 still works without manifest", %{dir: dir} do
      src_dir = Path.join(dir, "src")
      File.mkdir_p!(src_dir)

      File.write!(Path.join(src_dir, "Foo.va"), """
      (ns Foo)

      (defn bar [] :int 42)
      """)

      # Direct build without manifest — the old way
      assert {:ok, results} = Vaisto.Build.build(dir, prelude: @prelude, output_dir: dir)
      assert length(results) == 1
      assert hd(results).module == :"Elixir.Foo"
    end
  end

  describe "namespace enforcement" do
    test "validate_module rejects wrong namespace" do
      assert {:error, _} = Namespace.validate_module("Wrong.Module", "json")
    end

    test "validate_module accepts correct namespace" do
      assert :ok = Namespace.validate_module("Json.Parser", "json")
      assert :ok = Namespace.validate_module("Json", "json")
    end
  end
end
