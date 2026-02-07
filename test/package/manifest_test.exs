defmodule Vaisto.Package.ManifestTest do
  use ExUnit.Case, async: true

  alias Vaisto.Package.Manifest

  @tmp_base "test/tmp/manifest_test"

  setup do
    dir = Path.join(@tmp_base, "#{System.unique_integer([:positive])}")
    File.mkdir_p!(dir)
    on_exit(fn -> File.rm_rf!(@tmp_base) end)
    {:ok, dir: dir}
  end

  describe "load/1" do
    test "returns :none when no manifest exists", %{dir: dir} do
      assert Manifest.load(dir) == :none
    end

    test "parses minimal manifest", %{dir: dir} do
      write_manifest(dir, """
      [package]
      name = "my-app"
      """)

      File.mkdir_p!(Path.join(dir, "src"))

      assert {:ok, manifest} = Manifest.load(dir)
      assert manifest.name == "my-app"
      assert manifest.version == "0.1.0"
      assert manifest.edition == "2026"
      assert manifest.source_dirs == ["src"]
      assert manifest.output_dir == "build"
      assert manifest.dependencies == %{}
    end

    test "parses full manifest", %{dir: dir} do
      write_manifest(dir, """
      [package]
      name = "http-server"
      version = "0.3.1"
      edition = "2026"
      description = "Type-safe HTTP server"
      license = "MIT"

      [dependencies]
      json = "~> 1.2"

      [build]
      source-dirs = ["lib"]
      output-dir = "out"
      """)

      File.mkdir_p!(Path.join(dir, "lib"))

      assert {:ok, manifest} = Manifest.load(dir)
      assert manifest.name == "http-server"
      assert manifest.version == "0.3.1"
      assert manifest.description == "Type-safe HTTP server"
      assert manifest.license == "MIT"
      assert manifest.dependencies == %{"json" => "~> 1.2"}
      assert manifest.source_dirs == ["lib"]
      assert manifest.output_dir == "out"
    end

    test "parses path dependencies", %{dir: dir} do
      write_manifest(dir, """
      [package]
      name = "my-app"

      [dependencies]
      json = { path = "../json" }
      tcp = { path = "../tcp" }
      """)

      File.mkdir_p!(Path.join(dir, "src"))

      assert {:ok, manifest} = Manifest.load(dir)
      assert manifest.dependencies == %{
               "json" => %{path: "../json"},
               "tcp" => %{path: "../tcp"}
             }
    end

    test "rejects missing name", %{dir: dir} do
      write_manifest(dir, """
      [package]
      version = "1.0.0"
      """)

      assert {:error, msg} = Manifest.load(dir)
      assert msg =~ "missing"
      assert msg =~ "name"
    end

    test "rejects invalid package name", %{dir: dir} do
      write_manifest(dir, """
      [package]
      name = "MyApp"
      """)

      assert {:error, msg} = Manifest.load(dir)
      assert msg =~ "kebab-case"
    end

    test "rejects invalid version", %{dir: dir} do
      write_manifest(dir, """
      [package]
      name = "my-app"
      version = "bad"
      """)

      File.mkdir_p!(Path.join(dir, "src"))

      assert {:error, msg} = Manifest.load(dir)
      assert msg =~ "semver"
    end

    test "rejects missing source directory", %{dir: dir} do
      write_manifest(dir, """
      [package]
      name = "my-app"

      [build]
      source-dirs = ["nonexistent"]
      """)

      assert {:error, msg} = Manifest.load(dir)
      assert msg =~ "nonexistent"
      assert msg =~ "does not exist"
    end

    test "reports invalid TOML", %{dir: dir} do
      write_manifest(dir, "not valid toml {{{")
      assert {:error, msg} = Manifest.load(dir)
      assert msg =~ "invalid vaisto.toml"
    end
  end

  describe "exists?/1" do
    test "returns false when no manifest", %{dir: dir} do
      refute Manifest.exists?(dir)
    end

    test "returns true when manifest exists", %{dir: dir} do
      write_manifest(dir, "[package]\nname = \"x\"")
      assert Manifest.exists?(dir)
    end
  end

  describe "generate/1" do
    test "generates minimal manifest" do
      content = Manifest.generate("my-app")
      assert content =~ ~s(name = "my-app")
      assert content =~ ~s(version = "0.1.0")
    end
  end

  defp write_manifest(dir, content) do
    File.write!(Path.join(dir, "vaisto.toml"), content)
  end
end
