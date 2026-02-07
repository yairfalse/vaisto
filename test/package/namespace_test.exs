defmodule Vaisto.Package.NamespaceTest do
  use ExUnit.Case, async: true

  alias Vaisto.Package.Namespace

  describe "to_prefix/1" do
    test "simple name" do
      assert Namespace.to_prefix("json") == "Json"
    end

    test "hyphenated name" do
      assert Namespace.to_prefix("json-parser") == "JsonParser"
    end

    test "multiple hyphens" do
      assert Namespace.to_prefix("http-server-utils") == "HttpServerUtils"
    end

    test "single char segments" do
      assert Namespace.to_prefix("a-b-c") == "ABC"
    end
  end

  describe "to_source_root/3" do
    test "default source dir" do
      assert Namespace.to_source_root("../json", "json") == [{"../json/src/", ""}]
    end

    test "custom source dirs" do
      assert Namespace.to_source_root("../tcp", "tcp", ["lib", "src"]) == [
               {"../tcp/lib/", ""},
               {"../tcp/src/", ""}
             ]
    end

    test "hyphenated package name" do
      assert Namespace.to_source_root("deps/json-parser", "json-parser") == [
               {"deps/json-parser/src/", ""}
             ]
    end
  end

  describe "validate_module/2" do
    test "exact match" do
      assert Namespace.validate_module("Json", "json") == :ok
    end

    test "prefixed match" do
      assert Namespace.validate_module("Json.Parser", "json") == :ok
      assert Namespace.validate_module("Json.Parser.Tokenizer", "json") == :ok
    end

    test "rejects wrong namespace" do
      assert {:error, msg} = Namespace.validate_module("Foo.Bar", "json")
      assert msg =~ "Foo.Bar"
      assert msg =~ "Json"
    end

    test "rejects partial prefix match" do
      assert {:error, _} = Namespace.validate_module("JsonExtra", "json")
    end

    test "strips Elixir. prefix" do
      assert Namespace.validate_module("Elixir.Json.Parser", "json") == :ok
    end

    test "hyphenated package" do
      assert Namespace.validate_module("JsonParser.Tokenizer", "json-parser") == :ok
      assert {:error, _} = Namespace.validate_module("Json.Parser", "json-parser")
    end
  end
end
