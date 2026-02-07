defmodule Vaisto.BackendTest do
  use ExUnit.Case
  alias Vaisto.{Backend, Parser, TypeChecker}

  describe "Backend behaviour" do
    test "Backend.Core implements compile/3" do
      source = "(defn add [x :int y :int] :int (+ x y))"
      {:ok, ast} = parse(source)
      {:ok, _type, typed_ast} = TypeChecker.check(ast)

      assert {:ok, _mod, binary} = Backend.Core.compile(typed_ast, :BackendCoreTest1)
      assert is_binary(binary)
    end

    test "Backend.Elixir implements compile/3" do
      # Elixir backend works better with simple expressions
      source = "(defn main [] :int 42)"
      {:ok, ast} = parse(source)
      {:ok, _type, typed_ast} = TypeChecker.check(ast)

      assert {:ok, _mod, binary} = Backend.Elixir.compile(typed_ast, :BackendElixirTest1)
      assert is_binary(binary)
    end

    test "Backend.Core implements to_intermediate/2" do
      source = "(defn main [] :int 42)"
      {:ok, ast} = parse(source)
      {:ok, _type, typed_ast} = TypeChecker.check(ast)

      core_ast = Backend.Core.to_intermediate(typed_ast, :IntermediateTest1)
      # Core Erlang AST is a tuple starting with :c_module
      assert is_tuple(core_ast)
    end

    test "Backend.Elixir implements to_intermediate/2" do
      typed_ast = {:lit, :int, 42}

      elixir_ast = Backend.Elixir.to_intermediate(typed_ast, :IntermediateTest2)
      # Should be just 42 for an integer literal
      assert elixir_ast == 42
    end
  end

  describe "Backend.compile/4 dispatch" do
    test "dispatches to Core backend" do
      source = "(defn main [] :int 42)"
      {:ok, ast} = parse(source)
      {:ok, _type, typed_ast} = TypeChecker.check(ast)

      assert {:ok, mod, _binary} = Backend.compile(:core, typed_ast, :DispatchCore)
      assert mod == :DispatchCore
      assert apply(mod, :main, []) == 42
    end

    test "dispatches to Elixir backend" do
      source = "(defn main [] :int 42)"
      {:ok, ast} = parse(source)
      {:ok, _type, typed_ast} = TypeChecker.check(ast)

      assert {:ok, mod, _binary} = Backend.compile(:elixir, typed_ast, :DispatchElixir)
      assert mod == :DispatchElixir
      assert apply(mod, :main, []) == 42
    end
  end

  describe "Backend.get_backend/1" do
    test "returns Core module for :core" do
      assert Backend.get_backend(:core) == Backend.Core
    end

    test "returns Elixir module for :elixir" do
      assert Backend.get_backend(:elixir) == Backend.Elixir
    end
  end

  describe "Backend.default/0" do
    test "returns :core as default" do
      assert Backend.default() == :core
    end
  end

  describe "Backend.Shared utilities" do
    alias Backend.Shared

    test "extract_pattern_vars from variable" do
      assert Shared.extract_pattern_vars({:var, :x, :int}) == [:x]
    end

    test "extract_pattern_vars from tuple pattern" do
      pattern = {:tuple_pattern, [{:var, :a, :int}, {:var, :b, :int}], :tuple}
      assert Shared.extract_pattern_vars(pattern) == [:a, :b]
    end

    test "extract_pattern_vars from cons pattern" do
      pattern = {:cons, {:var, :h, :int}, {:var, :t, {:list, :int}}, {:list, :int}}
      assert Shared.extract_pattern_vars(pattern) == [:h, :t]
    end

    test "extract_pattern_vars from constructor pattern" do
      pattern = {:pattern, :Ok, [{:var, :v, :any}], :any}
      assert Shared.extract_pattern_vars(pattern) == [:v]
    end

    test "extract_pattern_vars ignores wildcards" do
      assert Shared.extract_pattern_vars(:_) == []
      assert Shared.extract_pattern_vars({:lit, :int, 42}) == []
    end

    test "camelize converts snake_case to CamelCase" do
      assert Shared.camelize(:foo_bar) == FooBar
      assert Shared.camelize(:counter) == Counter
      assert Shared.camelize(:my_process_name) == MyProcessName
    end

    test "is_literal? detects literals" do
      assert Shared.is_literal?({:lit, :int, 42})
      assert Shared.is_literal?({:lit, :string, "hello"})
      assert Shared.is_literal?(42)
      assert Shared.is_literal?(3.14)
      assert Shared.is_literal?(true)
      refute Shared.is_literal?({:var, :x, :int})
    end

    test "literal_value extracts value from literal" do
      assert Shared.literal_value({:lit, :int, 42}) == 42
      assert Shared.literal_value({:lit, :string, "hello"}) == "hello"
      assert Shared.literal_value({:lit, :bool, true}) == true
      assert Shared.literal_value({:lit, :atom, :foo}) == :foo
    end

    test "defn_arity returns function arity" do
      defn = {:defn, :add, [:x, :y], {:call, :+, [{:var, :x, :int}, {:var, :y, :int}], :int}, :int}
      assert Shared.defn_arity(defn) == 2
    end

    test "defn_name returns function name" do
      defn = {:defn, :my_func, [:x], {:var, :x, :int}, :int}
      assert Shared.defn_name(defn) == :my_func
    end

    test "is_defn? detects function definitions" do
      assert Shared.is_defn?({:defn, :foo, [], {:lit, :int, 42}, :int})
      assert Shared.is_defn?({:defn_multi, :foo, [], nil})
      refute Shared.is_defn?({:defval, :x, {:lit, :int, 42}, nil})
    end

    test "is_module? detects module AST" do
      assert Shared.is_module?({:module, []})
      refute Shared.is_module?({:defn, :foo, [], {:lit, :int, 42}, :int})
    end
  end

  # Helper to parse source
  defp parse(source) do
    try do
      {:ok, Parser.parse(source)}
    rescue
      e -> {:error, Exception.message(e)}
    end
  end
end
