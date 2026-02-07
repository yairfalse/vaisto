defmodule Vaisto.TypeSystem.InferTest do
  use ExUnit.Case
  alias Vaisto.TypeSystem.Infer
  alias Vaisto.TypeSystem.Core

  describe "literals" do
    test "infers integer type" do
      assert {:ok, :int, {:lit, :int, 42}} = Infer.infer(42)
    end

    test "infers float type" do
      assert {:ok, :float, {:lit, :float, 3.14}} = Infer.infer(3.14)
    end

    test "infers boolean types" do
      assert {:ok, :bool, {:lit, :bool, true}} = Infer.infer(true)
      assert {:ok, :bool, {:lit, :bool, false}} = Infer.infer(false)
    end

    test "infers string type" do
      assert {:ok, :string, {:lit, :string, "hello"}} = Infer.infer({:string, "hello"})
    end

    test "infers atom type" do
      assert {:ok, {:atom, :foo}, {:lit, :atom, :foo}} = Infer.infer({:atom, :foo})
    end
  end

  describe "variables" do
    test "looks up variable from environment" do
      env = %{:x => :int}
      {:ok, type, _ast} = Infer.infer({:var, :x}, env)
      assert type == :int
    end

    test "errors on undefined variable" do
      assert {:error, msg} = Infer.infer({:var, :unknown})
      assert msg =~ "Undefined variable"
    end

    test "bare atom as variable" do
      env = %{:x => :int}
      {:ok, type, _ast} = Infer.infer(:x, env)
      assert type == :int
    end

    test "bare atom as literal when not in env" do
      {:ok, type, _ast} = Infer.infer(:foo)
      assert type == {:atom, :foo}
    end
  end

  describe "function calls" do
    test "infers + operator" do
      {:ok, type, _ast} = Infer.infer({:call, :+, [1, 2]})
      assert type == :int
    end

    test "infers arithmetic expression" do
      {:ok, type, _ast} = Infer.infer({:call, :*, [{:call, :+, [1, 2]}, 3]})
      assert type == :int
    end

    test "errors on arity mismatch" do
      assert {:error, msg} = Infer.infer({:call, :+, [1]})
      assert msg =~ "Arity mismatch"
    end

    test "errors on unknown function" do
      assert {:error, msg} = Infer.infer({:call, :unknown_fn, [1]})
      assert msg =~ "Unknown function"
    end

    test "infers comparison operators" do
      {:ok, type, _ast} = Infer.infer({:call, :<, [1, 2]})
      assert type == :bool
    end
  end

  describe "anonymous functions" do
    test "infers identity function type" do
      # (fn [x] x) should infer a -> a
      {:ok, type, _ast} = Infer.infer({:fn, [:x], :x})

      # The function type should have same input/output type variable
      assert {:fn, [param_type], ret_type} = type
      assert param_type == ret_type
    end

    test "infers function that uses arithmetic" do
      # (fn [x] (+ x 1)) should infer int -> int
      {:ok, type, _ast} = Infer.infer({:fn, [:x], {:call, :+, [:x, 1]}})

      assert type == {:fn, [:int], :int}
    end

    test "infers multi-param function" do
      # (fn [x y] (+ x y)) should infer (int, int) -> int
      {:ok, type, _ast} = Infer.infer({:fn, [:x, :y], {:call, :+, [:x, :y]}})

      assert type == {:fn, [:int, :int], :int}
    end

    test "infers nested function" do
      # (fn [x] (fn [y] (+ x y))) should infer int -> (int -> int)
      {:ok, type, _ast} = Infer.infer({:fn, [:x], {:fn, [:y], {:call, :+, [:x, :y]}}})

      assert {:fn, [:int], {:fn, [:int], :int}} = type
    end
  end

  describe "let bindings" do
    test "infers let with simple binding" do
      # (let [x 42] x) should infer int
      {:ok, type, _ast} = Infer.infer({:let, [{:x, 42}], :x})
      assert type == :int
    end

    test "infers let with expression binding" do
      # (let [x (+ 1 2)] (+ x 3)) should infer int
      {:ok, type, _ast} = Infer.infer({:let, [{:x, {:call, :+, [1, 2]}}], {:call, :+, [:x, 3]}})
      assert type == :int
    end

    test "infers let with multiple bindings" do
      # (let [x 1 y 2] (+ x y)) should infer int
      {:ok, type, _ast} = Infer.infer({:let, [{:x, 1}, {:y, 2}], {:call, :+, [:x, :y]}})
      assert type == :int
    end

    test "let-polymorphism: identity used at different types" do
      # (let [id (fn [x] x)] (+ (id 1) (id 2)))
      # id should be polymorphic, usable with int
      {:ok, type, _ast} = Infer.infer(
        {:let, [{:id, {:fn, [:x], :x}}],
         {:call, :+, [{:call, :id, [1]}, {:call, :id, [2]}]}}
      )
      assert type == :int
    end
  end

  describe "if expressions" do
    test "infers if with bool condition" do
      # (if true 1 2) should infer int
      {:ok, type, _ast} = Infer.infer({:if, true, 1, 2})
      assert type == :int
    end

    test "infers if with comparison" do
      # (if (< 1 2) 10 20) should infer int
      {:ok, type, _ast} = Infer.infer({:if, {:call, :<, [1, 2]}, 10, 20})
      assert type == :int
    end

    test "errors on non-bool condition" do
      # (if 1 10 20) should error
      assert {:error, msg} = Infer.infer({:if, 1, 10, 20})
      assert msg =~ "unify"
    end

    test "errors on branch type mismatch" do
      # (if true 1 "hello") should error
      assert {:error, msg} = Infer.infer({:if, true, 1, {:string, "hello"}})
      assert msg =~ "unify"
    end
  end

  describe "list literals" do
    test "infers empty list" do
      {:ok, type, _ast} = Infer.infer({:list, []})
      assert {:list, _elem_type} = type
    end

    test "infers list of integers" do
      {:ok, type, _ast} = Infer.infer({:list, [1, 2, 3]})
      assert type == {:list, :int}
    end

    test "infers list of booleans" do
      {:ok, type, _ast} = Infer.infer({:list, [true, false]})
      assert type == {:list, :bool}
    end

    test "errors on heterogeneous list" do
      assert {:error, msg} = Infer.infer({:list, [1, {:string, "hello"}]})
      assert msg =~ "mismatch"
    end
  end

  describe "polymorphic operators" do
    test "== works with integers" do
      {:ok, type, _ast} = Infer.infer({:call, :==, [1, 2]})
      assert type == :bool
    end

    test "== works with booleans" do
      {:ok, type, _ast} = Infer.infer({:call, :==, [true, false]})
      assert type == :bool
    end

    test "!= works with strings" do
      {:ok, type, _ast} = Infer.infer({:call, :!=, [{:string, "a"}, {:string, "b"}]})
      assert type == :bool
    end
  end

  describe "typed AST output" do
    test "literals have type annotations" do
      {:ok, _type, ast} = Infer.infer(42)
      assert {:lit, :int, 42} = ast
    end

    test "variables have type annotations" do
      env = %{:x => :int}
      {:ok, _type, ast} = Infer.infer(:x, env)
      assert {:var, :x, :int} = ast
    end

    test "function calls have return type" do
      {:ok, _type, ast} = Infer.infer({:call, :+, [1, 2]})
      assert {:call, :+, _, :int} = ast
    end

    test "functions have full type" do
      {:ok, _type, ast} = Infer.infer({:fn, [:x], {:call, :+, [:x, 1]}})
      assert {:fn, [:x], _body, {:fn, [:int], :int}} = ast
    end
  end

  describe "field access" do
    test "single field access infers row-polymorphic type" do
      # (fn [p] (. p :name)) → ({name: 'a | ..b}) -> 'a
      {:ok, type, _ast} = Infer.infer({:fn, [:p], {:field_access, :p, :name}})

      assert {:fn, [param_type], ret_type} = type
      # Return type should be a type variable (the field's type)
      assert match?({:tvar, _}, ret_type)
      # Param type should be a row type with :name field
      assert {:row, fields, tail} = param_type
      assert [{:name, ^ret_type}] = fields
      assert match?({:rvar, _}, tail)
    end

    test "field access constrained by arithmetic infers int fields" do
      # (fn [p] (+ (. p :x) (. p :y))) → ({x: Int, y: Int | ..r}) -> Int
      {:ok, type, _ast} = Infer.infer(
        {:fn, [:p], {:call, :+, [{:field_access, :p, :x}, {:field_access, :p, :y}]}}
      )

      assert {:fn, [param_type], :int} = type
      # Param type is a nested row: {x: Int | {y: Int | ..r}}
      # This is standard Algorithm W row polymorphism - second field unifies with the tail
      assert {:row, [{:x, :int}], inner_row} = param_type
      assert {:row, [{:y, :int}], tail} = inner_row
      assert match?({:rvar, _}, tail)
    end

    test "nested field access infers nested row types" do
      # (fn [p] (. (. p :inner) :x)) → ({inner: {x: 'a | ..r1} | ..r2}) -> 'a
      {:ok, type, _ast} = Infer.infer(
        {:fn, [:p], {:field_access, {:field_access, :p, :inner}, :x}}
      )

      assert {:fn, [param_type], ret_type} = type
      assert match?({:tvar, _}, ret_type)
      # Outer row should have :inner field
      assert {:row, [{:inner, inner_type}], _tail} = param_type
      # Inner type should be a row with :x field
      assert {:row, [{:x, ^ret_type}], _inner_tail} = inner_type
    end

    test "location stripping works for field access" do
      loc = %Vaisto.Parser.Loc{line: 1, col: 1, file: nil}

      {:ok, type, _ast} = Infer.infer(
        {:fn, [:p], {:field_access, :p, :name, loc}}
      )

      assert {:fn, [_param], _ret} = type
    end

    test "typed AST contains field_access with correct types" do
      {:ok, _type, ast} = Infer.infer(
        {:fn, [:p], {:field_access, :p, :name}}
      )

      assert {:fn, [:p], body, _fn_type} = ast
      assert {:field_access, {:var, :p, _record_type}, :name, field_type} = body
      assert match?({:tvar, _}, field_type) or is_atom(field_type)
    end
  end

  describe "type formatting" do
    test "formats primitive types" do
      assert Core.format_type(:int) == "Int"
      assert Core.format_type(:bool) == "Bool"
    end

    test "formats function types" do
      assert Core.format_type({:fn, [:int, :int], :int}) == "(Int, Int) -> Int"
    end

    test "formats type variables with ML-style names" do
      assert Core.format_type({:tvar, 0}) == "'a"
      assert Core.format_type({:tvar, 1}) == "'b"
    end

    test "formats list types" do
      assert Core.format_type({:list, :int}) == "List(Int)"
    end
  end
end
