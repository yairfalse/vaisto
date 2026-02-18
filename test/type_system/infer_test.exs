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

  describe "list builtins" do
    test "head returns element type" do
      env = Map.merge(Infer.__primitives__(), %{:xs => {:list, :int}})
      {:ok, type, _ast} = Infer.infer({:call, :head, [:xs]}, env)
      assert type == :int
    end

    test "head on :any returns :any" do
      env = Map.merge(Infer.__primitives__(), %{:xs => :any})
      {:ok, type, _ast} = Infer.infer({:call, :head, [:xs]}, env)
      assert type == :any
    end

    test "head errors on non-list" do
      assert {:error, msg} = Infer.infer({:call, :head, [42]})
      assert msg =~ "head expects a list"
    end

    test "tail returns list type" do
      env = Map.merge(Infer.__primitives__(), %{:xs => {:list, :int}})
      {:ok, type, _ast} = Infer.infer({:call, :tail, [:xs]}, env)
      assert type == {:list, :int}
    end

    test "tail on :any returns list of :any" do
      env = Map.merge(Infer.__primitives__(), %{:xs => :any})
      {:ok, type, _ast} = Infer.infer({:call, :tail, [:xs]}, env)
      assert type == {:list, :any}
    end

    test "tail errors on non-list" do
      assert {:error, msg} = Infer.infer({:call, :tail, [42]})
      assert msg =~ "tail expects a list"
    end

    test "empty? returns bool" do
      env = Map.merge(Infer.__primitives__(), %{:xs => {:list, :int}})
      {:ok, type, _ast} = Infer.infer({:call, :empty?, [:xs]}, env)
      assert type == :bool
    end

    test "empty? on :any returns bool" do
      env = Map.merge(Infer.__primitives__(), %{:xs => :any})
      {:ok, type, _ast} = Infer.infer({:call, :empty?, [:xs]}, env)
      assert type == :bool
    end

    test "empty? errors on non-list" do
      assert {:error, msg} = Infer.infer({:call, :empty?, [42]})
      assert msg =~ "empty? expects a list"
    end

    test "length returns int" do
      env = Map.merge(Infer.__primitives__(), %{:xs => {:list, :int}})
      {:ok, type, _ast} = Infer.infer({:call, :length, [:xs]}, env)
      assert type == :int
    end

    test "length errors on non-list" do
      assert {:error, msg} = Infer.infer({:call, :length, [42]})
      assert msg =~ "length expects a list"
    end

    test "head typed AST shape" do
      env = Map.merge(Infer.__primitives__(), %{:xs => {:list, :int}})
      {:ok, _type, ast} = Infer.infer({:call, :head, [:xs]}, env)
      assert {:call, :head, [_], :int} = ast
    end
  end

  describe "str builtin" do
    test "str returns string" do
      {:ok, type, _ast} = Infer.infer({:call, :str, [{:string, "hello"}, {:string, " world"}]})
      assert type == :string
    end

    test "str accepts any types" do
      {:ok, type, _ast} = Infer.infer({:call, :str, [{:string, "value: "}, 42]})
      assert type == :string
    end

    test "str with single arg" do
      {:ok, type, _ast} = Infer.infer({:call, :str, [42]})
      assert type == :string
    end

    test "str typed AST shape" do
      {:ok, _type, ast} = Infer.infer({:call, :str, [1, true]})
      assert {:call, :str, [_, _], :string} = ast
    end
  end

  describe "function references" do
    test "fn_ref from environment" do
      env = Map.merge(Infer.__primitives__(), %{:my_fn => {:fn, [:int], :bool}})
      {:ok, type, ast} = Infer.infer({:fn_ref, :my_fn, 1}, env)
      assert type == {:fn, [:int], :bool}
      assert {:fn_ref, :my_fn, 1, {:fn, [:int], :bool}} = ast
    end

    test "fn_ref errors on undefined" do
      assert {:error, msg} = Infer.infer({:fn_ref, :unknown, 1})
      assert msg =~ "Undefined function reference"
    end

    test "variable lookup emits fn_ref for function types" do
      env = Map.merge(Infer.__primitives__(), %{:add => {:fn, [:int, :int], :int}})
      {:ok, type, ast} = Infer.infer({:var, :add}, env)
      assert type == {:fn, [:int, :int], :int}
      assert {:fn_ref, :add, 2, _} = ast
    end

    test "variable lookup emits var for non-function types" do
      env = Map.merge(Infer.__primitives__(), %{:x => :int})
      {:ok, _type, ast} = Infer.infer({:var, :x}, env)
      assert {:var, :x, :int} = ast
    end

    test "bare atom lookup emits fn_ref for function types" do
      env = Map.merge(Infer.__primitives__(), %{:f => {:fn, [:int], :int}})
      {:ok, _type, ast} = Infer.infer(:f, env)
      assert {:fn_ref, :f, 1, _} = ast
    end
  end

  describe "map builtin" do
    test "map with named function" do
      env = Map.merge(Infer.__primitives__(), %{
        :inc => {:fn, [:int], :int},
        :xs => {:list, :int}
      })
      {:ok, type, _ast} = Infer.infer({:call, :map, [:inc, :xs]}, env)
      assert type == {:list, :int}
    end

    test "map with anonymous function" do
      env = Map.merge(Infer.__primitives__(), %{:xs => {:list, :int}})
      {:ok, type, _ast} = Infer.infer(
        {:call, :map, [{:fn, [:x], {:call, :+, [:x, 1]}}, :xs]},
        env
      )
      assert type == {:list, :int}
    end

    test "map errors on wrong arity" do
      env = Map.merge(Infer.__primitives__(), %{
        :add => {:fn, [:int, :int], :int},
        :xs => {:list, :int}
      })
      assert {:error, msg} = Infer.infer({:call, :map, [:add, :xs]}, env)
      assert msg =~ "1 argument"
    end

    test "map errors on non-function" do
      env = Map.merge(Infer.__primitives__(), %{:xs => {:list, :int}})
      assert {:error, msg} = Infer.infer({:call, :map, [42, :xs]}, env)
      assert msg =~ "function"
    end

    test "map typed AST shape" do
      env = Map.merge(Infer.__primitives__(), %{
        :inc => {:fn, [:int], :int},
        :xs => {:list, :int}
      })
      {:ok, _type, ast} = Infer.infer({:call, :map, [:inc, :xs]}, env)
      assert {:call, :map, [_, _], {:list, :int}} = ast
    end
  end

  describe "filter builtin" do
    test "filter with predicate" do
      env = Map.merge(Infer.__primitives__(), %{
        :pos? => {:fn, [:int], :bool},
        :xs => {:list, :int}
      })
      {:ok, type, _ast} = Infer.infer({:call, :filter, [:pos?, :xs]}, env)
      assert type == {:list, :int}
    end

    test "filter with anonymous predicate" do
      env = Map.merge(Infer.__primitives__(), %{:xs => {:list, :int}})
      {:ok, type, _ast} = Infer.infer(
        {:call, :filter, [{:fn, [:x], {:call, :>, [:x, 0]}}, :xs]},
        env
      )
      assert type == {:list, :int}
    end

    test "filter errors when predicate doesn't return bool" do
      env = Map.merge(Infer.__primitives__(), %{
        :inc => {:fn, [:int], :int},
        :xs => {:list, :int}
      })
      assert {:error, msg} = Infer.infer({:call, :filter, [:inc, :xs]}, env)
      assert msg =~ "Bool"
    end

    test "filter errors on wrong arity" do
      env = Map.merge(Infer.__primitives__(), %{
        :add => {:fn, [:int, :int], :int},
        :xs => {:list, :int}
      })
      assert {:error, msg} = Infer.infer({:call, :filter, [:add, :xs]}, env)
      assert msg =~ "1 argument"
    end
  end

  describe "fold builtin" do
    test "fold with accumulator function" do
      env = Map.merge(Infer.__primitives__(), %{:xs => {:list, :int}})
      {:ok, type, _ast} = Infer.infer(
        {:call, :fold, [{:fn, [:acc, :x], {:call, :+, [:acc, :x]}}, 0, :xs]},
        env
      )
      assert type == :int
    end

    test "fold with named function" do
      env = Map.merge(Infer.__primitives__(), %{
        :add => {:fn, [:int, :int], :int},
        :xs => {:list, :int}
      })
      {:ok, type, _ast} = Infer.infer({:call, :fold, [:add, 0, :xs]}, env)
      assert type == :int
    end

    test "fold errors on wrong arity" do
      env = Map.merge(Infer.__primitives__(), %{
        :inc => {:fn, [:int], :int},
        :xs => {:list, :int}
      })
      assert {:error, msg} = Infer.infer({:call, :fold, [:inc, 0, :xs]}, env)
      assert msg =~ "2 arguments"
    end

    test "fold errors on non-function" do
      env = Map.merge(Infer.__primitives__(), %{:xs => {:list, :int}})
      assert {:error, msg} = Infer.infer({:call, :fold, [42, 0, :xs]}, env)
      assert msg =~ "function"
    end

    test "fold typed AST shape" do
      env = Map.merge(Infer.__primitives__(), %{
        :add => {:fn, [:int, :int], :int},
        :xs => {:list, :int}
      })
      {:ok, _type, ast} = Infer.infer({:call, :fold, [:add, 0, :xs]}, env)
      assert {:call, :fold, [_, _, _], :int} = ast
    end
  end

  describe "qualified calls" do
    test "qualified call with known extern returns typed result" do
      env = Map.merge(Infer.__primitives__(), %{
        :"Std.List:length" => {:fn, [{:list, :any}], :int}
      })
      {:ok, type, _ast} = Infer.infer(
        {:call, {:qualified, :"Std.List", :length}, [{:list, [1, 2]}]},
        env
      )
      assert type == :int
    end

    test "qualified call with unknown extern returns :any" do
      {:ok, type, _ast} = Infer.infer(
        {:call, {:qualified, :SomeModule, :func}, [42]}
      )
      assert type == :any
    end

    test "qualified call typed AST shape" do
      {:ok, _type, ast} = Infer.infer(
        {:call, {:qualified, :Mod, :func}, [1, 2]}
      )
      assert {:call, {:qualified, :Mod, :func}, [_, _], :any} = ast
    end

    test "location stripping for qualified calls" do
      loc = %Vaisto.Parser.Loc{line: 1, col: 1, file: nil}
      {:ok, type, _ast} = Infer.infer(
        {:call, {:qualified, :Mod, :func}, [42], loc}
      )
      assert type == :any
    end
  end

  describe "higher-order apply" do
    test "applying a function variable via let" do
      # (let [f (fn [x] (+ x 1))] (f 42)) — f is a let-bound function
      {:ok, type, _ast} = Infer.infer(
        {:let, [{:f, {:fn, [:x], {:call, :+, [:x, 1]}}}],
         {:call, :f, [42]}}
      )
      assert type == :int
    end

    test "applying an anonymous function expression" do
      # ((fn [x] (+ x 1)) 42)
      {:ok, type, _ast} = Infer.infer(
        {:call, {:fn, [:x], {:call, :+, [:x, 1]}}, [42]}
      )
      assert type == :int
    end

    test "apply emits :apply AST node for expression calls" do
      {:ok, _type, ast} = Infer.infer(
        {:call, {:fn, [:x], :x}, [42]}
      )
      assert {:apply, {:fn, _, _, _}, [_], _} = ast
    end

    test "apply errors on non-function expression" do
      assert {:error, msg} = Infer.infer(
        {:call, 42, [1]}
      )
      assert msg =~ "non-function"
    end

    test "apply with location stripping" do
      loc = %Vaisto.Parser.Loc{line: 1, col: 1, file: nil}
      {:ok, type, _ast} = Infer.infer(
        {:call, {:fn, [:x], :x}, [42], loc}
      )
      assert type == :int
    end
  end

  describe "do blocks" do
    test "empty do block infers unit" do
      {:ok, type, _ast} = Infer.infer({:do, []})
      assert type == :unit
    end

    test "single expression do block infers that type" do
      {:ok, type, _ast} = Infer.infer({:do, [42]})
      assert type == :int
    end

    test "multi-expression do block infers type of last expression" do
      {:ok, type, _ast} = Infer.infer({:do, [true, 42]})
      assert type == :int
    end

    test "do block threads context through expressions" do
      # let binding in do block should be visible to later expressions
      {:ok, type, _ast} = Infer.infer(
        {:do, [{:let, [{:x, 42}], {:call, :+, [:x, 1]}}]}
      )
      assert type == :int
    end

    test "location stripping works for do blocks" do
      loc = %Vaisto.Parser.Loc{line: 1, col: 1, file: nil}
      {:ok, type, _ast} = Infer.infer({:do, [42], loc})
      assert type == :int
    end

    test "typed AST shape for do blocks" do
      {:ok, _type, ast} = Infer.infer({:do, [1, true]})
      assert {:do, [_, _], :bool} = ast
    end
  end

  describe "tuple expressions" do
    test "tuple_pattern infers :any" do
      {:ok, type, _ast} = Infer.infer({:tuple_pattern, [1, 2]})
      assert type == :any
    end

    test "tuple infers :any" do
      {:ok, type, _ast} = Infer.infer({:tuple, [1, {:string, "hello"}]})
      assert type == :any
    end

    test "empty tuple infers :any" do
      {:ok, type, _ast} = Infer.infer({:tuple, []})
      assert type == :any
    end

    test "typed AST shape for tuples" do
      {:ok, _type, ast} = Infer.infer({:tuple, [1, true]})
      assert {:tuple, [_, _], :any} = ast
    end

    test "location stripping works for tuples" do
      loc = %Vaisto.Parser.Loc{line: 1, col: 1, file: nil}
      {:ok, type, _ast} = Infer.infer({:tuple, [1, 2], loc})
      assert type == :any
    end
  end

  describe "cons expressions" do
    test "cons with known list type" do
      env = Map.merge(Infer.__primitives__(), %{:xs => {:list, :int}})
      {:ok, type, _ast} = Infer.infer({:cons, 1, :xs}, env)
      assert type == {:list, :int}
    end

    test "cons infers element type from head" do
      {:ok, type, _ast} = Infer.infer({:cons, 42, {:list, []}})
      assert type == {:list, :int}
    end

    test "typed AST for cons" do
      {:ok, _type, ast} = Infer.infer({:cons, 1, {:list, []}})
      assert {:cons, {:lit, :int, 1}, {:list, [], _}, {:list, :int}} = ast
    end
  end

  describe "bracket expressions" do
    test "empty bracket infers polymorphic list" do
      {:ok, type, _ast} = Infer.infer({:bracket, []})
      assert {:list, _} = type
    end

    test "bracket with elements infers typed list" do
      {:ok, type, _ast} = Infer.infer({:bracket, [1, 2, 3]})
      assert type == {:list, :int}
    end

    test "bracket with cons" do
      {:ok, type, _ast} = Infer.infer({:bracket, {:cons, 1, {:list, []}}})
      assert type == {:list, :int}
    end

    test "location stripping works for brackets" do
      loc = %Vaisto.Parser.Loc{line: 1, col: 1, file: nil}
      {:ok, type, _ast} = Infer.infer({:bracket, [1, 2], loc})
      assert type == {:list, :int}
    end
  end

  describe "match expressions" do
    test "match with literal patterns" do
      {:ok, type, _ast} = Infer.infer(
        {:match, 42, [{1, {:string, "one"}}, {2, {:string, "two"}}]}
      )
      assert type == :string
    end

    test "variable pattern binds scrutinee type" do
      {:ok, type, _ast} = Infer.infer(
        {:match, 42, [{:x, {:call, :+, [:x, 1]}}]}
      )
      assert type == :int
    end

    test "wildcard pattern" do
      {:ok, type, _ast} = Infer.infer(
        {:match, true, [{:_, 42}]}
      )
      assert type == :int
    end

    test "branch type unification" do
      {:ok, type, _ast} = Infer.infer(
        {:match, true, [{true, 1}, {false, 2}]}
      )
      assert type == :int
    end

    test "branch type mismatch returns error" do
      assert {:error, _msg} = Infer.infer(
        {:match, true, [{true, 1}, {false, {:string, "no"}}]}
      )
    end

    test "cons pattern on list" do
      env = Map.merge(Infer.__primitives__(), %{:xs => {:list, :int}})
      {:ok, type, _ast} = Infer.infer(
        {:match, :xs, [{{:cons, :h, :t}, :h}, {[], 0}]},
        env
      )
      assert type == :int
    end

    test "location stripping works for match" do
      loc = %Vaisto.Parser.Loc{line: 1, col: 1, file: nil}
      {:ok, type, _ast} = Infer.infer(
        {:match, 42, [{:x, :x}], loc}
      )
      assert type == :int
    end

    test "typed AST shape for match" do
      {:ok, _type, ast} = Infer.infer(
        {:match, 42, [{:x, :x}]}
      )
      assert {:match, {:lit, :int, 42}, [{_, _, :int}], :int} = ast
    end

    test "clause bindings don't leak between clauses" do
      # x is bound in first clause, should not be visible in second clause
      {:ok, type, _ast} = Infer.infer(
        {:match, 42, [{:x, {:call, :+, [:x, 1]}}, {:y, {:call, :+, [:y, 2]}}]}
      )
      assert type == :int
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

  describe "pattern type mismatch detection" do
    test "integer literal pattern against string scrutinee errors" do
      # (match "hello" [42 ...]) should fail — int pattern can't match string
      assert {:error, _msg} = Infer.infer(
        {:match, {:string, "hello"}, [{42, {:string, "matched"}}]}
      )
    end

    test "string literal pattern against int scrutinee errors" do
      # (match 42 ["hello" ...]) should fail — string pattern can't match int
      assert {:error, _msg} = Infer.infer(
        {:match, 42, [{{:string, "hello"}, {:string, "matched"}}]}
      )
    end

    test "bool literal pattern against int scrutinee errors" do
      # (match 42 [true ...]) should fail — bool pattern can't match int
      assert {:error, _msg} = Infer.infer(
        {:match, 42, [{true, {:string, "yes"}}]}
      )
    end

    test "float literal pattern against string scrutinee errors" do
      # (match "hello" [3.14 ...]) should fail — float pattern can't match string
      assert {:error, _msg} = Infer.infer(
        {:match, {:string, "hello"}, [{3.14, {:string, "matched"}}]}
      )
    end

    test "empty list pattern against int scrutinee errors" do
      # (match 42 [[] ...]) should fail — list pattern can't match int
      assert {:error, _msg} = Infer.infer(
        {:match, 42, [{[], {:string, "empty"}}]}
      )
    end
  end
end
