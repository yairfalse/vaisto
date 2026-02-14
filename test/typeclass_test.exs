defmodule Vaisto.TypeClassTest do
  use ExUnit.Case
  alias Vaisto.Parser
  alias Vaisto.TypeChecker
  alias Vaisto.Runner
  alias Vaisto.Error

  defp error_text(%Error{} = err), do: Error.to_string(err)
  defp error_text(msg) when is_binary(msg), do: msg
  defp error_text(other), do: inspect(other)

  # =========================================================================
  # Parsing
  # =========================================================================

  describe "parsing defclass" do
    test "parses defclass with one method" do
      code = "(defclass Eq [a] (eq [x :a y :a] :bool))"
      ast = Parser.parse(code)
      assert {:defclass, :Eq, [:a], methods, _loc} = ast
      assert [{:eq, [{:x, :a}, {:y, :a}], :bool}] = methods
    end

    test "parses defclass with multiple methods" do
      code = """
      (defclass Show [a]
        (show [x :a] :string))
      """
      ast = Parser.parse(code)
      assert {:defclass, :Show, [:a], [{:show, [{:x, :a}], :string}], _loc} = ast
    end
  end

  describe "parsing instance" do
    test "parses instance for primitive type" do
      code = "(instance Eq :int (eq [x y] (== x y)))"
      ast = Parser.parse(code)
      assert {:instance, :Eq, :int, methods, _loc} = ast
      assert [{:eq, [:x, :y], {:call, :==, [:x, :y], _}}] = methods
    end

    test "parses instance with multiple methods" do
      code = """
      (instance Eq :string
        (eq [x y] (== x y)))
      """
      ast = Parser.parse(code)
      assert {:instance, :Eq, :string, [{:eq, [:x, :y], _body}], _loc} = ast
    end
  end

  # =========================================================================
  # Type Checking — Built-in Eq
  # =========================================================================

  describe "type checking eq" do
    test "eq with matching int types succeeds" do
      code = "(eq 1 2)"
      ast = Parser.parse(code)
      assert {:ok, :bool, _typed_ast} = TypeChecker.check(ast)
    end

    test "eq with matching string types succeeds" do
      code = ~s|(eq "a" "b")|
      ast = Parser.parse(code)
      assert {:ok, :bool, _typed_ast} = TypeChecker.check(ast)
    end

    test "eq with matching bool types succeeds" do
      code = "(eq true false)"
      ast = Parser.parse(code)
      assert {:ok, :bool, _typed_ast} = TypeChecker.check(ast)
    end

    test "eq resolves to class_call in typed AST" do
      code = "(eq 1 2)"
      ast = Parser.parse(code)
      {:ok, :bool, typed_ast} = TypeChecker.check(ast)
      assert {:class_call, :Eq, :eq, :int, _args, :bool} = typed_ast
    end
  end

  # =========================================================================
  # Type Checking — Built-in Show
  # =========================================================================

  describe "type checking show" do
    test "show with int succeeds" do
      code = "(show 42)"
      ast = Parser.parse(code)
      assert {:ok, :string, _typed_ast} = TypeChecker.check(ast)
    end

    test "show with string succeeds" do
      code = ~s|(show "hello")|
      ast = Parser.parse(code)
      assert {:ok, :string, _typed_ast} = TypeChecker.check(ast)
    end

    test "show resolves to class_call" do
      code = "(show 42)"
      ast = Parser.parse(code)
      {:ok, :string, typed_ast} = TypeChecker.check(ast)
      assert {:class_call, :Show, :show, :int, _args, :string} = typed_ast
    end
  end

  # =========================================================================
  # Type Checking — User-defined class + instance
  # =========================================================================

  describe "type checking user-defined classes" do
    test "user-defined class and instance type checks" do
      code = """
      (defclass Printable [a]
        (to-str [x :a] :string))
      (instance Printable :int
        (to-str [x] (str x)))
      (to-str 42)
      """
      ast = Parser.parse(code)
      assert {:ok, _, _typed_ast} = TypeChecker.check(ast)
    end

    test "missing instance produces error" do
      code = "(eq (list 1 2) (list 3 4))"
      ast = Parser.parse(code)
      result = TypeChecker.check(ast)
      assert {:error, _} = result
    end

    test "missing method in instance errors" do
      code = """
      (defclass MyClass [a]
        (method1 [x :a] :int)
        (method2 [x :a] :string))
      (instance MyClass :int
        (method1 [x] x))
      """
      ast = Parser.parse(code)
      result = TypeChecker.check(ast)
      assert {:error, err} = result
      assert error_text(err) =~ "missing methods"
    end
  end

  # =========================================================================
  # Code Generation + Runtime — Built-in Eq
  # =========================================================================

  describe "codegen: eq" do
    test "(eq 1 1) => true" do
      {:ok, mod} = Runner.compile_and_load("(eq 1 1)", :EqIntTrue, backend: :core)
      assert true == Runner.call(mod, :main)
    end

    test "(eq 1 2) => false" do
      {:ok, mod} = Runner.compile_and_load("(eq 1 2)", :EqIntFalse, backend: :core)
      assert false == Runner.call(mod, :main)
    end

    test "(eq true true) => true" do
      {:ok, mod} = Runner.compile_and_load("(eq true true)", :EqBoolTrue, backend: :core)
      assert true == Runner.call(mod, :main)
    end

    test "(eq true false) => false" do
      {:ok, mod} = Runner.compile_and_load("(eq true false)", :EqBoolFalse, backend: :core)
      assert false == Runner.call(mod, :main)
    end

    test ~s|(eq "hello" "hello") => true| do
      {:ok, mod} = Runner.compile_and_load(~s|(eq "hello" "hello")|, :EqStrTrue, backend: :core)
      assert true == Runner.call(mod, :main)
    end

    test ~s|(eq "hello" "world") => false| do
      {:ok, mod} = Runner.compile_and_load(~s|(eq "hello" "world")|, :EqStrFalse, backend: :core)
      assert false == Runner.call(mod, :main)
    end
  end

  # =========================================================================
  # Code Generation + Runtime — Built-in Show
  # =========================================================================

  describe "codegen: show" do
    test "(show 42) => string" do
      {:ok, mod} = Runner.compile_and_load("(show 42)", :ShowInt, backend: :core)
      result = Runner.call(mod, :main)
      assert result == "42"
    end

    test "(show true) => string" do
      {:ok, mod} = Runner.compile_and_load("(show true)", :ShowBool, backend: :core)
      result = Runner.call(mod, :main)
      assert result == "true"
    end

    test ~s|(show "hi") => "hi"| do
      {:ok, mod} = Runner.compile_and_load(~s|(show "hi")|, :ShowStr, backend: :core)
      result = Runner.call(mod, :main)
      assert result == "hi"
    end
  end

  # =========================================================================
  # Code Generation + Runtime — User-defined instance
  # =========================================================================

  describe "codegen: user-defined class" do
    test "user-defined class and instance works end-to-end" do
      code = """
      (defclass Describable [a]
        (describe [x :a] :string))
      (instance Describable :int
        (describe [x] (str "number:" x)))
      (describe 42)
      """
      {:ok, mod} = Runner.compile_and_load(code, :DescribableE2E, backend: :core)
      assert "number:42" == Runner.call(mod, :main)
    end
  end

  # =========================================================================
  # Code Generation + Runtime — eq in defn
  # =========================================================================

  describe "codegen: eq in defn" do
    test "eq inside a function definition" do
      code = """
      (defn same? [x :int y :int] :bool (eq x y))
      (same? 5 5)
      """
      {:ok, mod} = Runner.compile_and_load(code, :EqInDefn, backend: :core)
      assert true == Runner.call(mod, :main)
    end

    test "eq inside a function with different values" do
      code = """
      (defn same? [x :int y :int] :bool (eq x y))
      (same? 5 3)
      """
      {:ok, mod} = Runner.compile_and_load(code, :EqInDefnFalse, backend: :core)
      assert false == Runner.call(mod, :main)
    end
  end

  # =========================================================================
  # Code Generation + Runtime — show in let
  # =========================================================================

  describe "codegen: show in let" do
    test "show in let binding" do
      code = """
      (let [s (show 99)]
        s)
      """
      {:ok, mod} = Runner.compile_and_load(code, :ShowInLet, backend: :core)
      assert "99" == Runner.call(mod, :main)
    end
  end
end
