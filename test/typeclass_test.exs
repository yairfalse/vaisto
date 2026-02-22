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
      assert [{:eq, [{:x, :a}, {:y, :a}], :bool, nil}] = methods
    end

    test "parses defclass with multiple methods" do
      code = """
      (defclass Show [a]
        (show [x :a] :string))
      """
      ast = Parser.parse(code)
      assert {:defclass, :Show, [:a], [{:show, [{:x, :a}], :string, nil}], _loc} = ast
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

  # =========================================================================
  # Parsing — Default Methods
  # =========================================================================

  describe "parsing defclass with defaults" do
    test "parses method with default body" do
      code = """
      (defclass Eq [a]
        (eq [x :a y :a] :bool)
        (neq [x :a y :a] :bool (if (eq x y) false true)))
      """
      ast = Parser.parse(code)
      assert {:defclass, :Eq, [:a], methods, _loc} = ast
      assert [{:eq, _, :bool, nil}, {:neq, _, :bool, body}] = methods
      assert body != nil
    end

    test "parses method without default body as nil" do
      code = "(defclass Show [a] (show [x :a] :string))"
      ast = Parser.parse(code)
      assert {:defclass, :Show, [:a], [{:show, [{:x, :a}], :string, nil}], _loc} = ast
    end
  end

  # =========================================================================
  # Type Checking — Default Methods (neq)
  # =========================================================================

  describe "type checking defaults" do
    test "neq with int types succeeds" do
      code = "(neq 1 2)"
      ast = Parser.parse(code)
      assert {:ok, :bool, _typed_ast} = TypeChecker.check(ast)
    end

    test "neq resolves to class_call" do
      code = "(neq 1 2)"
      ast = Parser.parse(code)
      {:ok, :bool, typed_ast} = TypeChecker.check(ast)
      assert {:class_call, :Eq, :neq, :int, _args, :bool} = typed_ast
    end

    test "instance without neq inherits default" do
      code = """
      (defclass Comparable [a]
        (cmp-eq [x :a y :a] :bool)
        (cmp-neq [x :a y :a] :bool (if (cmp-eq x y) false true)))
      (instance Comparable :int
        (cmp-eq [x y] (== x y)))
      (cmp-neq 1 2)
      """
      ast = Parser.parse(code)
      assert {:ok, :module, _typed_ast} = TypeChecker.check(ast)
    end

    test "instance can override default" do
      code = """
      (defclass Comparable2 [a]
        (cmp-eq2 [x :a y :a] :bool)
        (cmp-neq2 [x :a y :a] :bool (if (cmp-eq2 x y) false true)))
      (instance Comparable2 :int
        (cmp-eq2 [x y] (== x y))
        (cmp-neq2 [x y] (!= x y)))
      (cmp-neq2 1 2)
      """
      ast = Parser.parse(code)
      assert {:ok, :module, _typed_ast} = TypeChecker.check(ast)
    end
  end

  # =========================================================================
  # Code Generation + Runtime — neq
  # =========================================================================

  describe "codegen: neq" do
    test "(neq 1 2) => true" do
      {:ok, mod} = Runner.compile_and_load("(neq 1 2)", :NeqIntTrue, backend: :core)
      assert true == Runner.call(mod, :main)
    end

    test "(neq 1 1) => false" do
      {:ok, mod} = Runner.compile_and_load("(neq 1 1)", :NeqIntFalse, backend: :core)
      assert false == Runner.call(mod, :main)
    end

    test "user-defined class default method works" do
      code = """
      (defclass Testable [a]
        (is-ok [x :a] :bool)
        (is-bad [x :a] :bool (if (is-ok x) false true)))
      (instance Testable :int
        (is-ok [x] (> x 0)))
      (is-bad 5)
      """
      {:ok, mod} = Runner.compile_and_load(code, :DefaultMethodE2E, backend: :core)
      assert false == Runner.call(mod, :main)
    end

    test "overridden default method works" do
      code = """
      (defclass Testable2 [a]
        (is-ok2 [x :a] :bool)
        (is-bad2 [x :a] :bool (if (is-ok2 x) false true)))
      (instance Testable2 :int
        (is-ok2 [x] (> x 0))
        (is-bad2 [x] (<= x 0)))
      (is-bad2 5)
      """
      {:ok, mod} = Runner.compile_and_load(code, :OverrideDefaultE2E, backend: :core)
      assert false == Runner.call(mod, :main)
    end
  end

  # =========================================================================
  # Type Checking — ADT Instances
  # =========================================================================

  describe "type checking ADT instances" do
    test "ADT instance type checks" do
      code = """
      (deftype Color (Red) (Green) (Blue))
      (instance Eq Color
        (eq [x y] (== x y)))
      (eq (Red) (Blue))
      """
      ast = Parser.parse(code)
      assert {:ok, :module, _typed_ast} = TypeChecker.check(ast)
    end

    test "ADT neq works via default method" do
      code = """
      (deftype Color (Red) (Green) (Blue))
      (instance Eq Color
        (eq [x y] (== x y)))
      (neq (Red) (Blue))
      """
      ast = Parser.parse(code)
      assert {:ok, :module, _typed_ast} = TypeChecker.check(ast)
    end

    test "parametric ADT instance type checks" do
      code = """
      (deftype Maybe (Just v) (Nothing))
      (instance Eq Maybe
        (eq [x y] (== x y)))
      (eq (Just 1) (Just 2))
      """
      ast = Parser.parse(code)
      assert {:ok, :module, _typed_ast} = TypeChecker.check(ast)
    end

    test "missing ADT instance errors" do
      code = """
      (deftype Color (Red) (Green) (Blue))
      (eq (Red) (Blue))
      """
      ast = Parser.parse(code)
      result = TypeChecker.check(ast)
      assert {:error, _} = result
    end

    test "ADT instance resolves to class_call with atom key" do
      code = """
      (deftype Color (Red) (Green) (Blue))
      (instance Eq Color
        (eq [x y] (== x y)))
      (eq (Red) (Blue))
      """
      ast = Parser.parse(code)
      {:ok, :module, {:module, forms}} = TypeChecker.check(ast)
      # The last form should be a class_call with :Color as the instance key
      last = List.last(forms)
      assert {:class_call, :Eq, :eq, :Color, _args, :bool} = last
    end
  end

  # =========================================================================
  # Code Generation + Runtime — ADT Instances
  # =========================================================================

  describe "codegen: ADT eq" do
    test "(eq (Red) (Red)) => true" do
      code = """
      (deftype Color (Red) (Green) (Blue))
      (instance Eq Color
        (eq [x y] (== x y)))
      (eq (Red) (Red))
      """
      {:ok, mod} = Runner.compile_and_load(code, :AdtEqTrue, backend: :core)
      assert true == Runner.call(mod, :main)
    end

    test "(eq (Red) (Blue)) => false" do
      code = """
      (deftype Color (Red) (Green) (Blue))
      (instance Eq Color
        (eq [x y] (== x y)))
      (eq (Red) (Blue))
      """
      {:ok, mod} = Runner.compile_and_load(code, :AdtEqFalse, backend: :core)
      assert false == Runner.call(mod, :main)
    end

    test "(neq (Red) (Blue)) => true via default" do
      code = """
      (deftype Color (Red) (Green) (Blue))
      (instance Eq Color
        (eq [x y] (== x y)))
      (neq (Red) (Blue))
      """
      {:ok, mod} = Runner.compile_and_load(code, :AdtNeqDefault, backend: :core)
      assert true == Runner.call(mod, :main)
    end

    test "show ADT with match" do
      code = """
      (deftype Color (Red) (Green) (Blue))
      (instance Show Color
        (show [x] (match x
          [(Red) "red"]
          [(Green) "green"]
          [(Blue) "blue"])))
      (show (Green))
      """
      {:ok, mod} = Runner.compile_and_load(code, :AdtShowMatch, backend: :core)
      assert "green" == Runner.call(mod, :main)
    end

    test "parametric ADT eq" do
      code = """
      (deftype Maybe (Just v) (Nothing))
      (instance Eq Maybe
        (eq [x y] (== x y)))
      (eq (Just 1) (Just 1))
      """
      {:ok, mod} = Runner.compile_and_load(code, :AdtParamEq, backend: :core)
      assert true == Runner.call(mod, :main)
    end
  end

  # =========================================================================
  # Parsing — Deriving
  # =========================================================================

  describe "parsing deriving" do
    test "sum type with deriving one class" do
      ast = Parser.parse("(deftype Color (Red) (Green) deriving [Eq])")
      assert {:deftype_deriving, :Color, {:sum, _variants}, [:Eq], _loc} = ast
    end

    test "sum type with deriving multiple classes" do
      ast = Parser.parse("(deftype Color (Red) deriving [Eq Show])")
      assert {:deftype_deriving, :Color, {:sum, _}, [:Eq, :Show], _loc} = ast
    end

    test "product type with deriving" do
      ast = Parser.parse("(deftype Point [x :int y :int] deriving [Eq])")
      assert {:deftype_deriving, :Point, {:product, _}, [:Eq], _loc} = ast
    end

    test "sum type without deriving unchanged" do
      ast = Parser.parse("(deftype Color (Red) (Green))")
      assert {:deftype, :Color, {:sum, _}, _loc} = ast
    end
  end

  # =========================================================================
  # Type Checking — Deriving
  # =========================================================================

  describe "type checking deriving" do
    test "enum with deriving Eq type checks" do
      code = """
      (deftype Color (Red) (Green) (Blue) deriving [Eq])
      (eq (Red) (Blue))
      """
      ast = Parser.parse(code)
      assert {:ok, :module, _} = TypeChecker.check(ast)
    end

    test "enum with deriving Show type checks" do
      code = """
      (deftype Color (Red) (Green) (Blue) deriving [Show])
      (show (Green))
      """
      ast = Parser.parse(code)
      assert {:ok, :module, _} = TypeChecker.check(ast)
    end

    test "enum with deriving [Eq Show] type checks both" do
      code = """
      (deftype Color (Red) (Green) (Blue) deriving [Eq Show])
      (eq (Red) (Blue))
      (show (Green))
      """
      ast = Parser.parse(code)
      assert {:ok, :module, _} = TypeChecker.check(ast)
    end

    test "parametric ADT deriving Eq works" do
      code = """
      (deftype Maybe (Just v) (Nothing) deriving [Eq])
      (eq (Just 1) (Nothing))
      """
      ast = Parser.parse(code)
      assert {:ok, :module, _} = TypeChecker.check(ast)
    end

    test "parametric ADT deriving Show errors (has fields)" do
      code = """
      (deftype Maybe (Just v) (Nothing) deriving [Show])
      (show (Nothing))
      """
      ast = Parser.parse(code)
      result = TypeChecker.check(ast)
      assert {:error, _} = result
    end

    test "record deriving Eq works" do
      code = """
      (deftype Point [x :int y :int] deriving [Eq])
      (eq (Point 1 2) (Point 3 4))
      """
      ast = Parser.parse(code)
      assert {:ok, :module, _} = TypeChecker.check(ast)
    end

    test "record deriving Show errors" do
      code = """
      (deftype Point [x :int y :int] deriving [Show])
      """
      ast = Parser.parse(code)
      result = TypeChecker.check(ast)
      assert {:error, _} = result
    end

    test "unknown class deriving errors" do
      code = """
      (deftype Color (Red) (Blue) deriving [Foo])
      """
      ast = Parser.parse(code)
      result = TypeChecker.check(ast)
      assert {:error, _} = result
    end
  end

  # =========================================================================
  # Code Generation + Runtime — Deriving
  # =========================================================================

  describe "codegen: deriving Eq" do
    test "derived (eq (Red) (Red)) => true" do
      code = """
      (deftype Color (Red) (Green) (Blue) deriving [Eq])
      (eq (Red) (Red))
      """
      {:ok, mod} = Runner.compile_and_load(code, :DeriveEqTrue, backend: :core)
      assert true == Runner.call(mod, :main)
    end

    test "derived (eq (Red) (Blue)) => false" do
      code = """
      (deftype Color (Red) (Green) (Blue) deriving [Eq])
      (eq (Red) (Blue))
      """
      {:ok, mod} = Runner.compile_and_load(code, :DeriveEqFalse, backend: :core)
      assert false == Runner.call(mod, :main)
    end

    test "derived eq on record" do
      code = """
      (deftype Point [x :int y :int] deriving [Eq])
      (eq (Point 1 2) (Point 1 2))
      """
      {:ok, mod} = Runner.compile_and_load(code, :DeriveEqRecord, backend: :core)
      assert true == Runner.call(mod, :main)
    end

    test "derived eq on record (not equal)" do
      code = """
      (deftype Point [x :int y :int] deriving [Eq])
      (eq (Point 1 2) (Point 3 4))
      """
      {:ok, mod} = Runner.compile_and_load(code, :DeriveEqRecordNe, backend: :core)
      assert false == Runner.call(mod, :main)
    end
  end

  describe "codegen: deriving Show" do
    test "derived (show (Green)) => \"Green\"" do
      code = """
      (deftype Color (Red) (Green) (Blue) deriving [Show])
      (show (Green))
      """
      {:ok, mod} = Runner.compile_and_load(code, :DeriveShowGreen, backend: :core)
      assert "Green" == Runner.call(mod, :main)
    end

    test "derived show each variant" do
      code = """
      (deftype Dir (North) (South) deriving [Show])
      (str (show (North)) "," (show (South)))
      """
      {:ok, mod} = Runner.compile_and_load(code, :DeriveShowAll, backend: :core)
      assert "North,South" == Runner.call(mod, :main)
    end
  end

  describe "codegen: deriving both Eq and Show" do
    test "derived Eq + Show combined" do
      code = """
      (deftype Status (Active) (Inactive) deriving [Eq Show])
      (str (show (Active)) ":" (if (eq (Active) (Inactive)) "yes" "no"))
      """
      {:ok, mod} = Runner.compile_and_load(code, :DeriveBoth, backend: :core)
      assert "Active:no" == Runner.call(mod, :main)
    end
  end

  describe "codegen: derived neq via default method" do
    test "derived (neq (Red) (Blue)) => true" do
      code = """
      (deftype Color (Red) (Green) (Blue) deriving [Eq])
      (neq (Red) (Blue))
      """
      {:ok, mod} = Runner.compile_and_load(code, :DeriveNeqDefault, backend: :core)
      assert true == Runner.call(mod, :main)
    end
  end

  # =========================================================================
  # Parsing — Constrained Instances
  # =========================================================================

  describe "parsing constrained instances" do
    test "parses instance with where clause" do
      code = """
      (instance Show (Maybe a) where [(Show a)]
        (show [x] "hi"))
      """
      ast = Parser.parse(code)
      assert {:instance_constrained, :Show, :Maybe, [:a], [{:Show, :a}], _, _} = ast
    end

    test "parses multiple constraints" do
      code = """
      (instance MyClass (Pair a b) where [(Eq a) (Show b)]
        (method1 [x] x))
      """
      ast = Parser.parse(code)
      assert {:instance_constrained, :MyClass, :Pair, [:a, :b], [{:Eq, :a}, {:Show, :b}], _, _} = ast
    end
  end

  # =========================================================================
  # Type Checking — Constrained Instances
  # =========================================================================

  describe "type checking constrained instances" do
    test "constrained Show Maybe type checks" do
      code = """
      (deftype Maybe (Just v) (Nothing))
      (instance Show (Maybe a) where [(Show a)]
        (show [x] (match x
          [(Just v) (str "Just(" (show v) ")")]
          [(Nothing) "Nothing"])))
      (show (Just 42))
      """
      ast = Parser.parse(code)
      assert {:ok, :module, _} = TypeChecker.check(ast)
    end

    test "constrained instance resolves to 7-element class_call" do
      code = """
      (deftype Maybe (Just v) (Nothing))
      (instance Show (Maybe a) where [(Show a)]
        (show [x] "hi"))
      (show (Just 1))
      """
      ast = Parser.parse(code)
      {:ok, :module, {:module, forms}} = TypeChecker.check(ast)
      last = List.last(forms)
      assert {:class_call, :Show, :show, :Maybe, _, :string, [{:Show, :int}]} = last
    end

    test "constraint_call emitted inside constrained instance body" do
      code = """
      (deftype Maybe (Just v) (Nothing))
      (instance Show (Maybe a) where [(Show a)]
        (show [x] (match x
          [(Just v) (show v)]
          [(Nothing) "Nothing"])))
      (show (Just 1))
      """
      ast = Parser.parse(code)
      {:ok, :module, {:module, forms}} = TypeChecker.check(ast)
      # The instance_constrained form should contain a constraint_call in its body
      instance_form = Enum.find(forms, fn
        {:instance_constrained, _, _, _, _, _, _} -> true
        _ -> false
      end)
      assert {:instance_constrained, :Show, :Maybe, [:a], _, methods, :instance} = instance_form
      [{:show, [:x], body}] = methods
      # The body has a match, and the Just branch calls constraint_call
      {:match, _, clauses, _} = body
      [{_, just_body, _} | _] = clauses
      assert {:constraint_call, 0, :show, _, :string} = just_body
    end

    test "constraint not satisfied produces error" do
      code = """
      (deftype Wrapper (Wrap v))
      (deftype Maybe (Just v) (Nothing))
      (instance Show (Maybe a) where [(Show a)]
        (show [x] "hi"))
      (show (Just (Wrap 1)))
      """
      ast = Parser.parse(code)
      result = TypeChecker.check(ast)
      assert {:error, _} = result
    end
  end

  # =========================================================================
  # Code Generation + Runtime — Constrained Instances
  # =========================================================================

  describe "codegen: constrained Show Maybe" do
    test "(show (Just 42)) => \"Just(42)\"" do
      code = """
      (deftype Maybe (Just v) (Nothing))
      (instance Show (Maybe a) where [(Show a)]
        (show [x] (match x
          [(Just v) (str "Just(" (show v) ")")]
          [(Nothing) "Nothing"])))
      (show (Just 42))
      """
      {:ok, mod} = Runner.compile_and_load(code, :ConstrainedShowJustInt, backend: :core)
      assert "Just(42)" == Runner.call(mod, :main)
    end

    test "(show (Nothing)) => \"Nothing\"" do
      code = """
      (deftype Maybe (Just v) (Nothing))
      (instance Show (Maybe a) where [(Show a)]
        (show [x] (match x
          [(Just v) (str "Just(" (show v) ")")]
          [(Nothing) "Nothing"])))
      (show (Nothing))
      """
      {:ok, mod} = Runner.compile_and_load(code, :ConstrainedShowNothing, backend: :core)
      assert "Nothing" == Runner.call(mod, :main)
    end

    test "(show (Just \"hello\")) => \"Just(hello)\"" do
      code = """
      (deftype Maybe (Just v) (Nothing))
      (instance Show (Maybe a) where [(Show a)]
        (show [x] (match x
          [(Just v) (str "Just(" (show v) ")")]
          [(Nothing) "Nothing"])))
      (show (Just "hello"))
      """
      {:ok, mod} = Runner.compile_and_load(code, :ConstrainedShowJustStr, backend: :core)
      assert "Just(hello)" == Runner.call(mod, :main)
    end

    test "(show (Just true)) => \"Just(true)\"" do
      code = """
      (deftype Maybe (Just v) (Nothing))
      (instance Show (Maybe a) where [(Show a)]
        (show [x] (match x
          [(Just v) (str "Just(" (show v) ")")]
          [(Nothing) "Nothing"])))
      (show (Just true))
      """
      {:ok, mod} = Runner.compile_and_load(code, :ConstrainedShowJustBool, backend: :core)
      assert "Just(true)" == Runner.call(mod, :main)
    end
  end

  describe "codegen: constrained Eq Maybe" do
    test "constrained Eq with structural equality" do
      code = """
      (deftype Maybe (Just v) (Nothing))
      (instance Eq (Maybe a) where [(Eq a)]
        (eq [x y] (== x y)))
      (eq (Just 1) (Just 1))
      """
      {:ok, mod} = Runner.compile_and_load(code, :ConstrainedEqTrue, backend: :core)
      assert true == Runner.call(mod, :main)
    end

    test "constrained Eq returns false for different values" do
      code = """
      (deftype Maybe (Just v) (Nothing))
      (instance Eq (Maybe a) where [(Eq a)]
        (eq [x y] (== x y)))
      (eq (Just 1) (Just 2))
      """
      {:ok, mod} = Runner.compile_and_load(code, :ConstrainedEqFalse, backend: :core)
      assert false == Runner.call(mod, :main)
    end
  end

  describe "codegen: constrained instance with user-defined inner type" do
    test "Show Maybe with Show Color inner type" do
      code = """
      (deftype Color (Red) (Green) (Blue))
      (instance Show Color
        (show [x] (match x
          [(Red) "red"]
          [(Green) "green"]
          [(Blue) "blue"])))
      (deftype Maybe (Just v) (Nothing))
      (instance Show (Maybe a) where [(Show a)]
        (show [x] (match x
          [(Just v) (str "Just(" (show v) ")")]
          [(Nothing) "Nothing"])))
      (show (Just (Green)))
      """
      {:ok, mod} = Runner.compile_and_load(code, :ConstrainedShowJustColor, backend: :core)
      assert "Just(green)" == Runner.call(mod, :main)
    end
  end

  # =========================================================================
  # Constrained Instances — Default Methods
  # =========================================================================

  describe "codegen: constrained instance with default methods" do
    test "neq inherited by constrained Eq instance" do
      code = """
      (deftype Maybe (Just v) (Nothing))
      (instance Eq (Maybe a) where [(Eq a)]
        (eq [x y] (== x y)))
      (neq (Just 1) (Just 2))
      """
      {:ok, mod} = Runner.compile_and_load(code, :ConstrainedNeqDefault, backend: :core)
      assert true == Runner.call(mod, :main)
    end

    test "neq returns false when equal" do
      code = """
      (deftype Maybe (Just v) (Nothing))
      (instance Eq (Maybe a) where [(Eq a)]
        (eq [x y] (== x y)))
      (neq (Just 1) (Just 1))
      """
      {:ok, mod} = Runner.compile_and_load(code, :ConstrainedNeqSame, backend: :core)
      assert false == Runner.call(mod, :main)
    end
  end

  # =========================================================================
  # Constrained Instances — In defn and let
  # =========================================================================

  describe "codegen: constrained instance in defn" do
    test "constrained show called from defn" do
      code = """
      (deftype Maybe (Just v) (Nothing))
      (instance Show (Maybe a) where [(Show a)]
        (show [x] (match x
          [(Just v) (str "Just(" (show v) ")")]
          [(Nothing) "Nothing"])))
      (defn show-int-maybe [x :int] :string
        (show (Just x)))
      (show-int-maybe 99)
      """
      {:ok, mod} = Runner.compile_and_load(code, :ConstrainedInDefn, backend: :core)
      assert "Just(99)" == Runner.call(mod, :main)
    end
  end

  describe "codegen: constrained instance in let" do
    test "constrained show in let binding" do
      code = """
      (deftype Maybe (Just v) (Nothing))
      (instance Show (Maybe a) where [(Show a)]
        (show [x] (match x
          [(Just v) (str "Just(" (show v) ")")]
          [(Nothing) "Nothing"])))
      (let [s (show (Just 42))] s)
      """
      {:ok, mod} = Runner.compile_and_load(code, :ConstrainedInLet, backend: :core)
      assert "Just(42)" == Runner.call(mod, :main)
    end
  end

  # =========================================================================
  # Constrained Instances — Error Cases
  # =========================================================================

  describe "error: constrained instance errors" do
    test "unknown class in where clause errors" do
      code = """
      (deftype Maybe (Just v) (Nothing))
      (instance Show (Maybe a) where [(NonExistent a)]
        (show [x] "hi"))
      (show (Just 1))
      """
      ast = Parser.parse(code)
      result = TypeChecker.check(ast)
      assert {:error, _} = result
    end

    test "missing instance for chained constraint" do
      code = """
      (deftype Wrapper (Wrap v))
      (deftype Maybe (Just v) (Nothing))
      (instance Show (Maybe a) where [(Show a)]
        (show [x] (match x
          [(Just v) (str "Just(" (show v) ")")]
          [(Nothing) "Nothing"])))
      (show (Just (Wrap 1)))
      """
      ast = Parser.parse(code)
      result = TypeChecker.check(ast)
      {:error, errs} = result
      err = if is_list(errs), do: hd(errs), else: errs
      msg = error_text(err)
      assert msg =~ "no instance of"
    end

    test "unsatisfied constraint includes context message" do
      code = """
      (deftype Wrapper (Wrap v))
      (deftype Maybe (Just v) (Nothing))
      (instance Show (Maybe a) where [(Show a)]
        (show [x] "hi"))
      (show (Just (Wrap 1)))
      """
      ast = Parser.parse(code)
      {:error, errs} = TypeChecker.check(ast)
      err = if is_list(errs), do: hd(errs), else: errs
      msg = error_text(err)
      assert msg =~ "no instance of"
      assert msg =~ "required by constrained instance"
    end
  end

  # =========================================================================
  # Chained Constraints (v6)
  # =========================================================================

  describe "codegen: chained constraints — Show Maybe(Maybe)" do
    test "(show (Just (Just 42))) => nested show" do
      code = """
      (deftype Maybe (Just v) (Nothing))
      (instance Show (Maybe a) where [(Show a)]
        (show [x] (match x
          [(Just v) (str "Just(" (show v) ")")]
          [(Nothing) "Nothing"])))
      (show (Just (Just 42)))
      """
      {:ok, mod} = Runner.compile_and_load(code, :ChainedShowJustJust, backend: :core)
      assert "Just(Just(42))" == Runner.call(mod, :main)
    end

    test "(show (Just (Just (Just 42)))) => triple chain" do
      code = """
      (deftype Maybe (Just v) (Nothing))
      (instance Show (Maybe a) where [(Show a)]
        (show [x] (match x
          [(Just v) (str "Just(" (show v) ")")]
          [(Nothing) "Nothing"])))
      (show (Just (Just (Just 42))))
      """
      {:ok, mod} = Runner.compile_and_load(code, :ChainedShowTriple, backend: :core)
      assert "Just(Just(Just(42)))" == Runner.call(mod, :main)
    end

    test "(show (Just (Nothing))) => chained with nullary" do
      code = """
      (deftype Maybe (Just v) (Nothing))
      (instance Show (Maybe a) where [(Show a)]
        (show [x] (match x
          [(Just v) (str "Just(" (show v) ")")]
          [(Nothing) "Nothing"])))
      (show (Just (Nothing)))
      """
      {:ok, mod} = Runner.compile_and_load(code, :ChainedShowJustNothing, backend: :core)
      assert "Just(Nothing)" == Runner.call(mod, :main)
    end
  end

  describe "codegen: chained constraints — Eq Maybe(Maybe)" do
    test "(eq (Just (Just 1)) (Just (Just 1))) => true" do
      code = """
      (deftype Maybe (Just v) (Nothing))
      (instance Eq (Maybe a) where [(Eq a)]
        (eq [x y] (== x y)))
      (eq (Just (Just 1)) (Just (Just 1)))
      """
      {:ok, mod} = Runner.compile_and_load(code, :ChainedEqTrue, backend: :core)
      assert true == Runner.call(mod, :main)
    end

    test "(eq (Just (Just 1)) (Just (Just 2))) => false" do
      code = """
      (deftype Maybe (Just v) (Nothing))
      (instance Eq (Maybe a) where [(Eq a)]
        (eq [x y] (== x y)))
      (eq (Just (Just 1)) (Just (Just 2)))
      """
      {:ok, mod} = Runner.compile_and_load(code, :ChainedEqFalse, backend: :core)
      assert false == Runner.call(mod, :main)
    end
  end

  describe "type checking: chained constraints produce constrained_ref" do
    test "class_call contains {:constrained_ref, ...} for nested constrained instance" do
      code = """
      (deftype Maybe (Just v) (Nothing))
      (instance Show (Maybe a) where [(Show a)]
        (show [x] (match x
          [(Just v) (str "Just(" (show v) ")")]
          [(Nothing) "Nothing"])))
      (show (Just (Just 42)))
      """
      ast = Parser.parse(code)
      {:ok, _type, typed_ast} = TypeChecker.check(ast)
      # The typed AST should contain a 7-element class_call with constrained_ref in resolved constraints
      class_call = find_class_call(typed_ast)
      assert {:class_call, :Show, :show, :Maybe, _args, _ret, resolved} = class_call
      assert [{:constrained_ref, :Show, :Maybe, sub}] = resolved
      # The sub-constraint should resolve to a flat Show :int
      assert [{:Show, :int}] = sub
    end
  end

  defp find_class_call(ast) when is_list(ast) do
    Enum.find_value(ast, &find_class_call/1)
  end
  describe "multi-constraint resolution" do
    test "instance with two where constraints type checks" do
      code = """
      (deftype Pair [fst :int snd :int])
      (instance Eq Pair
        (eq [a b] true))
      (instance Show Pair
        (show [x] "pair"))
      (deftype Box (MkBox v))
      (instance Show (Box a) where [(Show a) (Eq a)]
        (show [x] (match x
          [(MkBox v) (str "Box(" (show v) ")")])))
      (show (MkBox (Pair 1 2)))
      """
      ast = Parser.parse(code)
      assert {:ok, :module, _} = TypeChecker.check(ast)
    end
  end

  defp find_class_call({:class_call, _, _, _, _, _, _} = call), do: call
  defp find_class_call(tuple) when is_tuple(tuple) do
    tuple
    |> Tuple.to_list()
    |> Enum.find_value(&find_class_call/1)
  end
  defp find_class_call(_), do: nil

  # =========================================================================
  # Structured Type Class Errors
  # =========================================================================

  describe "structured type class errors" do
    test "unknown_type_class produces structured error" do
      err = Vaisto.Errors.unknown_type_class(:Printable)
      assert %Error{message: "unknown type class"} = err
      assert err.note =~ "Printable"
    end

    test "unknown_type_class_in_constraint produces structured error" do
      err = Vaisto.Errors.unknown_type_class_in_constraint(:Printable)
      assert %Error{message: "unknown type class in constraint"} = err
      assert err.note =~ "Printable"
      assert err.hint != nil
    end

    test "missing_instance_methods produces structured error" do
      err = Vaisto.Errors.missing_instance_methods(:Show, :int, [:show, :display])
      assert %Error{message: "missing instance methods"} = err
      assert err.note =~ "missing methods"
      assert err.note =~ "show"
      assert err.note =~ "display"
    end

    test "no_instance_for_type produces structured error" do
      err = Vaisto.Errors.no_instance_for_type(:Show, :int)
      assert err.message =~ "no instance of"
      assert err.message =~ "Show"
      assert err.message =~ "Int"
    end

    test "no_instance_for_type with required_by includes context" do
      err = Vaisto.Errors.no_instance_for_type(:Show, :int, required_by: "Show Maybe")
      text = error_text(err)
      assert text =~ "no instance of"
      assert text =~ "required by constrained instance"
    end

    test "constraint_depth_exceeded produces structured error" do
      err = Vaisto.Errors.constraint_depth_exceeded()
      assert %Error{message: "constraint resolution depth exceeded"} = err
      assert err.note != nil
      assert err.hint != nil
    end

    test "derive_show_has_fields produces structured error" do
      err = Vaisto.Errors.derive_show_has_fields(:Color)
      assert %Error{message: "cannot derive Show"} = err
      assert err.note =~ "Color"
      assert err.hint =~ "manual"
    end

    test "derive_show_record produces structured error" do
      err = Vaisto.Errors.derive_show_record(:Point)
      assert %Error{message: "cannot derive Show"} = err
      assert err.note =~ "Point"
    end

    test "derive_not_supported produces structured error" do
      err = Vaisto.Errors.derive_not_supported(:Functor)
      assert err.message =~ "cannot derive"
      assert err.message =~ "Functor"
      assert err.note =~ "only Eq and Show"
    end

    test "unknown_type_class suggests similar name" do
      err = Vaisto.Errors.unknown_type_class(:Sho, [:Show, :Eq, :Ord])
      assert err.hint =~ "did you mean `Show`?"
    end

    test "unknown_type_class no suggestion for unrelated name" do
      err = Vaisto.Errors.unknown_type_class(:Zzzzz, [:Show, :Eq, :Ord])
      assert err.hint == nil
    end

    test "unknown_type_class_in_constraint suggests similar name" do
      err = Vaisto.Errors.unknown_type_class_in_constraint(:Sho, [:Show, :Eq])
      assert err.hint =~ "did you mean `Show`?"
    end

    test "unknown_type_class_in_constraint falls back without suggestion" do
      err = Vaisto.Errors.unknown_type_class_in_constraint(:Zzzzz, [:Show, :Eq])
      assert err.hint =~ "constraints must reference"
    end
  end
end
