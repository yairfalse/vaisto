defmodule Vaisto.TypeCheckerTest do
  use ExUnit.Case
  alias Vaisto.TypeChecker

  describe "literals" do
    test "integers have type :int" do
      assert {:ok, :int, _} = TypeChecker.check(42)
    end

    test "floats have type :float" do
      assert {:ok, :float, _} = TypeChecker.check(3.14)
    end
  end

  describe "function calls" do
    test "addition of integers returns :int" do
      ast = {:call, :+, [1, 2]}
      assert {:ok, :int, _} = TypeChecker.check(ast)
    end

    test "rejects addition of integer and atom" do
      ast = {:call, :+, [1, :atom]}
      assert {:error, _} = TypeChecker.check(ast)
    end

    test "rejects function call with insufficient arguments" do
      ast = {:call, :+, [1]}
      assert {:error, _} = TypeChecker.check(ast)
    end
  end

  describe "supervision" do
    test ":one_for_one strategy passes" do
      ast = {:supervise, :one_for_one, []}
      assert {:ok, :supervisor, _} = TypeChecker.check(ast)
    end

    test ":all_for_one strategy passes" do
      ast = {:supervise, :all_for_one, []}
      assert {:ok, :supervisor, _} = TypeChecker.check(ast)
    end

    test ":rest_for_one strategy passes" do
      ast = {:supervise, :rest_for_one, []}
      assert {:ok, :supervisor, _} = TypeChecker.check(ast)
    end

    test "invalid strategy fails" do
      ast = {:supervise, :bogus_strategy, []}
      assert {:error, _} = TypeChecker.check(ast)
    end
  end

  describe "typed PIDs" do
    # Helper to create an env with a counter process defined
    defp env_with_counter do
      # Base primitives + counter process type
      %{
        :+ => {:fn, [:int, :int], :int},
        :counter => {:process, :int, [:increment, :get]}
      }
    end

    test "spawn returns typed PID" do
      env = env_with_counter()
      ast = {:call, :spawn, [:counter, 0]}

      assert {:ok, {:pid, :counter, [:increment, :get]}, _} = TypeChecker.check(ast, env)
    end

    test "send valid message to typed PID succeeds" do
      env = env_with_counter()
      spawn_ast = {:call, :spawn, [:counter, 0]}
      ast = {:call, :"!", [spawn_ast, :increment]}

      assert {:ok, :ok, _} = TypeChecker.check(ast, env)
    end

    test "send invalid message to typed PID fails" do
      env = env_with_counter()
      spawn_ast = {:call, :spawn, [:counter, 0]}
      ast = {:call, :"!", [spawn_ast, :invalid_msg]}

      result = TypeChecker.check(ast, env)
      assert {:error, error} = result
      # Can be either a structured error or a string (for backward compat)
      if is_binary(error) do
        assert error =~ "does not accept message"
      else
        assert error.note =~ "does not accept"
      end
    end

    test "full module with valid send compiles" do
      code = """
      (process counter 0
        :increment (+ state 1)
        :get state)
      (! (spawn counter 0) :increment)
      """
      ast = Vaisto.Parser.parse(code)
      assert {:ok, :module, _} = TypeChecker.check(ast)
    end

    test "full module with invalid send fails" do
      code = """
      (process counter 0
        :increment (+ state 1)
        :get state)
      (! (spawn counter 0) :reset)
      """
      ast = Vaisto.Parser.parse(code)
      result = TypeChecker.check(ast)
      case result do
        {:error, error} when is_binary(error) ->
          assert error =~ "does not accept message :reset"
        {:error, [error | _]} ->
          assert error.note =~ "does not accept `:reset`"
        {:error, %Vaisto.Error{} = error} ->
          assert error.note =~ "does not accept `:reset`"
      end
    end
  end

  describe "typed record fields" do
    test "parses typed record syntax into product type with field types" do
      code = "(deftype point [x :int y :int])"
      ast = Vaisto.Parser.parse(code)
      # Product types are now wrapped with {:product, fields}
      assert {:deftype, :point, {:product, [{:x, {:atom, :int}}, {:y, {:atom, :int}}]}, %Vaisto.Parser.Loc{}} = ast
    end

    test "parser converts legacy untyped syntax to :any fields" do
      code = "(deftype point x y)"
      ast = Vaisto.Parser.parse(code)
      assert {:deftype, :point, {:product, [{:x, :any}, {:y, :any}]}, %Vaisto.Parser.Loc{}} = ast
    end

    test "typed record constructor has correct field types" do
      code = "(deftype point [x :int y :int])"
      ast = Vaisto.Parser.parse(code)
      {:ok, record_type, _typed_ast} = TypeChecker.check(ast)
      assert {:record, :point, [{:x, :int}, {:y, :int}]} = record_type
    end

    test "pattern matching extracts field types" do
      code = """
      (deftype point [x :int y :int])
      (match (point 1 2) [(point a b) (+ a b)])
      """
      ast = Vaisto.Parser.parse(code)
      # This should succeed because a and b get type :int from pattern
      assert {:ok, :module, _} = TypeChecker.check(ast)
    end

    test "infers field types when destructuring typed records in match" do
      code = """
      (deftype point [x :int y :int])
      (match (point 1 2) [(point a b) a])
      """
      ast = Vaisto.Parser.parse(code)
      {:ok, :module, {:module, [_deftype, match_expr]}} = TypeChecker.check(ast)
      # The match result should be :int (the type of 'a' which comes from field x)
      {:match, _expr, _clauses, result_type} = match_expr
      assert result_type == :int
    end

    test "constructor enforces field types" do
      code = """
      (deftype point [x :int y :int])
      (point 1 2)
      """
      ast = Vaisto.Parser.parse(code)
      {:ok, :module, {:module, [_deftype, call_expr]}} = TypeChecker.check(ast)
      {:call, :point, _args, result_type} = call_expr
      assert {:record, :point, [{:x, :int}, {:y, :int}]} = result_type
    end
  end

  describe "error messages with location" do
    test "type mismatch includes line and column" do
      code = "(+ 1 :atom)"
      ast = Vaisto.Parser.parse(code)
      {:error, error} = TypeChecker.check(ast)
      # Now returns structured error
      assert %Vaisto.Error{} = error
      assert error.message == "type mismatch"
      assert error.primary_span.line == 1
      assert error.primary_span.col == 1
    end

    test "type mismatch includes filename when provided" do
      code = "(+ 1 :atom)"
      ast = Vaisto.Parser.parse(code, file: "test.va")
      {:error, error} = TypeChecker.check(ast)
      assert %Vaisto.Error{} = error
      assert error.file == "test.va"
      assert error.primary_span.line == 1
    end

    test "nested error shows innermost location" do
      code = """
      (+ 1
         (+ 2 :bad))
      """
      ast = Vaisto.Parser.parse(code, file: "nested.va")
      {:error, error} = TypeChecker.check(ast)
      # Should show innermost call location (line 2)
      assert %Vaisto.Error{} = error
      assert error.file == "nested.va"
      assert error.primary_span.line == 2
    end

    test "unknown function includes location" do
      code = "(unknown-func 1 2)"
      ast = Vaisto.Parser.parse(code)
      {:error, error} = TypeChecker.check(ast)
      assert %Vaisto.Error{} = error
      assert error.message == "unknown function"
      assert error.primary_span.line == 1
    end
  end

  describe "check_with_source/3 - formatted errors" do
    test "formats type mismatch with source snippet" do
      code = "(+ 1 :atom)"
      ast = Vaisto.Parser.parse(code, file: "test.va")

      {:error, formatted} = TypeChecker.check_with_source(ast, code)

      # Should have formatted error with source context
      assert formatted =~ "error"
      assert formatted =~ "test.va"
      assert formatted =~ "(+ 1 :atom)"
      assert formatted =~ "^"  # Pointer
    end

    test "formats error on correct line in multiline code" do
      code = """
      (+ 1 2)
      (+ 3 :bad)
      """
      ast = Vaisto.Parser.parse(code, file: "multi.va")

      {:error, formatted} = TypeChecker.check_with_source(ast, code)

      assert formatted =~ "multi.va"
      assert formatted =~ "(+ 3 :bad)"
    end

    test "passes through success results unchanged" do
      code = "(+ 1 2)"
      ast = Vaisto.Parser.parse(code)

      result = TypeChecker.check_with_source(ast, code)

      assert {:ok, :int, _} = result
    end

    test "collects multiple errors from module" do
      code = """
      (defn foo [x :int] :string x)
      (defn bar [] :int (+ 1 "hello"))
      """
      ast = Vaisto.Parser.parse(code)

      # check returns {:error, list} for multiple errors
      result = TypeChecker.check(ast)
      assert {:error, errors} = result
      assert length(errors) == 2

      # check_with_source formats all errors
      {:error, formatted} = TypeChecker.check_with_source(ast, code)
      assert formatted =~ "return type mismatch"
      assert formatted =~ "type mismatch"
    end
  end

  describe "Hindley-Milner inference" do
    test "infers polymorphic type variable for identity function" do
      # (fn [x] x) should infer a polymorphic type
      ast = {:fn, [:x], :x}
      {:ok, type, _} = TypeChecker.infer(ast)

      # Should be {:fn, [tvar], tvar} where both are the same
      assert {:fn, [param_type], ret_type} = type
      assert param_type == ret_type
    end

    test "infers int -> int for arithmetic function" do
      # (fn [x] (+ x 1)) should infer int -> int
      ast = {:fn, [:x], {:call, :+, [:x, 1]}}
      {:ok, type, _} = TypeChecker.infer(ast)

      assert type == {:fn, [:int], :int}
    end

    test "check uses inference for anonymous functions" do
      # When check encounters an anonymous function, it should use inference
      ast = {:fn, [:x], {:call, :+, [:x, 1]}}
      {:ok, type, _} = TypeChecker.check(ast)

      # Should get int -> int, not any -> int
      assert {:fn, [:int], :int} = type
    end

    test "let-polymorphism works" do
      # (let [id (fn [x] x)] (+ (id 1) 2))
      ast = {:let, [{:id, {:fn, [:x], :x}}],
             {:call, :+, [{:call, :id, [1]}, 2]}}
      {:ok, type, _} = TypeChecker.infer(ast)

      assert type == :int
    end
  end

  describe "polymorphic == and != operators" do
    test "(== 1 2) passes — same type" do
      ast = Vaisto.Parser.parse("(== 1 2)")
      assert {:ok, :bool, _} = TypeChecker.check(ast)
    end

    test "(== 1 \"a\") errors — different types" do
      ast = Vaisto.Parser.parse(~s|(== 1 "a")|)
      assert {:error, _} = TypeChecker.check(ast)
    end

    test "(!= 1 \"a\") errors — different types" do
      ast = Vaisto.Parser.parse(~s|(!= 1 "a")|)
      assert {:error, _} = TypeChecker.check(ast)
    end

    test "(== 1 1.0) passes — numeric widening" do
      ast = Vaisto.Parser.parse("(== 1 1.0)")
      assert {:ok, :bool, _} = TypeChecker.check(ast)
    end
  end

  describe "match clause type unification" do
    test "match with int and float arms unifies to num" do
      code = """
      (match true [true 1] [false 1.5])
      """
      ast = Vaisto.Parser.parse(code)
      {:ok, :num, {:match, _, _, :num}} = TypeChecker.check(ast)
    end

    test "match with defn returning consistent type across arms" do
      code = """
      (defn f [x :int] :int
        (match x
          [0 42]
          [_ x]))
      """
      ast = Vaisto.Parser.parse(code)
      assert {:ok, {:fn, [:int], :int}, _} = TypeChecker.check(ast)
    end
  end
end
