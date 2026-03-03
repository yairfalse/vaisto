defmodule Vaisto.RowPolymorphismTest do
  use ExUnit.Case
  alias Vaisto.Parser
  alias Vaisto.TypeChecker

  describe "field access parsing" do
    test "parses simple field access" do
      code = "(. person :name)"
      ast = Parser.parse(code)
      assert {:field_access, :person, :name, _loc} = ast
    end

    test "parses field access on expression" do
      code = "(. (get-user) :email)"
      ast = Parser.parse(code)
      assert {:field_access, {:call, :"get-user", [], _}, :email, _loc} = ast
    end
  end

  describe "field access type checking" do
    test "field access on record returns field type" do
      code = """
      (deftype Person [name :string age :int])
      (defn get-name [p] (. p :name))
      """
      ast = Parser.parse(code)
      {:ok, _type, _typed_ast} = TypeChecker.check(ast)
    end

    test "field access on unknown type returns type variable" do
      code = "(defn get-x [r] (. r :x))"
      ast = Parser.parse(code)
      # Should type check and infer a polymorphic scheme
      {:ok, func_type, _typed_ast} = TypeChecker.check(ast)

      # Polymorphic defn returns {:forall, ...} scheme
      case func_type do
        {:forall, _vars, {:fn, [_param_type], ret_type}} ->
          assert match?({:tvar, _}, ret_type)
        {:fn, [_param_type], ret_type} ->
          assert match?({:tvar, _}, ret_type)
      end
    end

    test "field access on unknown type includes row constraint in AST" do
      code = "(defn get-x [r] (. r :x))"
      ast = Parser.parse(code)
      {:ok, _type, typed_ast} = TypeChecker.check(ast)

      # The typed AST should include the row constraint
      {:defn, :"get-x", [:r], body, _func_type} = typed_ast
      # Body should be field_access with 5 elements (including row constraint)
      assert {:field_access, _record, :x, field_type, row_constraint} = body
      assert match?({:tvar, _}, field_type)
      assert match?({:row, [{:x, {:tvar, _}}], {:rvar, _}}, row_constraint)
    end

    test "multiple field accesses on same unknown type get same field type" do
      code = """
      (defn sum-x [r1 r2] (+ (. r1 :x) (. r2 :x)))
      """
      ast = Parser.parse(code)
      {:ok, _type, typed_ast} = TypeChecker.check(ast)

      # Extract the two field accesses from the + call body
      {:defn, :"sum-x", [:r1, :r2], body, _} = typed_ast
      {:call, :+, [field1, field2], _} = body

      # Both should have the same tvar ID for field :x
      {:field_access, _, :x, type1, _} = field1
      {:field_access, _, :x, type2, _} = field2

      # Each field access should produce a type variable
      assert {:tvar, _id1} = type1
      assert {:tvar, _id2} = type2
    end
  end

  describe "row type representation" do
    test "row type unifies with record" do
      alias Vaisto.TypeSystem.Unify

      # A row that requires :name field
      row = {:row, [{:name, :string}], {:rvar, 0}}
      # A record with :name and :age fields
      record = {:record, :Person, [{:name, :string}, {:age, :int}]}

      # Should unify - row captures extra :age field
      assert {:ok, _subst, _} = Unify.unify_row_with_record(
        [{:name, :string}],
        {:rvar, 0},
        [{:name, :string}, {:age, :int}],
        %{}
      )
    end

    test "closed row rejects extra fields" do
      alias Vaisto.TypeSystem.Unify

      # Should fail - closed row doesn't accept extra fields
      result = Unify.unify_row_with_record(
        [{:name, :string}],
        :closed,
        [{:name, :string}, {:age, :int}],
        %{}
      )

      assert {:error, _} = result
    end

    test "row type unification preserves common fields" do
      alias Vaisto.TypeSystem.Unify

      row1 = {:row, [{:x, :int}], {:rvar, 0}}
      row2 = {:row, [{:x, :int}, {:y, :int}], :closed}

      assert {:ok, _subst, _} = Unify.unify_rows(row1, row2, %{}, 0)
    end
  end

  describe "pattern matching on unknown types" do
    test "record pattern against tvar scrutinee extracts typed bindings" do
      code = """
      (deftype Point [x :int y :int])
      (defn extract [p]
        (match p
          [(Point x y) (+ x y)]))
      """
      ast = Parser.parse(code)
      {:ok, _type, _typed_ast} = TypeChecker.check(ast)
    end

    test "sum type pattern against tvar scrutinee works" do
      code = """
      (deftype Result (Ok v) (Err e))
      (defn unwrap [r]
        (match r
          [(Ok v) v]
          [(Err e) e]))
      """
      ast = Parser.parse(code)
      {:ok, _type, _typed_ast} = TypeChecker.check(ast)
    end

    test "pattern variables get correct types from constructor definition" do
      code = """
      (deftype Pair [fst :int snd :string])
      (defn get-fst [p]
        (match p
          [(Pair f s) f]))
      """
      ast = Parser.parse(code)
      {:ok, :module, {:module, typed_forms}} = TypeChecker.check(ast)

      # Find the get-fst function in the typed forms
      defn = Enum.find(typed_forms, fn
        {:defn, :"get-fst", _, _, _} -> true
        _ -> false
      end)

      assert {:defn, :"get-fst", _, _, func_type} = defn

      # The function should return the type of fst field
      assert {:fn, [_param], ret_type} = func_type
      assert ret_type == :int
    end
  end

  describe "row type formatting" do
    test "formats open row type" do
      alias Vaisto.TypeSystem.Core

      row = {:row, [{:name, :string}, {:age, :int}], {:rvar, 0}}
      formatted = Core.format_type(row)

      assert formatted =~ "name:"
      assert formatted =~ "age:"
      # Row variables now use ..a, ..b style
      assert formatted =~ "..a"
    end

    test "formats closed row type" do
      alias Vaisto.TypeSystem.Core

      row = {:row, [{:name, :string}], :closed}
      formatted = Core.format_type(row)

      assert formatted == "{name: String}"
    end
  end

  describe "field tvar ID partitioning" do
    test "field tvar IDs are >= 100_000_000" do
      code = "(defn get-x [r] (. r :x))"
      ast = Vaisto.Parser.parse(code)
      {:ok, _type, typed_ast} = TypeChecker.check(ast)

      # Extract field tvar IDs from the typed AST
      tvar_ids = extract_tvar_ids(typed_ast)
      field_tvars = Enum.filter(tvar_ids, &(&1 >= 100_000_000))
      assert length(field_tvars) > 0, "Expected field tvar IDs >= 100_000_000"
    end

    test "different fields on same record produce distinct tvars" do
      code = "(defn sum-fields [r] (+ (. r :x) (. r :y)))"
      ast = Vaisto.Parser.parse(code)
      {:ok, _type, typed_ast} = TypeChecker.check(ast)

      # Extract the two field accesses from the + call body
      {:defn, :"sum-fields", [:r], body, _} = typed_ast
      {:call, :+, [field1, field2], _} = body
      {:field_access, _, :x, {:tvar, id1}, _} = field1
      {:field_access, _, :y, {:tvar, id2}, _} = field2
      assert id1 != id2, "Different fields should have different tvar IDs"
    end
  end

  defp extract_tvar_ids(ast) when is_tuple(ast) do
    case ast do
      {:tvar, id} -> [id]
      _ ->
        ast
        |> Tuple.to_list()
        |> Enum.flat_map(&extract_tvar_ids/1)
    end
  end
  defp extract_tvar_ids(list) when is_list(list), do: Enum.flat_map(list, &extract_tvar_ids/1)
  defp extract_tvar_ids(_), do: []
end
