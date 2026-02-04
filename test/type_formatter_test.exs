defmodule Vaisto.TypeFormatterTest do
  use ExUnit.Case
  alias Vaisto.TypeFormatter

  describe "format/1 primitives" do
    test "formats int" do
      assert TypeFormatter.format(:int) == "Int"
    end

    test "formats float" do
      assert TypeFormatter.format(:float) == "Float"
    end

    test "formats bool" do
      assert TypeFormatter.format(:bool) == "Bool"
    end

    test "formats string" do
      assert TypeFormatter.format(:string) == "String"
    end

    test "formats atom" do
      assert TypeFormatter.format(:atom) == "Atom"
    end

    test "formats any" do
      assert TypeFormatter.format(:any) == "Any"
    end

    test "formats unit" do
      assert TypeFormatter.format(:unit) == "()"
    end

    test "formats ok" do
      assert TypeFormatter.format(:ok) == "Ok"
    end

    test "formats num" do
      assert TypeFormatter.format(:num) == "Num"
    end
  end

  describe "format/1 atom literals" do
    test "formats atom literal" do
      assert TypeFormatter.format({:atom, :ok}) == "Atom(:ok)"
    end

    test "formats complex atom literal" do
      assert TypeFormatter.format({:atom, :my_atom}) == "Atom(:my_atom)"
    end
  end

  describe "format/1 function types" do
    test "formats nullary function" do
      assert TypeFormatter.format({:fn, [], :int}) == "() -> Int"
    end

    test "formats unary function" do
      assert TypeFormatter.format({:fn, [:int], :int}) == "(Int) -> Int"
    end

    test "formats binary function" do
      assert TypeFormatter.format({:fn, [:int, :int], :int}) == "(Int, Int) -> Int"
    end

    test "formats function with different arg/return types" do
      assert TypeFormatter.format({:fn, [:int, :string], :bool}) == "(Int, String) -> Bool"
    end

    test "formats higher-order function" do
      inner_fn = {:fn, [:int], :int}
      assert TypeFormatter.format({:fn, [inner_fn], :int}) == "((Int) -> Int) -> Int"
    end
  end

  describe "format/1 list types" do
    test "formats list of ints" do
      assert TypeFormatter.format({:list, :int}) == "List(Int)"
    end

    test "formats list of strings" do
      assert TypeFormatter.format({:list, :string}) == "List(String)"
    end

    test "formats nested list" do
      assert TypeFormatter.format({:list, {:list, :int}}) == "List(List(Int))"
    end
  end

  describe "format/1 tuple types" do
    test "formats simple tuple" do
      assert TypeFormatter.format({:tuple, [:int, :string]}) == "(Int, String)"
    end

    test "formats triple tuple" do
      assert TypeFormatter.format({:tuple, [:int, :string, :bool]}) == "(Int, String, Bool)"
    end
  end

  describe "format/1 record types" do
    test "formats named record" do
      assert TypeFormatter.format({:record, :Person, [{:name, :string}, {:age, :int}]}) == "Person"
    end
  end

  describe "format/1 sum types" do
    test "formats named sum type" do
      assert TypeFormatter.format({:sum, :Result, [{:Ok, [:any]}, {:Err, [:string]}]}) == "Result"
    end
  end

  describe "format/1 pid types" do
    test "formats typed pid" do
      assert TypeFormatter.format({:pid, :Counter, [:increment, :get]}) == "Pid(Counter)"
    end
  end

  describe "format/1 process types" do
    test "formats process with int state" do
      assert TypeFormatter.format({:process, :int, [:increment, :get]}) == "Process(Int)"
    end
  end

  describe "format/1 type variables" do
    test "formats integer type variable" do
      assert TypeFormatter.format({:tvar, 0}) == "t0"
    end

    test "formats named type variable" do
      assert TypeFormatter.format({:tvar, :a}) == "a"
    end
  end

  describe "format/1 row types" do
    test "formats closed row" do
      assert TypeFormatter.format({:row, [{:name, :string}], :closed}) == "{name: String}"
    end

    test "formats open row with row variable" do
      assert TypeFormatter.format({:row, [{:name, :string}], {:rvar, 0}}) == "{name: String | r0}"
    end

    test "formats multiple fields" do
      row = {:row, [{:x, :int}, {:y, :int}], :closed}
      assert TypeFormatter.format(row) == "{x: Int, y: Int}"
    end
  end

  describe "format/1 fallbacks" do
    test "formats unknown atom as string" do
      assert TypeFormatter.format(:custom_type) == "custom_type"
    end

    test "formats unknown structure with inspect" do
      assert TypeFormatter.format(%{unknown: :struct}) == "%{unknown: :struct}"
    end
  end

  describe "format_list/1" do
    test "formats empty list" do
      assert TypeFormatter.format_list([]) == ""
    end

    test "formats single type" do
      assert TypeFormatter.format_list([:int]) == "Int"
    end

    test "formats multiple types" do
      assert TypeFormatter.format_list([:int, :string, :bool]) == "Int, String, Bool"
    end
  end

  describe "format_annotation/1" do
    test "formats atom type as annotation" do
      assert TypeFormatter.format_annotation(:int) == ":int"
    end

    test "formats complex type normally" do
      assert TypeFormatter.format_annotation({:list, :int}) == "List(Int)"
    end
  end
end
