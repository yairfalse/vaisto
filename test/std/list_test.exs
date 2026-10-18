defmodule Vaisto.Std.ListTest do
  use ExUnit.Case
  alias Vaisto.{Compilation, Interface, Runner}

  # Load Std.List module before tests
  setup_all do
    prelude = File.read!("std/prelude.va")
    std_list_source = File.read!("std/List.va")
    full_source = prelude <> "\n\n" <> std_list_source
    {:ok, core} = Compilation.check_liquid(full_source, :"Std.List", format_errors: false)
    {:ok, interface} = Interface.from_core(core)
    {:ok, _} = Runner.compile_and_load(full_source, :"Std.List")
    {:ok, interfaces: %{:"Std.List" => interface}}
  end

  describe "fold operations" do
    test "fold sums a list", %{interfaces: interfaces} do
      source = """
      (import Std.List)
      (Std.List/fold (fn [acc x] (+ acc x)) 0 (list 1 2 3 4))
      """
      assert {:ok, mod} = Runner.compile_and_load(source, :FoldSum, interfaces: interfaces)
      assert Runner.call(mod, :main) == 10
    end

    test "fold-right concatenates strings", %{interfaces: interfaces} do
      source = """
      (import Std.List)
      (Std.List/fold-right (fn [x acc] (str x acc)) "" (list "a" "b" "c"))
      """
      assert {:ok, mod} = Runner.compile_and_load(source, :FoldRight, interfaces: interfaces)
      assert Runner.call(mod, :main) == "abc"
    end
  end

  describe "map and filter" do
    test "map doubles each element", %{interfaces: interfaces} do
      source = """
      (import Std.List)
      (Std.List/map (fn [x] (* x 2)) (list 1 2 3))
      """
      assert {:ok, mod} = Runner.compile_and_load(source, :MapDouble, interfaces: interfaces)
      assert Runner.call(mod, :main) == [2, 4, 6]
    end

    test "filter keeps numbers greater than 3", %{interfaces: interfaces} do
      source = """
      (import Std.List)
      (Std.List/filter (fn [x] (> x 3)) (list 1 2 3 4 5 6))
      """
      assert {:ok, mod} = Runner.compile_and_load(source, :FilterGT3, interfaces: interfaces)
      assert Runner.call(mod, :main) == [4, 5, 6]
    end

    test "filter with empty result", %{interfaces: interfaces} do
      source = """
      (import Std.List)
      (Std.List/filter (fn [x] (> x 100)) (list 1 2 3))
      """
      assert {:ok, mod} = Runner.compile_and_load(source, :FilterEmpty, interfaces: interfaces)
      assert Runner.call(mod, :main) == []
    end
  end

  describe "length" do
    test "length of non-empty list", %{interfaces: interfaces} do
      source = """
      (import Std.List)
      (Std.List/length (list 1 2 3 4 5))
      """
      assert {:ok, mod} = Runner.compile_and_load(source, :Length1, interfaces: interfaces)
      assert Runner.call(mod, :main) == 5
    end

    test "length of empty list", %{interfaces: interfaces} do
      source = """
      (import Std.List)
      (Std.List/length (the (List :int) (list)))
      """
      assert {:ok, mod} = Runner.compile_and_load(source, :Length2, interfaces: interfaces)
      assert Runner.call(mod, :main) == 0
    end
  end

  describe "reverse" do
    test "reverse a list", %{interfaces: interfaces} do
      source = """
      (import Std.List)
      (Std.List/reverse (list 1 2 3))
      """
      assert {:ok, mod} = Runner.compile_and_load(source, :Reverse, interfaces: interfaces)
      assert Runner.call(mod, :main) == [3, 2, 1]
    end
  end

  describe "append" do
    test "append two lists", %{interfaces: interfaces} do
      source = """
      (import Std.List)
      (Std.List/append (list 1 2) (list 3 4))
      """
      assert {:ok, mod} = Runner.compile_and_load(source, :Append, interfaces: interfaces)
      assert Runner.call(mod, :main) == [1, 2, 3, 4]
    end

    test "append with empty list", %{interfaces: interfaces} do
      source = """
      (import Std.List)
      (Std.List/append (list) (list 1 2 3))
      """
      assert {:ok, mod} = Runner.compile_and_load(source, :AppendEmpty, interfaces: interfaces)
      assert Runner.call(mod, :main) == [1, 2, 3]
    end
  end

  describe "take and drop" do
    test "take first n elements", %{interfaces: interfaces} do
      source = """
      (import Std.List)
      (Std.List/take 3 (list 1 2 3 4 5))
      """
      assert {:ok, mod} = Runner.compile_and_load(source, :Take, interfaces: interfaces)
      assert Runner.call(mod, :main) == [1, 2, 3]
    end

    test "drop first n elements", %{interfaces: interfaces} do
      source = """
      (import Std.List)
      (Std.List/drop 2 (list 1 2 3 4 5))
      """
      assert {:ok, mod} = Runner.compile_and_load(source, :Drop, interfaces: interfaces)
      assert Runner.call(mod, :main) == [3, 4, 5]
    end

    test "take more than list length", %{interfaces: interfaces} do
      source = """
      (import Std.List)
      (Std.List/take 10 (list 1 2))
      """
      assert {:ok, mod} = Runner.compile_and_load(source, :TakeMore, interfaces: interfaces)
      assert Runner.call(mod, :main) == [1, 2]
    end
  end

  describe "nth" do
    test "nth returns Some for valid index", %{interfaces: interfaces} do
      source = """
      (import Std.List)
      (Std.List/nth 1 (list 10 20 30))
      """
      assert {:ok, mod} = Runner.compile_and_load(source, :NthValid, interfaces: interfaces)
      assert Runner.call(mod, :main) == {:Some, 20}
    end

    test "nth returns None for out of bounds", %{interfaces: interfaces} do
      source = """
      (import Std.List)
      (Std.List/nth 10 (list 1 2 3))
      """
      assert {:ok, mod} = Runner.compile_and_load(source, :NthOOB, interfaces: interfaces)
      assert Runner.call(mod, :main) == {:None}
    end
  end

  describe "find" do
    test "find returns Some when element exists", %{interfaces: interfaces} do
      source = """
      (import Std.List)
      (Std.List/find (fn [x] (> x 5)) (list 1 3 7 9))
      """
      assert {:ok, mod} = Runner.compile_and_load(source, :FindSome, interfaces: interfaces)
      assert Runner.call(mod, :main) == {:Some, 7}
    end

    test "find returns None when no match", %{interfaces: interfaces} do
      source = """
      (import Std.List)
      (Std.List/find (fn [x] (> x 100)) (list 1 2 3))
      """
      assert {:ok, mod} = Runner.compile_and_load(source, :FindNone, interfaces: interfaces)
      assert Runner.call(mod, :main) == {:None}
    end
  end

  describe "predicates" do
    test "any? returns true when at least one matches", %{interfaces: interfaces} do
      source = """
      (import Std.List)
      (Std.List/any? (fn [x] (> x 5)) (list 1 2 10))
      """
      assert {:ok, mod} = Runner.compile_and_load(source, :AnyTrue, interfaces: interfaces)
      assert Runner.call(mod, :main) == true
    end

    test "any? returns false when none match", %{interfaces: interfaces} do
      source = """
      (import Std.List)
      (Std.List/any? (fn [x] (> x 100)) (list 1 2 3))
      """
      assert {:ok, mod} = Runner.compile_and_load(source, :AnyFalse, interfaces: interfaces)
      assert Runner.call(mod, :main) == false
    end

    test "all? returns true when all match", %{interfaces: interfaces} do
      source = """
      (import Std.List)
      (Std.List/all? (fn [x] (> x 0)) (list 1 2 3))
      """
      assert {:ok, mod} = Runner.compile_and_load(source, :AllTrue, interfaces: interfaces)
      assert Runner.call(mod, :main) == true
    end

    test "all? returns false when one doesn't match", %{interfaces: interfaces} do
      source = """
      (import Std.List)
      (Std.List/all? (fn [x] (> x 2)) (list 1 2 3))
      """
      assert {:ok, mod} = Runner.compile_and_load(source, :AllFalse, interfaces: interfaces)
      assert Runner.call(mod, :main) == false
    end
  end

  describe "range" do
    test "range creates list of integers", %{interfaces: interfaces} do
      source = """
      (import Std.List)
      (Std.List/range 1 5)
      """
      assert {:ok, mod} = Runner.compile_and_load(source, :Range, interfaces: interfaces)
      assert Runner.call(mod, :main) == [1, 2, 3, 4]
    end

    test "range with start >= end returns empty", %{interfaces: interfaces} do
      source = """
      (import Std.List)
      (Std.List/range 5 3)
      """
      assert {:ok, mod} = Runner.compile_and_load(source, :RangeEmpty, interfaces: interfaces)
      assert Runner.call(mod, :main) == []
    end
  end

  describe "sum and product" do
    test "sum adds all elements", %{interfaces: interfaces} do
      source = """
      (import Std.List)
      (Std.List/sum (list 1 2 3 4))
      """
      assert {:ok, mod} = Runner.compile_and_load(source, :Sum, interfaces: interfaces)
      assert Runner.call(mod, :main) == 10
    end

    test "product multiplies all elements", %{interfaces: interfaces} do
      source = """
      (import Std.List)
      (Std.List/product (list 1 2 3 4))
      """
      assert {:ok, mod} = Runner.compile_and_load(source, :Product, interfaces: interfaces)
      assert Runner.call(mod, :main) == 24
    end
  end

  describe "min and max" do
    test "max returns Some with maximum", %{interfaces: interfaces} do
      source = """
      (import Std.List)
      (Std.List/max (list 3 1 4 1 5 9))
      """
      assert {:ok, mod} = Runner.compile_and_load(source, :Max, interfaces: interfaces)
      assert Runner.call(mod, :main) == {:Some, 9}
    end

    test "min returns Some with minimum", %{interfaces: interfaces} do
      source = """
      (import Std.List)
      (Std.List/min (list 3 1 4 1 5 9))
      """
      assert {:ok, mod} = Runner.compile_and_load(source, :Min, interfaces: interfaces)
      assert Runner.call(mod, :main) == {:Some, 1}
    end

    test "max returns None for empty list", %{interfaces: interfaces} do
      source = """
      (import Std.List)
      (the (Option :int) (Std.List/max (list)))
      """
      assert {:ok, mod} = Runner.compile_and_load(source, :MaxEmpty, interfaces: interfaces)
      assert Runner.call(mod, :main) == {:None}
    end
  end

  describe "sort" do
    test "sort orders list ascending", %{interfaces: interfaces} do
      source = """
      (import Std.List)
      (Std.List/sort (list 3 1 4 1 5))
      """
      assert {:ok, mod} = Runner.compile_and_load(source, :Sort, interfaces: interfaces)
      assert Runner.call(mod, :main) == [1, 1, 3, 4, 5]
    end

    test "sort-desc orders list descending", %{interfaces: interfaces} do
      source = """
      (import Std.List)
      (Std.List/sort-desc (list 3 1 4 1 5))
      """
      assert {:ok, mod} = Runner.compile_and_load(source, :SortDesc, interfaces: interfaces)
      assert Runner.call(mod, :main) == [5, 4, 3, 1, 1]
    end
  end
end
