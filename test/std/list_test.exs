defmodule Vaisto.Std.ListTest do
  use ExUnit.Case
  alias Vaisto.Runner

  # Load Std.List module before tests
  setup_all do
    prelude = File.read!("std/prelude.va")
    std_list_source = File.read!("std/List.va")
    full_source = prelude <> "\n\n" <> std_list_source
    {:ok, _} = Runner.compile_and_load(full_source, :"Std.List")
    :ok
  end

  describe "fold operations" do
    test "fold sums a list" do
      source = """
      (import Std.List)
      (Std.List/fold (fn [acc x] (+ acc x)) 0 (list 1 2 3 4))
      """
      assert {:ok, mod} = Runner.compile_and_load(source, :FoldSum)
      assert Runner.call(mod, :main) == 10
    end

    test "fold-right concatenates strings" do
      source = """
      (import Std.List)
      (Std.List/fold-right (fn [x acc] (str x acc)) "" (list "a" "b" "c"))
      """
      assert {:ok, mod} = Runner.compile_and_load(source, :FoldRight)
      assert Runner.call(mod, :main) == "abc"
    end
  end

  describe "map and filter" do
    test "map doubles each element" do
      source = """
      (import Std.List)
      (Std.List/map (fn [x] (* x 2)) (list 1 2 3))
      """
      assert {:ok, mod} = Runner.compile_and_load(source, :MapDouble)
      assert Runner.call(mod, :main) == [2, 4, 6]
    end

    test "filter keeps numbers greater than 3" do
      source = """
      (import Std.List)
      (Std.List/filter (fn [x] (> x 3)) (list 1 2 3 4 5 6))
      """
      assert {:ok, mod} = Runner.compile_and_load(source, :FilterGT3)
      assert Runner.call(mod, :main) == [4, 5, 6]
    end

    test "filter with empty result" do
      source = """
      (import Std.List)
      (Std.List/filter (fn [x] (> x 100)) (list 1 2 3))
      """
      assert {:ok, mod} = Runner.compile_and_load(source, :FilterEmpty)
      assert Runner.call(mod, :main) == []
    end
  end

  describe "length" do
    test "length of non-empty list" do
      source = """
      (import Std.List)
      (Std.List/length (list 1 2 3 4 5))
      """
      assert {:ok, mod} = Runner.compile_and_load(source, :Length1)
      assert Runner.call(mod, :main) == 5
    end

    test "length of empty list" do
      source = """
      (import Std.List)
      (Std.List/length (list))
      """
      assert {:ok, mod} = Runner.compile_and_load(source, :Length2)
      assert Runner.call(mod, :main) == 0
    end
  end

  describe "reverse" do
    test "reverse a list" do
      source = """
      (import Std.List)
      (Std.List/reverse (list 1 2 3))
      """
      assert {:ok, mod} = Runner.compile_and_load(source, :Reverse)
      assert Runner.call(mod, :main) == [3, 2, 1]
    end
  end

  describe "append" do
    @tag :skip
    test "append two lists" do
      # Skip: fold-right has issue with cons as first-class function
      source = """
      (import Std.List)
      (Std.List/append (list 1 2) (list 3 4))
      """
      assert {:ok, mod} = Runner.compile_and_load(source, :Append)
      assert Runner.call(mod, :main) == [1, 2, 3, 4]
    end

    @tag :skip
    test "append with empty list" do
      # Skip: fold-right has issue with cons as first-class function
      source = """
      (import Std.List)
      (Std.List/append (list) (list 1 2 3))
      """
      assert {:ok, mod} = Runner.compile_and_load(source, :AppendEmpty)
      assert Runner.call(mod, :main) == [1, 2, 3]
    end
  end

  describe "take and drop" do
    test "take first n elements" do
      source = """
      (import Std.List)
      (Std.List/take 3 (list 1 2 3 4 5))
      """
      assert {:ok, mod} = Runner.compile_and_load(source, :Take)
      assert Runner.call(mod, :main) == [1, 2, 3]
    end

    test "drop first n elements" do
      source = """
      (import Std.List)
      (Std.List/drop 2 (list 1 2 3 4 5))
      """
      assert {:ok, mod} = Runner.compile_and_load(source, :Drop)
      assert Runner.call(mod, :main) == [3, 4, 5]
    end

    test "take more than list length" do
      source = """
      (import Std.List)
      (Std.List/take 10 (list 1 2))
      """
      assert {:ok, mod} = Runner.compile_and_load(source, :TakeMore)
      assert Runner.call(mod, :main) == [1, 2]
    end
  end

  describe "nth" do
    test "nth returns Some for valid index" do
      source = """
      (import Std.List)
      (Std.List/nth 1 (list 10 20 30))
      """
      assert {:ok, mod} = Runner.compile_and_load(source, :NthValid)
      assert Runner.call(mod, :main) == {:Some, 20}
    end

    test "nth returns None for out of bounds" do
      source = """
      (import Std.List)
      (Std.List/nth 10 (list 1 2 3))
      """
      assert {:ok, mod} = Runner.compile_and_load(source, :NthOOB)
      assert Runner.call(mod, :main) == {:None}
    end
  end

  describe "find" do
    test "find returns Some when element exists" do
      source = """
      (import Std.List)
      (Std.List/find (fn [x] (> x 5)) (list 1 3 7 9))
      """
      assert {:ok, mod} = Runner.compile_and_load(source, :FindSome)
      assert Runner.call(mod, :main) == {:Some, 7}
    end

    test "find returns None when no match" do
      source = """
      (import Std.List)
      (Std.List/find (fn [x] (> x 100)) (list 1 2 3))
      """
      assert {:ok, mod} = Runner.compile_and_load(source, :FindNone)
      assert Runner.call(mod, :main) == {:None}
    end
  end

  describe "predicates" do
    test "any? returns true when at least one matches" do
      source = """
      (import Std.List)
      (Std.List/any? (fn [x] (> x 5)) (list 1 2 10))
      """
      assert {:ok, mod} = Runner.compile_and_load(source, :AnyTrue)
      assert Runner.call(mod, :main) == true
    end

    test "any? returns false when none match" do
      source = """
      (import Std.List)
      (Std.List/any? (fn [x] (> x 100)) (list 1 2 3))
      """
      assert {:ok, mod} = Runner.compile_and_load(source, :AnyFalse)
      assert Runner.call(mod, :main) == false
    end

    test "all? returns true when all match" do
      source = """
      (import Std.List)
      (Std.List/all? (fn [x] (> x 0)) (list 1 2 3))
      """
      assert {:ok, mod} = Runner.compile_and_load(source, :AllTrue)
      assert Runner.call(mod, :main) == true
    end

    test "all? returns false when one doesn't match" do
      source = """
      (import Std.List)
      (Std.List/all? (fn [x] (> x 2)) (list 1 2 3))
      """
      assert {:ok, mod} = Runner.compile_and_load(source, :AllFalse)
      assert Runner.call(mod, :main) == false
    end
  end

  describe "range" do
    test "range creates list of integers" do
      source = """
      (import Std.List)
      (Std.List/range 1 5)
      """
      assert {:ok, mod} = Runner.compile_and_load(source, :Range)
      assert Runner.call(mod, :main) == [1, 2, 3, 4]
    end

    test "range with start >= end returns empty" do
      source = """
      (import Std.List)
      (Std.List/range 5 3)
      """
      assert {:ok, mod} = Runner.compile_and_load(source, :RangeEmpty)
      assert Runner.call(mod, :main) == []
    end
  end

  describe "sum and product" do
    test "sum adds all elements" do
      source = """
      (import Std.List)
      (Std.List/sum (list 1 2 3 4))
      """
      assert {:ok, mod} = Runner.compile_and_load(source, :Sum)
      assert Runner.call(mod, :main) == 10
    end

    test "product multiplies all elements" do
      source = """
      (import Std.List)
      (Std.List/product (list 1 2 3 4))
      """
      assert {:ok, mod} = Runner.compile_and_load(source, :Product)
      assert Runner.call(mod, :main) == 24
    end
  end

  describe "min and max" do
    test "max returns Some with maximum" do
      source = """
      (import Std.List)
      (Std.List/max (list 3 1 4 1 5 9))
      """
      assert {:ok, mod} = Runner.compile_and_load(source, :Max)
      assert Runner.call(mod, :main) == {:Some, 9}
    end

    test "min returns Some with minimum" do
      source = """
      (import Std.List)
      (Std.List/min (list 3 1 4 1 5 9))
      """
      assert {:ok, mod} = Runner.compile_and_load(source, :Min)
      assert Runner.call(mod, :main) == {:Some, 1}
    end

    test "max returns None for empty list" do
      source = """
      (import Std.List)
      (Std.List/max (list))
      """
      assert {:ok, mod} = Runner.compile_and_load(source, :MaxEmpty)
      assert Runner.call(mod, :main) == {:None}
    end
  end

  describe "sort" do
    test "sort orders list ascending" do
      source = """
      (import Std.List)
      (Std.List/sort (list 3 1 4 1 5))
      """
      assert {:ok, mod} = Runner.compile_and_load(source, :Sort)
      assert Runner.call(mod, :main) == [1, 1, 3, 4, 5]
    end

    test "sort-desc orders list descending" do
      source = """
      (import Std.List)
      (Std.List/sort-desc (list 3 1 4 1 5))
      """
      assert {:ok, mod} = Runner.compile_and_load(source, :SortDesc)
      assert Runner.call(mod, :main) == [5, 4, 3, 1, 1]
    end
  end
end
