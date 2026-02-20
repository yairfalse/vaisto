defmodule Vaisto.TypeSystem.UnifyTest do
  use ExUnit.Case
  alias Vaisto.TypeSystem.Unify
  alias Vaisto.TypeSystem.Core

  describe "unify/3 with primitives" do
    test "same primitives unify" do
      assert {:ok, %{}, _} = Unify.unify(:int, :int)
      assert {:ok, %{}, _} = Unify.unify(:bool, :bool)
    end

    test "different primitives fail" do
      assert {:error, msg} = Unify.unify(:int, :bool)
      assert msg =~ "cannot unify"
    end
  end

  describe "unify/3 with type variables" do
    test "type variable unifies with primitive" do
      {:ok, subst, _} = Unify.unify({:tvar, 0}, :int)
      assert Core.apply_subst(subst, {:tvar, 0}) == :int
    end

    test "primitive unifies with type variable" do
      {:ok, subst, _} = Unify.unify(:string, {:tvar, 1})
      assert Core.apply_subst(subst, {:tvar, 1}) == :string
    end

    test "two type variables unify" do
      {:ok, subst, _} = Unify.unify({:tvar, 0}, {:tvar, 1})
      # One should be bound to the other
      t0 = Core.apply_subst(subst, {:tvar, 0})
      t1 = Core.apply_subst(subst, {:tvar, 1})
      assert t0 == t1
    end

    test "same type variable unifies with itself" do
      assert {:ok, %{}, _} = Unify.unify({:tvar, 0}, {:tvar, 0})
    end
  end

  describe "unify/3 with list types" do
    test "list types with same element type unify" do
      assert {:ok, %{}, _} = Unify.unify({:list, :int}, {:list, :int})
    end

    test "list types with different element types fail" do
      assert {:error, _} = Unify.unify({:list, :int}, {:list, :bool})
    end

    test "list with type variable element unifies" do
      {:ok, subst, _} = Unify.unify({:list, {:tvar, 0}}, {:list, :string})
      assert Core.apply_subst(subst, {:tvar, 0}) == :string
    end
  end

  describe "unify/3 with function types" do
    test "same function types unify" do
      fn1 = {:fn, [:int, :int], :int}
      fn2 = {:fn, [:int, :int], :int}
      assert {:ok, %{}, _} = Unify.unify(fn1, fn2)
    end

    test "function types with different arities fail" do
      fn1 = {:fn, [:int], :int}
      fn2 = {:fn, [:int, :int], :int}
      assert {:error, msg} = Unify.unify(fn1, fn2)
      assert msg =~ "arity"
    end

    test "function with type variables unifies" do
      fn1 = {:fn, [{:tvar, 0}], {:tvar, 1}}
      fn2 = {:fn, [:int], :bool}
      {:ok, subst, _} = Unify.unify(fn1, fn2)

      assert Core.apply_subst(subst, {:tvar, 0}) == :int
      assert Core.apply_subst(subst, {:tvar, 1}) == :bool
    end

    test "unifies polymorphic identity function" do
      # id : a -> a
      id_type = {:fn, [{:tvar, 0}], {:tvar, 0}}
      # used as int -> int
      int_fn = {:fn, [:int], :int}

      {:ok, subst, _} = Unify.unify(id_type, int_fn)
      assert Core.apply_subst(subst, {:tvar, 0}) == :int
    end
  end

  describe "unify/3 with record types" do
    test "same record types unify" do
      r1 = {:record, :point, [{:x, :int}, {:y, :int}]}
      r2 = {:record, :point, [{:x, :int}, {:y, :int}]}
      assert {:ok, %{}, _} = Unify.unify(r1, r2)
    end

    test "different record names fail" do
      r1 = {:record, :point, [{:x, :int}]}
      r2 = {:record, :vec, [{:x, :int}]}
      assert {:error, msg} = Unify.unify(r1, r2)
      assert msg =~ "cannot unify records"
    end

    test "record with type variable fields unifies" do
      r1 = {:record, :pair, [{:a, {:tvar, 0}}, {:b, {:tvar, 1}}]}
      r2 = {:record, :pair, [{:a, :int}, {:b, :string}]}
      {:ok, subst, _} = Unify.unify(r1, r2)

      assert Core.apply_subst(subst, {:tvar, 0}) == :int
      assert Core.apply_subst(subst, {:tvar, 1}) == :string
    end
  end

  describe "unify/3 with tuple types" do
    test "same tuple types unify" do
      t1 = {:tuple, [:int, :bool]}
      t2 = {:tuple, [:int, :bool]}
      assert {:ok, %{}, _} = Unify.unify(t1, t2)
    end

    test "different size tuples fail" do
      t1 = {:tuple, [:int, :bool]}
      t2 = {:tuple, [:int]}
      assert {:error, msg} = Unify.unify(t1, t2)
      assert msg =~ "size mismatch"
    end
  end

  describe "unify/3 with row types (same tail rvar)" do
    test "same rvar tail, no extras on either side succeeds" do
      # Two rows with same fields and same open tail — should unify trivially
      r1 = {:row, [{:x, :int}], {:rvar, 5}}
      r2 = {:row, [{:x, :int}], {:rvar, 5}}
      assert {:ok, _subst, _} = Unify.unify(r1, r2)
    end

    test "same rvar tail with extra fields on both sides errors" do
      # Row 1 has :y, Row 2 has :z — same tail can't absorb both
      r1 = {:row, [{:x, :int}, {:y, :bool}], {:rvar, 5}}
      r2 = {:row, [{:x, :int}, {:z, :string}], {:rvar, 5}}
      assert {:error, msg} = Unify.unify(r1, r2)
      assert msg =~ "same row variable"
    end

    test "different rvar tails with extras still works" do
      # Different tails — existing cross-bind logic handles this
      r1 = {:row, [{:x, :int}, {:y, :bool}], {:rvar, 5}}
      r2 = {:row, [{:x, :int}, {:z, :string}], {:rvar, 6}}
      assert {:ok, subst, _} = Unify.unify(r1, r2)
      # tail1 (rvar 5) should contain z from r2
      resolved = Core.apply_subst(subst, {:rvar, 5})
      assert {:row, fields, _} = resolved
      assert Enum.any?(fields, fn {name, _} -> name == :z end)
    end
  end

  describe "occurs check" do
    test "prevents infinite types" do
      # Trying to unify a with List(a) should fail
      result = Unify.unify({:tvar, 0}, {:list, {:tvar, 0}})
      assert {:error, msg} = result
      assert msg =~ "infinite type"
    end

    test "detects occurs in nested types" do
      # a = (a -> int) should fail
      result = Unify.unify({:tvar, 0}, {:fn, [{:tvar, 0}], :int})
      assert {:error, msg} = result
      assert msg =~ "infinite type"
    end
  end

  describe "unify/3 with :any" do
    test ":any unifies with any type on either side" do
      assert {:ok, %{}, _} = Unify.unify(:any, :int)
      assert {:ok, %{}, _} = Unify.unify(:string, :any)
      assert {:ok, %{}, _} = Unify.unify(:any, {:list, :int})
      assert {:ok, %{}, _} = Unify.unify({:fn, [:int], :bool}, :any)
    end

    test ":any does not bind type variables" do
      {:ok, subst, _} = Unify.unify(:any, :int)
      assert subst == %{}
    end
  end

  describe "unify/3 with :num" do
    test ":num unifies with :int" do
      assert {:ok, %{}, _} = Unify.unify(:num, :int)
      assert {:ok, %{}, _} = Unify.unify(:int, :num)
    end

    test ":num unifies with :float" do
      assert {:ok, %{}, _} = Unify.unify(:num, :float)
      assert {:ok, %{}, _} = Unify.unify(:float, :num)
    end

    test ":num unifies with itself" do
      assert {:ok, %{}, _} = Unify.unify(:num, :num)
    end

    test ":int and :float do NOT unify" do
      assert {:error, _} = Unify.unify(:int, :float)
      assert {:error, _} = Unify.unify(:float, :int)
    end
  end

  describe "unify/3 with atom subtyping" do
    test "singleton atom unifies with :atom" do
      assert {:ok, %{}, _} = Unify.unify({:atom, :foo}, :atom)
      assert {:ok, %{}, _} = Unify.unify(:atom, {:atom, :bar})
    end

    test "two singleton atoms unify" do
      assert {:ok, %{}, _} = Unify.unify({:atom, :foo}, {:atom, :bar})
    end
  end

  describe "unify/3 with pid types" do
    test "same process name pids unify" do
      assert {:ok, %{}, _} = Unify.unify({:pid, :counter, [:inc]}, {:pid, :counter, [:dec]})
    end

    test "different process name pids fail" do
      assert {:error, _} = Unify.unify({:pid, :counter, [:inc]}, {:pid, :timer, [:tick]})
    end
  end

  describe "unify/3 with process types" do
    test "process types with same state unify" do
      assert {:ok, %{}, _} = Unify.unify({:process, :int, [:inc]}, {:process, :int, [:dec]})
    end

    test "process types with tvar state unify and bind" do
      {:ok, subst, _} = Unify.unify({:process, {:tvar, 0}, [:inc]}, {:process, :int, [:dec]})
      assert Core.apply_subst(subst, {:tvar, 0}) == :int
    end

    test "process types with incompatible state fail" do
      assert {:error, _} = Unify.unify({:process, :int, [:inc]}, {:process, :string, [:dec]})
    end
  end

  describe "chained unification" do
    test "propagates constraints through chain" do
      # t0 = t1, t1 = int => t0 = int
      {:ok, subst1, _} = Unify.unify({:tvar, 0}, {:tvar, 1})
      {:ok, subst2, _} = Unify.unify({:tvar, 1}, :int, subst1)

      assert Core.apply_subst(subst2, {:tvar, 0}) == :int
      assert Core.apply_subst(subst2, {:tvar, 1}) == :int
    end

    test "detects conflicts in chain" do
      # t0 = int, t0 = bool => error
      {:ok, subst1, _} = Unify.unify({:tvar, 0}, :int)
      result = Unify.unify({:tvar, 0}, :bool, subst1)
      assert {:error, _} = result
    end
  end
end
