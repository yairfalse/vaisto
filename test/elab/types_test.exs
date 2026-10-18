defmodule Vaisto.Elab.TypesTest do
  use ExUnit.Case, async: true
  alias Vaisto.Elab.Types

  for type <- [
        :Int,
        :Float,
        :String,
        :Atom,
        :Unit,
        :Dyn,
        [:Bool],
        [:List, :Int],
        [:Tuple, :Int, :String],
        [:Point],
        [:Pid, [:Message]],
        [:Map, :Atom, :Int],
        [:->, [:Int], [:eff, :closed], :Int]
      ] do
    test "type equality is reflexive for #{inspect(type)}" do
      type = unquote(Macro.escape(type))
      assert {:ok, %{}, 0} = Types.unify(type, type, %{}, 0)
    end
  end

  for {left, right} <- [
        {:Int, :Float},
        {:Int, [:Bool]},
        {:Dyn, :Int},
        {[:List, :Int], [:List, :String]},
        {[:Point], [:Vector]},
        {[:Tuple, :Int], [:Tuple, :Int, :Int]},
        {[:Pid, [:Counter]], [:Pid, [:Timer]]},
        {[:Map, :Atom, :Int], [:Map, :Atom, :String]},
        {[:->, [:Int], [:eff, :closed], :Int], [:->, [:Int, :Int], [:eff, :closed], :Int]},
        {[:tvar, :a], :Int},
        {[:tvar, :a], [:tvar, :b]}
      ] do
    test "distinct types do not unify: #{inspect(left)} and #{inspect(right)}" do
      left = unquote(Macro.escape(left))
      right = unquote(Macro.escape(right))
      assert :error = Types.unify(left, right, %{}, 0)
      assert :error = Types.unify(right, left, %{}, 0)
    end
  end

  test "local metavariables solve in either direction and through chains" do
    for {a, b} <- [{{:meta, 0}, :Int}, {:Int, {:meta, 0}}] do
      assert {:ok, s, 1} = Types.unify(a, b, %{}, 1)
      assert Types.zonk({:meta, 0}, s) == :Int
    end

    assert {:ok, s, 2} = Types.unify({:meta, 0}, {:meta, 1}, %{}, 2)
    assert {:ok, s, 2} = Types.unify({:meta, 1}, :String, s, 2)
    assert Types.zonk({:meta, 0}, s) == :String
    assert Types.zonk({:meta, 1}, s) == :String
  end

  test "unification propagates through every structural type constructor" do
    for {generic, concrete} <- [
          {[:List, {:meta, 0}], [:List, :String]},
          {[:Pair, {:meta, 0}, {:meta, 1}], [:Pair, :Int, :String]},
          {[:Tuple, {:meta, 0}, {:meta, 1}], [:Tuple, :Int, :String]},
          {[:Map, {:meta, 0}, {:meta, 1}], [:Map, :Atom, :String]},
          {[:Pid, {:meta, 0}], [:Pid, [:Message]]},
          {[:->, [{:meta, 0}], [:eff, :closed], {:meta, 1}],
           [:->, [:Int], [:eff, :closed], :String]}
        ] do
      assert {:ok, s, 2} = Types.unify(generic, concrete, %{}, 2)
      assert Types.zonk(generic, s) == concrete
      assert Types.zonk(Types.zonk(generic, s), s) == concrete
    end
  end

  test "occurs checks reject recursive types, including nested arrows" do
    for recursive <- [
          [:List, {:meta, 0}],
          [:Tuple, :Int, {:meta, 0}],
          [:->, [{:meta, 0}], [:eff, :closed], :Int],
          [:Map, :Atom, [:List, {:meta, 0}]]
        ] do
      assert :error = Types.unify({:meta, 0}, recursive, %{}, 1)
      assert :error = Types.unify(recursive, {:meta, 0}, %{}, 1)
    end
  end

  test "row unification absorbs extra fields and threads a fresh shared tail" do
    a = [:row, [:x, :Int], {:meta, 0}]
    b = [:row, [:y, :String], {:meta, 1}]
    assert {:ok, s, 3} = Types.unify(a, b, %{}, 2)
    assert Types.zonk(a, s) == Types.zonk(b, s)
    assert Types.zonk({:meta, 0}, s) == [:row, [:y, :String], {:meta, 2}]
    assert Types.zonk({:meta, 1}, s) == [:row, [:x, :Int], {:meta, 2}]

    assert {:ok, s, 3} =
             Types.unify(Types.zonk(a, s), [:row, [:x, :Int], [:y, :String], :closed], s, 3)

    assert Types.zonk(a, s) == [:row, [:x, :Int], [:y, :String], :closed]
  end

  test "closed rows, mismatched fields and cyclic shared tails are rejected" do
    assert :error =
             Types.unify(
               [:row, [:x, :Int], :closed],
               [:row, [:x, :Int], [:y, :Int], :closed],
               %{},
               0
             )

    assert :error =
             Types.unify(
               [:row, [:x, :Int], {:meta, 0}],
               [:row, [:x, :String], {:meta, 1}],
               %{},
               2
             )

    assert :error =
             Types.unify([:row, [:x, :Int], {:meta, 0}], [:row, [:y, :Int], {:meta, 0}], %{}, 1)

    assert :error = Types.unify([:row, [:rvar, :r]], [:row, [:x, :Int], :closed], %{}, 0)
  end

  test "effect rows solve both tails without colliding with existing metavariables" do
    a = [:eff, :external, {:meta, 0}]
    b = [:eff, :send, {:meta, 1}]
    assert {:ok, s, 3} = Types.unify(a, b, %{}, 2)
    assert Types.zonk(a, s) == Types.zonk(b, s)
    assert Types.zonk({:meta, 0}, s) == [:eff, :send, {:meta, 2}]
    assert :error = Types.unify(a, [:eff, :send, {:meta, 0}], %{}, 2)
    assert :error = Types.unify([:eff, :external, :closed], [:eff, :closed], %{}, 0)
    assert :error = Types.unify([:eff, [:evar, :fx]], [:eff, :external, :closed], %{}, 0)
  end

  test "named substitution splices kinded rows rather than nesting them" do
    arrow = [
      :->,
      [[:Record, [:row, [:x, [:tvar, :a]], [:rvar, :r]]]],
      [:eff, :external, [:evar, :fx]],
      [:tvar, :a]
    ]

    subst = %{a: :Int, r: [:row, [:y, :String], :closed], fx: [:eff, :send, :closed]}

    assert Types.subst_tvars(arrow, subst) == [
             :->,
             [[:Record, [:row, [:x, :Int], [:y, :String], :closed]]],
             [:eff, :external, :send, :closed],
             :Int
           ]

    assert Types.subst_tvars([:tvar, :other], subst) == [:tvar, :other]
  end

  test "substitution cannot smuggle duplicate effect labels into a splice" do
    assert {:type_error, _} =
             catch_throw(
               Types.subst_tvars([:eff, :external, [:evar, :fx]], %{
                 fx: [:eff, :external, :closed]
               })
             )
  end

  test "signature generalization preserves Type, Row and Eff kinds" do
    type = [
      :->,
      [[:Record, [:row, [:x, [:tvar, :a]], [:rvar, :r]]]],
      [:eff, [:evar, :fx]],
      [:tvar, :a]
    ]

    assert [:forall, binders, ^type] = Types.generalize(type)
    assert MapSet.new(binders) == MapSet.new([[:a, :Type], [:r, :Row], [:fx, :Eff]])

    assert Types.generalize([:->, [:Int], [:eff, :closed], :Int]) == [
             :->,
             [:Int],
             [:eff, :closed],
             :Int
           ]

    assert {:type_error, _} =
             catch_throw(Types.generalize([:->, [[:tvar, :a]], [:eff, [:evar, :a]], :Int]))
  end

  test "erased universal types are not source types" do
    for annotation <- [:any, :num] do
      assert {:error, _} = Types.from_surface(annotation, %{})
    end

    assert {:ok, :Dyn} = Types.from_surface(:dyn, %{})
  end
end
