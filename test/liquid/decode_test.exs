defmodule Vaisto.Liquid.DecodeTest do
  use ExUnit.Case, async: true

  alias Vaisto.Liquid.{Canonical, Eval, Lint}

  defp core(text), do: Canonical.parse!(text, atoms: :create)

  defp decode(type, value, opts \\ []) do
    term = [
      :decode,
      type,
      [:perform, :external, [:atom, :foreign], [:atom, :value], [:inj, [:List, :Dyn], :Nil]]
    ]

    assert {:ok, [:"Core.Result", [:"Core.DecodeError"], ^type]} = Lint.synth(term, opts)

    Eval.eval(
      term,
      Keyword.put(opts, :handler, {:script, [{:external, [:foreign, :value, []], {:ok, value}}]})
    )
  end

  test "embedding then decoding is the identity, without numeric coercion" do
    for {type, term, value} <- [
          {"Int", "42", 42},
          {"Float", "42.0", 42.0},
          {"String", ~S|"hi"|, "hi"},
          {"Atom", "(atom hello)", :hello},
          {"Unit", "(unit)", nil},
          {"(Bool)", "(inj (Bool) false)", false},
          {"(Tuple Int String)", ~S|(tuple 1 "x")|, {1, "x"}},
          {"(List Int)", "(inj (List Int) Cons 1 (inj (List Int) Nil))", [1]}
        ] do
      tree = core("(decode #{type} (up #{type} #{term}))")
      assert {:ok, _} = Lint.synth(tree)
      assert {:ok, {:"Core.Ok", ^value}} = Eval.eval(tree)
    end

    assert {:ok, {:"Core.Err", {:"Core.BadDecode"}}} = decode(:Int, 42.0)
    assert {:ok, {:"Core.Err", {:"Core.BadDecode"}}} = decode(:Float, 42)
  end

  test "projection preserves the foreign representation exactly" do
    for n <- -25..25 do
      value = {n, [n, n + 1], %{key: n}}
      type = [:Tuple, :Int, [:List, :Int], [:Map, :Atom, :Int]]
      assert {:ok, {:"Core.Ok", ^value}} = decode(type, value)

      assert {:ok, {:"Core.Err", {:"Core.BadDecode"}}} =
               decode(type, {n, [n | :bad_tail], %{key: n}})
    end
  end

  test "closed rows reject extra keys and maps check both keys and values" do
    row = [:Record, [:row, [:x, :Int], :closed]]
    assert {:ok, {:"Core.Ok", %{x: 1}}} = decode(row, %{x: 1})

    for invalid <- [%{}, %{x: "1"}, %{x: 1, y: 2}, {:x, 1}] do
      assert {:ok, {:"Core.Err", {:"Core.BadDecode"}}} = decode(row, invalid)
    end

    for invalid <- [%{1 => 2}, %{key: "2"}] do
      assert {:ok, {:"Core.Err", {:"Core.BadDecode"}}} = decode([:Map, :Atom, :Int], invalid)
    end
  end

  test "recursive parameterized sums decode all fields, arities and tags" do
    opts = [
      types: [
        core("(type Tree (a) (sum (Leaf (tvar a)) (Branch (Tree (tvar a)) (Tree (tvar a)))))")
      ]
    ]

    type = [:Tree, :Int]
    tree = {:Branch, {:Leaf, 1}, {:Branch, {:Leaf, 2}, {:Leaf, 3}}}
    assert {:ok, {:"Core.Ok", ^tree}} = decode(type, tree, opts)

    for bad <- [
          {:Leaf, "1"},
          {:Leaf},
          {:Leaf, 1, 2},
          {:Other, 1},
          {:Branch, {:Leaf, 1}, {:Leaf, "2"}}
        ] do
      assert {:ok, {:"Core.Err", {:"Core.BadDecode"}}} = decode(type, bad, opts)
    end
  end

  test "decodability excludes hidden functions, variables, refinements and typed pid protocols" do
    function = [:->, [:Int], [:eff, :closed], :Int]

    for type <- [
          function,
          [:Tuple, function],
          [:List, function],
          [:Map, :Int, function],
          [:Pid, :Int],
          [:Tuple, [:Pid, [:List, :Int]]],
          [:tvar, :a],
          [:Record, [:row, [:x, :Int], [:rvar, :r]]],
          [:refine, :x, :Int, true]
        ] do
      refute Lint.decodable?(type)
    end

    opts = [types: [core("(type Box (a) (record (contents (tvar a))))")]]
    refute Lint.decodable?([:Box, function], opts)
    refute Lint.decodable?([:Box, [:Pid, :Int]], opts)
    assert Lint.decodable?([:Box, [:Pid, :Dyn]], opts)
    assert {:ok, {:"Core.Ok", pid}} = decode([:Pid, :Dyn], self())
    assert pid == self()
    assert {:ok, {:"Core.Ok", ref}} = decode(:Ref, make_ref())
    assert is_reference(ref)
  end

  test "a decode error is ordinary typed data and decoding does not swallow crashes" do
    term =
      core("""
      (match (decode Int (up String "wrong"))
        ((inj (Core.Result (Core.DecodeError) Int) Core.Ok n) n)
        ((inj (Core.Result (Core.DecodeError) Int) Core.Err err) 0))
      """)

    assert {:ok, :Int} = Lint.synth(term)
    assert {:ok, 0} = Eval.eval(term)
    crashing = core("(decode Int (perform crash (inj (Reason) badarg)))")
    assert {:ok, _} = Lint.synth(crashing)
    assert {:crash, :badarg} = Eval.eval(crashing)
  end
end
