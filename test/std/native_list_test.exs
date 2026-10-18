defmodule Vaisto.Std.NativeListTest do
  use ExUnit.Case, async: true
  alias Vaisto.{Compilation, Elab, Interface, Parser}
  alias Vaisto.Liquid.{Eval, Lint, Runtime}

  test "real List library keeps numeric dictionaries and typed sorting in native Core" do
    source =
      File.read!("std/prelude.va") <>
        "\n" <>
        File.read!("std/List.va") <>
        ~S"""
        (extern foreign:key [:int] :int)
        (defn main [] (Tuple :int :float (List :int) (List (Record [key :int name :string])) (List :int))
          {(sum [1 2 3]) (product [1.5 2.0]) (sort [3 1 2 1])
           (sort-by (fn [p] (foreign:key (. p :key))) [#{:key 2 :name "b"} #{:key 1 :name "a"}])
           (sort-with (fn [a b] (>= a b)) [1 3 2])})
        """

    name = :"NativeList#{System.unique_integer([:positive])}"
    assert {:ok, core} = source |> Parser.parse() |> Elab.module(name: name)
    assert :ok = Lint.check_module(core)
    assert {:ok, _} = Interface.from_core(core)
    assert {:ok, ^name, _} = Compilation.compile(source, name, format_errors: false)

    on_exit(fn ->
      :code.purge(name)
      :code.delete(name)
    end)

    events = for key <- [2, 1], do: {:external, [:foreign, :key, [key]], {:ok, key}}
    expected = {6, 3.0, [1, 1, 2, 3], [%{key: 1, name: "a"}, %{key: 2, name: "b"}], [3, 2, 1]}
    assert Runtime.run(name, :main, [], handler: {:script, events}) == {:ok, expected}
    assert Eval.run(core, :main, [], handler: {:script, events}) == {:ok, expected}
  end
end
