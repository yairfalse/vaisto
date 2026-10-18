defmodule Vaisto.Std.NativeMapTest do
  use ExUnit.Case, async: true
  alias Vaisto.{Compilation, Elab, Parser}
  alias Vaisto.Liquid.{Eval, Lint, Runtime}

  test "real Map helpers retain key/value types and callback effects" do
    source =
      File.read!("std/prelude.va") <>
        "\n" <>
        File.read!("std/Map.va") <>
        ~S"""
        (extern foreign:next [:int] :int)
        (defn main [] (Tuple :int (Option :int) (Map :atom :int) (Map :atom :int))
          (let [m (the (Map :atom :int) #{:answer 41})]
            {(get-or m :missing 0) (lookup m :answer)
             (update m :answer 0 (fn [x] (foreign:next x)))
             (update m :missing 7 (fn [x] (foreign:next x)))}))
        """

    name = :"NativeMapLibrary#{System.unique_integer([:positive])}"
    assert {:ok, core} = source |> Parser.parse() |> Elab.module(name: name)
    assert :ok = Lint.check_module(core)
    assert {:ok, ^name, _} = Compilation.compile_liquid(source, name, format_errors: false)

    on_exit(fn ->
      :code.purge(name)
      :code.delete(name)
    end)

    script = {:script, [{:external, [:foreign, :next, [41]], {:ok, 42}}]}
    expected = {0, {:Some, 41}, %{answer: 42}, %{answer: 41, missing: 7}}
    assert Runtime.run(name, :main, [], handler: script) == {:ok, expected}
    assert Eval.run(core, :main, [], handler: script) == {:ok, expected}
    assert apply(name, :lookup, [%{}, :absent]) == {:None}
  end
end
