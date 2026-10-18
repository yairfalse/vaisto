defmodule Vaisto.Std.NativeStateTest do
  use ExUnit.Case, async: true
  alias Vaisto.{Compilation, Elab, Interface, Parser}
  alias Vaisto.Liquid.{Eval, Lint, Runtime}

  setup do
    source =
      File.read!("std/prelude.va") <>
        "\n" <>
        File.read!("std/State.va") <>
        """
        (defn demo [] (Tuple :int :int :int)
          (counter-reset! :counter)
          (let [first (counter-inc! :counter)
                isolated (with-isolated-state (fn []
                  (counter-reset! :counter) (counter-inc! :counter) (counter-inc! :counter)))
                restored (counter-inc! :counter)]
            {first isolated restored}))
        (defn scoped [] :int
          (with-state (list {(up :atom :counter) (up :int 41)}) (fn [] (counter-inc! :counter))))
        (defn failing [] :int
          (set! (up :atom :counter) (up :int 7))
          (with-isolated-state (fn []
            (set! (up :atom :counter) (up :int 8)) (crash :badarg))))
        """

    name = :"NativeState#{System.unique_integer([:positive])}"
    assert {:ok, core} = source |> Parser.parse() |> Elab.module(name: name)
    assert :ok = Lint.check_module(core)
    assert {:ok, _} = Interface.from_core(core)
    assert {:ok, ^name, _} = Compilation.compile(source, name, format_errors: false)

    on_exit(fn ->
      :code.purge(name)
      :code.delete(name)
    end)

    %{name: name, core: core}
  end

  test "actual process-local state is restored on success and failure", %{name: name} do
    assert apply(name, :demo, []) == {1, 2, 2}
    assert Process.get(:counter) == 2
    assert apply(name, :scoped, []) == 42
    assert Process.get(:counter) == 2
    assert Runtime.run(name, :failing, [], handler: :native) == {:crash, :badarg}
    assert Process.get(:counter) == 7
  end

  test "native and independent scripted execution agree on storage and restoration order", %{
    name: name,
    core: core
  } do
    events =
      [
        {:put, [:counter, 0], :undefined},
        {:get, [:counter], 0},
        {:put, [:counter, 1], 0},
        {:get, [], [counter: 1]},
        {:put, [:counter, 0], 1},
        {:get, [:counter], 0},
        {:put, [:counter, 1], 0},
        {:get, [:counter], 1},
        {:put, [:counter, 2], 1},
        {:erase, [], [counter: 2]},
        {:put, [:counter, 1], :undefined},
        {:get, [:counter], 1},
        {:put, [:counter, 2], 1}
      ]
      |> Enum.map(fn {f, args, result} -> {:external, [:erlang, f, args], {:ok, result}} end)

    assert Runtime.run(name, :demo, [], handler: {:script, events}) == {:ok, {1, 2, 2}}
    assert Eval.run(core, :demo, [], handler: {:script, events}) == {:ok, {1, 2, 2}}
  end

  test "typed utilities decode stored data rather than assuming the default's type", %{name: name} do
    Process.put(:counter, "not an integer")

    assert Runtime.run(name, :"counter-inc!", [:counter], handler: :native) ==
             {:crash, :bad_decode}

    Process.put(:errors, [1 | :bad_tail])

    assert Runtime.run(name, :"get-accumulated", [:errors], handler: :native) ==
             {:crash, :bad_decode}

    Process.delete(:errors)
    assert apply(name, :accumulate!, [:errors, %{line: 1}]) == {:None}
    assert apply(name, :accumulate!, [:errors, %{line: 2}]) == {:Some, [%{line: 1}]}
    assert apply(name, :"clear-accumulated!", [:errors]) == [%{line: 1}, %{line: 2}]
    assert apply(name, :"get-accumulated", [:errors]) == []
    assert apply(name, :"unique-id", [:native_symbol]) == :native_symbol_0
    assert apply(name, :"unique-id", [:native_symbol]) == :native_symbol_1
    assert apply(name, :fst, [{1, 2, 3}]) == 1
    assert apply(name, :snd, [{1, 2, 3}]) == 2
  end
end
