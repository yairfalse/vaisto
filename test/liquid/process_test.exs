defmodule Vaisto.Liquid.ProcessTest do
  use ExUnit.Case, async: true

  alias Vaisto.Liquid.{Canonical, Eval, Lint, Lower, Runtime}

  defp tree(text), do: Canonical.parse!(text, atoms: :create)

  defp types,
    do: [tree("(type Protocol () (sum (inc) (get)))"), tree("(type Other () (sum (inc)))")]

  defp rejects(term, message, opts \\ []) do
    assert {:error, [%{message: text} | _]} =
             Lint.synth(tree(term), Keyword.put(opts, :types, types()))

    assert text =~ message
  end

  test "spawn checks state, message and result types independently of elaboration" do
    good = "(perform spawn (Protocol) Int (inc get) (fn ((s Int) (m (Protocol))) s) 0)"
    assert {:ok, [:Pid, [:Protocol]]} = Lint.synth(tree(good), types: types())
    rejects(String.replace(good, ") 0)", ") \"wrong\")"), "expected Int")
    rejects(String.replace(good, "(m (Protocol))", "(m (Other))"), "same state type")
    rejects(String.replace(good, "))) s)", "))) \"wrong\")"), "same state type")
    rejects("(perform spawn (Protocol) Int (inc) 42 0)", "must be a function")

    rejects(
      "(perform spawn (-> (Int) (eff closed) Int) Int () (fn ((s Int) (m Int)) s) 0)",
      "decodable"
    )

    rejects(String.replace(good, "(inc get)", "(inc inc)"), "distinct constructor tags")
    rejects(String.replace(good, "(inc get)", "(missing)"), "outside its protocol")
  end

  test "send preserves the relation between a PID and its message type" do
    opts = [types: types(), bind: [[:p, [:Pid, [:Protocol]]]]]

    assert {:ok, :Unit} =
             Lint.synth(tree("(perform send (Protocol) p (inj (Protocol) inc))"), opts)

    rejects("(perform send (Other) p (inj (Other) inc))", "expected (Pid (Other))",
      bind: [[:p, [:Pid, [:Protocol]]]]
    )

    rejects("(perform send (Protocol) 42 (inj (Protocol) inc))", "expected (Pid")

    rejects("(perform send (Protocol) p (atom inc))", "expected (Protocol)",
      bind: [[:p, [:Pid, [:Protocol]]]]
    )
  end

  test "spawn, send and link cannot escape a pure effect row" do
    for {effect, body, params} <- [
          {:spawn, "(perform spawn (Protocol) Int (inc get) (fn ((s Int) (m (Protocol))) s) 0)",
           ""},
          {:send, "(perform send (Protocol) p (inj (Protocol) inc))", "(p (Pid (Protocol)))"},
          {:link, "(perform link (Protocol) p)", "(p (Pid (Protocol)))"}
        ] do
      result = if effect == :spawn, do: "(Pid (Protocol))", else: "Unit"
      arguments = if params == "", do: "", else: "(Pid (Protocol))"

      source =
        "(module Bad (core-version 0) (type Protocol () (sum (inc) (get))) (def main (-> (#{arguments}) (eff closed) #{result}) (fn (#{params}) #{body})))"

      assert {:error, errors} = Lint.check_module(tree(source))
      assert Enum.any?(errors, &(&1.message =~ "#{effect} is performed here"))
    end
  end

  test "scripted send and link have the same meaning on native BEAM and the evaluator" do
    for operation <- [
          "(perform send (Protocol) p (inj (Protocol) inc))",
          "(perform link (Protocol) p)"
        ] do
      effect = if String.contains?(operation, "send"), do: :send, else: :link
      args = if effect == :send, do: [self(), {:inc}], else: [self()]
      opts = [handler: {:script, [{effect, args, {:ok, nil}}]}]

      core =
        tree(
          "(module Script (core-version 0) (type Protocol () (sum (inc))) (def main (-> ((Pid (Protocol))) (eff #{effect} closed) Unit) (fn ((p (Pid (Protocol)))) #{operation})))"
        )

      name = :"NativeProcScript#{System.unique_integer([:positive])}"
      assert {:ok, ^name, _} = Lower.compile(core, name)

      try do
        assert {:ok, nil} = Eval.run(core, :main, [self()], opts)
        assert {:ok, nil} = Runtime.run(name, :main, [self()], opts)
      after
        :code.purge(name)
        :code.delete(name)
      end
    end
  end

  test "native selection respects built-in Bool, List and Reason representations" do
    for {protocol, tags, messages} <- [
          {[:Bool], [true, false], [true, false]},
          {[:List, :Int], [:Nil, :Cons], [[], [1, 2]]},
          {[:Reason], [:badarg, :user], [:badarg, {:user, "details"}]}
        ] do
      core = [
        :module,
        :BuiltinProtocol,
        [:"core-version", 0],
        [
          :def,
          :step,
          [:->, [:Int, protocol], [:eff, :closed], :Int],
          [:fn, [[:state, :Int], [:message, protocol]], [:prim, :add, :state, 1]]
        ],
        [
          :def,
          :main,
          [:->, [], [:eff, :spawn, :closed], [:Pid, protocol]],
          [:fn, [], [:perform, :spawn, protocol, :Int, tags, :step, 0]]
        ]
      ]

      name = :"NativeBuiltinProtocol#{System.unique_integer([:positive])}"
      assert {:ok, ^name, _} = Lower.compile(core, name)
      pid = apply(name, :main, [])

      try do
        Enum.reduce(messages, 0, fn message, state ->
          assert {:ok, expected} = Eval.run(core, :step, [state, message])
          assert GenServer.call(pid, message) == expected
          expected
        end)
      after
        if Process.alive?(pid), do: GenServer.stop(pid)
        :code.purge(name)
        :code.delete(name)
      end
    end
  end
end
