defmodule Vaisto.Elab.SupervisionTest do
  use ExUnit.Case, async: true

  alias Vaisto.{Compilation, Elab, Parser}
  alias Vaisto.Liquid.{Eval, Lint, Runtime}

  @processes """
  (process first 0 :inc (+ state 1) :get state)
  (process second 0 :inc (+ state 1) :get state)
  """

  defp compile(source) do
    name = :"NativeSupervisor#{System.unique_integer([:positive])}"
    assert {:ok, core} = source |> Parser.parse() |> Elab.module(name: name)
    assert :ok = Lint.check_module(core)
    assert {:ok, ^name, _} = Compilation.compile_liquid(source, name, format_errors: false)

    on_exit(fn ->
      :code.purge(name)
      :code.delete(name)
    end)

    {name, core}
  end

  defp start(name) do
    assert {:ok, sup} = apply(name, :start_link, [nil])
    Process.unlink(sup)
    on_exit(fn -> if Process.alive?(sup), do: Supervisor.stop(sup) end)
    sup
  end

  defp children(sup) do
    Map.new(Supervisor.which_children(sup), fn {id, pid, :worker, _} -> {id, pid} end)
  end

  defp await_restart(sup, id, old, remaining \\ 100)
  defp await_restart(_sup, _id, _old, 0), do: flunk("supervisor did not restart the child")

  defp await_restart(sup, id, old, remaining) do
    case Map.fetch!(children(sup), id) do
      pid when is_pid(pid) and pid != old ->
        pid

      _ ->
        Process.sleep(10)
        await_restart(sup, id, old, remaining - 1)
    end
  end

  @tag :capture_log
  test "one_for_one restarts only the failed child with its checked initial state" do
    {name, _} = compile(@processes <> "(supervise :one_for_one (first 3) (second 10))")
    sup = start(name)
    before = children(sup)
    assert GenServer.call(before.first, :inc) == 4
    assert GenServer.call(before.second, :inc) == 11
    Process.exit(before.first, :kill)
    first = await_restart(sup, :first, before.first)
    assert GenServer.call(first, :get) == 3
    assert children(sup).second == before.second
    assert GenServer.call(before.second, :get) == 11
  end

  @tag :capture_log
  test "historical all_for_one and OTP one_for_all both restart all children" do
    for strategy <- [:all_for_one, :one_for_all] do
      {name, _} = compile(@processes <> "(supervise :#{strategy} (first 3) (second 10))")
      sup = start(name)
      before = children(sup)
      Process.exit(before.first, :kill)
      first = await_restart(sup, :first, before.first)
      second = await_restart(sup, :second, before.second)
      assert GenServer.call(first, :get) == 3
      assert GenServer.call(second, :get) == 10
    end
  end

  @tag :capture_log
  test "rest_for_one preserves earlier children and restarts later ones" do
    {name, _} = compile(@processes <> "(supervise :rest_for_one (first 3) (second 10))")
    sup = start(name)
    before = children(sup)
    Process.exit(before.second, :kill)
    second = await_restart(sup, :second, before.second)
    assert children(sup).first == before.first
    Process.exit(before.first, :kill)
    await_restart(sup, :first, before.first)
    await_restart(sup, :second, second)
  end

  test "supervise expressions work in explicitly typed functions" do
    {name, _} =
      compile(
        @processes <>
          """
          (defn boot [] (Tuple :atom (Pid :dyn))
            (supervise :one_for_one (first 8)))
          """
      )

    assert {:ok, sup} = apply(name, :boot, [])
    Process.unlink(sup)

    try do
      assert GenServer.call(children(sup).first, :get) == 8
    after
      Supervisor.stop(sup)
    end
  end

  test "empty supervision has a scripted meaning and pure runs reject it" do
    {name, core} = compile("(supervise :one_for_one)")
    answer = {:ok, self()}
    handler = {:script, [{:supervise, [:one_for_one, []], {:ok, answer}}]}
    assert Runtime.run(name, :start_link, [nil], handler: handler) == {:ok, answer}
    assert Eval.run(core, :start_link, [nil], handler: handler) == {:ok, answer}
    assert_raise ArgumentError, fn -> Runtime.run(name, :start_link, [nil]) end
    assert_raise Eval.Stuck, fn -> Eval.run(core, :start_link, [nil]) end
  end

  test "strategies, child state types and identifiers are checked before startup" do
    for tree <- [
          "(supervise :wrong (first 0))",
          "(supervise :one_for_one (first false))",
          "(supervise :one_for_one (first 0) (first 1))",
          "(supervise :one_for_one (missing 0))",
          "(supervise :one_for_one (first))",
          "(supervise :one_for_one (first 0 1))"
        ] do
      assert {:error, _} = (@processes <> tree) |> Parser.parse() |> Elab.module()
    end
  end

  test "independent lint rejects malformed supervision and missing effect permissions" do
    for {eff, operation} <- [
          {[:eff, :closed], [:perform, :supervise, :one_for_one]},
          {[:eff, :link, :spawn, :supervise, :closed], [:perform, :supervise, :invalid]},
          {[:eff, :link, :spawn, :supervise, :closed],
           [:perform, :supervise, :one_for_one, [:child]]}
        ] do
      core = [
        :module,
        :Bad,
        [:"core-version", 0],
        [:def, :boot, [:->, [], eff, [:Tuple, :Atom, [:Pid, :Dyn]]], [:fn, [], operation]]
      ]

      assert {:error, _} = Lint.check_module(core)
    end
  end

  test "independent lint checks child states, selectors, transition types and duplicate ids" do
    types = [[:type, :Message, [], [:sum, [:tick]]]]

    child = [
      :counter,
      [:Message],
      :Int,
      [:tick],
      [:fn, [[:state, :Int], [:message, [:Message]]], :state],
      0
    ]

    for children <- [
          [child, child],
          [List.replace_at(child, 5, false)],
          [List.replace_at(child, 3, [:unknown])],
          [List.replace_at(child, 4, [:fn, [[:state, :Int], [:message, [:Message]]], false])]
        ] do
      core = [
        :module,
        :Bad,
        [:"core-version", 0]
        | types ++
            [
              [
                :def,
                :boot,
                [
                  :->,
                  [],
                  [:eff, :link, :spawn, :supervise, :closed],
                  [:Tuple, :Atom, [:Pid, :Dyn]]
                ],
                [:fn, [], [:perform, :supervise, :one_for_one | children]]
              ]
            ]
      ]

      assert {:error, _} = Lint.check_module(core)
    end
  end
end
