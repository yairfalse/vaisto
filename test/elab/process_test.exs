defmodule Vaisto.Elab.ProcessTest do
  use ExUnit.Case, async: true

  alias Vaisto.{Compilation, Elab, Parser}
  alias Vaisto.Liquid.{Eval, Lint, Runtime}

  @counter """
  (process counter 0
    :inc (+ state 1)
    :dec (- state 1)
    :get state)
  """

  defp compile(source) do
    name = :"NativeProcess#{System.unique_integer([:positive])}"
    assert {:ok, core} = source |> Parser.parse() |> Elab.module(name: name)
    assert :ok = Lint.check_module(core)
    assert {:ok, ^name, _} = Compilation.compile_liquid(source, name, format_errors: false)

    on_exit(fn ->
      :code.purge(name)
      :code.delete(name)
    end)

    {name, core}
  end

  defp track(pid) do
    on_exit(fn -> if Process.alive?(pid), do: Process.exit(pid, :kill) end)
    pid
  end

  defp error(source) do
    assert {:error, [error | _]} = source |> Parser.parse() |> Elab.module()
    error
  end

  test "a native actor's state transitions agree with its independent Core meaning" do
    {name, core} = compile(@counter)
    pid = track(apply(name, :counter, [0]))
    assert is_pid(pid)

    Enum.reduce([:get, :inc, :inc, :dec, :get], 0, fn message, state ->
      assert {:ok, expected} = Eval.run(core, :"Process.counter.step", [state, {message}])
      assert GenServer.call(pid, message) == expected
      expected
    end)
  end

  test "the single-process module preserves start_link and Runner interop" do
    {name, _} = compile(@counter)
    assert {:ok, pid} = Vaisto.Runner.spawn_process(name, 10)
    track(pid)
    assert pid in elem(Process.info(self(), :links), 1)
    assert Vaisto.Runner.send_msg(pid, :inc) == 11
    assert Vaisto.Runner.send_msg(pid, :get) == 11
    Process.unlink(pid)
  end

  test "raw Core-backend request/reply messages still work" do
    {name, _} = compile(@counter)
    pid = track(apply(name, :counter, [4]))
    send(pid, {:inc, self()})
    assert_receive 5
    send(pid, {:get, self()})
    assert_receive 5
  end

  test "typed spawn and asynchronous send work through explicit signatures" do
    {name, _} =
      compile(
        @counter <>
          """
          (defn start [] (Pid :Process.counter.Message) (spawn counter 0))
          (defn bump [p (Pid :Process.counter.Message)] :unit (! p :inc))
          (defn twice [p (Pid :Process.counter.Message)] :unit (do (bump p) (bump p)))
          """
      )

    pid = track(apply(name, :start, []))
    assert apply(name, :twice, [pid]) == nil
    assert GenServer.call(pid, :get) == 2
  end

  test "unchecked sends are explicit Dyn boundaries, not permission to send to non-pids" do
    {name, _} =
      compile(
        @counter <>
          """
          (defn unchecked [p (Pid :Process.counter.Message)] :unit (!! p :unknown))
          """
      )

    pid = track(apply(name, :counter, [2]))
    assert apply(name, :unchecked, [pid]) == nil
    assert GenServer.call(pid, :get) == 2
    assert error("(defn main [] :unit (!! 42 :inc))").message =~ "PID"
    assert error("(defn main [] :unit (! 42 :inc))").message =~ "PID"
  end

  test "unexpected messages, including malformed typed messages, are dropped" do
    {name, _} = compile(@counter)
    pid = track(apply(name, :counter, [7]))

    for message <- [:unknown, {:inc, "not a field"}, {:inc, 1, 2}, [1, 2], "inc"] do
      send(pid, message)
    end

    GenServer.cast(pid, :unknown)
    assert GenServer.call(pid, :get) == 7
    assert Process.alive?(pid)
  end

  test "state types are checked at spawn sites and every handler" do
    e = error(@counter <> "(defn main [] (Pid :Process.counter.Message) (spawn counter \"bad\"))")
    assert e.message == "type mismatch"
    assert e.note =~ "Int"
    assert error("(process counter 0 :inc \"bad\")").message == "type mismatch"
    assert error("(process counter 0 :inc (+ state 1.0))").message == "type mismatch"
    assert error("(process counter [] :get state)").message =~ "cannot be determined"
  end

  test "the exported foreign entry also checks its initial state before spawning" do
    {name, _} = compile(@counter)
    assert catch_throw(apply(name, :counter, ["bad"])) == {:liquid_crash, :bad_decode}
    assert catch_throw(apply(name, :start_link, ["bad"])) == {:liquid_crash, :bad_decode}
  end

  test "records and empty lists can be process states with a known ground type" do
    {name, _} =
      compile("""
      (deftype State [items (List :int)])
      (process cache (State (the (List :int) []))
        :clear (State [])
        :get state)
      """)

    pid = track(apply(name, :cache, [{:State, [1, 2]}]))
    assert GenServer.call(pid, :get) == {:State, [1, 2]}
    assert GenServer.call(pid, :clear) == {:State, []}
  end

  test "two processes can share tags but their PID protocols cannot be interchanged" do
    source =
      @counter <>
        """
        (process other 10 :get state)
        (defn bump [p (Pid :Process.counter.Message)] :unit (! p :inc))
        """

    {name, _} = compile(source)
    first = track(apply(name, :counter, [1]))
    second = track(apply(name, :other, [10]))
    assert GenServer.call(first, :get) == 1
    assert GenServer.call(second, :get) == 10

    assert error(source <> "(defn main [] :unit (bump (spawn other 10)))").message ==
             "type mismatch"

    assert error(source <> "(defn main [] :unit (! (spawn other 10) :inc))").message =~
             "does not accept"
  end

  test "message errors are located and pure callbacks cannot hide process effects" do
    e = error(@counter <> "(defn main [] :unit (! (spawn counter 0) :missing))")
    assert e.message =~ "does not accept"
    assert e.note =~ ":inc"
    assert e.primary_span != nil

    e =
      error(
        @counter <>
          "(defn pure-map [f (Fn :int :unit) xs (List :int)] (List :unit) (map f xs)) (defn main [] (List :unit) (pure-map (fn [x] (! (spawn counter 0) :inc)) [1]))"
      )

    assert e.message =~ "pure function"
  end

  test "duplicate processes, duplicate message handlers and incomplete handler pairs are errors" do
    assert error(@counter <> @counter).message =~ "declared twice"
    assert error("(process counter 0 :inc state :inc state)").message =~ "twice"
    assert error("(process counter 0 :inc)").message =~ "message/body pairs"
    assert error(@counter <> "(defn counter [n :int] :int n)").message =~ "defined twice"
  end

  test "a pure handler does not silently execute live process operations" do
    {name, _} = compile(@counter)

    assert_raise ArgumentError, ~r/scripted handler/, fn ->
      Runtime.run(name, :counter, [0])
    end

    assert {:ok, pid} = Runtime.run(name, :counter, [0], handler: :native)
    track(pid)
    assert GenServer.call(pid, :get) == 0
  end

  test "an actor with no handlers drops everything, including its inert protocol representation" do
    {name, _} = compile("(process idle 0)")
    pid = track(apply(name, :idle, [5]))

    for message <- [:unknown, {:"Process.idle.Message"}, {:unknown, self()}],
        do: send(pid, message)

    assert :sys.get_state(pid).value == 5
    assert Process.alive?(pid)

    assert error("(process idle 0) (defn main [] :unit (! (spawn idle 0) nil))").message =~
             "accepts no messages"
  end

  test "an atom command does not claim a user constructor's namespace" do
    {name, _} = compile("(deftype Value (Get :int)) (process counter 0 :Get (+ state 1))")
    pid = track(apply(name, :counter, [0]))
    assert GenServer.call(pid, :Get) == 1
  end

  test "the optional start_link entry does not overwrite a user's function" do
    {name, _} = compile(@counter <> "(defn start_link [n :int] :int (+ n 1))")
    assert apply(name, :start_link, [41]) == 42
    pid = track(apply(name, :counter, [0]))
    assert GenServer.call(pid, :inc) == 1
  end
end
