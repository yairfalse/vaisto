defmodule Vaisto.CoreEmitterTest do
  use ExUnit.Case
  alias Vaisto.{Parser, TypeChecker, CoreEmitter}

  describe "arithmetic expressions" do
    test "compiles simple addition" do
      {:ok, _type, typed} = TypeChecker.check({:call, :+, [1, 2]})
      {:ok, mod, _binary} = CoreEmitter.compile(typed, CoreAdd)
      :code.load_binary(mod, ~c"test", _binary)

      assert CoreAdd.main() == 3
    end

    test "compiles nested arithmetic" do
      code = "(* (+ 1 2) (- 10 3))"
      ast = Parser.parse(code)
      {:ok, _type, typed} = TypeChecker.check(ast)
      {:ok, mod, _binary} = CoreEmitter.compile(typed, CoreNested)
      :code.load_binary(mod, ~c"test", _binary)

      assert CoreNested.main() == 21
    end
  end

  describe "process compilation" do
    test "compiles a counter process" do
      code = """
      (process counter 0
        :increment (+ state 1)
        :get state)
      """
      ast = Parser.parse(code)
      {:ok, _type, typed} = TypeChecker.check(ast)
      {:ok, mod, binary} = CoreEmitter.compile(typed, CoreTestCounter)

      # Should be small - raw BEAM process
      assert byte_size(binary) < 1000

      :code.load_binary(mod, ~c"test", binary)

      # Start the process
      pid = CoreTestCounter.start_link(0)
      assert is_pid(pid)

      # Helper to call
      call = fn msg ->
        send(pid, {msg, self()})
        receive do
          response -> response
        after
          1000 -> :timeout
        end
      end

      # Test operations
      assert call.(:get) == 0
      assert call.(:increment) == 1
      assert call.(:get) == 1
      assert call.(:increment) == 2
      assert call.(:increment) == 3
      assert call.(:get) == 3
    end
  end

  describe "bytecode size comparison" do
    test "Core Erlang produces smaller bytecode than Elixir path" do
      code = "(+ 1 2)"
      ast = Parser.parse(code)
      {:ok, _type, typed} = TypeChecker.check(ast)

      {:ok, _mod1, core_binary} = CoreEmitter.compile(typed, SizeTestCore)
      {:ok, _mod2, elixir_binary} = Vaisto.Emitter.compile(typed, SizeTestElixir)

      # Core Erlang should be significantly smaller
      assert byte_size(core_binary) < byte_size(elixir_binary)
    end
  end
end
