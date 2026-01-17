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

  describe "ADT (sum type) compilation" do
    test "compiles variant construction" do
      code = """
      (deftype Result (Ok v) (Error msg))
      (Ok 42)
      """
      ast = Parser.parse(code)
      {:ok, _type, typed} = TypeChecker.check(ast)
      {:ok, mod, binary} = CoreEmitter.compile(typed, CoreOkCtor)
      :code.load_binary(mod, ~c"test", binary)

      assert {:Ok, 42} = CoreOkCtor.main()
    end

    test "compiles variant construction with no fields" do
      code = """
      (deftype Option (Some v) (None))
      (None)
      """
      ast = Parser.parse(code)
      {:ok, _type, typed} = TypeChecker.check(ast)
      {:ok, mod, binary} = CoreEmitter.compile(typed, CoreNoneCtor)
      :code.load_binary(mod, ~c"test", binary)

      assert {:None} = CoreNoneCtor.main()
    end

    test "compiles pattern matching on variants" do
      code = """
      (deftype Result (Ok v) (Error msg))
      (match (Ok 42)
        [(Ok v) v]
        [(Error _) 0])
      """
      ast = Parser.parse(code)
      {:ok, _type, typed} = TypeChecker.check(ast)
      {:ok, mod, binary} = CoreEmitter.compile(typed, CoreMatchOk)
      :code.load_binary(mod, ~c"test", binary)

      assert 42 = CoreMatchOk.main()
    end

    test "compiles pattern matching on Error variant" do
      code = """
      (deftype Result (Ok v) (Error msg))
      (match (Error "failed")
        [(Ok _) 0]
        [(Error m) 1])
      """
      ast = Parser.parse(code)
      {:ok, _type, typed} = TypeChecker.check(ast)
      {:ok, mod, binary} = CoreEmitter.compile(typed, CoreMatchErr)
      :code.load_binary(mod, ~c"test", binary)

      assert 1 = CoreMatchErr.main()
    end

    test "compiles function returning sum type" do
      code = """
      (deftype Maybe (Just v) (Nothing))
      (defn wrap [x] (Just x))
      (match (wrap 100)
        [(Just v) v]
        [(Nothing) 0])
      """
      ast = Parser.parse(code)
      {:ok, _type, typed} = TypeChecker.check(ast)
      {:ok, mod, binary} = CoreEmitter.compile(typed, CoreWrapFn)
      :code.load_binary(mod, ~c"test", binary)

      assert 100 = CoreWrapFn.main()
    end

    test "compiles recursive ADT (binary tree)" do
      code = """
      (deftype Tree (Leaf v) (Node left right))
      (defn tree-sum [t]
        (match t
          [(Leaf v) v]
          [(Node l r) (+ (tree-sum l) (tree-sum r))]))
      (tree-sum (Node (Leaf 1) (Node (Leaf 2) (Leaf 3))))
      """
      ast = Parser.parse(code)
      {:ok, _type, typed} = TypeChecker.check(ast)
      {:ok, mod, binary} = CoreEmitter.compile(typed, CoreTreeSum)
      :code.load_binary(mod, ~c"test", binary)

      assert 6 = CoreTreeSum.main()
    end
  end

  describe "match-tuple (Erlang interop)" do
    test "matches raw Erlang-style tuples" do
      code = """
      (defn safe-div [x y]
        (match-tuple (if (== y 0) {:error "div by zero"} {:ok (/ x y)})
          [{:ok result} result]
          [{:error msg} 0]))
      (safe-div 10 2)
      """
      ast = Parser.parse(code)
      {:ok, _type, typed} = TypeChecker.check(ast)
      {:ok, mod, binary} = CoreEmitter.compile(typed, CoreSafeDiv)
      :code.load_binary(mod, ~c"test", binary)

      assert 5.0 = CoreSafeDiv.main()
    end

    test "matches error case" do
      code = """
      (match-tuple {:error 42}
        [{:ok v} v]
        [{:error e} (+ e 100)])
      """
      ast = Parser.parse(code)
      {:ok, _type, typed} = TypeChecker.check(ast)
      {:ok, mod, binary} = CoreEmitter.compile(typed, CoreMatchError)
      :code.load_binary(mod, ~c"test", binary)

      assert 142 = CoreMatchError.main()
    end

    test "constructs raw tuples" do
      code = "{:ok 42}"
      ast = Parser.parse(code)
      {:ok, _type, typed} = TypeChecker.check(ast)
      {:ok, mod, binary} = CoreEmitter.compile(typed, CoreTuple)
      :code.load_binary(mod, ~c"test", binary)

      assert {:ok, 42} = CoreTuple.main()
    end
  end
end
