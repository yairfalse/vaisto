defmodule Vaisto.RunnerTest do
  use ExUnit.Case
  alias Vaisto.Runner

  describe "compile_and_load/2" do
    test "compiles and loads simple expression" do
      source = "(+ 1 2)"
      assert {:ok, :SimpleExpr} = Runner.compile_and_load(source, :SimpleExpr)
      assert Runner.module_loaded?(:SimpleExpr)
      assert Runner.call(:SimpleExpr, :main) == 3
    end

    test "compiles and loads function definition with expression" do
      # Note: defn alone doesn't create main, need an expression too
      source = """
      (defn double [x] (* x 2))
      (double 21)
      """
      assert {:ok, :DoubleMod} = Runner.compile_and_load(source, :DoubleMod)
      assert Runner.call(:DoubleMod, :main) == 42
      assert Runner.call(:DoubleMod, :double, [10]) == 20
    end

    test "compiles multiple functions" do
      source = """
      (defn square [x] (* x x))
      (defn cube [x] (* x (square x)))
      (cube 3)
      """
      assert {:ok, :MathMod} = Runner.compile_and_load(source, :MathMod)
      assert Runner.call(:MathMod, :main) == 27
      assert Runner.call(:MathMod, :square, [4]) == 16
      assert Runner.call(:MathMod, :cube, [2]) == 8
    end

    test "returns error for type mismatch" do
      source = "(+ 1 :atom)"
      assert {:error, _} = Runner.compile_and_load(source, :BadMod)
    end

    test "returns error for parse error" do
      source = "(+ 1"  # Missing closing paren
      assert {:error, _} = Runner.compile_and_load(source, :ParseErr)
    end
  end

  describe "run/1" do
    test "runs simple expression" do
      assert {:ok, 3} = Runner.run("(+ 1 2)")
    end

    test "runs let binding" do
      assert {:ok, 100} = Runner.run("(let [x 10] (* x x))")
    end

    test "runs if expression" do
      assert {:ok, 1} = Runner.run("(if true 1 2)")
      assert {:ok, 2} = Runner.run("(if false 1 2)")
    end

    test "runs list operations" do
      assert {:ok, [1, 2, 3]} = Runner.run("(list 1 2 3)")
      assert {:ok, 1} = Runner.run("(head (list 1 2 3))")
      assert {:ok, [2, 3]} = Runner.run("(tail (list 1 2 3))")
    end

    test "runs nested arithmetic" do
      assert {:ok, 21} = Runner.run("(+ (* 2 3) (* 3 5))")
    end

    test "cleans up temporary module" do
      {:ok, _} = Runner.run("(+ 1 1)")
      # Can't easily check cleanup, but it shouldn't error
    end
  end

  describe "call/3" do
    test "calls function with arguments" do
      source = """
      (defn add [x y] (+ x y))
      (add 1 1)
      """
      {:ok, :AddMod} = Runner.compile_and_load(source, :AddMod)

      assert Runner.call(:AddMod, :add, [10, 20]) == 30
    end

    test "calls main with no arguments" do
      source = "(* 6 7)"
      {:ok, :MainMod} = Runner.compile_and_load(source, :MainMod)

      assert Runner.call(:MainMod, :main) == 42
    end
  end

  describe "module management" do
    test "module_loaded? returns true for loaded module" do
      source = "(+ 1 1)"
      {:ok, :LoadCheck} = Runner.compile_and_load(source, :LoadCheck)

      assert Runner.module_loaded?(:LoadCheck)
    end

    test "module_loaded? returns false for unloaded module" do
      refute Runner.module_loaded?(:NonExistentModule12345)
    end

    test "unload_module removes module" do
      source = "(+ 2 2)"
      {:ok, :UnloadTest} = Runner.compile_and_load(source, :UnloadTest)
      assert Runner.module_loaded?(:UnloadTest)

      Runner.unload_module(:UnloadTest)
      refute Runner.module_loaded?(:UnloadTest)
    end
  end

  describe "process interop" do
    test "spawns and interacts with process" do
      source = """
      (process counter 0
        :inc (+ state 1)
        :dec (- state 1)
        :get state)
      """
      # Process module is scoped under parent: CounterApp.Counter
      {:ok, mod} = Runner.compile_and_load(source, :CounterApp)

      {:ok, pid} = Runner.spawn_process(mod, 0)
      assert is_pid(pid)

      assert Runner.send_msg(pid, :get) == 0
      assert Runner.send_msg(pid, :inc) == 1
      assert Runner.send_msg(pid, :inc) == 2
      assert Runner.send_msg(pid, :dec) == 1
      assert Runner.send_msg(pid, :get) == 1

      GenServer.stop(pid)
    end
  end

  describe "polymorphic functions" do
    test "identity function works with integers" do
      source = """
      (defn id [x] x)
      (id 42)
      """
      {:ok, :IdIntMod} = Runner.compile_and_load(source, :IdIntMod)
      assert Runner.call(:IdIntMod, :main) == 42
    end

    test "identity used multiple times in let" do
      source = """
      (defn id [x] x)
      (let [a (id 1)
            b (id 2)]
        (+ a b))
      """
      {:ok, :IdLetMod} = Runner.compile_and_load(source, :IdLetMod)
      assert Runner.call(:IdLetMod, :main) == 3
    end

    test "map with anonymous function" do
      source = """
      (map (fn [x] (* x 2)) (list 1 2 3 4 5))
      """
      {:ok, :MapAnonMod} = Runner.compile_and_load(source, :MapAnonMod)
      assert Runner.call(:MapAnonMod, :main) == [2, 4, 6, 8, 10]
    end

    test "filter with anonymous function" do
      source = """
      (filter (fn [x] (> x 2)) (list 1 2 3 4 5))
      """
      {:ok, :FilterAnonMod} = Runner.compile_and_load(source, :FilterAnonMod)
      assert Runner.call(:FilterAnonMod, :main) == [3, 4, 5]
    end

    test "fold with anonymous function" do
      source = """
      (fold (fn [acc x] (+ acc x)) 0 (list 1 2 3 4 5))
      """
      {:ok, :FoldAnonMod} = Runner.compile_and_load(source, :FoldAnonMod)
      assert Runner.call(:FoldAnonMod, :main) == 15
    end

    test "chained higher-order functions" do
      source = """
      (fold (fn [acc x] (+ acc x)) 0
        (filter (fn [x] (> x 2))
          (map (fn [x] (* x 2)) (list 1 2 3 4 5))))
      """
      # map: [2, 4, 6, 8, 10]
      # filter (> x 2): [4, 6, 8, 10]
      # fold: 4 + 6 + 8 + 10 = 28
      {:ok, :ChainMod} = Runner.compile_and_load(source, :ChainMod)
      assert Runner.call(:ChainMod, :main) == 28
    end
  end

  describe "recursive functions" do
    test "factorial" do
      source = """
      (defn fact [n]
        (if (== n 0)
          1
          (* n (fact (- n 1)))))
      (fact 5)
      """
      {:ok, :FactMod} = Runner.compile_and_load(source, :FactMod)
      assert Runner.call(:FactMod, :main) == 120
    end

    test "fibonacci" do
      source = """
      (defn fib [n]
        (if (< n 2)
          n
          (+ (fib (- n 1)) (fib (- n 2)))))
      (fib 10)
      """
      {:ok, :FibMod} = Runner.compile_and_load(source, :FibMod)
      assert Runner.call(:FibMod, :main) == 55
    end
  end

  describe "records" do
    test "record construction and pattern matching" do
      source = """
      (deftype point [x :int y :int])
      (defn get-x [p]
        (match p
          [(point x y) x]))
      (get-x (point 10 20))
      """
      {:ok, :PointMod} = Runner.compile_and_load(source, :PointMod)
      assert Runner.call(:PointMod, :main) == 10
    end
  end
end
