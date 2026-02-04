defmodule Vaisto.Std.OptionResultTest do
  use ExUnit.Case
  alias Vaisto.Runner

  # Load prelude content for tests
  @prelude File.read!("std/prelude.va")

  defp with_prelude(source) do
    @prelude <> "\n\n" <> source
  end

  describe "Option type" do
    test "option-unwrap returns value for Some" do
      source = """
      (option-unwrap (Some 42) 0)
      """
      assert {:ok, mod} = Runner.compile_and_load(with_prelude(source), :OptionUnwrapSome)
      assert Runner.call(mod, :main) == 42
    end

    test "option-unwrap returns default for None" do
      source = """
      (option-unwrap (None) 99)
      """
      assert {:ok, mod} = Runner.compile_and_load(with_prelude(source), :OptionUnwrapNone)
      assert Runner.call(mod, :main) == 99
    end

    test "option-map applies function to Some" do
      source = """
      (option-map (Some 5) (fn [x] (* x 2)))
      """
      assert {:ok, mod} = Runner.compile_and_load(with_prelude(source), :OptionMapSome)
      assert Runner.call(mod, :main) == {:Some, 10}
    end

    test "option-map returns None for None" do
      source = """
      (option-map (None) (fn [x] (* x 2)))
      """
      assert {:ok, mod} = Runner.compile_and_load(with_prelude(source), :OptionMapNone)
      assert Runner.call(mod, :main) == {:None}
    end

    test "option-then chains operations" do
      source = """
      (option-then (Some 10) (fn [x] (if (> x 5) (Some (* x 2)) (None))))
      """
      assert {:ok, mod} = Runner.compile_and_load(with_prelude(source), :OptionThenSome)
      assert Runner.call(mod, :main) == {:Some, 20}
    end

    test "option-then returns None when chained on None" do
      source = """
      (option-then (None) (fn [x] (Some x)))
      """
      assert {:ok, mod} = Runner.compile_and_load(with_prelude(source), :OptionThenNone)
      assert Runner.call(mod, :main) == {:None}
    end

    test "option-some? returns true for Some" do
      source = """
      (option-some? (Some 1))
      """
      assert {:ok, mod} = Runner.compile_and_load(with_prelude(source), :OptionSomeTrue)
      assert Runner.call(mod, :main) == true
    end

    test "option-some? returns false for None" do
      source = """
      (option-some? (None))
      """
      assert {:ok, mod} = Runner.compile_and_load(with_prelude(source), :OptionSomeFalse)
      assert Runner.call(mod, :main) == false
    end
  end

  describe "Result type" do
    test "result-unwrap returns value for Ok" do
      source = """
      (result-unwrap (Ok 42) 0)
      """
      assert {:ok, mod} = Runner.compile_and_load(with_prelude(source), :ResultUnwrapOk)
      assert Runner.call(mod, :main) == 42
    end

    test "result-unwrap returns default for Err" do
      source = """
      (result-unwrap (Err "failed") 99)
      """
      assert {:ok, mod} = Runner.compile_and_load(with_prelude(source), :ResultUnwrapErr)
      assert Runner.call(mod, :main) == 99
    end

    test "result-map applies function to Ok" do
      source = """
      (result-map (Ok 5) (fn [x] (* x 2)))
      """
      assert {:ok, mod} = Runner.compile_and_load(with_prelude(source), :ResultMapOk)
      assert Runner.call(mod, :main) == {:Ok, 10}
    end

    test "result-map preserves Err" do
      source = """
      (result-map (Err "oops") (fn [x] (* x 2)))
      """
      assert {:ok, mod} = Runner.compile_and_load(with_prelude(source), :ResultMapErr)
      assert Runner.call(mod, :main) == {:Err, "oops"}
    end

    test "result-then chains successful operations" do
      source = """
      (defn safe-divide [n d]
        (if (== d 0)
          (Err "division by zero")
          (Ok (/ n d))))
      (result-then (Ok 10) (fn [x] (safe-divide x 2)))
      """
      assert {:ok, mod} = Runner.compile_and_load(with_prelude(source), :ResultThenOk)
      assert Runner.call(mod, :main) == {:Ok, 5.0}
    end

    test "result-then short-circuits on Err" do
      source = """
      (result-then (Err "first error") (fn [x] (Ok (* x 2))))
      """
      assert {:ok, mod} = Runner.compile_and_load(with_prelude(source), :ResultThenErr)
      assert Runner.call(mod, :main) == {:Err, "first error"}
    end

    test "result-ok? returns true for Ok" do
      source = """
      (result-ok? (Ok 42))
      """
      assert {:ok, mod} = Runner.compile_and_load(with_prelude(source), :ResultOkTrue)
      assert Runner.call(mod, :main) == true
    end

    test "result-ok? returns false for Err" do
      source = """
      (result-ok? (Err "nope"))
      """
      assert {:ok, mod} = Runner.compile_and_load(with_prelude(source), :ResultOkFalse)
      assert Runner.call(mod, :main) == false
    end
  end

  describe "Erlang interop" do
    test "from-erlang-result converts {:ok, v} to Ok" do
      source = """
      (from-erlang-result {:ok 42})
      """
      assert {:ok, mod} = Runner.compile_and_load(with_prelude(source), :FromErlangOk)
      assert Runner.call(mod, :main) == {:Ok, 42}
    end

    test "from-erlang-result converts {:error, e} to Err" do
      source = """
      (from-erlang-result {:error "bad"})
      """
      assert {:ok, mod} = Runner.compile_and_load(with_prelude(source), :FromErlangErr)
      assert Runner.call(mod, :main) == {:Err, "bad"}
    end

    test "from-erlang-result converts :ok atom to Ok(:ok)" do
      source = """
      (from-erlang-result :ok)
      """
      assert {:ok, mod} = Runner.compile_and_load(with_prelude(source), :FromErlangAtom)
      assert Runner.call(mod, :main) == {:Ok, :ok}
    end

    test "to-erlang-result converts Ok to {:ok, v}" do
      source = """
      (to-erlang-result (Ok 42))
      """
      assert {:ok, mod} = Runner.compile_and_load(with_prelude(source), :ToErlangOk)
      assert Runner.call(mod, :main) == {:ok, 42}
    end

    test "to-erlang-result converts Err to {:error, e}" do
      source = """
      (to-erlang-result (Err "failed"))
      """
      assert {:ok, mod} = Runner.compile_and_load(with_prelude(source), :ToErlangErr)
      assert Runner.call(mod, :main) == {:error, "failed"}
    end
  end

  describe "Boolean helpers" do
    test "not negation works via if" do
      # Test boolean negation pattern used in prelude
      source = """
      (defn my-not [b]
        (if b false true))
      (list (my-not true) (my-not false))
      """
      assert {:ok, mod} = Runner.compile_and_load(with_prelude(source), :MyNot)
      assert Runner.call(mod, :main) == [false, true]
    end
  end
end
