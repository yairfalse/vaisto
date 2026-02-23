defmodule Vaisto.ErrorSuggestionsTest do
  use ExUnit.Case
  alias Vaisto.{Parser, TypeChecker, Error}

  defp check(code) do
    ast = Parser.parse(code)
    TypeChecker.check(ast)
  end

  defp error_hint({:error, %Error{hint: hint}}), do: hint
  defp error_hint({:error, errors}) when is_list(errors) do
    errors |> List.first() |> Map.get(:hint)
  end

  describe "did-you-mean for variables" do
    test "suggests similar variable in Errors.undefined_variable/2" do
      error = Vaisto.Errors.undefined_variable(:cont, [:count, :total])
      assert error.hint =~ "did you mean `count`?"
    end

    test "no suggestion when no similar variable" do
      error = Vaisto.Errors.undefined_variable(:zzzzz, [:x, :y])
      assert error.hint == nil
    end
  end

  describe "did-you-mean for functions" do
    test "suggests similar function name" do
      code = """
      (defn greet [name :string] :string name)
      (greet "hi")
      (gret "hi")
      """
      result = check(code)
      assert {:error, _} = result
      hint = error_hint(result)
      assert hint =~ "did you mean `greet`?"
    end

    test "falls back to builtin suggestions" do
      code = "(maap [1 2 3] (fn [x] x))"
      result = check(code)
      assert {:error, _} = result
      hint = error_hint(result)
      assert hint =~ "did you mean `map`?"
    end
  end

  describe "did-you-mean for processes" do
    test "suggests similar process name" do
      code = """
      (process counter 0
        :increment (+ state 1))
      (spawn conter 0)
      """
      result = check(code)
      assert {:error, _} = result
      hint = error_hint(result)
      assert hint =~ "did you mean `counter`?"
    end
  end
end
