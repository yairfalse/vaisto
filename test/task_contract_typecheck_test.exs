defmodule Vaisto.TaskContractTypecheckTest do
  use ExUnit.Case
  alias Vaisto.{Elab, Error, Parser}
  alias Vaisto.Liquid.Lint

  test "prompt and pipeline elaborate to a checked typed effect boundary" do
    source = """
    (deftype Question [text :string])
    (deftype Answer [text :string])
    (defprompt summarize :input Question :output Answer :template "{text}")
    (pipeline answer-question :input Question :output Answer
      (generate :prompt summarize :extract Answer))
    """
    assert {:ok, [:module, _, _ | items] = core} = source |> Parser.parse() |> Elab.module()
    assert :ok = Lint.check_module(core)
    assert [:def, :"answer-question", [:->, [[:Question]], [:eff, :external, :closed], [:"Core.TaskResult", [:Answer]]], _] = Enum.find(items, &match?([:def, :"answer-question", _, _], &1))
  end

  test "the prompt template is retained at the checked generate boundary" do
    source = "(deftype A [text :string]) (defprompt p :input A :output A :template \"Summarize: {text}\") (pipeline q :input A :output A (generate :prompt p :extract A))"
    assert {:ok, core} = source |> Parser.parse() |> Elab.module()
    assert :ok = Lint.check_module(core)
    assert "Summarize: {text}" in List.flatten(core)
  end

  test "missing field in prompt output yields prompt_output_mismatch" do
    code = """
    (deftype Full [x :int y :string])
    (deftype Partial [x :int])
    (defprompt p :input Full :output Partial :template "prompt")
    (pipeline q :input Full :output Full
      (generate :prompt p :extract Full))
    """

    ast = Parser.parse(code)

    assert {:error, [%Error{} = error]} = Elab.module(ast)
    assert error.message == "task projection is missing field `y`"
    assert error.primary_span.line == 5
  end

  test "wrong field type in prompt output yields prompt_output_mismatch" do
    code = """
    (deftype Source [x :string])
    (deftype Target [x :int])
    (defprompt p :input Source :output Source :template "prompt")
    (pipeline q :input Source :output Target
      (generate :prompt p :extract Target))
    """

    ast = Parser.parse(code)

    assert {:error, [%Error{} = error]} = Elab.module(ast)
    assert error.message == "type mismatch"
    assert error.note == "expected Int, found String"
    assert error.primary_span.line == 5
  end

  test "unknown prompt yields undefined_prompt with suggestion" do
    code = """
    (deftype A [x :int])
    (defprompt summarize :input A :output A :template "prompt")
    (pipeline q :input A :output A
      (generate :prompt sumarize :extract A))
    """

    ast = Parser.parse(code)

    assert {:error, [%Error{} = error]} = Elab.module(ast)
    assert error.message == "undefined prompt"
    assert error.hint == "did you mean `summarize`?"
    assert error.primary_span.line == 4
  end

  test "extra fields in prompt output pass because extract is a projection" do
    code = """
    (deftype Small [x :int])
    (deftype Large [x :int y :string])
    (defprompt p :input Small :output Large :template "prompt")
    (pipeline q :input Small :output Small
      (generate :prompt p :extract Small))
    """

    ast = Parser.parse(code)

    # v1 design choice: generate extract targets project the prompt output.
    assert {:ok, core} = Elab.module(ast)
    assert :ok = Lint.check_module(core)
  end
end
