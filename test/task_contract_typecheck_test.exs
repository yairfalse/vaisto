defmodule Vaisto.TaskContractTypecheckTest do
  use ExUnit.Case

  alias Vaisto.{Error, Parser, TypeChecker}

  test "defprompt and pipeline type-check on the happy path" do
    code = """
    (deftype Question [text :string])
    (deftype Answer [text :string])
    (defprompt summarize :input Question :output Answer)
    (pipeline answer-question
      :input Question
      :output Answer
      (generate :prompt summarize :extract Answer))
    """

    ast = Parser.parse(code)

    assert {:ok, :module,
            {:module,
             [
               {:deftype, :Question, {:product, [{:text, :string}]}, {:record, :Question, [{:text, :string}]}},
               {:deftype, :Answer, {:product, [{:text, :string}]}, {:record, :Answer, [{:text, :string}]}},
               {:defprompt, :summarize,
                {:record, :Question, [{:text, :string}]},
                {:record, :Answer, [{:text, :string}]},
                nil,
                :unit},
               {:pipeline, :"answer-question",
                {:record, :Question, [{:text, :string}]},
                {:record, :Answer, [{:text, :string}]},
                [
                  {:generate, :summarize, {:record, :Answer, [{:text, :string}]}, {:record, :Answer, [{:text, :string}]}}
                ],
                :unit}
             ]}} = TypeChecker.check(ast)
  end

  test "defprompt with template type-checks and carries template into typed AST" do
    code = """
    (deftype Question [text :string])
    (deftype Answer [text :string])
    (defprompt summarize
      :input Question
      :output Answer
      :template \"\"\"
    Summarize:
    {text}
    \"\"\")
    """

    ast = Parser.parse(code)

    assert {:ok, :module,
            {:module,
             [
               {:deftype, :Question, {:product, [{:text, :string}]}, {:record, :Question, [{:text, :string}]}},
               {:deftype, :Answer, {:product, [{:text, :string}]}, {:record, :Answer, [{:text, :string}]}},
               {:defprompt, :summarize,
                {:record, :Question, [{:text, :string}]},
                {:record, :Answer, [{:text, :string}]},
                "\nSummarize:\n{text}\n",
                :unit}
             ]}} = TypeChecker.check(ast)
  end

  test "missing field in prompt output yields prompt_output_mismatch" do
    code = """
    (deftype Full [x :int y :string])
    (deftype Partial [x :int])
    (defprompt p :input Full :output Partial)
    (pipeline q :input Full :output Full
      (generate :prompt p :extract Full))
    """

    ast = Parser.parse(code)

    assert {:error, [%Error{} = error]} = TypeChecker.check(ast)
    assert error.message == "prompt output type mismatch"
    assert error.note =~ "missing field: y : String"
    assert error.primary_span.line == 5
  end

  test "wrong field type in prompt output yields prompt_output_mismatch" do
    code = """
    (deftype Source [x :string])
    (deftype Target [x :int])
    (defprompt p :input Source :output Source)
    (pipeline q :input Source :output Target
      (generate :prompt p :extract Target))
    """

    ast = Parser.parse(code)

    assert {:error, [%Error{} = error]} = TypeChecker.check(ast)
    assert error.message == "prompt output type mismatch"
    assert error.note =~ "field `x` has type String, expected Int"
    assert error.primary_span.line == 5
  end

  test "unknown prompt yields undefined_prompt with suggestion" do
    code = """
    (deftype A [x :int])
    (defprompt summarize :input A :output A)
    (pipeline q :input A :output A
      (generate :prompt sumarize :extract A))
    """

    ast = Parser.parse(code)

    assert {:error, [%Error{} = error]} = TypeChecker.check(ast)
    assert error.message == "undefined prompt"
    assert error.hint == "did you mean `summarize`?"
    assert error.primary_span.line == 4
  end

  test "extra fields in prompt output pass because extract is a projection" do
    code = """
    (deftype Small [x :int])
    (deftype Large [x :int y :string])
    (defprompt p :input Small :output Large)
    (pipeline q :input Small :output Small
      (generate :prompt p :extract Small))
    """

    ast = Parser.parse(code)

    # v1 design choice: generate extract targets project the prompt output.
    assert {:ok, :module, _} = TypeChecker.check(ast)
  end
end
