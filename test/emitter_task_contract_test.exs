defmodule Vaisto.EmitterTaskContractTest do
  use ExUnit.Case, async: false

  alias Vaisto.{Compilation, Error}

  setup do
    prev_llm = Application.get_env(:vaisto, :llm)
    prev_mock = Application.get_env(:vaisto, Vaisto.LLM.Mock)

    on_exit(fn ->
      restore_env(:llm, prev_llm)
      restore_env(Vaisto.LLM.Mock, prev_mock)
    end)

    :ok
  end

  test "pipeline with one generate op compiles and runs against the mock" do
    Application.put_env(:vaisto, :llm, Vaisto.LLM.Mock)

    parent = self()

    Application.put_env(:vaisto, Vaisto.LLM.Mock,
      call: fn prompt_text, input_data, output_type, opts ->
        send(parent, {:llm_call, prompt_text, input_data, output_type, opts})
        {:ok, %{text: "Short answer"}}
      end
    )

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
    (pipeline summarize-question
      :input Question
      :output Answer
      (generate :prompt summarize :extract Answer))
    """

    assert {:ok, EmitTaskContractE2E, _} = Compilation.compile(code, EmitTaskContractE2E)

    assert {:ok, {:Answer, "Short answer"}} ==
             apply(EmitTaskContractE2E, :"summarize-question", [{:Question, "A long passage"}])

    assert_received {:llm_call, "\nSummarize:\nA long passage\n", %{text: "A long passage"},
                     {:record, :Answer, [{:text, :string}]}, []}
  end

  test "pipeline referencing a prompt without template fails at emit time" do
    code = """
    (deftype Question [text :string])
    (deftype Answer [text :string])
    (defprompt summarize :input Question :output Answer)
    (pipeline summarize-question
      :input Question
      :output Answer
      (generate :prompt summarize :extract Answer))
    """


    assert {:error, [%Error{} = error]} = Compilation.compile(code, EmitTaskContractMissingTemplate, format_errors: false)
    assert error.message == "prompt missing template"
    assert error.note =~ "prompt `summarize` requires a :template clause"
  end

  defp restore_env(key, nil), do: Application.delete_env(:vaisto, key)
  defp restore_env(key, value), do: Application.put_env(:vaisto, key, value)
end
