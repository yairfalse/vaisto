defmodule Vaisto.LLMTest do
  use ExUnit.Case, async: false

  defmodule DispatchStub do
    @behaviour Vaisto.LLM

    @impl true
    def call(prompt_text, input_data, output_type, opts) do
      send(self(), {:llm_dispatch, prompt_text, input_data, output_type, opts})
      {:ok, %{status: "dispatched"}}
    end
  end

  setup do
    prev_impl = Application.get_env(:vaisto, :llm)

    on_exit(fn ->
      restore_env(:llm, prev_impl)
    end)

    :ok
  end

  test "call/4 dispatches to the configured implementation" do
    Application.put_env(:vaisto, :llm, DispatchStub)

    output_type = {:record, :Answer, [{:text, :string}]}

    assert {:ok, %{status: "dispatched"}} ==
             Vaisto.LLM.call("Summarize this", %{text: "input"}, output_type, temperature: 0.2)

    assert_received {:llm_dispatch, "Summarize this", %{text: "input"}, ^output_type,
                     [temperature: 0.2]}
  end

  defp restore_env(key, nil), do: Application.delete_env(:vaisto, key)
  defp restore_env(key, value), do: Application.put_env(:vaisto, key, value)
end
