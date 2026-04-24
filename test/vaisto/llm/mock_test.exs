defmodule Vaisto.LLM.MockTest do
  use ExUnit.Case, async: false

  setup do
    prev_config = Application.get_env(:vaisto, Vaisto.LLM.Mock)

    on_exit(fn ->
      restore_env(Vaisto.LLM.Mock, prev_config)
    end)

    :ok
  end

  test "returns a configured override when one is set" do
    Application.put_env(:vaisto, Vaisto.LLM.Mock,
      call: fn prompt_text, input_data, output_type, opts ->
        send(self(), {:mock_call, prompt_text, input_data, output_type, opts})
        {:ok, %{text: "configured"}}
      end
    )

    output_type = {:record, :Answer, [{:text, :string}]}

    assert {:ok, %{text: "configured"}} ==
             Vaisto.LLM.Mock.call("Prompt", %{text: "input"}, output_type, model: "mock-1")

    assert_received {:mock_call, "Prompt", %{text: "input"}, ^output_type, [model: "mock-1"]}
  end

  test "returns a default-shaped response when no override is set" do
    Application.delete_env(:vaisto, Vaisto.LLM.Mock)

    assert {:ok, %{ok: false, text: "", count: 0, items: []}} ==
             Vaisto.LLM.Mock.call(
               "Prompt",
               %{},
               {:record, :Answer, [{:ok, :bool}, {:text, :string}, {:count, :int}, {:items, {:list, :string}}]},
               []
             )
  end

  test "default generation handles a nested record correctly" do
    Application.delete_env(:vaisto, Vaisto.LLM.Mock)

    output_type =
      {:record, :Outer,
       [
         {:title, :string},
         {:meta, {:record, :Meta, [{:published, :bool}, {:summary, :string}]}},
         {:scores, {:list, :int}}
       ]}

    assert {:ok,
            %{
              title: "",
              meta: %{published: false, summary: ""},
              scores: []
            }} == Vaisto.LLM.Mock.call("Prompt", %{}, output_type, [])
  end

  test "returns a configured static response when one is set" do
    Application.put_env(:vaisto, Vaisto.LLM.Mock, response: %{text: "static"})

    assert {:ok, %{text: "static"}} ==
             Vaisto.LLM.Mock.call("Prompt", %{}, {:record, :Answer, [{:text, :string}]}, [])
  end

  defp restore_env(key, nil), do: Application.delete_env(:vaisto, key)
  defp restore_env(key, value), do: Application.put_env(:vaisto, key, value)
end
