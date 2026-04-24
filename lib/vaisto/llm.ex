defmodule Vaisto.LLM do
  @moduledoc false

  @callback call(
              prompt_text :: String.t(),
              input_data :: map(),
              output_type :: term(),
              opts :: keyword()
            ) :: {:ok, map()} | {:error, term()}

  @spec call(String.t(), map(), term(), keyword()) :: {:ok, map()} | {:error, term()}
  def call(prompt_text, input_data, output_type, opts \\ []) do
    impl().call(prompt_text, input_data, output_type, opts)
  end

  @spec impl() :: module()
  def impl do
    Application.get_env(:vaisto, :llm, Vaisto.LLM.Mock)
  end
end
