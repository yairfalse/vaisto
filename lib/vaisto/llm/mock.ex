defmodule Vaisto.LLM.Mock do
  @moduledoc false

  @behaviour Vaisto.LLM

  @type config :: keyword()

  @impl true
  def call(prompt_text, input_data, output_type, opts) do
    case Application.get_env(:vaisto, __MODULE__, []) do
      config when is_list(config) ->
        dispatch(config, prompt_text, input_data, output_type, opts)

      _other ->
        {:ok, default_output(output_type)}
    end
  end

  @spec default_output(term()) :: map()
  def default_output({:record, _name, fields}) when is_list(fields) do
    Map.new(fields, fn {field_name, field_type} ->
      {field_name, default_value(field_type)}
    end)
  end

  def default_output(_other), do: %{}

  defp dispatch(config, prompt_text, input_data, output_type, opts) do
    cond do
      fun = Keyword.get(config, :call) ->
        fun.(prompt_text, input_data, output_type, opts)

      Keyword.has_key?(config, :response) ->
        {:ok, Keyword.fetch!(config, :response)}

      true ->
        {:ok, default_output(output_type)}
    end
  end

  defp default_value(:string), do: ""
  defp default_value(:int), do: 0
  defp default_value(:bool), do: false
  defp default_value(:float), do: 0.0
  defp default_value(:num), do: 0
  defp default_value(:atom), do: nil
  defp default_value(:unit), do: nil
  defp default_value({:list, _elem_type}), do: []
  defp default_value({:record, _name, fields}), do: default_output({:record, nil, fields})
  defp default_value(_other), do: nil
end
