defmodule Vaisto.LLM.OpenAI.Schema do
  @moduledoc false

  @doc "Whether a closed Core type has a finite structured-output schema."
  def core_supported?(type, declarations) do
    core_descriptor(type, declarations)
    true
  rescue
    ArgumentError -> false
  end

  @doc "Translate checked Core data into the LLM driver's runtime schema descriptor."
  def core_descriptor(type, declarations) when is_list(declarations) do
    types = Map.new(declarations, fn [:type, name, params, [kind | members]] -> {name, {params, kind, members}} end)
    core_descriptor(type, types)
  end
  def core_descriptor(type, types) when is_map(types), do: core_descriptor(type, types, MapSet.new())

  defp core_descriptor(:Int, _, _), do: :int
  defp core_descriptor(:Float, _, _), do: :float
  defp core_descriptor(:String, _, _), do: :string
  defp core_descriptor([:Bool], _, _), do: :bool
  defp core_descriptor([:List, t], types, seen), do: {:list, core_descriptor(t, types, seen)}
  defp core_descriptor([name | args] = type, types, seen) do
    case Map.get(types, name) do
      {params, :record, fields} when length(params) == length(args) ->
        if MapSet.member?(seen, name), do: raise(ArgumentError, "recursive output schemas are not supported")
        subst = Map.new(Enum.zip(params, args))
        fields = Enum.map(fields, fn [label, t] ->
          {label, core_descriptor(Vaisto.Elab.Types.subst_tvars(t, subst), types, MapSet.put(seen, name))}
        end)
        {:record, name, fields}
      _ -> raise ArgumentError, "unsupported structured-output Core type: #{inspect(type)}"
    end
  end
  defp core_descriptor(type, _, _), do: raise(ArgumentError, "unsupported structured-output Core type: #{inspect(type)}")

  @spec to_json_schema(term()) :: map()
  def to_json_schema(:string), do: %{"type" => "string"}
  def to_json_schema(:int), do: %{"type" => "integer"}
  def to_json_schema(:bool), do: %{"type" => "boolean"}
  def to_json_schema(:float), do: %{"type" => "number"}

  def to_json_schema({:list, elem_type}) do
    %{
      "type" => "array",
      "items" => to_json_schema(elem_type)
    }
  end

  def to_json_schema({:record, _name, fields}) when is_list(fields) do
    properties =
      Map.new(fields, fn {field_name, field_type} ->
        {Atom.to_string(field_name), to_json_schema(field_type)}
      end)

    %{
      "type" => "object",
      "properties" => properties,
      "required" => Enum.map(fields, fn {field_name, _field_type} -> Atom.to_string(field_name) end),
      "additionalProperties" => false
    }
  end

  def to_json_schema(other) do
    raise ArgumentError, "OpenAI structured outputs do not support type: #{inspect(other)}"
  end
end
