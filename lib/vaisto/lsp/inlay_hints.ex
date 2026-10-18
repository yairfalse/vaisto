defmodule Vaisto.LSP.InlayHints do
  @moduledoc """
  Inferred local binding and lambda-parameter hints from verified native Core.
  Every hint comes from the expression's actual lexical context, not a name scan.
  """

  alias Vaisto.{Compilation, Parser, TypeFormatter}
  alias Vaisto.LSP.{Hover, Position}

  def collect(source, file \\ nil) do
    with {:ok, ast} <- Compilation.parse(source, file: file) do
      source_map = Parser.source_map(source, file: file)
      candidates = candidates(ast, source, source_map)

      ranges =
        candidates
        |> Enum.map(& &1.range)
        |> Enum.uniq_by(&{&1.offset, &1.length})
        |> Enum.with_index(fn range, id -> Map.put(range, :id, id) end)

      ids = Map.new(ranges, &{{&1.offset, &1.length}, &1.id})

      if ranges == [] do
        []
      else
        with {:ok, types, _} <- Hover.infer_ranges(source, ranges, file) do
          candidates
          |> Enum.flat_map(fn candidate ->
            id = Map.fetch!(ids, {candidate.range.offset, candidate.range.length})

            with %{type: type} <- Map.get(types, id),
                 type when type != nil <- hinted_type(type, candidate.mode) do
              [
                %{
                  "position" => Position.from_offset(source, candidate.position),
                  "label" => ": " <> TypeFormatter.format(type),
                  "kind" => 1,
                  "paddingLeft" => false,
                  "paddingRight" => true
                }
              ]
            else
              _ -> []
            end
          end)
          |> Enum.uniq()
          |> Enum.sort_by(&{&1["position"]["line"], &1["position"]["character"]})
        else
          _ -> []
        end
      end
    else
      _ -> []
    end
  rescue
    _ -> []
  end

  defp hinted_type(type, :value), do: type

  defp hinted_type([:->, params, _, _], {:parameter, index, count}) when length(params) == count,
    do: Enum.at(params, index)

  defp hinted_type(_, _), do: nil

  defp candidates({:let, bindings, _, loc} = node, source, source_map) do
    own =
      case parameter_source(node, "let", loc, source, source_map) do
        {:ok, content, start} ->
          entries = Parser.parse_with_source(content, locations: true)

          Enum.zip(bindings, Enum.chunk_every(entries, 2))
          |> Enum.flat_map(fn
            {{name, _}, [{name, _, name_loc}, {_, _, value_loc}]}
            when is_atom(name) and name not in [:_, true, false, nil] ->
              [
                %{
                  range: %{offset: start + offset(content, value_loc), length: value_loc.length},
                  position: start + offset(content, name_loc) + name_loc.length,
                  mode: :value
                }
              ]

            _ ->
              []
          end)

        _ ->
          []
      end

    own ++ descendants(node, source, source_map)
  end

  defp candidates({:fn, params, _, loc} = node, source, source_map) do
    own =
      if Enum.all?(params, &(is_atom(&1) and &1 not in [:_, true, false, nil])) do
        with {:ok, content, start} <- parameter_source(node, "fn", loc, source, source_map),
             range when not is_nil(range) <- Map.get(source_map.spans, {loc.line, loc.col}) do
          entries = Parser.parse_with_source(content, locations: true)

          if length(entries) == length(params) do
            entries
            |> Enum.with_index()
            |> Enum.flat_map(fn
              {{name, _, name_loc}, index} when is_atom(name) ->
                [
                  %{
                    range: range,
                    position: start + offset(content, name_loc) + name_loc.length,
                    mode: {:parameter, index, length(params)}
                  }
                ]

              _ ->
                []
            end)
          else
            []
          end
        else
          _ -> []
        end
      else
        []
      end

    own ++ descendants(node, source, source_map)
  end

  defp candidates(node, source, source_map) when is_tuple(node),
    do: descendants(node, source, source_map)

  defp candidates(nodes, source, source_map) when is_list(nodes),
    do: Enum.flat_map(nodes, &candidates(&1, source, source_map))

  defp candidates(_, _, _), do: []

  defp descendants(node, source, source_map),
    do: node |> Tuple.to_list() |> Enum.flat_map(&candidates(&1, source, source_map))

  defp parameter_source(_node, head, loc, source, source_map) do
    tokens =
      Enum.drop_while(source_map.tokens, fn {_, token_loc} ->
        token_loc.line != loc.line or token_loc.col != loc.col
      end)

    case tokens do
      [{"(", _}, {^head, _}, {"[", bracket} | _] ->
        %{offset: start, length: length} =
          Map.fetch!(source_map.spans, {bracket.line, bracket.col})

        {:ok, String.slice(source, start + 1, length - 2), start + 1}

      _ ->
        :not_found
    end
  end

  defp offset(source, loc) do
    source
    |> String.split("\n")
    |> Enum.take(loc.line - 1)
    |> Enum.reduce(loc.col - 1, fn line, n -> n + String.length(line) + 1 end)
  end
end
