defmodule Vaisto.Parser do
  @moduledoc """
  Minimalist parser for Vaisto S-expressions.
  
  Transforms text into AST:
    "(+ 1 2)" → {:call, :+, [1, 2]}
  """

  @doc """
  Parse a Vaisto source string into an AST.
  """
  def parse(code) do
    code
    |> tokenize()
    |> do_parse([])
  end

  # Tokenizer: Split by parens and whitespace, strip comments
  defp tokenize(code) do
    code
    |> strip_comments()
    |> String.replace("(", " ( ")
    |> String.replace(")", " ) ")
    |> String.split()
  end

  # Remove ; comments (everything from ; to end of line)
  defp strip_comments(code) do
    code
    |> String.split("\n")
    |> Enum.map(fn line ->
      case String.split(line, ";", parts: 2) do
        [before, _comment] -> before
        [line] -> line
      end
    end)
    |> Enum.join("\n")
  end

  # Recursive descent parser
  defp do_parse([], [acc]), do: acc
  defp do_parse([], []), do: nil
  defp do_parse([], acc), do: acc

  defp do_parse(["(" | tail], stack) do
    {rest, list} = parse_list(tail, [])
    do_parse(rest, stack ++ [list])
  end

  defp do_parse([token | tail], stack) do
    do_parse(tail, stack ++ [parse_token(token)])
  end

  # Handle contents of ( ... )
  defp parse_list([")" | tail], acc) do
    ast = case acc do
      # Special forms
      [:process | rest] -> parse_process(rest)
      [:supervise | rest] -> parse_supervise(rest)
      [:def | rest] -> parse_def(rest)
      [:deftype | rest] -> parse_deftype(rest)
      # Regular function call
      [func | args] -> {:call, func, args}
      [] -> {:unit}
    end
    {tail, ast}
  end

  defp parse_list(["(" | tail], acc) do
    {rest, nested} = parse_list(tail, [])
    parse_list(rest, acc ++ [nested])
  end

  defp parse_list([token | tail], acc) do
    parse_list(tail, acc ++ [parse_token(token)])
  end

  # Special form parsers
  defp parse_process([name, initial_state | handlers]) do
    {:process, name, initial_state, parse_handlers(handlers)}
  end

  defp parse_supervise([strategy | children]) do
    {:supervise, strategy, children}
  end

  defp parse_def([name, args, body]) do
    {:def, name, args, body}
  end

  defp parse_deftype([name | fields]) do
    {:deftype, name, fields}
  end

  defp parse_handlers(handlers) do
    handlers
    |> Enum.chunk_every(2)
    |> Enum.map(fn [msg, body] -> {msg, body} end)
  end

  # Token conversion
  defp parse_token(token) do
    cond do
      token =~ ~r/^-?\d+$/ -> String.to_integer(token)
      token =~ ~r/^-?\d+\.\d+$/ -> String.to_float(token)
      String.starts_with?(token, ":") -> 
        token |> String.trim_leading(":") |> String.to_atom()
      true -> String.to_atom(token)
    end
  end
end
