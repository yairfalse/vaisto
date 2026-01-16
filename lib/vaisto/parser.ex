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

  # Tokenizer: Split by parens/brackets and whitespace, strip comments
  # Preserves quoted strings as single tokens
  defp tokenize(code) do
    code
    |> strip_comments()
    |> extract_strings([])
    |> then(fn {code_with_placeholders, strings} ->
      tokens = code_with_placeholders
        |> String.replace("(", " ( ")
        |> String.replace(")", " ) ")
        |> String.replace("[", " [ ")
        |> String.replace("]", " ] ")
        |> String.split()

      # Restore strings in their placeholder positions
      restore_strings(tokens, strings)
    end)
  end

  # Extract quoted strings and replace with placeholders
  defp extract_strings(code, acc) do
    case Regex.run(~r/"([^"\\]|\\.)*"/, code, return: :index) do
      [{start, len} | _] ->
        string = String.slice(code, start, len)
        placeholder = "__STRING_#{length(acc)}__"
        new_code = String.slice(code, 0, start) <> placeholder <> String.slice(code, start + len, String.length(code))
        extract_strings(new_code, acc ++ [string])
      nil ->
        {code, acc}
    end
  end

  # Restore strings from placeholders
  defp restore_strings(tokens, strings) do
    Enum.map(tokens, fn token ->
      case Regex.run(~r/^__STRING_(\d+)__$/, token) do
        [_, idx] -> Enum.at(strings, String.to_integer(idx))
        nil -> token
      end
    end)
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
      [:if | rest] -> parse_if(rest)
      [:let | rest] -> parse_let(rest)
      [:match | rest] -> parse_match(rest)
      [:process | rest] -> parse_process(rest)
      [:supervise | rest] -> parse_supervise(rest)
      [:def | rest] -> parse_def(rest)
      [:defn | rest] -> parse_defn(rest)
      [:deftype | rest] -> parse_deftype(rest)
      # List literal: (list 1 2 3) → {:list, [1, 2, 3]}
      [:list | elements] -> {:list, elements}
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

  defp parse_list(["[" | tail], acc) do
    {rest, bracket_contents} = parse_bracket(tail, [])
    parse_list(rest, acc ++ [{:bracket, bracket_contents}])
  end

  defp parse_list([token | tail], acc) do
    parse_list(tail, acc ++ [parse_token(token)])
  end

  # Parse bracket contents [...] - used for let bindings
  defp parse_bracket(["]" | tail], acc), do: {tail, acc}

  defp parse_bracket(["(" | tail], acc) do
    {rest, nested} = parse_list(tail, [])
    parse_bracket(rest, acc ++ [nested])
  end

  defp parse_bracket([token | tail], acc) do
    parse_bracket(tail, acc ++ [parse_token(token)])
  end

  # Special form parsers

  # (if cond then else) → {:if, cond, then, else}
  defp parse_if([condition, then_branch, else_branch]) do
    {:if, condition, then_branch, else_branch}
  end

  # (let [x 1 y 2] body) → {:let, [{:x, 1}, {:y, 2}], body}
  defp parse_let([{:bracket, bindings}, body]) do
    pairs = bindings
      |> Enum.chunk_every(2)
      |> Enum.map(fn [name, value] -> {name, value} end)
    {:let, pairs, body}
  end

  # (match expr [pattern1 body1] [pattern2 body2] ...) → {:match, expr, [{pattern, body}, ...]}
  defp parse_match([expr | clauses]) do
    parsed_clauses = Enum.map(clauses, fn {:bracket, [pattern, body]} ->
      {pattern, body}
    end)
    {:match, expr, parsed_clauses}
  end

  defp parse_process([name, initial_state | handlers]) do
    {:process, name, initial_state, parse_handlers(handlers)}
  end

  defp parse_supervise([strategy | children]) do
    {:supervise, strategy, children}
  end

  defp parse_def([name, args, body]) do
    {:def, name, args, body}
  end

  # (defn add [x y] (+ x y)) → {:defn, :add, [:x, :y], body}
  defp parse_defn([name, {:bracket, params}, body]) do
    {:defn, name, params, body}
  end

  # (deftype point [x :int y :int]) → {:deftype, :point, [{:x, :int}, {:y, :int}]}
  defp parse_deftype([name, {:bracket, fields}]) do
    # Parse field pairs: [x :int y :int] → [{:x, :int}, {:y, :int}]
    field_pairs = fields
      |> Enum.chunk_every(2)
      |> Enum.map(fn [field_name, type] -> {field_name, type} end)
    {:deftype, name, field_pairs}
  end

  # Legacy support: (deftype point x y) → untyped fields
  defp parse_deftype([name | fields]) when is_list(fields) and length(fields) > 0 do
    # Old-style untyped fields - convert to {:field_name, :any}
    field_pairs = Enum.map(fields, fn field -> {field, :any} end)
    {:deftype, name, field_pairs}
  end

  defp parse_handlers(handlers) do
    handlers
    |> Enum.chunk_every(2)
    |> Enum.map(fn [msg, body] -> {msg, body} end)
  end

  # Token conversion
  defp parse_token(token) do
    cond do
      # String literal: "hello" → {:string, "hello"}
      String.starts_with?(token, "\"") ->
        # Remove surrounding quotes and unescape
        token
        |> String.slice(1..-2//1)
        |> unescape_string()
        |> then(&{:string, &1})
      token =~ ~r/^-?\d+$/ -> String.to_integer(token)
      token =~ ~r/^-?\d+\.\d+$/ -> String.to_float(token)
      String.starts_with?(token, ":") ->
        token |> String.trim_leading(":") |> String.to_atom()
      true -> String.to_atom(token)
    end
  end

  # Handle common escape sequences in strings
  defp unescape_string(s) do
    s
    |> String.replace("\\n", "\n")
    |> String.replace("\\t", "\t")
    |> String.replace("\\\"", "\"")
    |> String.replace("\\\\", "\\")
  end
end
