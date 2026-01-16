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
      [:receive | rest] -> parse_receive(rest)
      [:process | rest] -> parse_process(rest)
      [:supervise | rest] -> parse_supervise(rest)
      [:def | rest] -> parse_def(rest)
      [:defn | rest] -> parse_defn(rest)
      [:deftype | rest] -> parse_deftype(rest)
      [:fn | rest] -> parse_fn(rest)
      [:extern | rest] -> parse_extern(rest)
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

  # Parse bracket contents [...] - used for let bindings and patterns
  # After accumulating, check for cons pattern: [h | t] → {:cons, h, t}
  defp parse_bracket(["]" | tail], acc) do
    result = normalize_bracket_pattern(acc)
    {tail, result}
  end

  defp parse_bracket(["(" | tail], acc) do
    {rest, nested} = parse_list(tail, [])
    parse_bracket(rest, acc ++ [nested])
  end

  defp parse_bracket(["[" | tail], acc) do
    # Nested bracket (e.g., [[]] for empty list pattern)
    {rest, nested} = parse_bracket(tail, [])
    parse_bracket(rest, acc ++ [nested])
  end

  defp parse_bracket([token | tail], acc) do
    parse_bracket(tail, acc ++ [parse_token(token)])
  end

  # Normalize bracket contents: detect cons pattern [h | t] → {:cons, h, t}
  defp normalize_bracket_pattern(acc) do
    case acc do
      # Cons pattern: [h | t] where | is in the middle
      [head, :|, tail] -> {:cons, head, tail}
      # Empty list stays as empty list
      [] -> []
      # Regular list of elements (for let bindings, param lists, etc.)
      _ -> acc
    end
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

  # (receive [pattern1 body1] [pattern2 body2] ...) → {:receive, [{pattern, body}, ...]}
  # Blocks until a message matching one of the patterns arrives
  defp parse_receive(clauses) do
    parsed_clauses = Enum.map(clauses, fn {:bracket, [pattern, body]} ->
      {pattern, body}
    end)
    {:receive, parsed_clauses}
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

  # (defn add [x y] (+ x y)) → {:defn, :add, [{:x, :any}, {:y, :any}], body}
  # (defn add [x :int y :int] (+ x y)) → {:defn, :add, [{:x, :int}, {:y, :int}], body}
  # Single-clause: name, params bracket, non-bracket body
  defp parse_defn([name, {:bracket, params}, body]) when not is_tuple(body) or elem(body, 0) != :bracket do
    # Check if params look like typed (name :type pairs) or untyped (just names)
    typed_params = if looks_typed?(params) do
      # Parse as pairs: [x :int y :int] → [{:x, :int}, {:y, :int}]
      params
      |> Enum.chunk_every(2)
      |> Enum.map(fn [param_name, type] -> {param_name, type} end)
    else
      # Untyped params: [x y] → [{:x, :any}, {:y, :any}]
      Enum.map(params, fn p -> {p, :any} end)
    end
    {:defn, name, typed_params, body}
  end

  # Multi-clause function definition
  # (defn len
  #   [[] 0]
  #   [[h | t] (+ 1 (len t))])
  # All clauses are brackets: [pattern body]
  defp parse_defn([name | clauses]) when is_list(clauses) and length(clauses) > 0 do
    # Check if this is multi-clause style (all elements are brackets)
    all_brackets? = Enum.all?(clauses, fn
      {:bracket, _} -> true
      _ -> false
    end)

    if all_brackets? do
      # Multi-clause function - each clause is [pattern body]
      parsed_clauses = Enum.map(clauses, fn {:bracket, clause_content} ->
        extract_pattern_body(clause_content)
      end)
      {:defn_multi, name, parsed_clauses}
    else
      # Single clause with bracket body (unlikely but handle gracefully)
      {:error, "Invalid defn syntax"}
    end
  end

  # Extract pattern and body from a clause
  # Clause content is [pattern, body] where pattern is already normalized
  # [] → empty list, {:cons, h, t} → cons pattern, etc.
  defp extract_pattern_body([pattern, body]), do: {pattern, body}

  # Check if params list looks like typed pairs (alternating atoms and types)
  defp looks_typed?(params) when length(params) >= 2 do
    # If second element is a type atom (starts with : in original syntax), it's typed
    params
    |> Enum.drop(1)
    |> Enum.take_every(2)
    |> Enum.all?(fn p -> is_atom(p) and p in [:int, :float, :string, :bool, :any, :atom] end)
  end
  defp looks_typed?(_), do: false

  # (fn [x] (* x 2)) → {:fn, [:x], body}
  defp parse_fn([{:bracket, params}, body]) do
    {:fn, params, body}
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

  # (extern erlang:hd [(List :any)] :any) → {:extern, :erlang, :hd, [type_expr], ret_type}
  defp parse_extern([{:qualified, mod, func}, {:bracket, arg_types}, ret_type]) do
    {:extern, mod, func, arg_types, ret_type}
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
      # Atom literal: :foo → {:atom, :foo}
      # This distinguishes atoms from variable names in the AST
      String.starts_with?(token, ":") ->
        {:atom, token |> String.trim_leading(":") |> String.to_atom()}
      # Qualified name: erlang:hd → {:qualified, :erlang, :hd}
      # Must contain : but not start with :
      String.contains?(token, ":") ->
        [mod, func] = String.split(token, ":", parts: 2)
        {:qualified, String.to_atom(mod), String.to_atom(func)}
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
