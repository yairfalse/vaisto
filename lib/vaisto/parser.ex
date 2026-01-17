defmodule Vaisto.Parser do
  @moduledoc """
  Minimalist parser for Vaisto S-expressions with line/column tracking.

  Transforms text into AST:
    "(+ 1 2)" → {:call, :+, [1, 2], %{line: 1, col: 1}}

  Each AST node includes location metadata for error reporting.
  """

  # Location struct for source positions
  defmodule Loc do
    @moduledoc "Source location: line, column, and optional filename"
    defstruct line: 1, col: 1, file: nil

    def new(line, col, file \\ nil), do: %__MODULE__{line: line, col: col, file: file}
  end

  @doc """
  Parse a Vaisto source string into an AST with location metadata.
  """
  def parse(code, opts \\ []) do
    file = Keyword.get(opts, :file)
    code
    |> tokenize(file)
    |> do_parse([])
  end

  # Position-tracking tokenizer
  # Returns list of {token_string, %Loc{}} tuples
  defp tokenize(code, file) do
    tokenize_chars(String.graphemes(code), 1, 1, file, [], nil)
  end

  # Tokenizer state machine
  # Args: chars, line, col, file, accumulated_tokens, current_token_state
  # current_token_state: nil | {chars_acc, start_line, start_col} | {:string, chars_acc, start_line, start_col}

  # End of input
  defp tokenize_chars([], _line, _col, _file, tokens, nil), do: Enum.reverse(tokens)
  defp tokenize_chars([], _line, _col, file, tokens, {acc, start_line, start_col}) do
    token = acc |> Enum.reverse() |> Enum.join()
    Enum.reverse([{token, Loc.new(start_line, start_col, file)} | tokens])
  end
  defp tokenize_chars([], _line, _col, _file, _tokens, {:string, _acc, start_line, start_col}) do
    raise "Unterminated string starting at line #{start_line}, column #{start_col}"
  end

  # Inside a string literal
  defp tokenize_chars(["\\" | [escaped | rest]], line, col, file, tokens, {:string, acc, sl, sc}) do
    tokenize_chars(rest, line, col + 2, file, tokens, {:string, [escaped, "\\" | acc], sl, sc})
  end
  defp tokenize_chars(["\"" | rest], line, col, file, tokens, {:string, acc, sl, sc}) do
    # End of string - include the closing quote
    str_content = acc |> Enum.reverse() |> Enum.join()
    token = "\"" <> str_content <> "\""
    tokenize_chars(rest, line, col + 1, file, [{token, Loc.new(sl, sc, file)} | tokens], nil)
  end
  defp tokenize_chars(["\n" | rest], line, _col, file, tokens, {:string, acc, sl, sc}) do
    tokenize_chars(rest, line + 1, 1, file, tokens, {:string, ["\n" | acc], sl, sc})
  end
  defp tokenize_chars([c | rest], line, col, file, tokens, {:string, acc, sl, sc}) do
    tokenize_chars(rest, line, col + 1, file, tokens, {:string, [c | acc], sl, sc})
  end

  # Start of string
  defp tokenize_chars(["\"" | rest], line, col, file, tokens, nil) do
    tokenize_chars(rest, line, col + 1, file, tokens, {:string, [], line, col})
  end
  defp tokenize_chars(["\"" | rest], line, col, file, tokens, {acc, sl, sc}) do
    # Flush current token, start string
    token = acc |> Enum.reverse() |> Enum.join()
    new_tokens = [{token, Loc.new(sl, sc, file)} | tokens]
    tokenize_chars(rest, line, col + 1, file, new_tokens, {:string, [], line, col})
  end

  # Comment: skip to end of line
  defp tokenize_chars([";" | rest], line, _col, file, tokens, state) do
    tokens = flush_token(tokens, state, file)
    {remaining, new_line} = skip_comment(rest, line)
    tokenize_chars(remaining, new_line, 1, file, tokens, nil)
  end

  # Newline
  defp tokenize_chars(["\n" | rest], line, _col, file, tokens, state) do
    tokens = flush_token(tokens, state, file)
    tokenize_chars(rest, line + 1, 1, file, tokens, nil)
  end

  # Whitespace (space, tab, carriage return)
  defp tokenize_chars([ws | rest], line, col, file, tokens, state) when ws in [" ", "\t", "\r"] do
    tokens = flush_token(tokens, state, file)
    tokenize_chars(rest, line, col + 1, file, tokens, nil)
  end

  # Delimiters: ( ) [ ] { }
  defp tokenize_chars([d | rest], line, col, file, tokens, state) when d in ["(", ")", "[", "]", "{", "}"] do
    tokens = flush_token(tokens, state, file)
    tokenize_chars(rest, line, col + 1, file, [{d, Loc.new(line, col, file)} | tokens], nil)
  end

  # Regular character - accumulate into current token
  defp tokenize_chars([c | rest], line, col, file, tokens, nil) do
    tokenize_chars(rest, line, col + 1, file, tokens, {[c], line, col})
  end
  defp tokenize_chars([c | rest], line, col, file, tokens, {acc, sl, sc}) do
    tokenize_chars(rest, line, col + 1, file, tokens, {[c | acc], sl, sc})
  end

  # Flush accumulated token if any
  defp flush_token(tokens, nil, _file), do: tokens
  defp flush_token(tokens, {acc, sl, sc}, file) do
    token = acc |> Enum.reverse() |> Enum.join()
    [{token, Loc.new(sl, sc, file)} | tokens]
  end

  # Skip to end of line for comments
  defp skip_comment([], line), do: {[], line}
  defp skip_comment(["\n" | rest], line), do: {rest, line + 1}
  defp skip_comment([_ | rest], line), do: skip_comment(rest, line)

  # Recursive descent parser
  # Tokens are now {token_string, %Loc{}} tuples
  defp do_parse([], [acc]), do: acc
  defp do_parse([], []), do: nil
  defp do_parse([], acc), do: acc

  defp do_parse([{"(", loc} | tail], stack) do
    {rest, list} = parse_list(tail, [], loc)
    do_parse(rest, stack ++ [list])
  end

  defp do_parse([{"{", loc} | tail], stack) do
    {rest, brace_contents} = parse_brace(tail, [], loc)
    do_parse(rest, stack ++ [{:tuple_pattern, brace_contents}])
  end

  defp do_parse([{token, loc} | tail], stack) do
    do_parse(tail, stack ++ [parse_token(token, loc)])
  end

  # Handle contents of ( ... )
  # open_loc is the location of the opening paren - attached to the resulting AST node
  defp parse_list([{")", _} | tail], acc, open_loc) do
    ast = case acc do
      # Special forms - location passed to parsers
      [:if | rest] -> parse_if(rest, open_loc)
      [:let | rest] -> parse_let(rest, open_loc)
      [:match | rest] -> parse_match(rest, open_loc)
      [:"match-tuple" | rest] -> parse_match_tuple(rest, open_loc)
      [:receive | rest] -> parse_receive(rest, open_loc)
      [:process | rest] -> parse_process(rest, open_loc)
      [:supervise | rest] -> parse_supervise(rest, open_loc)
      [:def | rest] -> parse_def(rest, open_loc)
      [:defn | rest] -> parse_defn(rest, open_loc)
      [:deftype | rest] -> parse_deftype(rest, open_loc)
      [:fn | rest] -> parse_fn(rest, open_loc)
      [:extern | rest] -> parse_extern(rest, open_loc)
      # Module system
      [:ns | rest] -> parse_ns(rest, open_loc)
      [:import | rest] -> parse_import(rest, open_loc)
      # List literal: (list 1 2 3) → {:list, [1, 2, 3], loc}
      [:list | elements] -> {:list, elements, open_loc}
      # Regular function call
      [func | args] -> {:call, func, args, open_loc}
      [] -> {:unit, open_loc}
    end
    {tail, ast}
  end

  defp parse_list([{"(", loc} | tail], acc, open_loc) do
    {rest, nested} = parse_list(tail, [], loc)
    parse_list(rest, acc ++ [nested], open_loc)
  end

  defp parse_list([{"[", loc} | tail], acc, open_loc) do
    {rest, bracket_contents} = parse_bracket(tail, [], loc)
    parse_list(rest, acc ++ [{:bracket, bracket_contents}], open_loc)
  end

  defp parse_list([{"{", loc} | tail], acc, open_loc) do
    {rest, brace_contents} = parse_brace(tail, [], loc)
    parse_list(rest, acc ++ [{:tuple_pattern, brace_contents}], open_loc)
  end

  defp parse_list([{token, loc} | tail], acc, open_loc) do
    parse_list(tail, acc ++ [parse_token(token, loc)], open_loc)
  end

  # Handle unclosed paren - provide helpful error with location
  defp parse_list([], _acc, open_loc) do
    raise "Unclosed parenthesis at line #{open_loc.line}, column #{open_loc.col}"
  end

  # Parse bracket contents [...] - used for let bindings and patterns
  # After accumulating, check for cons pattern: [h | t] → {:cons, h, t}
  defp parse_bracket([{"]", _} | tail], acc, _open_loc) do
    result = normalize_bracket_pattern(acc)
    {tail, result}
  end

  defp parse_bracket([{"(", loc} | tail], acc, open_loc) do
    {rest, nested} = parse_list(tail, [], loc)
    parse_bracket(rest, acc ++ [nested], open_loc)
  end

  defp parse_bracket([{"[", loc} | tail], acc, open_loc) do
    # Nested bracket (e.g., [[]] for empty list pattern)
    {rest, nested} = parse_bracket(tail, [], loc)
    parse_bracket(rest, acc ++ [nested], open_loc)
  end

  defp parse_bracket([{"{", loc} | tail], acc, open_loc) do
    # Tuple pattern inside bracket: [{:ok v} body]
    {rest, tuple_contents} = parse_brace(tail, [], loc)
    parse_bracket(rest, acc ++ [{:tuple_pattern, tuple_contents}], open_loc)
  end

  defp parse_bracket([{token, loc} | tail], acc, open_loc) do
    parse_bracket(tail, acc ++ [parse_token(token, loc)], open_loc)
  end

  # Handle unclosed bracket - provide helpful error with location
  defp parse_bracket([], _acc, open_loc) do
    raise "Unclosed bracket at line #{open_loc.line}, column #{open_loc.col}"
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

  # Parse brace contents {...} - raw Erlang tuple patterns for match-tuple
  # {tag arg1 arg2} → {:tuple_pattern, [tag, arg1, arg2]}
  defp parse_brace([{"}", _} | tail], acc, _open_loc) do
    {tail, acc}
  end

  defp parse_brace([{"(", loc} | tail], acc, open_loc) do
    {rest, nested} = parse_list(tail, [], loc)
    parse_brace(rest, acc ++ [nested], open_loc)
  end

  defp parse_brace([{"[", loc} | tail], acc, open_loc) do
    {rest, bracket_contents} = parse_bracket(tail, [], loc)
    parse_brace(rest, acc ++ [{:bracket, bracket_contents}], open_loc)
  end

  defp parse_brace([{"{", loc} | tail], acc, open_loc) do
    {rest, nested} = parse_brace(tail, [], loc)
    parse_brace(rest, acc ++ [{:tuple_pattern, nested}], open_loc)
  end

  defp parse_brace([{token, loc} | tail], acc, open_loc) do
    parse_brace(tail, acc ++ [parse_token(token, loc)], open_loc)
  end

  defp parse_brace([], _acc, open_loc) do
    raise "Unclosed brace at line #{open_loc.line}, column #{open_loc.col}"
  end

  # Special form parsers
  # All take location as last argument and include it in the AST node

  # (if cond then else) → {:if, cond, then, else, loc}
  defp parse_if([condition, then_branch, else_branch], loc) do
    {:if, condition, then_branch, else_branch, loc}
  end

  # (let [x 1 y 2] body) → {:let, [{:x, 1}, {:y, 2}], body, loc}
  defp parse_let([{:bracket, bindings}, body], loc) do
    pairs = bindings
      |> Enum.chunk_every(2)
      |> Enum.map(fn [name, value] -> {name, value} end)
    {:let, pairs, body, loc}
  end

  # (match expr [pattern1 body1] [pattern2 body2] ...) → {:match, expr, [{pattern, body}, ...], loc}
  defp parse_match([expr | clauses], loc) do
    parsed_clauses = Enum.map(clauses, fn {:bracket, [pattern, body]} ->
      {pattern, body}
    end)
    {:match, expr, parsed_clauses, loc}
  end

  # (match-tuple expr [{tag args...} body1] [{tag2 args...} body2] ...)
  # For matching raw Erlang tuples like {:ok, value} or {:error, reason}
  # → {:match_tuple, expr, [{tuple_pattern, body}, ...], loc}
  defp parse_match_tuple([expr | clauses], loc) do
    parsed_clauses = Enum.map(clauses, fn
      {:bracket, [{:tuple_pattern, elements}, body]} ->
        {{:tuple_pattern, elements}, body}
      {:bracket, [pattern, body]} ->
        # Allow non-tuple patterns too (atoms, variables as catch-all)
        {pattern, body}
    end)
    {:match_tuple, expr, parsed_clauses, loc}
  end

  # (receive [pattern1 body1] [pattern2 body2] ...) → {:receive, [{pattern, body}, ...], loc}
  # Blocks until a message matching one of the patterns arrives
  defp parse_receive(clauses, loc) do
    parsed_clauses = Enum.map(clauses, fn {:bracket, [pattern, body]} ->
      {pattern, body}
    end)
    {:receive, parsed_clauses, loc}
  end

  defp parse_process([name, initial_state | handlers], loc) do
    {:process, name, initial_state, parse_handlers(handlers), loc}
  end

  defp parse_supervise([strategy | children], loc) do
    {:supervise, strategy, children, loc}
  end

  defp parse_def([name, args, body], loc) do
    {:def, name, args, body, loc}
  end

  # Multi-clause function definition
  # (defn len
  #   [[] 0]
  #   [[h | t] (+ 1 (len t))])
  # All clauses are brackets: [pattern body]
  # This clause only matches when ALL remaining elements are brackets (multi-clause style)
  defp parse_defn([name | clauses], loc) when is_list(clauses) and length(clauses) >= 2 do
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
      {:defn_multi, name, parsed_clauses, loc}
    else
      # Not multi-clause - fall through to single-clause patterns
      parse_defn_single(name, clauses, loc)
    end
  end

  # Helper for single-clause defn to avoid pattern match conflicts
  defp parse_defn_single(name, [{:bracket, params}, ret_type, body], loc) do
    typed_params = parse_typed_params(params)
    if is_type_annotation?(ret_type) do
      {:defn, name, typed_params, body, unwrap_type(ret_type), loc}
    else
      # ret_type is actually the body, and there's a trailing element - error
      {:error, "Invalid defn syntax", loc}
    end
  end

  defp parse_defn_single(name, [{:bracket, params}, body], loc) do
    typed_params = parse_typed_params(params)
    {:defn, name, typed_params, body, :any, loc}
  end

  defp parse_defn_single(_name, _rest, loc) do
    {:error, "Invalid defn syntax", loc}
  end

  # Extract pattern and body from a clause
  # Clause content is [pattern, body] where pattern is already normalized
  # [] → empty list, {:cons, h, t} → cons pattern, etc.
  defp extract_pattern_body([pattern, body]), do: {pattern, body}

  # Check if params list looks like typed pairs (alternating names and types)
  # Types come from tokenizer as {:atom, :int} wrapped form
  defp looks_typed?(params) when length(params) >= 2 do
    # If second element is a type (either wrapped {:atom, :type} or raw :type), it's typed
    params
    |> Enum.drop(1)
    |> Enum.take_every(2)
    |> Enum.all?(&is_type_annotation?/1)
  end
  defp looks_typed?(_), do: false

  # Basic types
  defp is_type_annotation?({:atom, t}) when t in [:int, :float, :string, :bool, :any, :atom, :unit], do: true
  defp is_type_annotation?(t) when is_atom(t) and t in [:int, :float, :string, :bool, :any, :atom, :unit], do: true
  # Parameterized types: (List :int), (Result :int :string)
  defp is_type_annotation?({:call, type_name, _args, _loc}) when is_atom(type_name), do: true
  # User-defined types (capitalized atoms)
  defp is_type_annotation?({:atom, t}) when is_atom(t), do: String.match?(Atom.to_string(t), ~r/^[A-Z]/)
  defp is_type_annotation?(t) when is_atom(t), do: String.match?(Atom.to_string(t), ~r/^[A-Z]/)
  defp is_type_annotation?(_), do: false

  # Unwrap {:atom, :int} → :int for type annotations
  defp unwrap_type({:atom, t}), do: t
  defp unwrap_type(t), do: t

  # Helper to parse typed or untyped params
  defp parse_typed_params(params) do
    if looks_typed?(params) do
      # Parse as pairs: [x :int y :int] → [{:x, :int}, {:y, :int}]
      params
      |> Enum.chunk_every(2)
      |> Enum.map(fn [param_name, type] -> {param_name, unwrap_type(type)} end)
    else
      # Untyped params: [x y] → [{:x, :any}, {:y, :any}]
      Enum.map(params, fn p -> {p, :any} end)
    end
  end

  # (fn [x] (* x 2)) → {:fn, [:x], body, loc}
  defp parse_fn([{:bracket, params}, body], loc) do
    {:fn, params, body, loc}
  end

  # Product type (record): (deftype Point [x :int y :int])
  # → {:deftype, :Point, {:product, [{:x, :int}, {:y, :int}]}, loc}
  defp parse_deftype([name, {:bracket, fields}], loc) do
    # Parse field pairs: [x :int y :int] → [{:x, :int}, {:y, :int}]
    field_pairs = fields
      |> Enum.chunk_every(2)
      |> Enum.map(fn [field_name, type] -> {field_name, type} end)
    {:deftype, name, {:product, field_pairs}, loc}
  end

  # Sum type (variants): (deftype Result (Ok v) (Error msg))
  # → {:deftype, :Result, {:sum, [{:Ok, [:v]}, {:Error, [:msg]}]}, loc}
  # Each variant is parsed as {:call, CtorName, [type_params], loc}
  defp parse_deftype([name | rest], loc) when is_list(rest) and length(rest) > 0 do
    # Check if first element looks like a variant (parsed as a call)
    case rest do
      [{:call, _, _, _} | _] ->
        # Sum type: parse variants from call AST nodes
        parsed_variants = Enum.map(rest, fn
          {:call, ctor_name, args, _loc} ->
            {ctor_name, args}
          other ->
            {:error, "Invalid variant syntax: #{inspect(other)}"}
        end)
        # Check for errors
        case Enum.find(parsed_variants, &match?({:error, _}, &1)) do
          nil -> {:deftype, name, {:sum, parsed_variants}, loc}
          {:error, msg} -> {:error, msg, loc}
        end

      # Legacy: (deftype point x y) → untyped product fields
      fields ->
        field_pairs = Enum.map(fields, fn field -> {field, :any} end)
        {:deftype, name, {:product, field_pairs}, loc}
    end
  end

  # (extern erlang:hd [(List :any)] :any) → {:extern, :erlang, :hd, [type_expr], ret_type, loc}
  defp parse_extern([{:qualified, mod, func}, {:bracket, arg_types}, ret_type], loc) do
    {:extern, mod, func, arg_types, ret_type, loc}
  end

  # (ns MyModule) → {:ns, :MyModule, loc}
  # (ns Std.List) → {:ns, :"Std.List", loc}
  defp parse_ns([name], loc) do
    {:ns, normalize_module_name(name), loc}
  end

  # (import Std.List) → {:import, :"Std.List", loc}
  # (import Std.List :as List) → {:import, :"Std.List", :List, loc}
  defp parse_import([name], loc) do
    {:import, normalize_module_name(name), nil, loc}
  end

  defp parse_import([name, {:atom, :as}, alias_name], loc) do
    {:import, normalize_module_name(name), alias_name, loc}
  end

  # Normalize module names: handles both atoms and dotted names
  defp normalize_module_name({:module_path, parts}) do
    parts |> Enum.join(".") |> String.to_atom()
  end
  defp normalize_module_name(name) when is_atom(name), do: name

  # For use in token parsing (takes string)
  defp normalize_module_name_str(str) do
    if String.contains?(str, ".") do
      String.to_atom(str)
    else
      String.to_atom(str)
    end
  end

  defp parse_handlers(handlers) do
    handlers
    |> Enum.chunk_every(2)
    |> Enum.map(fn [msg, body] -> {msg, body} end)
  end

  # Token conversion
  # Takes token string and location, returns parsed value
  # Location is available for future use in error messages
  defp parse_token(token, _loc) do
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
      # Qualified call: Std.List/fold → {:qualified, module, func}
      # Forward slash for Vaisto module calls
      # Must have non-empty module and function parts (/ alone is division)
      String.contains?(token, "/") and token != "/" ->
        case String.split(token, "/", parts: 2) do
          [mod, func] when mod != "" and func != "" ->
            {:qualified, normalize_module_name_str(mod), String.to_atom(func)}
          _ ->
            # Just "/" or missing parts - treat as atom
            String.to_atom(token)
        end
      # Legacy Erlang-style qualified: erlang:hd → {:qualified, :erlang, :hd}
      String.contains?(token, ":") ->
        [mod, func] = String.split(token, ":", parts: 2)
        {:qualified, String.to_atom(mod), String.to_atom(func)}
      # Module path: Std.List → {:module_path, ["Std", "List"]}
      String.contains?(token, ".") and String.match?(token, ~r/^[A-Z]/) ->
        {:module_path, String.split(token, ".")}
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
