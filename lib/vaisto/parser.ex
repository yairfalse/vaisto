defmodule Vaisto.Parser do
  @moduledoc """
  Minimalist parser for Vaisto S-expressions with line/column tracking.

  Transforms text into AST:
    "(+ 1 2)" → {:call, :+, [1, 2], %{line: 1, col: 1}}

  Each AST node includes location metadata for error reporting.
  """

  alias Vaisto.{Error, Errors}

  # Location struct for source positions with span information
  defmodule Loc do
    @moduledoc """
    Source location with span information for error reporting.

    Fields:
    - line, col: Start position (1-indexed)
    - length: Span length in characters (for error underlining)
    - file: Optional filename
    """
    @type t :: %__MODULE__{
            line: pos_integer(),
            col: pos_integer(),
            length: pos_integer(),
            file: String.t() | nil
          }

    defstruct line: 1, col: 1, length: 1, file: nil

    def new(line, col, file \\ nil), do: %__MODULE__{line: line, col: col, length: 1, file: file}

    def new(line, col, length, file) do
      %__MODULE__{line: line, col: col, length: length, file: file}
    end
  end

  @type parse_opts :: [file: String.t()]

  @doc """
  Parse a Vaisto source string into an AST with location metadata.
  """
  @spec parse(String.t(), parse_opts()) :: term()
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
  # current_token_state: nil | {chars_acc, start_line, start_col} | {:string, chars_acc, start_line, start_col} | {:triple_string, chars_acc, start_line, start_col}

  # End of input
  defp tokenize_chars([], _line, _col, _file, tokens, nil), do: Enum.reverse(tokens)
  defp tokenize_chars([], _line, _col, file, tokens, {acc, start_line, start_col}) do
    token = acc |> Enum.reverse() |> Enum.join()
    length = String.length(token)
    Enum.reverse([{token, Loc.new(start_line, start_col, length, file)} | tokens])
  end
  defp tokenize_chars([], _line, _col, _file, _tokens, {:string, _acc, start_line, start_col}) do
    raise "Unterminated string starting at line #{start_line}, column #{start_col}"
  end
  defp tokenize_chars([], _line, _col, _file, _tokens, {:triple_string, _acc, start_line, start_col}) do
    raise "Unterminated string starting at line #{start_line}, column #{start_col}"
  end

  # Inside a triple-quoted string literal
  defp tokenize_chars(["\"", "\"", "\"" | rest], line, col, file, tokens, {:triple_string, acc, sl, sc}) do
    str_content = acc |> Enum.reverse() |> Enum.join()
    token = "\"\"\"" <> str_content <> "\"\"\""
    length = String.length(token)
    tokenize_chars(rest, line, col + 3, file, [{token, Loc.new(sl, sc, length, file)} | tokens], nil)
  end
  defp tokenize_chars(["\n" | rest], line, _col, file, tokens, {:triple_string, acc, sl, sc}) do
    tokenize_chars(rest, line + 1, 1, file, tokens, {:triple_string, ["\n" | acc], sl, sc})
  end
  defp tokenize_chars([c | rest], line, col, file, tokens, {:triple_string, acc, sl, sc}) do
    tokenize_chars(rest, line, col + 1, file, tokens, {:triple_string, [c | acc], sl, sc})
  end

  # Inside a string literal
  defp tokenize_chars(["\\" | [escaped | rest]], line, col, file, tokens, {:string, acc, sl, sc}) do
    tokenize_chars(rest, line, col + 2, file, tokens, {:string, [escaped, "\\" | acc], sl, sc})
  end
  defp tokenize_chars(["\"" | rest], line, col, file, tokens, {:string, acc, sl, sc}) do
    # End of string - include the closing quote
    str_content = acc |> Enum.reverse() |> Enum.join()
    token = "\"" <> str_content <> "\""
    length = String.length(token)
    tokenize_chars(rest, line, col + 1, file, [{token, Loc.new(sl, sc, length, file)} | tokens], nil)
  end
  defp tokenize_chars(["\n" | rest], line, _col, file, tokens, {:string, acc, sl, sc}) do
    tokenize_chars(rest, line + 1, 1, file, tokens, {:string, ["\n" | acc], sl, sc})
  end
  defp tokenize_chars([c | rest], line, col, file, tokens, {:string, acc, sl, sc}) do
    tokenize_chars(rest, line, col + 1, file, tokens, {:string, [c | acc], sl, sc})
  end

  # Start of triple-quoted string
  defp tokenize_chars(["\"", "\"", "\"" | rest], line, col, file, tokens, nil) do
    tokenize_chars(rest, line, col + 3, file, tokens, {:triple_string, [], line, col})
  end
  defp tokenize_chars(["\"", "\"", "\"" | rest], line, col, file, tokens, {acc, sl, sc}) do
    token = acc |> Enum.reverse() |> Enum.join()
    length = String.length(token)
    new_tokens = [{token, Loc.new(sl, sc, length, file)} | tokens]
    tokenize_chars(rest, line, col + 3, file, new_tokens, {:triple_string, [], line, col})
  end

  # Start of string
  defp tokenize_chars(["\"" | rest], line, col, file, tokens, nil) do
    tokenize_chars(rest, line, col + 1, file, tokens, {:string, [], line, col})
  end
  defp tokenize_chars(["\"" | rest], line, col, file, tokens, {acc, sl, sc}) do
    # Flush current token, start string
    token = acc |> Enum.reverse() |> Enum.join()
    length = String.length(token)
    new_tokens = [{token, Loc.new(sl, sc, length, file)} | tokens]
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

  # Map literal: #{ }
  defp tokenize_chars(["#", "{" | rest], line, col, file, tokens, state) do
    tokens = flush_token(tokens, state, file)
    tokenize_chars(rest, line, col + 2, file, [{"\#{", Loc.new(line, col, 2, file)} | tokens], nil)
  end

  # Backslash escape (start of char literal or escaped symbol): consume backslash + next char
  defp tokenize_chars(["\\", c | rest], line, col, file, tokens, nil) do
    tokenize_chars(rest, line, col + 2, file, tokens, {[c, "\\"], line, col})
  end
  defp tokenize_chars(["\\", c | rest], line, col, file, tokens, {acc, sl, sc}) do
    tokenize_chars(rest, line, col + 2, file, tokens, {[c, "\\" | acc], sl, sc})
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
    length = String.length(token)
    [{token, Loc.new(sl, sc, length, file)} | tokens]
  end

  # Skip to end of line for comments
  defp skip_comment([], line), do: {[], line}
  defp skip_comment(["\n" | rest], line), do: {rest, line + 1}
  defp skip_comment([_ | rest], line), do: skip_comment(rest, line)

  # Recursive descent parser
  # Tokens are now {token_string, %Loc{}} tuples
  # Note: stack is built in reverse order for O(1) cons, reversed on return
  defp do_parse([], [acc]), do: acc
  defp do_parse([], []), do: nil
  defp do_parse([], stack), do: Enum.reverse(stack)

  defp do_parse([{"(", loc} | tail], stack) do
    {rest, list} = parse_list(tail, [], loc)
    do_parse(rest, [list | stack])
  end

  defp do_parse([{"{", loc} | tail], stack) do
    {rest, brace_contents} = parse_brace(tail, [], loc)
    do_parse(rest, [{:tuple_pattern, brace_contents} | stack])
  end

  # Map literal: #{ :key val :key2 val2 }
  defp do_parse([{"\#{", loc} | tail], stack) do
    {rest, map_contents} = parse_map(tail, [], loc)
    do_parse(rest, [{:map, map_contents, loc} | stack])
  end

  defp do_parse([{"[", loc} | tail], stack) do
    {rest, bracket_contents} = parse_bracket(tail, [], loc)
    do_parse(rest, [{:bracket, bracket_contents} | stack])
  end

  defp do_parse([{")", loc} | _tail], _stack) do
    raise "Unexpected closing parenthesis at line #{loc.line}, column #{loc.col}"
  end

  defp do_parse([{"}", loc} | _tail], _stack) do
    raise "Unexpected closing brace at line #{loc.line}, column #{loc.col}"
  end

  defp do_parse([{"]", loc} | _tail], _stack) do
    raise "Unexpected closing bracket at line #{loc.line}, column #{loc.col}"
  end

  defp do_parse([{token, loc} | tail], stack) do
    do_parse(tail, [parse_token(token, loc) | stack])
  end

  # Handle contents of ( ... )
  # open_loc is the location of the opening paren - attached to the resulting AST node
  # Note: acc is built in reverse order for O(1) cons, reversed here on close
  defp parse_list([{")", _} | tail], acc, open_loc) do
    # Reverse to get correct order before pattern matching
    ast = case Enum.reverse(acc) do
      # Special forms - location passed to parsers
      [:if | rest] -> parse_if(rest, open_loc)
      [:cond | rest] -> parse_cond(rest, open_loc)
      [:let | rest] -> parse_let(rest, open_loc)
      [:match | rest] -> parse_match(rest, open_loc)
      [:receive | rest] -> parse_receive(rest, open_loc)
      [:process | rest] -> parse_process(rest, open_loc)
      [:supervise | rest] -> parse_supervise(rest, open_loc)
      [:def | rest] -> parse_def(rest, open_loc)
      [:defn | rest] -> parse_defn(rest, open_loc)
      [:defprompt | rest] -> parse_defprompt(rest, open_loc)
      [:pipeline | rest] -> parse_pipeline(rest, open_loc)
      [:deftype | rest] -> parse_deftype(rest, open_loc)
      [:fn | rest] -> parse_fn(rest, open_loc)
      [:extern | rest] -> parse_extern(rest, open_loc)
      [:defclass | rest] -> parse_defclass(rest, open_loc)
      [:instance | rest] -> parse_instance(rest, open_loc)
      # Do block: (do expr1 expr2 ...) → {:do, [expr1, expr2, ...], loc}
      [:do | exprs] -> {:do, exprs, open_loc}
      # Module system
      [:ns | rest] -> parse_ns(rest, open_loc)
      [:import | rest] -> parse_import(rest, open_loc)
      # List literal: (list 1 2 3) → {:list, [1, 2, 3], loc}
      [:list | elements] -> {:list, elements, open_loc}
      # Tuple literal: (tuple :tag a b) → {:tuple, [:tag, a, b], loc}
      # Creates an Erlang tuple at runtime
      [:tuple | elements] -> {:tuple, elements, open_loc}
      # Short-circuit booleans: desugar to if
      # (andalso a b) → (if a b false) — evaluates b only when a is true
      [:andalso, a, b] -> {:if, a, b, false, open_loc}
      # (orelse a b) → (if a true b) — evaluates b only when a is false
      [:orelse, a, b] -> {:if, a, true, b, open_loc}
      # For comprehension: (for [x xs] body) → (map (fn [x] body) xs)
      # With :when guard: (for [x xs :when pred] body) → (map (fn [x] body) (filter (fn [x] pred) xs))
      [:for | rest] -> parse_for(rest, open_loc)
      # Threading macros: compile-time transformation - zero runtime overhead
      # Thread-first: (-> x (f a) (g b)) → (g (f x a) b)
      [:-> | rest] -> parse_thread_first(rest, open_loc)
      # Thread-last: (->> x (f a) (g b)) → (g b (f a x))
      [:"->>" | rest] -> parse_thread_last(rest, open_loc)
      # Field access: (. record :field) → {:field_access, record, :field, loc}
      [:. | rest] -> parse_field_access(rest, open_loc)
      [:generate | rest] -> parse_generate(rest, open_loc)
      # Regular function call
      [func | args] -> {:call, func, args, open_loc}
      [] -> {:unit, open_loc}
    end
    {tail, ast}
  end

  defp parse_list([{"(", loc} | tail], acc, open_loc) do
    {rest, nested} = parse_list(tail, [], loc)
    parse_list(rest, [nested | acc], open_loc)
  end

  defp parse_list([{"[", loc} | tail], acc, open_loc) do
    {rest, bracket_contents} = parse_bracket(tail, [], loc)
    parse_list(rest, [{:bracket, bracket_contents} | acc], open_loc)
  end

  defp parse_list([{"{", loc} | tail], acc, open_loc) do
    {rest, brace_contents} = parse_brace(tail, [], loc)
    parse_list(rest, [{:tuple_pattern, brace_contents} | acc], open_loc)
  end

  # Map literal inside list: #{ :key val }
  defp parse_list([{"\#{", loc} | tail], acc, open_loc) do
    {rest, map_contents} = parse_map(tail, [], loc)
    parse_list(rest, [{:map, map_contents, loc} | acc], open_loc)
  end

  defp parse_list([{token, loc} | tail], acc, open_loc) do
    parse_list(tail, [parse_token(token, loc) | acc], open_loc)
  end

  # Handle unclosed paren - provide helpful error with location
  defp parse_list([], _acc, open_loc) do
    raise "Unclosed parenthesis at line #{open_loc.line}, column #{open_loc.col}"
  end

  # Parse bracket contents [...] - used for let bindings and patterns
  # After accumulating, check for cons pattern: [h | t] → {:cons, h, t}
  # Note: acc is built in reverse order for O(1) cons, reversed here on close
  defp parse_bracket([{"]", _} | tail], acc, _open_loc) do
    result = normalize_bracket_pattern(Enum.reverse(acc))
    {tail, result}
  end

  defp parse_bracket([{"(", loc} | tail], acc, open_loc) do
    {rest, nested} = parse_list(tail, [], loc)
    parse_bracket(rest, [nested | acc], open_loc)
  end

  defp parse_bracket([{"[", loc} | tail], acc, open_loc) do
    # Nested bracket (e.g., [[]] for empty list pattern)
    {rest, nested} = parse_bracket(tail, [], loc)
    parse_bracket(rest, [{:bracket, nested} | acc], open_loc)
  end

  defp parse_bracket([{"{", loc} | tail], acc, open_loc) do
    # Tuple pattern inside bracket: [{:ok v} body]
    {rest, tuple_contents} = parse_brace(tail, [], loc)
    parse_bracket(rest, [{:tuple_pattern, tuple_contents} | acc], open_loc)
  end

  # Map literal inside bracket: [m #{ :a 1 }]
  defp parse_bracket([{"\#{", loc} | tail], acc, open_loc) do
    {rest, map_contents} = parse_map(tail, [], loc)
    parse_bracket(rest, [{:map, map_contents, loc} | acc], open_loc)
  end

  defp parse_bracket([{token, loc} | tail], acc, open_loc) do
    parse_bracket(tail, [parse_token(token, loc) | acc], open_loc)
  end

  # Handle unclosed bracket - provide helpful error with location
  defp parse_bracket([], _acc, open_loc) do
    raise "Unclosed bracket at line #{open_loc.line}, column #{open_loc.col}"
  end

  # Normalize bracket contents: detect cons pattern [a, b, ... | t] → nested cons
  # [h | t] → {:cons, h, t}
  # [a, b | t] → {:cons, a, {:cons, b, t}}
  # [a, b, c | t] → {:cons, a, {:cons, b, {:cons, c, t}}}
  defp normalize_bracket_pattern(acc) do
    case find_pipe_index(acc) do
      nil ->
        # No pipe - empty list or regular list
        acc
      pipe_idx ->
        # Split at pipe: elements before, tail after
        {heads, [:|, tail]} = Enum.split(acc, pipe_idx)
        # Build nested cons from right to left
        List.foldr(heads, tail, fn head, rest -> {:cons, head, rest} end)
    end
  end

  # Find index of :| in list, or nil if not present
  defp find_pipe_index(list) do
    Enum.find_index(list, &(&1 == :|))
  end

  # Parse brace contents {...} - Erlang tuple literals and patterns
  # {:ok val} → {:tuple_pattern, [:ok, val]} (used in match and as expressions)
  # Note: acc is built in reverse order for O(1) cons, reversed here on close
  defp parse_brace([{"}", _} | tail], acc, _open_loc) do
    {tail, Enum.reverse(acc)}
  end

  defp parse_brace([{"(", loc} | tail], acc, open_loc) do
    {rest, nested} = parse_list(tail, [], loc)
    parse_brace(rest, [nested | acc], open_loc)
  end

  defp parse_brace([{"[", loc} | tail], acc, open_loc) do
    {rest, bracket_contents} = parse_bracket(tail, [], loc)
    parse_brace(rest, [{:bracket, bracket_contents} | acc], open_loc)
  end

  defp parse_brace([{"{", loc} | tail], acc, open_loc) do
    {rest, nested} = parse_brace(tail, [], loc)
    parse_brace(rest, [{:tuple_pattern, nested} | acc], open_loc)
  end

  defp parse_brace([{token, loc} | tail], acc, open_loc) do
    parse_brace(tail, [parse_token(token, loc) | acc], open_loc)
  end

  defp parse_brace([], _acc, open_loc) do
    raise "Unclosed brace at line #{open_loc.line}, column #{open_loc.col}"
  end

  # Parse map contents #{ :key val :key2 val2 } - closed by }
  # Returns list of key-value pairs
  # Note: acc is built in reverse order for O(1) cons, reversed here on close
  defp parse_map([{"}", _} | tail], acc, _open_loc) do
    # Convert flat list to pairs: [:a, 1, :b, 2] → [{:a, 1}, {:b, 2}]
    pairs = acc
      |> Enum.reverse()
      |> Enum.chunk_every(2)
      |> Enum.map(fn
        [key, value] -> {key, value}
        [_key] -> raise "Map literal has odd number of elements (missing value for key)"
      end)
    {tail, pairs}
  end

  defp parse_map([{"(", loc} | tail], acc, open_loc) do
    {rest, nested} = parse_list(tail, [], loc)
    parse_map(rest, [nested | acc], open_loc)
  end

  defp parse_map([{"[", loc} | tail], acc, open_loc) do
    {rest, bracket_contents} = parse_bracket(tail, [], loc)
    parse_map(rest, [{:bracket, bracket_contents} | acc], open_loc)
  end

  defp parse_map([{"{", loc} | tail], acc, open_loc) do
    {rest, nested} = parse_brace(tail, [], loc)
    parse_map(rest, [{:tuple_pattern, nested} | acc], open_loc)
  end

  defp parse_map([{"\#{", loc} | tail], acc, open_loc) do
    {rest, nested} = parse_map(tail, [], loc)
    parse_map(rest, [{:map, nested, loc} | acc], open_loc)
  end

  defp parse_map([{token, loc} | tail], acc, open_loc) do
    parse_map(tail, [parse_token(token, loc) | acc], open_loc)
  end

  defp parse_map([], _acc, open_loc) do
    raise "Unclosed map literal at line #{open_loc.line}, column #{open_loc.col}"
  end

  # Special form parsers
  # All take location as last argument and include it in the AST node

  # (if cond then else) → {:if, cond, then, else, loc}
  defp parse_if([condition, then_branch, else_branch], loc) do
    {:if, condition, then_branch, else_branch, loc}
  end
  defp parse_if(args, loc) when length(args) < 3 do
    {:error, Errors.parse_error("if requires 3 arguments: (if condition then else)", span: Error.span_from_loc(loc)), loc}
  end
  defp parse_if(args, loc) when length(args) > 3 do
    {:error, Errors.parse_error("if takes exactly 3 arguments, got #{length(args)}", span: Error.span_from_loc(loc)), loc}
  end

  # (cond (test1 body1) (test2 body2) :else body3)
  # → recursively transforms to nested ifs during parsing
  # or emits a special :cond node if we want to defer expansion
  # For simplicity, let's expand to nested ifs here so core emitter doesn't need changes.
  defp parse_cond(clauses, loc) do
    case clauses do
      # :else marked clause
      [{:bracket, [{:atom, :else}, body]} | rest] ->
        if rest == [] do
          body
        else
          {:error, Errors.parse_error(":else clause must be the last clause in cond", span: Error.span_from_loc(loc)), loc}
        end

      # (test body) clause
      [{:bracket, [test, body]} | rest] ->
        case parse_cond(rest, loc) do
          {:error, _, _} = err -> err
          else_branch -> {:if, test, body, else_branch, loc}
        end

      # Implicit else at the end? No, enforce :else.
      [] ->
        {:error, Errors.parse_error("cond requires an :else clause as the last argument", span: Error.span_from_loc(loc)), loc}

      other ->
        {:error, Errors.parse_error("invalid cond syntax: expected (test body) or :else body, got #{inspect(other)}", span: Error.span_from_loc(loc)), loc}
    end
  end

  # (let [x 1 y 2] body) → {:let, [{:x, 1}, {:y, 2}], body, loc}
  # (let [x 1 y 2] body1 body2 ...) → {:let, [{:x, 1}, {:y, 2}], {:do, [body1, body2, ...], loc}, loc}
  defp parse_let([{:bracket, bindings} | bodies], loc) when length(bodies) >= 1 do
    if rem(length(bindings), 2) != 0 do
      {:error, Errors.parse_error("let bindings must be pairs: (let [name value ...] body)", span: Error.span_from_loc(loc)), loc}
    else
      pairs = bindings
        |> Enum.chunk_every(2)
        |> Enum.map(fn [name, value] -> {name, value} end)
      body = case bodies do
        [single] -> single
        multiple -> {:do, multiple, loc}
      end
      {:let, pairs, body, loc}
    end
  end
  defp parse_let([{:bracket, _bindings}], loc) do
    {:error, Errors.parse_error("let requires a body expression", span: Error.span_from_loc(loc)), loc}
  end
  defp parse_let(_, loc) do
    {:error, Errors.parse_error("let requires bindings bracket: (let [name value ...] body)", span: Error.span_from_loc(loc)), loc}
  end

  # (. record :field) → {:field_access, record, field, loc}
  # For row polymorphic field access
  defp parse_field_access([record, {:atom, field}], loc) do
    {:field_access, record, field, loc}
  end
  defp parse_field_access([record, field], loc) when is_atom(field) do
    {:field_access, record, field, loc}
  end
  defp parse_field_access([_record], loc) do
    {:error, Errors.parse_error("field access requires a field name: (. record :field)", span: Error.span_from_loc(loc)), loc}
  end
  defp parse_field_access([], loc) do
    {:error, Errors.parse_error("field access requires record and field: (. record :field)", span: Error.span_from_loc(loc)), loc}
  end
  defp parse_field_access(args, loc) when length(args) > 2 do
    {:error, Errors.parse_error("field access takes 2 arguments, got #{length(args)}", span: Error.span_from_loc(loc)), loc}
  end

  # Threading macro: (-> x (f a) (g b)) → (g (f x a) b)
  # Thread-first: inserts the threaded value as the FIRST argument of each form
  # This is a compile-time transformation with zero runtime overhead.
  #
  # Examples:
  #   (-> x (f))       → (f x)
  #   (-> x (f a))     → (f x a)
  #   (-> x (f a) (g)) → (g (f x a))
  #   (-> x f)         → (f x)  ; bare symbol becomes a call
  defp parse_thread_first([initial | forms], loc) when forms != [] do
    Enum.reduce(forms, initial, fn
      # Case 1: (f arg1 arg2) - function call with args, insert initial as first arg
      {:call, func, args, call_loc}, acc ->
        {:call, func, [acc | args], call_loc}

      # Case 2: Bare atom 'f' - convert to (f acc)
      func, acc when is_atom(func) ->
        {:call, func, [acc], %Vaisto.Parser.Loc{file: "->", line: 0, col: 0, length: 0}}

      # Case 3: Invalid form (literal, string, etc.)
      other, _acc ->
        throw({:parse_error, Errors.parse_error("threading form must be a function call or name, got: #{inspect(other)}", span: Error.span_from_loc(loc))})
    end)
  catch
    {:parse_error, err} -> {:error, err, loc}
  end
  defp parse_thread_first([single], _loc) do
    # (-> x) with no forms just returns x
    single
  end
  defp parse_thread_first([], loc) do
    {:error, Errors.parse_error("threading macro requires at least one value: (-> x (f) ...)", span: Error.span_from_loc(loc)), loc}
  end

  # Thread-last: (->> x (f a) (g b)) → (g b (f a x))
  # Inserts the threaded value as the LAST argument of each form
  defp parse_thread_last([initial | forms], loc) when forms != [] do
    Enum.reduce(forms, initial, fn
      {:call, func, args, call_loc}, acc ->
        {:call, func, args ++ [acc], call_loc}
      func, acc when is_atom(func) ->
        {:call, func, [acc], %Vaisto.Parser.Loc{file: "->>", line: 0, col: 0, length: 0}}
      other, _acc ->
        throw({:parse_error, Errors.parse_error("threading form must be a function call or name, got: #{inspect(other)}", span: Error.span_from_loc(loc))})
    end)
  catch
    {:parse_error, err} -> {:error, err, loc}
  end
  defp parse_thread_last([single], _loc), do: single
  defp parse_thread_last([], loc) do
    {:error, Errors.parse_error("threading macro requires at least one value: (->> x (f) ...)", span: Error.span_from_loc(loc)), loc}
  end

  # For comprehension: (for [x xs] body) → (map (fn [x] body) xs)
  # Multi-binding: (for [x xs y ys] body) → (flat_map (fn [x] (map (fn [y] body) ys)) xs)
  # With :when: filter applies to innermost binding only
  defp parse_for([{:bracket, binding} | bodies], loc) when length(bodies) >= 1 do
    body = wrap_bodies(bodies, loc)
    {pairs, when_pred} = split_for_bindings_at_when(binding)

    cond do
      pairs == :error ->
        {:error, Errors.parse_error("for requires even number of binding elements [var collection ...]", span: Error.span_from_loc(loc)), loc}

      pairs == [] ->
        {:error, Errors.parse_error("for requires at least one binding [var collection]", span: Error.span_from_loc(loc)), loc}

      true ->
        desugar_for_pairs(pairs, when_pred, body, loc)
    end
  end
  defp parse_for([{:bracket, _binding}], loc) do
    {:error, Errors.parse_error("for requires a body expression: (for [x xs] body)", span: Error.span_from_loc(loc)), loc}
  end
  defp parse_for(_, loc) do
    {:error, Errors.parse_error("for requires a binding bracket: (for [x xs] body)", span: Error.span_from_loc(loc)), loc}
  end

  # Split binding tokens into {var, collection} pairs and optional :when predicate
  defp split_for_bindings_at_when(tokens) do
    {before_when, after_when} = Enum.split_while(tokens, fn
      {:atom, :when} -> false
      _ -> true
    end)

    when_pred = case after_when do
      [{:atom, :when}, pred] -> pred
      [] -> nil
      _ -> :error
    end

    if when_pred == :error do
      {:error, nil}
    else
      if rem(length(before_when), 2) != 0 do
        {:error, nil}
      else
        pairs = before_when |> Enum.chunk_every(2) |> Enum.map(fn [v, c] -> {v, c} end)
        {pairs, when_pred}
      end
    end
  end

  # Desugar binding pairs into nested flat_map/map calls
  # Single binding (innermost): use map (+ optional filter for :when)
  # Multiple bindings: outer uses flat_map, recurse for inner
  defp desugar_for_pairs([{var, collection}], when_pred, body, loc) do
    if when_pred do
      filter_fn = {:fn, [var], when_pred, loc}
      filtered = {:call, :filter, [filter_fn, collection], loc}
      map_fn = {:fn, [var], body, loc}
      {:call, :map, [map_fn, filtered], loc}
    else
      map_fn = {:fn, [var], body, loc}
      {:call, :map, [map_fn, collection], loc}
    end
  end

  defp desugar_for_pairs([{var, collection} | rest], when_pred, body, loc) do
    inner = desugar_for_pairs(rest, when_pred, body, loc)
    flat_map_fn = {:fn, [var], inner, loc}
    {:call, :flat_map, [flat_map_fn, collection], loc}
  end

  # (match expr [pattern1 body1] [pattern2 body2] ...) → {:match, expr, [{pattern, body}, ...], loc}
  # (match expr [pattern body] ...) or (match expr [pattern body1 body2 ...] ...)
  defp parse_match([expr | clauses], loc) when clauses != [] do
    case parse_clauses(clauses, loc) do
      {:ok, parsed_clauses} -> {:match, expr, parsed_clauses, loc}
      {:error, _, _} = err -> err
    end
  end
  defp parse_match([_expr], loc) do
    {:error, Errors.parse_error("match requires at least one clause: (match expr [pattern body] ...)", span: Error.span_from_loc(loc)), loc}
  end
  defp parse_match([], loc) do
    {:error, Errors.parse_error("match requires expression and clauses: (match expr [pattern body] ...)", span: Error.span_from_loc(loc)), loc}
  end

  defp parse_clauses(clauses, loc) do
    results = Enum.map(clauses, fn
      {:bracket, [pattern | bodies]} when length(bodies) >= 1 ->
        body = wrap_bodies(bodies, loc)
        {:ok, {pattern, body}}
      {:bracket, [_pattern]} ->
        {:error, Errors.parse_error("match clause requires a body: [pattern body]", span: Error.span_from_loc(loc)), loc}
      {:bracket, []} ->
        {:error, Errors.parse_error("match clause cannot be empty: [pattern body]", span: Error.span_from_loc(loc)), loc}
      other ->
        {:error, Errors.parse_error("match clause must be a bracket: [pattern body], got #{inspect(other)}", span: Error.span_from_loc(loc)), loc}
    end)
    case Enum.find(results, &match?({:error, %Error{}, _}, &1)) do
      nil -> {:ok, Enum.map(results, fn {:ok, c} -> c end)}
      err -> err
    end
  end
  # (receive [pattern1 body1] [pattern2 body2] ...) → {:receive, [{pattern, body}, ...], loc}
  # Blocks until a message matching one of the patterns arrives
  defp parse_receive(clauses, loc) when clauses != [] do
    case parse_clauses(clauses, loc) do
      {:ok, parsed_clauses} -> {:receive, parsed_clauses, loc}
      {:error, _, _} = err -> err
    end
  end
  defp parse_receive([], loc) do
    {:error, Errors.parse_error("receive requires at least one clause: (receive [pattern body] ...)", span: Error.span_from_loc(loc)), loc}
  end

  defp parse_process([name, initial_state | handlers], loc) when handlers != [] do
    {:process, name, initial_state, parse_handlers(handlers), loc}
  end
  defp parse_process([name, initial_state], loc) do
    {:process, name, initial_state, [], loc}
  end
  defp parse_process([_name], loc) do
    {:error, Errors.parse_error("process requires initial state: (process name initial-state :msg handler ...)", span: Error.span_from_loc(loc)), loc}
  end
  defp parse_process([], loc) do
    {:error, Errors.parse_error("process requires name and initial state", span: Error.span_from_loc(loc)), loc}
  end

  defp parse_supervise([strategy | children], loc) do
    {:supervise, strategy, children, loc}
  end
  defp parse_supervise([], loc) do
    {:error, Errors.parse_error("supervise requires a strategy: (supervise :one_for_one child ...)", span: Error.span_from_loc(loc)), loc}
  end

  # Function definition: (def name [args] body)
  defp parse_def([name, {:bracket, args}, body], loc) do
    {:def, name, {:bracket, args}, body, loc}
  end
  # Value binding: (def name value) - no args
  defp parse_def([name, value], loc) do
    {:defval, name, value, loc}
  end
  defp parse_def([_name], loc) do
    {:error, Errors.parse_error("def requires a value: (def name value)", span: Error.span_from_loc(loc)), loc}
  end
  defp parse_def([], loc) do
    {:error, Errors.parse_error("def requires a name and value", span: Error.span_from_loc(loc)), loc}
  end
  defp parse_def(args, loc) when length(args) > 3 do
    {:error, Errors.parse_error("def has too many arguments", span: Error.span_from_loc(loc)), loc}
  end

  defp parse_defprompt([name, {:atom, :input}, input_type, {:atom, :output}, output_type], loc) do
    with {:ok, parsed_input} <- parse_type_ref(input_type),
         {:ok, parsed_output} <- parse_type_ref(output_type) do
      {:defprompt, name, parsed_input, parsed_output, nil, loc}
    else
      {:error, msg} ->
        {:error, Errors.parse_error("defprompt #{msg}", span: Error.span_from_loc(loc)), loc}
    end
  end

  defp parse_defprompt([name, {:atom, :input}, input_type, {:atom, :output}, output_type, {:atom, :template}, {:string, template}], loc) do
    with {:ok, parsed_input} <- parse_type_ref(input_type),
         {:ok, parsed_output} <- parse_type_ref(output_type) do
      {:defprompt, name, parsed_input, parsed_output, template, loc}
    else
      {:error, msg} ->
        {:error, Errors.parse_error("defprompt #{msg}", span: Error.span_from_loc(loc)), loc}
    end
  end

  defp parse_defprompt(_args, loc) do
    {:error, Errors.parse_error("defprompt requires syntax: (defprompt NAME :input TYPE :output TYPE [:template STRING])", span: Error.span_from_loc(loc)), loc}
  end

  defp parse_pipeline([name, {:atom, :input}, input_type, {:atom, :output}, output_type | ops], loc) when ops != [] do
    with {:ok, parsed_input} <- parse_type_ref(input_type),
         {:ok, parsed_output} <- parse_type_ref(output_type) do
      {:pipeline, name, parsed_input, parsed_output, ops, loc}
    else
      {:error, msg} ->
        {:error, Errors.parse_error("pipeline #{msg}", span: Error.span_from_loc(loc)), loc}
    end
  end

  defp parse_pipeline([_name, {:atom, :input}, _input_type, {:atom, :output}, _output_type], loc) do
    {:error, Errors.parse_error("pipeline requires at least one operator", span: Error.span_from_loc(loc)), loc}
  end

  defp parse_pipeline(_args, loc) do
    {:error, Errors.parse_error("pipeline requires syntax: (pipeline NAME :input TYPE :output TYPE OP ...)", span: Error.span_from_loc(loc)), loc}
  end

  defp parse_generate([{:atom, :prompt}, prompt_name, {:atom, :extract}, extract_type], loc) do
    with {:ok, parsed_extract} <- parse_type_ref(extract_type) do
      {:generate, prompt_name, parsed_extract, loc}
    else
      {:error, msg} ->
        {:error, Errors.parse_error("generate #{msg}", span: Error.span_from_loc(loc)), loc}
    end
  end

  defp parse_generate(_args, loc) do
    {:error, Errors.parse_error("generate requires syntax: (generate :prompt PROMPT_NAME :extract TYPE)", span: Error.span_from_loc(loc)), loc}
  end

  # Multi-clause function definition
  # (defn len
  #   [[] 0]
  #   [[h | t] (+ 1 (len t))])
  # All clauses are brackets: [pattern body]
  # This clause only matches when ALL remaining elements are brackets (multi-clause style)
  defp parse_defn([name | clauses], loc) when is_list(clauses) and length(clauses) >= 1 do
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
  # (defn name [params] :ret_type body) or (defn name [params] :ret_type body1 body2 ...)
  # (defn name [params] body) or (defn name [params] body1 body2 ...)
  # (defn name [params :when guard] :ret_type body) — guarded variant
  defp parse_defn_single(name, [{:bracket, params} | rest], loc) when length(rest) >= 1 do
    case split_params_at_when(params) do
      {:error, msg} ->
        {:error, Errors.parse_error("in definition of `#{name}`: #{msg}", span: Error.span_from_loc(loc)), loc}

      {param_tokens, guard} ->
        typed_params = parse_typed_params(param_tokens)

        case rest do
          # Single element is always the body - even if it looks like a type
          # This allows (defn foo [] :int) to return the keyword :int
          [single_body] ->
            make_defn(name, typed_params, single_body, :any, guard, loc)

          # Type annotation followed by body: (defn name [params] :type body ...)
          [{:atom, type_atom} | bodies] when length(bodies) >= 1 ->
            if is_type_annotation?({:atom, type_atom}) do
              body = wrap_bodies(bodies, loc)
              make_defn(name, typed_params, body, unwrap_type({:atom, type_atom}), guard, loc)
            else
              # :keyword treated as body
              body = wrap_bodies(rest, loc)
              make_defn(name, typed_params, body, :any, guard, loc)
            end

          # Compound type annotation (List/Tuple/etc): (defn name [params] (Type ...) body ...)
          [{:call, _, _, _} = type_expr | bodies] when length(bodies) >= 1 ->
            if is_type_annotation?(type_expr) do
              body = wrap_bodies(bodies, loc)
              make_defn(name, typed_params, body, type_expr, guard, loc)
            else
              body = wrap_bodies(rest, loc)
              make_defn(name, typed_params, body, :any, guard, loc)
            end

          # Multiple body expressions (no type annotation): (defn name [params] body1 body2 ...)
          _ ->
            body = wrap_bodies(rest, loc)
            make_defn(name, typed_params, body, :any, guard, loc)
        end
    end
  end

  defp parse_defn_single(_name, [{:bracket, _params}], loc) do
    {:error, Errors.parse_error("defn requires a body expression", span: Error.span_from_loc(loc)), loc}
  end

  defp parse_defn_single(_name, _rest, loc) do
    {:error, Errors.parse_error("Invalid defn syntax", span: Error.span_from_loc(loc)), loc}
  end

  defp make_defn(name, params, body, ret_type, nil, loc) do
    {:defn, name, params, body, ret_type, loc}
  end
  defp make_defn(name, params, body, ret_type, guard, loc) do
    {:defn, name, params, body, ret_type, guard, loc}
  end

  # Split params tokens at :when — returns {param_tokens, guard_ast | nil}
  defp split_params_at_when(tokens) do
    {before_when, after_when} = Enum.split_while(tokens, fn
      {:atom, :when} -> false
      _ -> true
    end)

    case after_when do
      [{:atom, :when}, guard] -> {before_when, guard}
      [{:atom, :when}] -> {:error, "guard expression required after :when"}
      [{:atom, :when} | _] -> {:error, "guard must be a single expression after :when"}
      [] -> {tokens, nil}
    end
  end

  # Wrap multiple body expressions in a do block
  defp wrap_bodies([single], _loc), do: single
  defp wrap_bodies(multiple, loc), do: {:do, multiple, loc}

  # Extract pattern, optional guard, and body from a clause
  # Clause content is [pattern, body] or [pattern, :when, guard, body...]
  defp extract_pattern_body([pattern, {:atom, :when}, guard | bodies]) when bodies != [] do
    body = wrap_bodies(bodies, %Vaisto.Parser.Loc{file: "defn_multi", line: 0, col: 0, length: 0})
    {pattern, guard, body}
  end
  defp extract_pattern_body([pattern, body]), do: {pattern, nil, body}
  defp extract_pattern_body([pattern | bodies]) when length(bodies) > 1 do
    body = wrap_bodies(bodies, %Vaisto.Parser.Loc{file: "defn_multi", line: 0, col: 0, length: 0})
    {pattern, nil, body}
  end

  # Type annotation checks for param parsing
  defp is_type_annotation?({:atom, t}) when t in [:int, :float, :num, :string, :bool, :any, :atom, :unit], do: true
  defp is_type_annotation?(t) when is_atom(t) and t in [:int, :float, :num, :string, :bool, :any, :atom, :unit], do: true
  # Parameterized types: (List :int), (Result :int :string)
  defp is_type_annotation?({:call, type_name, _args, _loc}) when is_atom(type_name), do: true
  # User-defined types (capitalized atoms)
  defp is_type_annotation?({:atom, t}) when is_atom(t), do: String.match?(Atom.to_string(t), ~r/^[A-Z]/)
  defp is_type_annotation?(t) when is_atom(t), do: String.match?(Atom.to_string(t), ~r/^[A-Z]/)
  defp is_type_annotation?(_), do: false

  # Unwrap {:atom, :int} → :int for type annotations
  defp unwrap_type({:atom, t}), do: t
  defp unwrap_type(t), do: t

  defp parse_type_ref(type_ref) do
    if is_type_annotation?(type_ref) do
      {:ok, unwrap_type(type_ref)}
    else
      {:error, "requires a valid type reference"}
    end
  end

  # Left-to-right greedy scan: each param name optionally followed by a type annotation.
  # Supports mixed typed/untyped: [n :int xs] → [{:n, :int}, {:xs, :any}]
  defp parse_typed_params(params) do
    parse_typed_params_acc(params, [])
  end

  defp parse_typed_params_acc([], acc), do: Enum.reverse(acc)
  defp parse_typed_params_acc([name | rest], acc) do
    case rest do
      [maybe_type | tail] ->
        if is_type_annotation?(maybe_type) do
          parse_typed_params_acc(tail, [{name, unwrap_type(maybe_type)} | acc])
        else
          parse_typed_params_acc(rest, [{name, :any} | acc])
        end
      [] ->
        Enum.reverse([{name, :any} | acc])
    end
  end

  # (fn [x] (* x 2)) → {:fn, [:x], body, loc}
  # (fn [params] body) → {:fn, params, body, loc}
  # (fn [params] body1 body2 ...) → {:fn, params, {:do, [body1, ...], loc}, loc}
  defp parse_fn([{:bracket, params} | bodies], loc) when length(bodies) >= 1 do
    body = case bodies do
      [single] -> single
      multiple -> {:do, multiple, loc}
    end
    {:fn, params, body, loc}
  end

  # Product type with deriving: (deftype Point [x :int y :int] deriving [Eq])
  defp parse_deftype([name, {:bracket, fields}, :deriving, {:bracket, classes}], loc) do
    field_pairs = fields
      |> Enum.chunk_every(2)
      |> Enum.map(fn [field_name, type] -> {field_name, type} end)
    {:deftype_deriving, name, {:product, field_pairs}, classes, loc}
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
    # Split off trailing deriving clause if present
    {variant_rest, derive_classes} = split_deriving(rest)

    case variant_rest do
      [{:call, _, _, _} | _] ->
        # Sum type: parse variants from call AST nodes
        parsed_variants = Enum.map(variant_rest, fn
          {:call, ctor_name, args, _loc} ->
            {ctor_name, args}
          other ->
            {:error, Errors.parse_error("Invalid variant syntax: #{inspect(other)}")}
        end)
        # Check for errors
        case Enum.find(parsed_variants, &match?({:error, %Error{}}, &1)) do
          nil ->
            if derive_classes == [] do
              {:deftype, name, {:sum, parsed_variants}, loc}
            else
              {:deftype_deriving, name, {:sum, parsed_variants}, derive_classes, loc}
            end
          {:error, %Error{} = err} -> {:error, err, loc}
        end

      # Legacy: (deftype point x y) → untyped product fields
      fields ->
        field_pairs = Enum.map(fields, fn field -> {field, :any} end)
        {:deftype, name, {:product, field_pairs}, loc}
    end
  end

  defp split_deriving(rest) do
    case Enum.split(rest, -2) do
      {variants, [:deriving, {:bracket, classes}]} when variants != [] -> {variants, classes}
      _ -> {rest, []}
    end
  end

  # (extern erlang:hd [(List :any)] :any) → {:extern, :erlang, :hd, [type_expr], ret_type, loc}
  defp parse_extern([{:qualified, mod, func}, {:bracket, arg_types}, ret_type], loc) do
    {:extern, mod, func, arg_types, ret_type, loc}
  end

  # (defclass Eq [a] (eq [x :a y :a] :bool))
  # → {:defclass, :Eq, [:a], [{:eq, [{:x, :a}, {:y, :a}], :bool}], loc}
  defp parse_defclass([name, {:bracket, type_params} | methods], loc) when is_atom(name) do
    parsed_methods = Enum.map(methods, fn method ->
      parse_class_method(method)
    end)
    {:defclass, name, type_params, parsed_methods, loc}
  end

  # Parse a class method declaration:
  # No default:   (eq [x :a y :a] :bool)     → {:eq, [{:x, :a}, {:y, :a}], :bool, nil}
  # With default:  (neq [x :a y :a] :bool (if (eq x y) false true))
  #             → {:neq, [{:x, :a}, {:y, :a}], :bool, <body_ast>}
  defp parse_class_method({:call, method_name, args, _loc}) do
    parse_class_method_args(method_name, args)
  end
  defp parse_class_method({:call, method_name, args}) do
    parse_class_method_args(method_name, args)
  end

  defp parse_class_method_args(method_name, args) do
    # args is [{:bracket, params}, ret_type] or [{:bracket, params}, ret_type, body, ...]
    case args do
      [{:bracket, params}, ret_type | body] ->
        typed_params = parse_class_params(params)
        default_body = case body do
          [] -> nil
          [b] -> b
          _ -> nil
        end
        {method_name, typed_params, unwrap_type(ret_type), default_body}
      [{:bracket, params}] ->
        typed_params = parse_class_params(params)
        {method_name, typed_params, :any, nil}
    end
  end

  # Parse class method params: [x :a y :a] → [{:x, :a}, {:y, :a}]
  # Type params in class methods are always paired: name type name type
  defp parse_class_params(params) do
    params
    |> Enum.chunk_every(2)
    |> Enum.map(fn [param_name, type] -> {param_name, unwrap_type(type)} end)
  end

  # Constrained instance: (instance Show (Maybe a) where [(Show a)] (show [x] ...))
  # → {:instance_constrained, :Show, :Maybe, [:a], [{:Show, :a}], methods, loc}
  defp parse_instance([class_name, {:call, type_name, type_params, _tloc}, :where, {:bracket, constraints} | methods], loc) when is_atom(class_name) do
    parsed_constraints = Enum.map(constraints, fn
      {:call, cname, [tvar], _} -> {cname, tvar}
      {:call, cname, [tvar]} -> {cname, tvar}
    end)
    parsed_methods = Enum.map(methods, &parse_instance_method/1)
    {:instance_constrained, class_name, type_name, type_params, parsed_constraints, parsed_methods, loc}
  end

  # (instance Eq :int (eq [x y] (== x y)))
  # → {:instance, :Eq, :int, [{:eq, [:x, :y], body_ast}], loc}
  defp parse_instance([class_name, for_type | methods], loc) when is_atom(class_name) do
    parsed_methods = Enum.map(methods, fn method ->
      parse_instance_method(method)
    end)
    {:instance, class_name, unwrap_type(for_type), parsed_methods, loc}
  end

  # Parse an instance method: (eq [x y] (== x y)) → {:eq, [:x, :y], body_ast}
  defp parse_instance_method({:call, method_name, args, _loc}) do
    parse_instance_method_args(method_name, args)
  end
  defp parse_instance_method({:call, method_name, args}) do
    parse_instance_method_args(method_name, args)
  end

  defp parse_instance_method_args(method_name, args) do
    case args do
      [{:bracket, params} | bodies] when length(bodies) >= 1 ->
        body = case bodies do
          [single] -> single
          multiple -> {:do, multiple, %Loc{}}
        end
        {method_name, params, body}
    end
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
  defp normalize_module_name_str(str), do: String.to_atom(str)

  defp parse_handlers(handlers) when rem(length(handlers), 2) == 0 do
    handlers
    |> Enum.chunk_every(2)
    |> Enum.map(fn [msg, body] -> {msg, body} end)
  end
  defp parse_handlers(_handlers) do
    # Odd number of handlers - return empty and let type checker catch it
    # (Better: return error, but handlers are nested inside process so propagation is complex)
    []
  end

  # Token conversion
  # Takes token string and location, returns parsed value
  # Location is available for future use in error messages
  defp parse_token(token, loc) do
    cond do
      String.starts_with?(token, "\"\"\"") ->
        raw = token |> String.trim_leading("\"\"\"") |> String.trim_trailing("\"\"\"")
        {:string, raw}
      # String literal: "hello" → {:string, "hello"}
      # With interpolation: "hello #{name}" → {:call, :str, [{:string, "hello "}, name], loc}
      String.starts_with?(token, "\"") ->
        raw = String.slice(token, 1..-2//1)
        if String.contains?(raw, ~S(#{)) and not only_escaped_interpolations?(raw) do
          segments = split_interpolation(raw)
          |> Enum.reject(fn {:text, t} -> t == ""; _ -> false end)
          |> Enum.map(fn
            {:text, t} -> {:string, unescape_string(t)}
            {:expr, e} ->
              case parse(e) do
                list when is_list(list) -> {:do, list, loc}
                single -> single
              end
          end)

          {:call, :str, segments, loc}
        else
          raw |> unescape_string() |> then(&{:string, &1})
        end
      token =~ ~r/^0x[0-9a-fA-F]+$/ -> String.to_integer(String.trim_leading(token, "0x"), 16)
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
      # Character literal: \c or \name
      String.starts_with?(token, "\\") ->
        parse_char_literal(token)
      true -> String.to_atom(token)
    end
  end

  # Check if all #{...} occurrences in a string are escaped (\#{)
  defp only_escaped_interpolations?(raw) do
    # Remove all \#{ and see if any #{ remains
    marker = ~S(#{)
    raw
    |> String.replace(~S(\#{), "")
    |> then(&(not String.contains?(&1, marker)))
  end

  # Split string content on #{...} interpolation markers.
  # Returns [{:text, "hello "}, {:expr, "name"}, {:text, "!"}]
  # Handles nested braces and escaped \#{
  defp split_interpolation(raw) do
    raw
    |> String.graphemes()
    |> do_split_interpolation(:text, [], [], 0)
  end

  defp do_split_interpolation([], :text, acc, segments, _depth) do
    text = acc |> Enum.reverse() |> Enum.join()
    Enum.reverse([{:text, text} | segments])
  end

  defp do_split_interpolation([], :expr, _acc, _segments, _depth) do
    raise "Unterminated string interpolation"
  end

  # Escaped interpolation: \#{ → literal text
  defp do_split_interpolation(["\\", "#", "{" | rest], :text, acc, segments, depth) do
    do_split_interpolation(rest, :text, ["{", "#", "\\" | acc], segments, depth)
  end

  # Start interpolation: #{
  defp do_split_interpolation(["#", "{" | rest], :text, acc, segments, _depth) do
    text = acc |> Enum.reverse() |> Enum.join()
    do_split_interpolation(rest, :expr, [], [{:text, text} | segments], 0)
  end

  # Nested brace inside interpolation
  defp do_split_interpolation(["{" | rest], :expr, acc, segments, depth) do
    do_split_interpolation(rest, :expr, ["{" | acc], segments, depth + 1)
  end

  # Closing brace at depth 0 → end interpolation
  defp do_split_interpolation(["}" | rest], :expr, acc, segments, 0) do
    expr = acc |> Enum.reverse() |> Enum.join()
    do_split_interpolation(rest, :text, [], [{:expr, expr} | segments], 0)
  end

  # Closing brace at depth > 0 → nested brace
  defp do_split_interpolation(["}" | rest], :expr, acc, segments, depth) do
    do_split_interpolation(rest, :expr, ["}" | acc], segments, depth - 1)
  end

  # Regular char
  defp do_split_interpolation([c | rest], mode, acc, segments, depth) do
    do_split_interpolation(rest, mode, [c | acc], segments, depth)
  end

  # Handle common escape sequences in strings
  # Process left-to-right to correctly handle escaped backslashes (\\)
  # before other escape sequences like \n
  defp unescape_string(s) do
    s
    |> String.graphemes()
    |> unescape_chars([])
  end

  defp unescape_chars([], acc), do: acc |> Enum.reverse() |> Enum.join()
  defp unescape_chars(["\\", "\\" | rest], acc), do: unescape_chars(rest, ["\\" | acc])
  defp unescape_chars(["\\", "n" | rest], acc), do: unescape_chars(rest, ["\n" | acc])
  defp unescape_chars(["\\", "t" | rest], acc), do: unescape_chars(rest, ["\t" | acc])
  defp unescape_chars(["\\", "r" | rest], acc), do: unescape_chars(rest, ["\r" | acc])
  defp unescape_chars(["\\", "\"" | rest], acc), do: unescape_chars(rest, ["\"" | acc])
  defp unescape_chars(["\\", c | rest], acc), do: unescape_chars(rest, [c | acc])  # Unknown escape: keep char
  defp unescape_chars([c | rest], acc), do: unescape_chars(rest, [c | acc])

  # Character literal parsing
  # \newline, \space, \tab, \return or \c
  defp parse_char_literal(token) do
    content = String.slice(token, 1..-1//1)
    codepoint = case content do
      "newline" -> 10
      "space" -> 32
      "tab" -> 9
      "return" -> 13
      "null" -> 0
      simple when byte_size(simple) == 1 ->
        <<c::utf8>> = simple
        c
      _ ->
        raise "Invalid character literal: #{token}"
    end
    codepoint
  end
end
