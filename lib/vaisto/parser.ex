defmodule Vaisto.Parser do
  @moduledoc """
  Minimalist parser for Vaisto S-expressions with line/column tracking.

  Transforms text into AST:
    "(+ 1 2)" → {:call, :+, [1, 2], %{line: 1, col: 1}}

  Each AST node includes location metadata for error reporting.
  """

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
  # current_token_state: nil | {chars_acc, start_line, start_col} | {:string, chars_acc, start_line, start_col}

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
      [:deftype | rest] -> parse_deftype(rest, open_loc)
      [:fn | rest] -> parse_fn(rest, open_loc)
      [:extern | rest] -> parse_extern(rest, open_loc)
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
      # Threading macro: (-> x (f a) (g b)) → (g (f x a) b)
      # Compile-time transformation - zero runtime overhead
      [:-> | rest] -> parse_thread_first(rest, open_loc)
      # Field access: (. record :field) → {:field_access, record, :field, loc}
      [:. | rest] -> parse_field_access(rest, open_loc)
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
    {:error, "if requires 3 arguments: (if condition then else)", loc}
  end
  defp parse_if(args, loc) when length(args) > 3 do
    {:error, "if takes exactly 3 arguments, got #{length(args)}", loc}
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
          {:error, ":else clause must be the last clause in cond", loc}
        end

      # (test body) clause
      [{:bracket, [test, body]} | rest] ->
        case parse_cond(rest, loc) do
          {:error, _, _} = err -> err
          else_branch -> {:if, test, body, else_branch, loc}
        end

      # Implicit else at the end? No, enforce :else.
      [] ->
        {:error, "cond requires an :else clause as the last argument", loc}

      other ->
        {:error, "invalid cond syntax: expected (test body) or :else body, got #{inspect(other)}", loc}
    end
  end

  # (let [x 1 y 2] body) → {:let, [{:x, 1}, {:y, 2}], body, loc}
  # (let [x 1 y 2] body1 body2 ...) → {:let, [{:x, 1}, {:y, 2}], {:do, [body1, body2, ...], loc}, loc}
  defp parse_let([{:bracket, bindings} | bodies], loc) when length(bodies) >= 1 do
    if rem(length(bindings), 2) != 0 do
      {:error, "let bindings must be pairs: (let [name value ...] body)", loc}
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
    {:error, "let requires a body expression", loc}
  end
  defp parse_let(_, loc) do
    {:error, "let requires bindings bracket: (let [name value ...] body)", loc}
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
    {:error, "field access requires a field name: (. record :field)", loc}
  end
  defp parse_field_access([], loc) do
    {:error, "field access requires record and field: (. record :field)", loc}
  end
  defp parse_field_access(args, loc) when length(args) > 2 do
    {:error, "field access takes 2 arguments, got #{length(args)}", loc}
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
  defp parse_thread_first([initial | forms], _loc) when forms != [] do
    Enum.reduce(forms, initial, fn
      # Case 1: (f arg1 arg2) - function call with args, insert initial as first arg
      {:call, func, args, loc}, acc ->
        {:call, func, [acc | args], loc}

      # Case 2: Bare atom 'f' - convert to (f acc)
      func, acc when is_atom(func) ->
        {:call, func, [acc], %Vaisto.Parser.Loc{file: "->", line: 0, col: 0, length: 0}}
    end)
  end
  defp parse_thread_first([single], _loc) do
    # (-> x) with no forms just returns x
    single
  end
  defp parse_thread_first([], loc) do
    {:error, "threading macro requires at least one value: (-> x (f) ...)", loc}
  end


  # (match expr [pattern1 body1] [pattern2 body2] ...) → {:match, expr, [{pattern, body}, ...], loc}
  # (match expr [pattern body] ...) or (match expr [pattern body1 body2 ...] ...)
  defp parse_match([expr | clauses], loc) when clauses != [] do
    case parse_clauses(clauses, loc) do
      {:ok, parsed_clauses} -> {:match, expr, parsed_clauses, loc}
      {:error, _} = err -> err
    end
  end
  defp parse_match([_expr], loc) do
    {:error, "match requires at least one clause: (match expr [pattern body] ...)", loc}
  end
  defp parse_match([], loc) do
    {:error, "match requires expression and clauses: (match expr [pattern body] ...)", loc}
  end

  defp parse_clauses(clauses, loc) do
    results = Enum.map(clauses, fn
      {:bracket, [pattern | bodies]} when length(bodies) >= 1 ->
        body = wrap_bodies(bodies, loc)
        {:ok, {pattern, body}}
      {:bracket, [_pattern]} ->
        {:error, "match clause requires a body: [pattern body]", loc}
      {:bracket, []} ->
        {:error, "match clause cannot be empty: [pattern body]", loc}
      other ->
        {:error, "match clause must be a bracket: [pattern body], got #{inspect(other)}", loc}
    end)
    case Enum.find(results, &match?({:error, _, _}, &1)) do
      nil -> {:ok, Enum.map(results, fn {:ok, c} -> c end)}
      err -> err
    end
  end
  # (receive [pattern1 body1] [pattern2 body2] ...) → {:receive, [{pattern, body}, ...], loc}
  # Blocks until a message matching one of the patterns arrives
  defp parse_receive(clauses, loc) when clauses != [] do
    case parse_clauses(clauses, loc) do
      {:ok, parsed_clauses} -> {:receive, parsed_clauses, loc}
      {:error, _} = err -> err
    end
  end
  defp parse_receive([], loc) do
    {:error, "receive requires at least one clause: (receive [pattern body] ...)", loc}
  end

  defp parse_process([name, initial_state | handlers], loc) when handlers != [] do
    {:process, name, initial_state, parse_handlers(handlers), loc}
  end
  defp parse_process([name, initial_state], loc) do
    {:process, name, initial_state, [], loc}
  end
  defp parse_process([_name], loc) do
    {:error, "process requires initial state: (process name initial-state :msg handler ...)", loc}
  end
  defp parse_process([], loc) do
    {:error, "process requires name and initial state", loc}
  end

  defp parse_supervise([strategy | children], loc) do
    {:supervise, strategy, children, loc}
  end
  defp parse_supervise([], loc) do
    {:error, "supervise requires a strategy: (supervise :one_for_one child ...)", loc}
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
    {:error, "def requires a value: (def name value)", loc}
  end
  defp parse_def([], loc) do
    {:error, "def requires a name and value", loc}
  end
  defp parse_def(args, loc) when length(args) > 3 do
    {:error, "def has too many arguments", loc}
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
  # (defn name [params] :ret_type body) or (defn name [params] :ret_type body1 body2 ...)
  # (defn name [params] body) or (defn name [params] body1 body2 ...)
  defp parse_defn_single(name, [{:bracket, params} | rest], loc) when length(rest) >= 1 do
    typed_params = parse_typed_params(params)

    case rest do
      # Single element is always the body - even if it looks like a type
      # This allows (defn foo [] :int) to return the keyword :int
      [single_body] ->
        {:defn, name, typed_params, single_body, :any, loc}

      # Type annotation followed by body: (defn name [params] :type body ...)
      [{:atom, type_atom} | bodies] when length(bodies) >= 1 ->
        if is_type_annotation?({:atom, type_atom}) do
          body = wrap_bodies(bodies, loc)
          {:defn, name, typed_params, body, unwrap_type({:atom, type_atom}), loc}
        else
          # :keyword treated as body
          body = wrap_bodies(rest, loc)
          {:defn, name, typed_params, body, :any, loc}
        end

      # Multiple body expressions (no type annotation): (defn name [params] body1 body2 ...)
      _ ->
        body = wrap_bodies(rest, loc)
        {:defn, name, typed_params, body, :any, loc}
    end
  end

  defp parse_defn_single(_name, [{:bracket, _params}], loc) do
    # No body - error
    {:error, "defn requires a body expression", loc}
  end

  defp parse_defn_single(_name, _rest, loc) do
    {:error, "Invalid defn syntax", loc}
  end

  # Wrap multiple body expressions in a do block
  defp wrap_bodies([single], _loc), do: single
  defp wrap_bodies(multiple, loc), do: {:do, multiple, loc}

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
  # (fn [params] body) → {:fn, params, body, loc}
  # (fn [params] body1 body2 ...) → {:fn, params, {:do, [body1, ...], loc}, loc}
  defp parse_fn([{:bracket, params} | bodies], loc) when length(bodies) >= 1 do
    body = case bodies do
      [single] -> single
      multiple -> {:do, multiple, loc}
    end
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
      # Character literal: \c or \name
      String.starts_with?(token, "\\") ->
        parse_char_literal(token)
      true -> String.to_atom(token)
    end
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
