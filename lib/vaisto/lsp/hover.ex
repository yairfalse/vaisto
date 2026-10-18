defmodule Vaisto.LSP.Hover do
  @moduledoc """
  Hover information provider for Vaisto LSP.

  Handles textDocument/hover requests by finding the token at cursor
  position and looking up its type information.
  """

  alias Vaisto.Parser
  alias Vaisto.{Compilation, Elab, TypeFormatter}

  @probe :__vaisto_type_probe__
  @builtins Map.new([:+, :-, :*, :<, :>, :<=, :>=], fn op ->
    result = if op in [:<, :>, :<=, :>=], do: "Bool", else: "Int"
    float_result = if result == "Bool", do: "Bool", else: "Float"
    {op, "(Int, Int) -> #{result} or (Float, Float) -> #{float_result}"}
  end) |> Map.merge(%{
    :/ => "(Float, Float) -> Float", :div => "(Int, Int) -> Int", :rem => "(Int, Int) -> Int",
    :== => "(a, a) -> Bool (ground data or an Eq dictionary)",
    :!= => "(a, a) -> Bool (ground data or an Eq dictionary)",
    :not => "(Bool) -> Bool", :and => "(Bool, Bool) -> Bool", :or => "(Bool, Bool) -> Bool",
    :length => "forall a. (List(a)) -> Int", :head => "forall a. (List(a)) -> a",
    :tail => "forall a. (List(a)) -> List(a)", :empty? => "forall a. (List(a)) -> Bool",
    :cons => "forall a. (a, List(a)) -> List(a)", :str => "ground data -> String"
  })

  # Safe atom conversion — avoids exhausting the atom table in a long-running LSP
  defp safe_to_atom(str) do
    String.to_existing_atom(str)
  rescue
    ArgumentError -> nil
  end
  alias Vaisto.Error
  alias Vaisto.LSP.ASTAnalyzer

  # Delimiters that separate tokens
  @delimiters [?(, ?), ?[, ?], ?{, ?}, ?\s, ?\t, ?\n, ?\r]

  @doc """
  Get the definition location for a symbol at a position.

  Returns {:ok, %{line: n, col: n}} or :not_found.
  """
  def get_definition(source, line, col, file \\ nil) do
    with {:ok, token, _span} <- token_at(source, line, col),
         false <- is_literal?(token),
         {:ok, loc} <- find_definition_loc(source, token, file) do
      {:ok, loc}
    else
      true -> :not_found  # literals have no definition
      :not_found -> :not_found
      {:error, _} -> :not_found
    end
  end

  @doc """
  Get hover information for a position in the source.

  Returns {:ok, hover_info} or :not_found where hover_info contains:
  - contents: Markdown string with type information
  - range: The span of the hovered token
  """
  def get_hover(source, line, col, file \\ nil) do
    with {:ok, token, span} <- token_at(source, line, col),
         {:ok, type, kind, constraints} <- lookup_type(source, token, span, file) do
      contents = format_hover(token, type, kind: kind, constraints: constraints)
      {:ok, %{contents: contents, range: span}}
    else
      :not_found -> :not_found
      {:error, _} -> :not_found
    end
  end

  @doc """
  Find the token at a given position in the source.

  Returns {:ok, token, span} or :not_found.
  Span is a map with :line, :col, :length keys.
  """
  def token_at(source, line, col) do
    lines = String.split(source, "\n")

    cond do
      line < 1 or line > length(lines) ->
        :not_found

      true ->
        line_text = Enum.at(lines, line - 1)
        find_token_in_line(line_text, col, line)
    end
  end

  @doc """
  Build an environment map from typed AST.

  Extracts top-level definitions and their types.
  Returns map of name => %{type: type, loc: loc, kind: kind}
  """
  def build_env([:module, _, _ | items]) do
    constraints = for [:meta | entries] <- items,
      [:"function-constraints" | cs] <- entries,
      [f | cs] <- cs, into: %{}, do: {f, Enum.map(cs, fn [c, t, _] -> {c, t} end)}
    Enum.reduce(items, %{}, fn
      [:def, f, type, _], env ->
        cs = Map.get(constraints, f, [])
        type = if cs == [] do
          type
        else
          case type do
            [:forall, binders, [:->, _, _, user]] -> [:forall, binders, user]
            [:->, _, _, user] -> user
          end
        end
        Map.put(env, f, %{type: type, loc: nil, kind: :function, constraints: cs})
      [:type, name, params, [kind | members]], env ->
        type = [name | Enum.map(params, &[:tvar, &1])]
        env = Map.put(env, name, %{type: Elab.Types.generalize(type), loc: nil, kind: :type})
        constructors = if kind == :sum, do: members, else: [[name | Enum.map(members, fn [_, t] -> t end)]]
        Enum.reduce(constructors, env, fn [c | args], env ->
          signature = if args == [], do: type, else: [:->, args, [:eff, :closed], type]
          Map.put(env, c, %{type: Elab.Types.generalize(signature), loc: nil, kind: :constructor})
        end)
      _, env -> env
    end)
  end

  def build_env(typed_asts) when is_list(typed_asts) do
    Enum.reduce(typed_asts, %{}, &extract_definition/2)
  end

  def build_env(typed_ast) do
    build_env([typed_ast])
  end

  @doc """
  Format hover content as markdown.

  Options:
  - :loc - Definition location (shows "defined at line N")
  - :kind - Symbol kind (:function, :variable, :type, :constructor)
  """
  def format_hover(name, type, opts \\ []) do
    type_str = if is_binary(type), do: type, else: Error.format_type(type)
    constraints = Keyword.get(opts, :constraints, [])
    type_str = if constraints == [], do: type_str, else:
      Enum.map_join(constraints, ", ", fn {c, t} -> "#{c} #{TypeFormatter.format(t)}" end) <> " => " <> type_str
    loc = Keyword.get(opts, :loc)
    kind = Keyword.get(opts, :kind)

    lines = ["```vaisto", type_str, "```"]

    # Add kind and name info
    kind_info = case kind do
      :function -> "*function* `#{name}`"
      :variable -> "*variable* `#{name}`"
      :type -> "*type* `#{name}`"
      :constructor -> "*constructor* `#{name}`"
      :process -> "*process* `#{name}`"
      :extern -> "*extern* `#{name}`"
      _ -> nil
    end

    lines = if kind_info, do: lines ++ ["", "---", kind_info], else: lines

    # Add location info
    loc_info = case loc do
      %Parser.Loc{line: l, file: f} when not is_nil(f) ->
        "defined at #{f}:#{l}"
      %Parser.Loc{line: l} ->
        "defined at line #{l}"
      _ ->
        nil
    end

    lines = if loc_info, do: lines ++ [loc_info], else: lines

    Enum.join(lines, "\n")
  end

  # ============================================================================
  # Token Finding
  # ============================================================================

  defp find_token_in_line(line_text, col, line_num) do
    # col is 1-indexed
    if col < 1 or col > String.length(line_text) + 1 do
      :not_found
    else
      chars = String.to_charlist(line_text)
      find_token_at_col(chars, col, 1, line_num)
    end
  end

  defp find_token_at_col(chars, target_col, current_col, line_num) do
    case scan_token(chars, current_col) do
      :eof ->
        :not_found

      {:skip, rest, new_col} ->
        # Skipped whitespace or delimiter
        if current_col == target_col do
          :not_found
        else
          find_token_at_col(rest, target_col, new_col, line_num)
        end

      {:token, token, start_col, end_col, rest} ->
        if target_col >= start_col and target_col < end_col do
          {:ok, token, %{line: line_num, col: start_col, length: end_col - start_col}}
        else
          find_token_at_col(rest, target_col, end_col, line_num)
        end
    end
  end

  # Scan and return the next token or skip
  defp scan_token([], _col), do: :eof

  defp scan_token([c | rest], col) when c in @delimiters do
    {:skip, rest, col + 1}
  end

  # String literal
  defp scan_token([?" | rest], col) do
    {str_chars, remaining} = scan_string(rest, [?"])
    token = List.to_string(str_chars)
    {:token, token, col, col + String.length(token), remaining}
  end

  # Regular token
  defp scan_token(chars, col) do
    {token_chars, rest} = scan_regular_token(chars, [])
    token = List.to_string(token_chars)
    {:token, token, col, col + String.length(token), rest}
  end

  defp scan_string([], acc), do: {Enum.reverse(acc), []}
  defp scan_string([?\\ , c | rest], acc), do: scan_string(rest, [c, ?\\ | acc])
  defp scan_string([?" | rest], acc), do: {Enum.reverse([?" | acc]), rest}
  defp scan_string([c | rest], acc), do: scan_string(rest, [c | acc])

  defp scan_regular_token([], acc), do: {Enum.reverse(acc), []}
  defp scan_regular_token([c | rest], acc) when c in @delimiters, do: {Enum.reverse(acc), [c | rest]}
  defp scan_regular_token([c | rest], acc), do: scan_regular_token(rest, [c | acc])

  # ============================================================================
  # Type Lookup
  # ============================================================================

  defp lookup_type(source, token, span, file) do
    # First check if it's a literal
    case literal_type(token) do
      {:ok, type} ->
        {:ok, type, :literal, []}

      :not_literal ->
        # Parse and type check, then look up in environment
        lookup_in_checked_source(source, token, span, file)
    end
  end

  defp is_literal?(token) do
    cond do
      Regex.match?(~r/^-?\d+$/, token) -> true
      Regex.match?(~r/^-?\d+\.\d+$/, token) -> true
      String.starts_with?(token, "\"") -> true
      String.starts_with?(token, ":") -> true
      token in ["true", "false"] -> true
      true -> false
    end
  end

  defp literal_type(token) do
    cond do
      # Integer
      Regex.match?(~r/^-?\d+$/, token) ->
        {:ok, :int}

      # Float
      Regex.match?(~r/^-?\d+\.\d+$/, token) ->
        {:ok, :float}

      # String
      String.starts_with?(token, "\"") and String.ends_with?(token, "\"") ->
        {:ok, :string}

      # Atom/keyword
      String.starts_with?(token, ":") ->
        case token |> String.trim_leading(":") |> safe_to_atom() do
          nil -> {:ok, :Atom}
          atom -> {:ok, {:atom, atom}}
        end

      # Boolean
      token in ["true", "false"] ->
        {:ok, :bool}

      true ->
        :not_literal
    end
  end

  # Find the definition location for a symbol in the source
  defp find_definition_loc(source, token, file) do
    try do
      ast = Parser.parse(source, file: file)
      case safe_to_atom(token) do
        nil -> :not_found
        token_atom ->
          ASTAnalyzer.find_definition_at(ast, token_atom)
      end
    rescue
      _ -> :not_found
    end
  end

  # Definition finding is now handled by ASTAnalyzer

  defp lookup_in_checked_source(source, token, span, file) do
    source_map = Parser.source_map(source)
    range = Map.fetch!(source_map.spans, {span.line, span.col}) |> Map.put(:id, 0)
    case infer_ranges(source, [range], file) do
      {:ok, %{0 => %{type: type, kind: kind}}, _core} -> {:ok, type, kind, []}
      _ -> lookup_declaration(source, token, file)
    end
  rescue
    _ -> :not_found
  end

  @doc false
  def infer_ranges(source, ranges, file \\ nil) do
    size = String.length(source)
    valid = Enum.all?(ranges, fn r -> is_integer(r.id) and is_integer(r.offset) and
      is_integer(r.length) and r.offset >= 0 and r.length > 0 and r.offset + r.length <= size end)
    if not valid or String.contains?(source, Atom.to_string(@probe)) do
      {:error, :invalid_type_query}
    else
      openings = Enum.group_by(ranges, & &1.offset)
      closings = Enum.group_by(ranges, &(&1.offset + &1.length))
      transformed = (String.graphemes(source) ++ [""])
        |> Enum.with_index()
        |> Enum.map_join(fn {char, offset} ->
          closing = Map.get(closings, offset, []) |> Enum.sort_by(&{&1.offset, &1.id}, :desc)
            |> Enum.map_join(fn _ -> ")" end)
          opening = Map.get(openings, offset, []) |> Enum.sort_by(&{-(&1.offset + &1.length), &1.id})
            |> Enum.map_join(fn r -> "(#{@probe} #{r.id} " end)
          closing <> opening <> char
        end)
      with {:ok, core} <- Compilation.check_liquid(transformed, :EditorQuery,
             Keyword.put(native_opts(file), :type_probe, true)) do
        {:ok, query_types(core, %{}), core}
      end
    end
  rescue
    _ -> {:error, :invalid_type_query}
  end

  defp query_types([:meta, [:"type-at", id, type, kind]], types), do: Map.put(types, id, %{type: type, kind: kind})
  defp query_types(items, types) when is_list(items), do: Enum.reduce(items, types, &query_types/2)
  defp query_types(_, types), do: types

  defp native_opts(file) do
    path = if is_binary(file), do: (URI.parse(file).path || file), else: "."
    [file: file, format_errors: false, search_paths: Enum.uniq([Path.dirname(URI.decode(path)), "."])]
  end

  defp lookup_declaration(source, token, file) do
    name = safe_to_atom(token)
    case Compilation.check_liquid(source, :EditorDeclarations, native_opts(file)) do
      {:ok, core} -> declaration_info(build_env(core), name)
      _ ->
        # A declared datatype is still useful when an incomplete expression
        # leaves a phantom parameter undetermined. This is declaration lookup,
        # not acceptance of the incomplete program.
        with {:ok, ast} <- Compilation.parse(source, file: file) do
          declarations = Enum.flat_map(List.wrap(ast), fn
            {:deftype, _, _, _} = form -> [form]
            {:deftype_deriving, n, body, _, loc} -> [{:deftype, n, body, loc}]
            {:import, _, _, _} = form -> [form]
            _ -> []
          end)
          with {:ok, core} <- Elab.module(declarations, Keyword.take(native_opts(file), [:search_paths])),
               :ok <- Vaisto.Liquid.Lint.check_module(core) do
            declaration_info(build_env(core), name)
          else
            _ -> builtin_info(name)
          end
        else
          _ -> builtin_info(name)
        end
    end
  end

  defp declaration_info(env, name) do
    case Map.fetch(env, name) do
      {:ok, info} -> {:ok, info.type, info.kind, Map.get(info, :constraints, [])}
      :error -> builtin_info(name)
    end
  end

  defp builtin_info(name) do
    case Map.fetch(@builtins, name) do
      {:ok, text} -> {:ok, text, :builtin, []}
      :error -> :not_found
    end
  end

  # ============================================================================
  # Environment Building
  # ============================================================================

  # Module wrapper from type checker: {:module, [definitions]}
  defp extract_definition({:module, defs}, env) do
    Enum.reduce(defs, env, &extract_definition/2)
  end

  # Typed AST function definition: {:defn, name, param_names, body, func_type}
  # Where func_type is {:fn, param_types, ret_type}
  defp extract_definition({:defn, name, _params, _body, {:fn, _, _} = func_type}, env) do
    Map.put(env, name, %{type: func_type, loc: nil, kind: :function})
  end

  # Function definition with location (from tests): {:defn, name, params, body, ret_type, loc}
  defp extract_definition({:defn, name, params, _body, ret_type, %Parser.Loc{} = loc}, env) do
    param_types = Enum.map(params, fn
      {_, type} -> type
      _ -> :any
    end)
    func_type = {:fn, param_types, ret_type}
    Map.put(env, name, %{type: func_type, loc: loc, kind: :function})
  end

  # Multi-clause function: {:defn_multi, name, clauses, func_type} or with loc
  defp extract_definition({:defn_multi, name, _clauses, {:fn, _, _} = func_type}, env) do
    Map.put(env, name, %{type: func_type, loc: nil, kind: :function})
  end

  defp extract_definition({:defn_multi, name, clauses, %Parser.Loc{} = loc}, env) when is_list(clauses) do
    # Infer arity from first clause
    {first_pattern, _} = hd(clauses)
    arity = if is_list(first_pattern), do: length(first_pattern), else: 1
    param_types = List.duplicate(:any, arity)
    func_type = {:fn, param_types, :any}
    Map.put(env, name, %{type: func_type, loc: loc, kind: :function})
  end

  # Value definition with location: {:defval, name, typed_expr, loc}
  # (must come before the catch-all type clause)
  defp extract_definition({:defval, name, typed_expr, %Parser.Loc{} = loc}, env) do
    type = extract_expr_type(typed_expr)
    Map.put(env, name, %{type: type, loc: loc, kind: :variable})
  end

  # Typed AST value definition: {:defval, name, typed_expr, type}
  defp extract_definition({:defval, name, _typed_expr, type}, env) when is_atom(type) or is_tuple(type) do
    Map.put(env, name, %{type: type, loc: nil, kind: :variable})
  end

  # Typed AST process: {:process, name, init, handlers, process_type}
  defp extract_definition({:process, name, _init, _handlers, {:process, state_type, messages}}, env) do
    Map.put(env, name, %{type: {:process, state_type, messages}, loc: nil, kind: :process})
  end

  # Process definition with location (from tests): {:process, name, init, handlers, loc}
  defp extract_definition({:process, name, _init, handlers, %Parser.Loc{} = loc}, env) do
    messages = Enum.map(handlers, fn {msg, _} -> msg end)
    process_type = {:process, :any, messages}
    Map.put(env, name, %{type: process_type, loc: loc, kind: :process})
  end

  # Sum type with location: {:deftype, name, {:sum, variants}, loc}
  # (must come before catch-all)
  defp extract_definition({:deftype, name, {:sum, variants}, %Parser.Loc{} = loc}, env) do
    sum_type = {:sum, name, variants}
    env = Map.put(env, name, %{type: sum_type, loc: loc, kind: :type})

    # Also add constructors
    Enum.reduce(variants, env, fn {ctor_name, args}, acc ->
      ctor_type = {:fn, args, name}
      Map.put(acc, ctor_name, %{type: ctor_type, loc: loc, kind: :constructor})
    end)
  end

  # Typed AST sum type: {:deftype, name, {:sum, variants}, type}
  defp extract_definition({:deftype, name, {:sum, variants}, _type}, env) do
    sum_type = {:sum, name, variants}
    env = Map.put(env, name, %{type: sum_type, loc: nil, kind: :type})

    # Also add constructors
    Enum.reduce(variants, env, fn {ctor_name, args}, acc ->
      ctor_type = {:fn, args, name}
      Map.put(acc, ctor_name, %{type: ctor_type, loc: nil, kind: :constructor})
    end)
  end

  # Product type with location: {:deftype, name, {:product, fields}, loc}
  # (must come before catch-all)
  defp extract_definition({:deftype, name, {:product, fields}, %Parser.Loc{} = loc}, env) do
    record_type = {:record, name, fields}
    Map.put(env, name, %{type: record_type, loc: loc, kind: :type})
  end

  # Typed AST product type: {:deftype, name, {:product, fields}, type}
  defp extract_definition({:deftype, name, {:product, fields}, _type}, env) do
    record_type = {:record, name, fields}
    Map.put(env, name, %{type: record_type, loc: nil, kind: :type})
  end

  # Extern declaration: {:extern, mod, func, arg_types, ret_type, loc} or {:extern, mod, func, arg_types, ret_type}
  defp extract_definition({:extern, _mod, func, arg_types, ret_type, %Parser.Loc{} = loc}, env) do
    func_type = {:fn, arg_types, ret_type}
    Map.put(env, func, %{type: func_type, loc: loc, kind: :extern})
  end

  defp extract_definition({:extern, _mod, func, arg_types, ret_type}, env) do
    func_type = {:fn, arg_types, ret_type}
    Map.put(env, func, %{type: func_type, loc: nil, kind: :extern})
  end

  # List of definitions - reduce over them
  defp extract_definition(defs, env) when is_list(defs) do
    Enum.reduce(defs, env, &extract_definition/2)
  end

  # Catch-all for unhandled nodes
  defp extract_definition(_other, env), do: env

  # Extract type from typed expression
  defp extract_expr_type({:lit, type, _value}), do: type
  defp extract_expr_type({:lit, type, _value, _loc}), do: type
  defp extract_expr_type({:var, _name, type}), do: type
  defp extract_expr_type({:var, _name, type, _loc}), do: type
  defp extract_expr_type({:call, _func, _args, type}), do: type
  defp extract_expr_type({:call, _func, _args, type, _loc}), do: type
  defp extract_expr_type({:list, _elems, type}), do: type
  defp extract_expr_type({:if, _c, _t, _e, type}), do: type
  defp extract_expr_type({:let, _bindings, _body, type}), do: type
  defp extract_expr_type({:fn, _params, _body, type}), do: type
  defp extract_expr_type({:do, _exprs, type}), do: type
  defp extract_expr_type(_), do: :any
end
