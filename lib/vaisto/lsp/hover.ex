defmodule Vaisto.LSP.Hover do
  @moduledoc """
  Hover information provider for Vaisto LSP.

  Handles textDocument/hover requests by finding the token at cursor
  position and looking up its type information.
  """

  alias Vaisto.Parser
  alias Vaisto.TypeChecker
  alias Vaisto.Error

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
         {:ok, type, kind} <- lookup_type(source, token, span, file) do
      contents = format_hover(token, type, kind: kind)
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
    type_str = Error.format_type(type)
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

  defp lookup_type(source, token, _span, file) do
    # First check if it's a literal
    case literal_type(token) do
      {:ok, type} ->
        {:ok, type, :literal}

      :not_literal ->
        # Parse and type check, then look up in environment
        lookup_in_checked_source(source, token, file)
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
        atom = token |> String.trim_leading(":") |> String.to_atom()
        {:ok, {:atom, atom}}

      # Boolean
      token in ["true", "false"] ->
        {:ok, :bool}

      true ->
        :not_literal
    end
  end

  # Find the definition location for a symbol in the source
  defp find_definition_loc(source, token, file) do
    token_atom = String.to_atom(token)

    try do
      ast = Parser.parse(source, file: file)
      forms = if is_list(ast), do: ast, else: [ast]

      # First, search top-level definitions
      case find_top_level_def(forms, token_atom) do
        {:ok, _} = found -> found
        :not_found ->
          # Search for local definitions (let bindings, params)
          find_local_def(forms, token_atom)
      end
    rescue
      _ -> :not_found
    end
  end

  # Search top-level forms for a definition
  defp find_top_level_def([], _name), do: :not_found

  defp find_top_level_def([{:defn, name, _params, _body, _ret, loc} | _rest], name) do
    {:ok, definition_loc(:defn, loc)}
  end

  defp find_top_level_def([{:defn, name, _params, _body, loc} | _rest], name) when is_struct(loc, Parser.Loc) do
    {:ok, definition_loc(:defn, loc)}
  end

  defp find_top_level_def([{:defn_multi, name, _clauses, loc} | _rest], name) do
    {:ok, definition_loc(:defn, loc)}
  end

  defp find_top_level_def([{:defval, name, _expr, loc} | _rest], name) do
    {:ok, definition_loc(:defval, loc)}
  end

  defp find_top_level_def([{:process, name, _init, _handlers, loc} | _rest], name) do
    {:ok, definition_loc(:process, loc)}
  end

  defp find_top_level_def([{:deftype, type_name, {:sum, variants}, loc} | _rest], name) do
    # Check if it's the type name
    if type_name == name do
      {:ok, definition_loc(:deftype, loc)}
    else
      # Check if it's a constructor name
      case Enum.find(variants, fn {ctor, _} -> ctor == name end) do
        {^name, _} -> {:ok, definition_loc(:deftype, loc)}
        nil -> :not_found
      end
    end
  end

  defp find_top_level_def([{:deftype, name, _def, loc} | _rest], name) do
    {:ok, definition_loc(:deftype, loc)}
  end

  defp find_top_level_def([{:extern, _mod, func, _args, _ret, loc} | _rest], func) do
    {:ok, definition_loc(:extern, loc)}
  end

  defp find_top_level_def([_ | rest], name), do: find_top_level_def(rest, name)

  # Calculate the actual location of the name within the definition
  # Offset depends on the keyword: "(defn " = 6, "(deftype " = 9, etc.
  @keyword_offsets %{
    defn: 6,      # "(defn "
    defval: 6,    # "(def " - defval is parsed from (def name value)
    deftype: 9,   # "(deftype "
    process: 9,   # "(process "
    extern: 8     # "(extern " - but extern has module:func format
  }

  defp definition_loc(keyword, %Parser.Loc{} = loc) do
    offset = Map.get(@keyword_offsets, keyword, 6)
    %{line: loc.line, col: loc.col + offset}
  end

  # Search for local definitions (let bindings, function params)
  defp find_local_def(forms, name) do
    Enum.find_value(forms, :not_found, fn form ->
      case search_local_in_form(form, name) do
        {:ok, _} = found -> found
        :not_found -> nil
      end
    end)
  end

  # Search within a form for local variable definitions
  defp search_local_in_form({:defn, _fname, params, body, _ret, loc}, name) do
    # Check params first
    case find_in_params(params, name, loc) do
      {:ok, _} = found -> found
      :not_found -> search_local_in_expr(body, name)
    end
  end

  defp search_local_in_form({:defn, _fname, params, body, loc}, name) when is_struct(loc, Parser.Loc) do
    case find_in_params(params, name, loc) do
      {:ok, _} = found -> found
      :not_found -> search_local_in_expr(body, name)
    end
  end

  defp search_local_in_form({:let, bindings, body, loc}, name) do
    case find_in_let_bindings(bindings, name, loc) do
      {:ok, _} = found -> found
      :not_found -> search_local_in_expr(body, name)
    end
  end

  defp search_local_in_form({:fn, params, body, _loc}, name) do
    # Anonymous functions - params are just atoms
    case Enum.find_index(params, &(&1 == name)) do
      nil -> search_local_in_expr(body, name)
      _idx -> :not_found  # TODO: track param locations in fn
    end
  end

  defp search_local_in_form({:call, _func, args, _loc}, name) do
    Enum.find_value(args, :not_found, fn arg ->
      case search_local_in_expr(arg, name) do
        {:ok, _} = found -> found
        :not_found -> nil
      end
    end)
  end

  defp search_local_in_form({:if, cond, then_b, else_b, _loc}, name) do
    search_local_in_expr(cond, name) ||
    search_local_in_expr(then_b, name) ||
    search_local_in_expr(else_b, name) ||
    :not_found
  end

  defp search_local_in_form({:do, exprs, _loc}, name) do
    Enum.find_value(exprs, :not_found, fn expr ->
      case search_local_in_expr(expr, name) do
        {:ok, _} = found -> found
        :not_found -> nil
      end
    end)
  end

  defp search_local_in_form(_, _name), do: :not_found

  defp search_local_in_expr(expr, name), do: search_local_in_form(expr, name)

  # Find a parameter in the params list
  # Params are [{name, type}, ...] with types
  # NOTE: Column calculation is approximate - assumes standard formatting.
  # A more robust solution would store exact positions during parsing.
  defp find_in_params(params, name, defn_loc) do
    case Enum.find_index(params, fn
      {pname, _type} -> pname == name
      pname when is_atom(pname) -> pname == name
      _ -> false
    end) do
      nil -> :not_found
      idx ->
        # Approximate: "(defn name [" is ~14 chars, each "x :type " is ~2 per param
        {:ok, %{line: defn_loc.line, col: defn_loc.col + 14 + idx * 2}}
    end
  end

  # Find a variable in let bindings
  # Bindings from parser are [{name, value}, ...] or keyword list
  # NOTE: Column calculation is approximate - assumes standard formatting.
  # A more robust solution would store exact positions during parsing.
  defp find_in_let_bindings(bindings, name, let_loc) do
    case Enum.find_index(bindings, fn
      {bname, _value} -> bname == name
      _ -> false
    end) do
      nil -> :not_found
      idx ->
        # Approximate: "(let [" is 6 chars, each binding "x val " is ~4 chars
        {:ok, %{line: let_loc.line, col: let_loc.col + 6 + idx * 4}}
    end
  end

  defp lookup_in_checked_source(source, token, file) do
    try do
      ast = Parser.parse(source, file: file)
      case TypeChecker.check(ast) do
        {:ok, _type, typed_ast} ->
          token_atom = String.to_atom(token)

          # First, search the typed AST for variable references with this name
          # This finds local variables in let bindings, function params, etc.
          case find_var_type(typed_ast, token_atom) do
            {:ok, type} ->
              {:ok, type, :variable}

            :not_found ->
              # Check top-level definitions
              env = build_env(typed_ast)
              case Map.get(env, token_atom) do
                %{type: type, kind: kind} ->
                  {:ok, type, kind}

                nil ->
                  # Check primitives
                  case Map.get(TypeChecker.primitives(), token_atom) do
                    nil -> :not_found
                    type -> {:ok, type, :builtin}
                  end
              end
          end

        {:error, _} ->
          # Type check failed, try primitives only
          token_atom = String.to_atom(token)
          case Map.get(TypeChecker.primitives(), token_atom) do
            nil -> :not_found
            type -> {:ok, type, :builtin}
          end
      end
    rescue
      _ -> :not_found
    end
  end

  # Find a variable reference in the typed AST and return its type
  defp find_var_type({:var, name, type}, name), do: {:ok, type}
  defp find_var_type({:var, name, type, _loc}, name), do: {:ok, type}

  # Let bindings: search both bindings and body
  defp find_var_type({:let, bindings, body, _type}, name) do
    # Check bindings first
    case find_in_bindings(bindings, name) do
      {:ok, _} = found -> found
      :not_found -> find_var_type(body, name)
    end
  end

  # Function call: search function and args
  defp find_var_type({:call, func, args, _type}, name) do
    case find_var_type(func, name) do
      {:ok, _} = found -> found
      :not_found -> find_in_list(args, name)
    end
  end
  defp find_var_type({:call, func, args, _type, _loc}, name) do
    case find_var_type(func, name) do
      {:ok, _} = found -> found
      :not_found -> find_in_list(args, name)
    end
  end

  # If expression: search condition, then, else
  defp find_var_type({:if, cond, then_b, else_b, _type}, name) do
    find_in_list([cond, then_b, else_b], name)
  end

  # Do block: search all expressions
  defp find_var_type({:do, exprs, _type}, name) do
    find_in_list(exprs, name)
  end

  # Function/lambda: search body
  defp find_var_type({:fn, _params, body, _type}, name) do
    find_var_type(body, name)
  end

  # Module: search all definitions
  defp find_var_type({:module, defs}, name) do
    find_in_list(defs, name)
  end

  # defn: search body
  defp find_var_type({:defn, _name, _params, body, _type}, search_name) do
    find_var_type(body, search_name)
  end

  # List: search all elements
  defp find_var_type(list, name) when is_list(list) do
    find_in_list(list, name)
  end

  # Literals and other nodes: not found
  defp find_var_type(_, _), do: :not_found

  defp find_in_list([], _name), do: :not_found
  defp find_in_list([head | tail], name) do
    case find_var_type(head, name) do
      {:ok, _} = found -> found
      :not_found -> find_in_list(tail, name)
    end
  end

  # Find in let bindings: [{name, typed_expr, type}, ...]
  defp find_in_bindings([], _name), do: :not_found
  defp find_in_bindings([{bind_name, _expr, type} | _rest], name) when bind_name == name do
    {:ok, type}
  end
  defp find_in_bindings([_ | rest], name), do: find_in_bindings(rest, name)

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
