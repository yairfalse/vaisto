defmodule Vaisto.LSP.ASTAnalyzer do
  @moduledoc """
  Centralized AST analysis utilities for LSP features.

  This module provides a single source of truth for:
  - Extracting definitions from AST (functions, types, processes, etc.)
  - Finding definition locations for symbols
  - Searching for variable references
  - Generic AST walking

  Used by Hover, Completion, SignatureHelp, and References modules.
  """

  alias Vaisto.Parser.Loc

  @type definition :: %{
    name: atom(),
    kind: :function | :type | :constructor | :variable | :process | :extern,
    arity: non_neg_integer() | nil,
    loc: Loc.t() | nil,
    type: term() | nil
  }

  # ============================================================================
  # Definition Extraction
  # ============================================================================

  @doc """
  Extract all top-level definitions from parsed AST.

  Returns a list of definition maps with name, kind, arity, location, and type info.
  """
  @spec extract_definitions(term()) :: [definition()]
  def extract_definitions(ast) when is_list(ast) do
    Enum.flat_map(ast, &extract_definition/1)
  end

  def extract_definitions(ast) do
    extract_definition(ast)
  end

  defp extract_definition({:defn, name, params, _body, _ret, loc}) do
    [%{name: name, kind: :function, arity: length(params), loc: loc, type: nil}]
  end

  defp extract_definition({:defn, name, params, _body, loc}) when is_struct(loc, Loc) do
    [%{name: name, kind: :function, arity: length(params), loc: loc, type: nil}]
  end

  defp extract_definition({:defn_multi, name, arity, _clauses, loc}) when is_integer(arity) do
    [%{name: name, kind: :function, arity: arity, loc: loc, type: nil}]
  end

  defp extract_definition({:defn_multi, name, clauses, loc}) when is_list(clauses) do
    arity = case clauses do
      [{pattern, _} | _] when is_list(pattern) -> length(pattern)
      _ -> 1
    end
    [%{name: name, kind: :function, arity: arity, loc: loc, type: nil}]
  end

  defp extract_definition({:deftype, name, {:sum, variants}, loc}) do
    type_def = %{name: name, kind: :type, arity: nil, loc: loc, type: {:sum, variants}}

    constructors = Enum.map(variants, fn {ctor_name, args} ->
      %{name: ctor_name, kind: :constructor, arity: length(args), loc: loc, type: nil}
    end)

    [type_def | constructors]
  end

  defp extract_definition({:deftype, name, {:product, fields}, loc}) do
    [%{name: name, kind: :type, arity: nil, loc: loc, type: {:record, fields}}]
  end

  defp extract_definition({:deftype, name, {:record, fields}, loc}) do
    [%{name: name, kind: :type, arity: nil, loc: loc, type: {:record, fields}}]
  end

  defp extract_definition({:defval, name, _expr, loc}) do
    [%{name: name, kind: :variable, arity: nil, loc: loc, type: nil}]
  end

  defp extract_definition({:process, name, _init, _handlers, loc}) do
    [%{name: name, kind: :process, arity: nil, loc: loc, type: nil}]
  end

  defp extract_definition({:extern, _mod, func, args, ret, loc}) do
    [%{name: func, kind: :extern, arity: length(args), loc: loc, type: {:fn, args, ret}}]
  end

  defp extract_definition(_), do: []

  # ============================================================================
  # Definition Location Finding
  # ============================================================================

  @doc """
  Find the definition location for a symbol name in the AST.

  Searches top-level definitions (defn, deftype, process, extern) and
  local bindings (let, function params).

  Returns `{:ok, %{line: n, col: n}}` or `:not_found`.
  """
  @spec find_definition_at(term(), atom()) :: {:ok, map()} | :not_found
  def find_definition_at(ast, name) when is_list(ast) do
    case find_top_level_definition(ast, name) do
      {:ok, _} = found -> found
      :not_found -> find_local_definition(ast, name)
    end
  end

  def find_definition_at(ast, name) do
    find_definition_at([ast], name)
  end

  # Search top-level definitions
  defp find_top_level_definition([], _name), do: :not_found

  defp find_top_level_definition([{:defn, name, _params, _body, _ret, loc} | _], name) do
    {:ok, definition_loc(:defn, loc)}
  end

  defp find_top_level_definition([{:defn, name, _params, _body, loc} | _], name) when is_struct(loc, Loc) do
    {:ok, definition_loc(:defn, loc)}
  end

  defp find_top_level_definition([{:defn_multi, name, _clauses, loc} | _], name) do
    {:ok, definition_loc(:defn, loc)}
  end

  defp find_top_level_definition([{:defval, name, _expr, loc} | _], name) do
    {:ok, definition_loc(:defval, loc)}
  end

  defp find_top_level_definition([{:process, name, _init, _handlers, loc} | _], name) do
    {:ok, definition_loc(:process, loc)}
  end

  defp find_top_level_definition([{:deftype, type_name, {:sum, variants}, loc} | _], name) do
    cond do
      type_name == name -> {:ok, definition_loc(:deftype, loc)}
      Enum.any?(variants, fn {ctor, _} -> ctor == name end) -> {:ok, definition_loc(:deftype, loc)}
      true -> :not_found
    end
  end

  defp find_top_level_definition([{:deftype, name, _def, loc} | _], name) do
    {:ok, definition_loc(:deftype, loc)}
  end

  defp find_top_level_definition([{:extern, _mod, func, _args, _ret, loc} | _], func) do
    {:ok, definition_loc(:extern, loc)}
  end

  defp find_top_level_definition([_ | rest], name) do
    find_top_level_definition(rest, name)
  end

  # Keyword offsets for calculating name position within definition
  @keyword_offsets %{
    defn: 6,      # "(defn "
    defval: 6,    # "(def " - defval is parsed from (def name value)
    deftype: 9,   # "(deftype "
    process: 9,   # "(process "
    extern: 8     # "(extern " - but extern has module:func format
  }

  defp definition_loc(keyword, %Loc{} = loc) do
    offset = Map.get(@keyword_offsets, keyword, 6)
    %{line: loc.line, col: loc.col + offset}
  end

  # Search for local definitions
  defp find_local_definition(forms, name) do
    Enum.find_value(forms, :not_found, fn form ->
      case search_local(form, name) do
        {:ok, _} = found -> found
        :not_found -> nil
      end
    end)
  end

  defp search_local({:defn, _fname, params, body, _ret, loc}, name) do
    case find_in_params(params, name, loc) do
      {:ok, _} = found -> found
      :not_found -> search_local(body, name)
    end
  end

  defp search_local({:defn, _fname, params, body, loc}, name) when is_struct(loc, Loc) do
    case find_in_params(params, name, loc) do
      {:ok, _} = found -> found
      :not_found -> search_local(body, name)
    end
  end

  defp search_local({:let, bindings, body, loc}, name) do
    case find_in_let_bindings(bindings, name, loc) do
      {:ok, _} = found -> found
      :not_found -> search_local(body, name)
    end
  end

  defp search_local({:fn, params, body, _loc}, name) do
    case Enum.find_index(params, &(&1 == name)) do
      nil -> search_local(body, name)
      _idx -> :not_found  # fn params don't have precise locations
    end
  end

  defp search_local({:call, _func, args, _loc}, name) do
    search_in_list(args, name)
  end

  defp search_local({:if, cond, then_b, else_b, _loc}, name) do
    search_in_list([cond, then_b, else_b], name)
  end

  defp search_local({:do, exprs, _loc}, name) do
    search_in_list(exprs, name)
  end

  defp search_local(_, _name), do: :not_found

  defp search_in_list([], _name), do: :not_found
  defp search_in_list([head | tail], name) do
    case search_local(head, name) do
      {:ok, _} = found -> found
      :not_found -> search_in_list(tail, name)
    end
  end

  defp find_in_params(params, name, defn_loc) do
    case Enum.find_index(params, fn
      {pname, _type} -> pname == name
      pname when is_atom(pname) -> pname == name
      _ -> false
    end) do
      nil -> :not_found
      idx -> {:ok, %{line: defn_loc.line, col: defn_loc.col + 14 + idx * 2}}
    end
  end

  defp find_in_let_bindings(bindings, name, let_loc) do
    case Enum.find(bindings, fn
      {bname, _value} -> bname == name
      _ -> false
    end) do
      nil -> :not_found
      _ -> {:ok, %{line: let_loc.line, col: let_loc.col}}
    end
  end

  # ============================================================================
  # Generic AST Walking
  # ============================================================================

  @doc """
  Walk an AST tree, calling a function on each node.

  The function receives `(node, acc)` and should return `{:cont, new_acc}` to continue,
  or `{:halt, result}` to stop with a result.
  """
  @spec walk(term(), term(), (term(), term() -> {:cont, term()} | {:halt, term()})) :: term()
  def walk(ast, acc, fun) do
    case fun.(ast, acc) do
      {:halt, result} -> result
      {:cont, new_acc} -> walk_children(ast, new_acc, fun)
    end
  end

  defp walk_children(ast, acc, fun) when is_list(ast) do
    Enum.reduce(ast, acc, fn node, acc ->
      walk(node, acc, fun)
    end)
  end

  defp walk_children(ast, acc, fun) when is_tuple(ast) do
    ast
    |> Tuple.to_list()
    |> Enum.reduce(acc, fn
      elem, acc when is_tuple(elem) -> walk(elem, acc, fun)
      elem, acc when is_list(elem) -> walk_children(elem, acc, fun)
      _elem, acc -> acc
    end)
  end

  defp walk_children(_ast, acc, _fun), do: acc

  # ============================================================================
  # Type Extraction from Typed AST
  # ============================================================================

  @doc """
  Find the type of a variable in a typed AST.

  Searches through the typed AST for variable references with the given name
  and returns their inferred type.
  """
  @spec find_var_type(term(), atom()) :: {:ok, term()} | :not_found
  def find_var_type(ast, name) do
    walk(ast, :not_found, fn
      {:var, ^name, type}, _acc -> {:halt, {:ok, type}}
      {:var, ^name, type, _loc}, _acc -> {:halt, {:ok, type}}
      _node, acc -> {:cont, acc}
    end)
  end

  @doc """
  Find all variable usages with a given name in an AST.

  Returns a list of locations where the variable is referenced.
  """
  @spec find_var_usages(term(), atom()) :: [Loc.t()]
  def find_var_usages(ast, name) do
    walk(ast, [], fn
      {:var, ^name, _type, loc}, acc when is_struct(loc, Loc) -> {:cont, [loc | acc]}
      {:symbol, ^name, loc}, acc when is_struct(loc, Loc) -> {:cont, [loc | acc]}
      _node, acc -> {:cont, acc}
    end)
    |> Enum.reverse()
  end
end
