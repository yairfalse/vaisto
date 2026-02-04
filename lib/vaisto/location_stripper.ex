defmodule Vaisto.LocationStripper do
  @moduledoc """
  Handles location metadata preprocessing for AST nodes.

  The Vaisto parser attaches location metadata (`%Vaisto.Parser.Loc{}`) as the
  final element of most AST tuples. This module provides utilities to:

  1. Strip locations from AST nodes before type checking
  2. Add location information to errors for proper diagnostics

  This approach eliminates the boilerplate of handling locations in every
  type checker clause.

  ## Usage in TypeChecker

  Instead of having separate clauses for each AST node with/without location:

      # Before (boilerplate)
      def check({:call, func, args, %Loc{} = loc}, env) do
        with_loc(check({:call, func, args}, env), loc)
      end

      # After (using LocationStripper)
      defp do_check({:call, func, args}, env, loc) do
        # actual checking logic, loc available for errors
      end
  """

  alias Vaisto.Parser.Loc
  alias Vaisto.Error

  @doc """
  Strip location metadata from an AST node.

  Returns `{stripped_node, loc}` where:
  - `stripped_node` is the AST without the trailing Loc
  - `loc` is the extracted location or nil
  """
  @spec strip_node(term()) :: {term(), Loc.t() | nil}

  # Tuple with Loc as final element - strip it
  def strip_node(node) when is_tuple(node) do
    list = Tuple.to_list(node)
    case List.last(list) do
      %Loc{} = loc ->
        stripped = list |> Enum.take(length(list) - 1) |> List.to_tuple()
        {stripped, loc}

      _ ->
        {node, nil}
    end
  end

  # Non-tuples don't have locations
  def strip_node(node), do: {node, nil}

  @doc """
  Strip location from AST node and recursively from children.

  Returns `{stripped_ast, loc_map}` where loc_map is a map that can be used
  to attach location information to errors.
  """
  @spec strip(term()) :: {term(), Loc.t() | nil}
  def strip(ast) when is_tuple(ast) do
    {stripped, loc} = strip_node(ast)
    {stripped, loc}
  end

  def strip(ast) when is_list(ast) do
    # For list of top-level forms, strip each one
    stripped = Enum.map(ast, fn node ->
      {stripped_node, _loc} = strip(node)
      stripped_node
    end)
    {stripped, nil}
  end

  def strip(ast), do: {ast, nil}

  @doc """
  Add location information to an error if not already present.
  """
  @spec add_loc_to_error({:error, term()}, Loc.t() | nil) :: {:error, term()}
  def add_loc_to_error({:error, %Error{primary_span: nil} = error}, %Loc{} = loc) do
    span = Error.span_from_loc(loc)
    {:error, %{error | file: loc.file, primary_span: span}}
  end

  def add_loc_to_error({:error, %Error{} = error}, %Loc{} = loc) do
    # Error already has span, just add file if missing
    {:error, %{error | file: error.file || loc.file}}
  end

  def add_loc_to_error({:error, msg}, %Loc{} = loc) when is_binary(msg) do
    span = Error.span_from_loc(loc)
    {:error, %Error{message: msg, file: loc.file, primary_span: span}}
  end

  def add_loc_to_error(result, _loc), do: result

  @doc """
  Check if a tuple has a Loc as its last element.
  """
  @spec has_loc?(term()) :: boolean()
  def has_loc?(node) when is_tuple(node) do
    list = Tuple.to_list(node)
    match?(%Loc{}, List.last(list))
  end
  def has_loc?(_), do: false

  @doc """
  Get the location from an AST node if present.
  """
  @spec get_loc(term()) :: Loc.t() | nil
  def get_loc(node) when is_tuple(node) do
    list = Tuple.to_list(node)
    case List.last(list) do
      %Loc{} = loc -> loc
      _ -> nil
    end
  end
  def get_loc(_), do: nil
end
