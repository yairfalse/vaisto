defmodule Vaisto.Backend.Shared do
  @moduledoc """
  Shared utilities for Vaisto compilation backends.

  Contains functions that are used by multiple backends for common operations
  like pattern variable extraction, module naming, and AST classification.
  """

  # ============================================================================
  # Pattern Variable Extraction
  # ============================================================================

  @doc """
  Extract all variable names from a pattern AST node.

  Used for generating bindings in let expressions and match clauses.

  ## Examples

      iex> Backend.Shared.extract_pattern_vars({:var, :x, :int})
      [:x]

      iex> Backend.Shared.extract_pattern_vars({:tuple_pattern, [{:var, :a, :int}, {:var, :b, :int}], :tuple})
      [:a, :b]
  """
  @spec extract_pattern_vars(term()) :: [atom()]
  def extract_pattern_vars({:var, name, _type}), do: [name]

  def extract_pattern_vars({:tuple_pattern, elements, _type}) do
    Enum.flat_map(elements, &extract_pattern_vars/1)
  end

  def extract_pattern_vars({:list, elements, _type}) do
    Enum.flat_map(elements, &extract_pattern_vars/1)
  end

  def extract_pattern_vars({:list_pattern, elements, _type}) do
    Enum.flat_map(elements, &extract_pattern_vars/1)
  end

  def extract_pattern_vars({:cons, head, tail, _type}) do
    extract_pattern_vars(head) ++ extract_pattern_vars(tail)
  end

  def extract_pattern_vars({:cons_pattern, head, tail, _type}) do
    extract_pattern_vars(head) ++ extract_pattern_vars(tail)
  end

  def extract_pattern_vars({:pattern, _name, args, _type}) do
    Enum.flat_map(args, &extract_pattern_vars/1)
  end

  def extract_pattern_vars(a) when is_atom(a) and a != :_ do
    [a]
  end

  def extract_pattern_vars(_), do: []

  # ============================================================================
  # AST Classification
  # ============================================================================

  @doc """
  Check if the AST represents a module with forms.
  """
  @spec is_module?(term()) :: boolean()
  def is_module?({:module, _forms}), do: true
  def is_module?(_), do: false

  @doc """
  Check if the AST represents a process definition.
  """
  @spec is_process?(term()) :: boolean()
  def is_process?({:process, _, _, _, _}), do: true
  def is_process?(_), do: false

  @doc """
  Check if the AST represents a function definition.
  """
  @spec is_defn?(term()) :: boolean()
  def is_defn?({:defn, _, _, _, _}), do: true
  def is_defn?({:defn, _, _, _, _, _}), do: true
  def is_defn?({:defn_multi, _, _, _}), do: true
  def is_defn?({:defn_multi, _, _, _, _}), do: true
  def is_defn?(_), do: false

  @doc """
  Check if the AST represents a type definition.
  """
  @spec is_deftype?(term()) :: boolean()
  def is_deftype?({:deftype, _, _, _}), do: true
  def is_deftype?(_), do: false

  @doc """
  Check if the AST represents a value definition.
  """
  @spec is_defval?(term()) :: boolean()
  def is_defval?({:defval, _, _, _}), do: true
  def is_defval?(_), do: false

  @doc """
  Check if the AST represents a supervision tree.
  """
  @spec is_supervise?(term()) :: boolean()
  def is_supervise?({:supervise, _, _, _}), do: true
  def is_supervise?(_), do: false

  # ============================================================================
  # Module Naming
  # ============================================================================

  @doc """
  Convert an atom to CamelCase module name.

  ## Examples

      iex> Backend.Shared.camelize(:foo_bar)
      FooBar

      iex> Backend.Shared.camelize(:counter)
      Counter
  """
  @spec camelize(atom()) :: module()
  def camelize(atom) when is_atom(atom) do
    name =
      atom
      |> Atom.to_string()
      |> String.split("_")
      |> Enum.map(&String.capitalize/1)
      |> Enum.join()

    Module.concat([name])
  end

  # ============================================================================
  # Literal Detection
  # ============================================================================

  @doc """
  Check if the AST represents a literal value.
  """
  @spec is_literal?(term()) :: boolean()
  def is_literal?({:lit, _, _}), do: true
  def is_literal?(n) when is_integer(n), do: true
  def is_literal?(f) when is_float(f), do: true
  def is_literal?(b) when is_boolean(b), do: true
  def is_literal?(s) when is_binary(s), do: true
  def is_literal?(_), do: false

  @doc """
  Extract the value from a literal AST node.
  """
  @spec literal_value(term()) :: term()
  def literal_value({:lit, :int, n}), do: n
  def literal_value({:lit, :float, f}), do: f
  def literal_value({:lit, :bool, b}), do: b
  def literal_value({:lit, :atom, a}), do: a
  def literal_value({:lit, :string, s}), do: s
  def literal_value({:lit, :unit, _}), do: nil
  def literal_value(n) when is_integer(n), do: n
  def literal_value(f) when is_float(f), do: f
  def literal_value(b) when is_boolean(b), do: b
  def literal_value(s) when is_binary(s), do: s

  # ============================================================================
  # Function Arity Helpers
  # ============================================================================

  @doc """
  Get the arity of a function definition.
  """
  @spec defn_arity(term()) :: non_neg_integer()
  def defn_arity({:defn, _name, params, _body, _type}), do: length(params)
  def defn_arity({:defn, _name, params, _body, _ret, _type}), do: length(params)
  def defn_arity({:defn_multi, _name, arity, _clauses, _loc}) when is_integer(arity), do: arity
  def defn_arity({:defn_multi, _name, clauses, _loc}) when is_list(clauses) do
    case clauses do
      [{patterns, _} | _] when is_list(patterns) -> length(patterns)
      _ -> 1
    end
  end

  @doc """
  Get the name of a function definition.
  """
  @spec defn_name(term()) :: atom()
  def defn_name({:defn, name, _, _, _}), do: name
  def defn_name({:defn, name, _, _, _, _}), do: name
  def defn_name({:defn_multi, name, _, _}), do: name
  def defn_name({:defn_multi, name, _, _, _}), do: name
end
