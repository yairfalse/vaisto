defmodule Vaisto.TypeFormatter do
  @moduledoc """
  Unified type formatting for display in error messages, hover info, and diagnostics.

  Provides a single source of truth for converting internal type representations
  to human-readable strings.

  ## Type Representations

  The Vaisto type system uses the following representations:

  - Primitives: `:int`, `:float`, `:string`, `:bool`, `:atom`, `:any`, `:unit`
  - Functions: `{:fn, [arg_types], ret_type}`
  - Lists: `{:list, elem_type}`
  - Records: `{:record, name, [{field, type}]}`
  - Sum types: `{:sum, name, [{ctor, [field_types]}]}`
  - Type variables: `{:tvar, id}`
  - Row types: `{:row, [{field, type}], tail}`
  - Typed PIDs: `{:pid, process_name, accepted_msgs}`
  - Processes: `{:process, state_type, msg_types}`
  - Atoms: `{:atom, atom_value}`
  """

  @doc """
  Format a type for human-readable display.

  ## Examples

      iex> Vaisto.TypeFormatter.format(:int)
      "Int"

      iex> Vaisto.TypeFormatter.format({:fn, [:int, :int], :int})
      "(Int, Int) -> Int"

      iex> Vaisto.TypeFormatter.format({:list, :string})
      "List(String)"
  """
  @spec format(term()) :: String.t()

  # Primitives
  def format(:int), do: "Int"
  def format(:float), do: "Float"
  def format(:bool), do: "Bool"
  def format(:string), do: "String"
  def format(:atom), do: "Atom"
  def format(:any), do: "Any"
  def format(:unit), do: "()"
  def format(:ok), do: "Ok"
  def format(:num), do: "Num"

  # Atom literals - use Atom(:value) format for clarity in diagnostics
  def format({:atom, a}), do: "Atom(:#{a})"

  # Function types
  def format({:fn, args, ret}) when is_list(args) do
    arg_str = args |> Enum.map(&format/1) |> Enum.join(", ")
    "(#{arg_str}) -> #{format(ret)}"
  end

  # List types
  def format({:list, elem}), do: "List(#{format(elem)})"

  # Tuple types
  def format({:tuple, elems}) when is_list(elems) do
    elem_str = elems |> Enum.map(&format/1) |> Enum.join(", ")
    "(#{elem_str})"
  end

  # Record/product types
  def format({:record, name, _fields}), do: "#{name}"

  # Sum types
  def format({:sum, name, _variants}), do: "#{name}"

  # Typed PIDs
  def format({:pid, name, _msgs}), do: "Pid(#{name})"

  # Process types
  def format({:process, state, _msgs}), do: "Process(#{format(state)})"

  # Type variables (for polymorphic types)
  def format({:tvar, id}) when is_integer(id), do: "t#{id}"
  def format({:tvar, id}), do: "#{id}"

  # Row variables
  def format({:rvar, id}), do: "r#{id}"

  # Row types (for row polymorphism)
  def format({:row, fields, :closed}) when is_list(fields) do
    field_str = fields
    |> Enum.map(fn {name, type} -> "#{name}: #{format(type)}" end)
    |> Enum.join(", ")
    "{#{field_str}}"
  end

  def format({:row, fields, tail}) when is_list(fields) do
    field_str = fields
    |> Enum.map(fn {name, type} -> "#{name}: #{format(type)}" end)
    |> Enum.join(", ")
    "{#{field_str} | #{format(tail)}}"
  end

  # Generic fallback for atoms (type names)
  def format(other) when is_atom(other), do: "#{other}"

  # Catch-all for unknown structures
  def format(other), do: inspect(other)

  @doc """
  Format a list of types separated by commas.

  ## Examples

      iex> Vaisto.TypeFormatter.format_list([:int, :string, :bool])
      "Int, String, Bool"
  """
  @spec format_list([term()]) :: String.t()
  def format_list(types) when is_list(types) do
    types |> Enum.map(&format/1) |> Enum.join(", ")
  end

  @doc """
  Format a type with a colon prefix (for type annotations).

  ## Examples

      iex> Vaisto.TypeFormatter.format_annotation(:int)
      ":int"
  """
  @spec format_annotation(term()) :: String.t()
  def format_annotation(type) when is_atom(type), do: ":#{type}"
  def format_annotation(type), do: format(type)
end
