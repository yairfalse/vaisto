defmodule Vaisto.TypeSystem.Core do
  @moduledoc """
  Core type system primitives for Hindley-Milner type inference.

  Type Variables: Placeholders like {:tvar, 0}, {:tvar, 1} representing unknown types.
  Row Variables: {:rvar, id} representing "and possibly more fields" in records.
  Substitutions: A map tracking what type variables resolve to.

  ## Row Polymorphism

  Row types allow functions to accept records with *at least* certain fields:

      ; This function works with any record that has a :name field
      (defn get-name [r] (. r :name))

  Internally, the record type is represented as:
      {:row, [{:name, :string}], {:rvar, 0}}

  The row variable {:rvar, 0} represents "and maybe more fields".
  When unified with a concrete record, it captures the extra fields.
  """

  @doc "Creates a type variable with the given id"
  def tvar(id), do: {:tvar, id}

  @doc "Creates a row variable with the given id"
  def rvar(id), do: {:rvar, id}

  @doc "Returns an empty substitution map"
  def empty_subst, do: %{}

  @doc """
  Applies substitutions to a type recursively.

  If we know {:tvar, 0} => :int, then apply_subst will replace
  all occurrences of {:tvar, 0} with :int.
  """
  def apply_subst(subst, {:tvar, id} = tv) do
    case Map.fetch(subst, id) do
      {:ok, ^tv} -> tv  # Self-reference, don't recurse
      {:ok, type} -> apply_subst(subst, type)
      :error -> tv
    end
  end

  def apply_subst(subst, {:fn, args, ret}) do
    {:fn, Enum.map(args, &apply_subst(subst, &1)), apply_subst(subst, ret)}
  end

  def apply_subst(subst, {:list, elem}) do
    {:list, apply_subst(subst, elem)}
  end

  def apply_subst(_subst, {:pid, name, msgs}) do
    {:pid, name, msgs}
  end

  def apply_subst(subst, {:record, name, fields}) do
    {:record, name, Enum.map(fields, fn {k, v} -> {k, apply_subst(subst, v)} end)}
  end

  # Sum type: {:sum, TypeName, [{CtorName, [field_types]}]}
  def apply_subst(subst, {:sum, name, variants}) do
    {:sum, name, Enum.map(variants, fn {ctor, types} ->
      {ctor, Enum.map(types, &apply_subst(subst, &1))}
    end)}
  end

  # Variant type: {:variant, SumTypeName, CtorName, [field_types]}
  def apply_subst(subst, {:variant, sum_name, ctor, types}) do
    {:variant, sum_name, ctor, Enum.map(types, &apply_subst(subst, &1))}
  end

  def apply_subst(subst, {:process, state_type, msgs}) do
    {:process, apply_subst(subst, state_type), msgs}
  end

  def apply_subst(_subst, {:atom, _} = t), do: t

  # Row variable - similar to type variable but for record fields
  def apply_subst(subst, {:rvar, id} = rv) do
    case Map.fetch(subst, {:row, id}) do
      {:ok, ^rv} -> rv
      {:ok, type} -> apply_subst(subst, type)
      :error -> rv
    end
  end

  # Row type: known fields + row variable for extension
  # {:row, [{field, type}, ...], row_tail}
  # row_tail is either {:rvar, id} (open) or :closed (closed row)
  def apply_subst(subst, {:row, fields, tail}) do
    new_fields = Enum.map(fields, fn {k, v} -> {k, apply_subst(subst, v)} end)
    new_tail = apply_subst(subst, tail)
    {:row, new_fields, new_tail}
  end

  def apply_subst(_subst, type), do: type

  @doc """
  Composes two substitutions: applies s1, then s2.
  """
  def compose_subst(s1, s2) do
    merged = Map.merge(s1, Map.new(s2, fn {k, v} -> {k, apply_subst(s1, v)} end))
    merged
  end

  @doc """
  Returns all free type variables in a type.
  """
  def free_vars({:tvar, id}), do: MapSet.new([id])
  def free_vars({:fn, args, ret}) do
    Enum.reduce(args, free_vars(ret), fn arg, acc ->
      MapSet.union(acc, free_vars(arg))
    end)
  end
  def free_vars({:list, elem}), do: free_vars(elem)
  def free_vars({:record, _name, fields}) do
    Enum.reduce(fields, MapSet.new(), fn {_k, v}, acc ->
      MapSet.union(acc, free_vars(v))
    end)
  end
  def free_vars({:sum, _name, variants}) do
    Enum.reduce(variants, MapSet.new(), fn {_ctor, types}, acc ->
      Enum.reduce(types, acc, fn t, inner_acc ->
        MapSet.union(inner_acc, free_vars(t))
      end)
    end)
  end
  def free_vars({:variant, _sum, _ctor, types}) do
    Enum.reduce(types, MapSet.new(), fn t, acc ->
      MapSet.union(acc, free_vars(t))
    end)
  end
  def free_vars({:rvar, id}), do: MapSet.new([{:row, id}])
  def free_vars({:row, fields, tail}) do
    field_vars = Enum.reduce(fields, MapSet.new(), fn {_k, v}, acc ->
      MapSet.union(acc, free_vars(v))
    end)
    MapSet.union(field_vars, free_vars(tail))
  end
  def free_vars(_), do: MapSet.new()

  @doc """
  Pretty-prints a type for error messages.
  """
  def format_type(:int), do: "Int"
  def format_type(:float), do: "Float"
  def format_type(:bool), do: "Bool"
  def format_type(:string), do: "String"
  def format_type(:any), do: "Any"
  def format_type(:ok), do: "Ok"
  def format_type({:tvar, id}), do: "t#{id}"
  def format_type({:atom, a}), do: ":#{a}"
  def format_type({:list, t}), do: "List(#{format_type(t)})"
  def format_type({:pid, name, _}), do: "Pid(#{name})"
  def format_type({:record, name, _}), do: "#{name}"
  def format_type({:sum, name, _variants}), do: "#{name}"
  def format_type({:variant, sum_name, ctor, _}), do: "#{sum_name}.#{ctor}"
  def format_type({:fn, args, ret}) do
    arg_str = args |> Enum.map(&format_type/1) |> Enum.join(", ")
    "(#{arg_str}) -> #{format_type(ret)}"
  end
  def format_type({:rvar, id}), do: "r#{id}"
  def format_type({:row, fields, :closed}) do
    field_str = fields |> Enum.map(fn {k, v} -> "#{k}: #{format_type(v)}" end) |> Enum.join(", ")
    "{#{field_str}}"
  end
  def format_type({:row, fields, tail}) do
    field_str = fields |> Enum.map(fn {k, v} -> "#{k}: #{format_type(v)}" end) |> Enum.join(", ")
    "{#{field_str} | #{format_type(tail)}}"
  end
  def format_type(other), do: inspect(other)
end
