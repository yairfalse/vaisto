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

  def apply_subst(subst, {:tuple, types}) do
    {:tuple, Enum.map(types, &apply_subst(subst, &1))}
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
  def free_vars({:tuple, types}) do
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

  # ===========================================================================
  # Type Formatting for Error Messages
  # ===========================================================================
  #
  # Uses 'a, 'b, 'c style for type variables (ML/Rust tradition).
  # Contextual normalization ensures that errors show 'a, 'b instead of
  # confusing raw IDs like t452, t890.

  @doc """
  Pretty-prints a type for error messages (simple version with raw IDs).
  For user-facing errors, prefer `format_type_pretty/1` or `format_types_for_error/2`.
  """
  def format_type(type), do: format_type_with_ctx(type, %{})

  @doc """
  Format a type with a normalization context (id -> display-index mapping).
  """
  def format_type_with_ctx(:int, _ctx), do: "Int"
  def format_type_with_ctx(:float, _ctx), do: "Float"
  def format_type_with_ctx(:bool, _ctx), do: "Bool"
  def format_type_with_ctx(:string, _ctx), do: "String"
  def format_type_with_ctx(:any, _ctx), do: "Any"
  def format_type_with_ctx(:ok, _ctx), do: "Ok"
  def format_type_with_ctx(:unit, _ctx), do: "Unit"
  def format_type_with_ctx({:tvar, id}, ctx) do
    display_id = Map.get(ctx, id, id)
    format_tvar_pretty(display_id)
  end
  def format_type_with_ctx({:rvar, id}, ctx) do
    display_id = Map.get(ctx, {:row, id}, id)
    format_rvar_pretty(display_id)
  end
  def format_type_with_ctx({:atom, a}, _ctx), do: ":#{a}"
  def format_type_with_ctx({:list, t}, ctx), do: "List(#{format_type_with_ctx(t, ctx)})"
  def format_type_with_ctx({:pid, name, _}, _ctx), do: "Pid(#{name})"
  def format_type_with_ctx({:record, name, _}, _ctx), do: "#{name}"
  def format_type_with_ctx({:sum, name, _variants}, _ctx), do: "#{name}"
  def format_type_with_ctx({:variant, sum_name, ctor, _}, _ctx), do: "#{sum_name}.#{ctor}"
  def format_type_with_ctx({:fn, args, ret}, ctx) do
    arg_str = args |> Enum.map(&format_type_with_ctx(&1, ctx)) |> Enum.join(", ")
    "(#{arg_str}) -> #{format_type_with_ctx(ret, ctx)}"
  end
  def format_type_with_ctx({:row, fields, :closed}, ctx) do
    field_str = fields |> Enum.map(fn {k, v} -> "#{k}: #{format_type_with_ctx(v, ctx)}" end) |> Enum.join(", ")
    "{#{field_str}}"
  end
  def format_type_with_ctx({:row, fields, tail}, ctx) do
    field_str = fields |> Enum.map(fn {k, v} -> "#{k}: #{format_type_with_ctx(v, ctx)}" end) |> Enum.join(", ")
    "{#{field_str} | #{format_type_with_ctx(tail, ctx)}}"
  end
  def format_type_with_ctx(other, _ctx), do: inspect(other)

  @doc """
  Convert normalized index to 'a, 'b, ... 'z, 'a1, 'b1, ...
  """
  def format_tvar_pretty(idx) when is_integer(idx) do
    base = rem(idx, 26)
    suffix = div(idx, 26)
    char = <<97 + base>>  # 97 = ?a
    if suffix == 0, do: "'#{char}", else: "'#{char}#{suffix}"
  end
  def format_tvar_pretty(other), do: "'?#{inspect(other)}"

  @doc """
  Convert normalized index to ..a, ..b, ... for row variables.
  """
  def format_rvar_pretty(idx) when is_integer(idx) do
    base = rem(idx, 26)
    suffix = div(idx, 26)
    char = <<97 + base>>
    if suffix == 0, do: "..#{char}", else: "..#{char}#{suffix}"
  end
  def format_rvar_pretty(other), do: "..?#{inspect(other)}"

  # ===========================================================================
  # Contextual Normalization
  # ===========================================================================
  #
  # Scan types to build a mapping: raw-id -> display-index (0, 1, 2...)
  # This ensures errors show 'a, 'b instead of 't452, 't890

  @doc """
  Collect all type variable IDs from a type (in order of appearance).
  """
  def collect_tvars(type), do: collect_tvars(type, []) |> Enum.reverse() |> Enum.uniq()

  defp collect_tvars({:tvar, id}, acc), do: [{:tvar, id} | acc]
  defp collect_tvars({:rvar, id}, acc), do: [{:rvar, id} | acc]
  defp collect_tvars({:list, elem}, acc), do: collect_tvars(elem, acc)
  defp collect_tvars({:fn, args, ret}, acc) do
    acc = Enum.reduce(args, acc, &collect_tvars/2)
    collect_tvars(ret, acc)
  end
  defp collect_tvars({:row, fields, tail}, acc) do
    acc = Enum.reduce(fields, acc, fn {_k, v}, a -> collect_tvars(v, a) end)
    collect_tvars(tail, acc)
  end
  defp collect_tvars({:record, _name, fields}, acc) do
    Enum.reduce(fields, acc, fn {_k, v}, a -> collect_tvars(v, a) end)
  end
  defp collect_tvars({:sum, _name, variants}, acc) do
    Enum.reduce(variants, acc, fn {_ctor, types}, a ->
      Enum.reduce(types, a, &collect_tvars/2)
    end)
  end
  defp collect_tvars(_other, acc), do: acc

  @doc """
  Build normalization context from multiple types (for error display).
  Returns a map: raw-id -> normalized-index
  """
  def build_display_ctx(types) when is_list(types) do
    types
    |> Enum.flat_map(&collect_tvars/1)
    |> Enum.uniq()
    |> Enum.with_index()
    |> Enum.reduce(%{}, fn
      {{:tvar, id}, idx}, acc -> Map.put(acc, id, idx)
      {{:rvar, id}, idx}, acc -> Map.put(acc, {:row, id}, idx)
    end)
  end

  @doc """
  Format types with automatic normalization (for error messages).
  Returns a map with :expected and :actual formatted strings.
  """
  def format_types_for_error(expected, actual) do
    ctx = build_display_ctx([expected, actual])
    %{
      expected: format_type_with_ctx(expected, ctx),
      actual: format_type_with_ctx(actual, ctx)
    }
  end

  @doc """
  Format a single type in context of others.
  """
  def format_type_in_context(type, context_types) do
    ctx = build_display_ctx([type | context_types])
    format_type_with_ctx(type, ctx)
  end
end
