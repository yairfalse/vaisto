defmodule Vaisto.TypeSystem.Unify do
  @moduledoc """
  Unification algorithm for Hindley-Milner type inference.

  Unification finds a substitution that makes two types equal.
  For example, unifying {:tvar, 0} with :int produces {0 => :int}.
  """

  import Vaisto.TypeSystem.Core

  @doc """
  Attempts to unify two types, returning an updated substitution.

  Returns {:ok, subst, row_counter} on success, {:error, reason} on failure.
  The row_counter is used to generate fresh row variables during unification.
  """
  def unify(t1, t2, subst \\ empty_subst(), row_counter \\ 0)

  def unify(t1, t2, subst, row_counter) do
    # Apply current substitutions first to get "real" types
    t1 = apply_subst(subst, t1)
    t2 = apply_subst(subst, t2)

    cond do
      # Same type - nothing to do
      t1 == t2 ->
        {:ok, subst, row_counter}

      # Type variable on left - bind it
      match?({:tvar, _}, t1) ->
        case bind_var(t1, t2, subst) do
          {:ok, subst} -> {:ok, subst, row_counter}
          error -> error
        end

      # Type variable on right - bind it
      match?({:tvar, _}, t2) ->
        case bind_var(t2, t1, subst) do
          {:ok, subst} -> {:ok, subst, row_counter}
          error -> error
        end

      # List types - unify element types
      match?({:list, _}, t1) and match?({:list, _}, t2) ->
        {:list, elem1} = t1
        {:list, elem2} = t2
        unify(elem1, elem2, subst, row_counter)

      # Function types - unify args and return
      match?({:fn, _, _}, t1) and match?({:fn, _, _}, t2) ->
        {:fn, args1, ret1} = t1
        {:fn, args2, ret2} = t2

        if length(args1) != length(args2) do
          {:error, "function arity mismatch: #{length(args1)} vs #{length(args2)}"}
        else
          case unify_lists(args1, args2, subst, row_counter) do
            {:ok, subst, row_counter} -> unify(ret1, ret2, subst, row_counter)
            error -> error
          end
        end

      # Record types - unify field types
      match?({:record, _, _}, t1) and match?({:record, _, _}, t2) ->
        {:record, name1, fields1} = t1
        {:record, name2, fields2} = t2

        if name1 != name2 do
          {:error, "cannot unify records #{name1} and #{name2}"}
        else
          unify_fields(fields1, fields2, subst, row_counter)
        end

      # Tuple types - unify element types
      match?({:tuple, _}, t1) and match?({:tuple, _}, t2) ->
        {:tuple, elems1} = t1
        {:tuple, elems2} = t2

        if length(elems1) != length(elems2) do
          {:error, "tuple size mismatch: #{length(elems1)} vs #{length(elems2)}"}
        else
          unify_lists(elems1, elems2, subst, row_counter)
        end

      # Row variable on left - bind it
      match?({:rvar, _}, t1) ->
        case bind_row_var(t1, t2, subst) do
          {:ok, subst} -> {:ok, subst, row_counter}
          error -> error
        end

      # Row variable on right - bind it
      match?({:rvar, _}, t2) ->
        case bind_row_var(t2, t1, subst) do
          {:ok, subst} -> {:ok, subst, row_counter}
          error -> error
        end

      # Row types - unify with row polymorphism
      match?({:row, _, _}, t1) and match?({:row, _, _}, t2) ->
        unify_rows(t1, t2, subst, row_counter)

      # Row type vs closed record - check if row has required fields
      match?({:row, _, _}, t1) and match?({:record, _, _}, t2) ->
        {:record, _name, fields2} = t2
        {:row, fields1, tail} = t1
        unify_row_with_record(fields1, tail, fields2, subst, row_counter)

      match?({:record, _, _}, t1) and match?({:row, _, _}, t2) ->
        {:record, _name, fields1} = t1
        {:row, fields2, tail} = t2
        unify_row_with_record(fields2, tail, fields1, subst, row_counter)

      # No match - types are incompatible
      true ->
        {:error, "cannot unify #{format_type(t1)} with #{format_type(t2)}"}
    end
  end

  @doc """
  Binds a type variable to a type, with occurs check.

  The occurs check prevents infinite types like: a = List(a)
  """
  def bind_var({:tvar, id}, type, subst) do
    # Occurs check: ensure we're not creating an infinite type
    if occurs?(id, type) do
      {:error, "infinite type: t#{id} occurs in #{format_type(type)}"}
    else
      {:ok, Map.put(subst, id, type)}
    end
  end

  @doc """
  Checks if a type variable occurs within a type.
  Used to prevent infinite types.
  """
  def occurs?(id, {:tvar, id}), do: true
  def occurs?(_id, {:tvar, _}), do: false
  def occurs?(id, {:list, elem}), do: occurs?(id, elem)
  def occurs?(id, {:fn, args, ret}) do
    Enum.any?(args, &occurs?(id, &1)) or occurs?(id, ret)
  end
  def occurs?(id, {:tuple, elems}) do
    Enum.any?(elems, &occurs?(id, &1))
  end
  def occurs?(id, {:record, _name, fields}) do
    Enum.any?(fields, fn {_k, v} -> occurs?(id, v) end)
  end
  def occurs?(id, {:row, fields, tail}) do
    Enum.any?(fields, fn {_k, v} -> occurs?(id, v) end) or occurs?(id, tail)
  end
  def occurs?(_id, _type), do: false

  @doc """
  Unifies two lists of types pairwise.
  """
  def unify_lists(list1, list2, subst, row_counter \\ 0)
  def unify_lists([], [], subst, row_counter), do: {:ok, subst, row_counter}
  def unify_lists([t1 | rest1], [t2 | rest2], subst, row_counter) do
    case unify(t1, t2, subst, row_counter) do
      {:ok, subst, row_counter} -> unify_lists(rest1, rest2, subst, row_counter)
      error -> error
    end
  end

  @doc """
  Unifies record fields by name.
  """
  def unify_fields(fields1, fields2, subst, row_counter \\ 0) do
    # Build a map for quick lookup
    map1 = Map.new(fields1)
    map2 = Map.new(fields2)

    # All keys must match
    if Map.keys(map1) |> Enum.sort() != Map.keys(map2) |> Enum.sort() do
      {:error, "record field mismatch"}
    else
      Enum.reduce_while(map1, {:ok, subst, row_counter}, fn {key, type1}, {:ok, acc_subst, acc_counter} ->
        type2 = Map.fetch!(map2, key)
        case unify(type1, type2, acc_subst, acc_counter) do
          {:ok, new_subst, new_counter} -> {:cont, {:ok, new_subst, new_counter}}
          error -> {:halt, error}
        end
      end)
    end
  end

  # ============================================================================
  # Row Polymorphism
  # ============================================================================

  @doc """
  Binds a row variable to a type (row or closed).
  """
  def bind_row_var({:rvar, id}, type, subst) do
    # Occurs check for row variables
    if row_occurs?(id, type) do
      {:error, "infinite row type: r#{id} occurs in #{format_type(type)}"}
    else
      {:ok, Map.put(subst, {:row, id}, type)}
    end
  end

  @doc """
  Checks if a row variable occurs within a type.
  """
  def row_occurs?(id, {:rvar, id}), do: true
  def row_occurs?(_id, {:rvar, _}), do: false
  def row_occurs?(id, {:row, fields, tail}) do
    Enum.any?(fields, fn {_k, v} -> row_occurs?(id, v) end) or row_occurs?(id, tail)
  end
  def row_occurs?(id, {:fn, args, ret}) do
    Enum.any?(args, &row_occurs?(id, &1)) or row_occurs?(id, ret)
  end
  def row_occurs?(id, {:list, elem}), do: row_occurs?(id, elem)
  def row_occurs?(_id, _), do: false

  @doc """
  Unifies two row types.

  Row unification is more flexible than record unification:
  - Common fields must unify
  - Extra fields in one row can be absorbed by the other's row variable
  """
  def unify_rows({:row, fields1, tail1}, {:row, fields2, tail2}, subst, row_counter) do
    map1 = Map.new(fields1)
    map2 = Map.new(fields2)

    common_keys = MapSet.intersection(MapSet.new(Map.keys(map1)), MapSet.new(Map.keys(map2)))
    only_in_1 = Map.drop(map1, MapSet.to_list(common_keys))
    only_in_2 = Map.drop(map2, MapSet.to_list(common_keys))

    # Unify common fields
    with {:ok, subst, row_counter} <- unify_common_fields(common_keys, map1, map2, subst, row_counter),
         # Handle extra fields based on tails
         {:ok, subst, row_counter} <- unify_row_tails(only_in_1, tail1, only_in_2, tail2, subst, row_counter) do
      {:ok, subst, row_counter}
    end
  end

  defp unify_common_fields(keys, map1, map2, subst, row_counter) do
    Enum.reduce_while(keys, {:ok, subst, row_counter}, fn key, {:ok, acc_subst, acc_counter} ->
      t1 = Map.fetch!(map1, key)
      t2 = Map.fetch!(map2, key)
      case unify(t1, t2, acc_subst, acc_counter) do
        {:ok, new_subst, new_counter} -> {:cont, {:ok, new_subst, new_counter}}
        error -> {:halt, error}
      end
    end)
  end

  defp unify_row_tails(only_in_1, tail1, only_in_2, tail2, subst, row_counter) do
    cond do
      # Both closed - extra fields not allowed
      tail1 == :closed and tail2 == :closed ->
        if map_size(only_in_1) == 0 and map_size(only_in_2) == 0 do
          {:ok, subst, row_counter}
        else
          {:error, "row field mismatch: closed rows have different fields"}
        end

      # tail1 is open - absorb only_in_2 fields
      match?({:rvar, _}, tail1) and tail2 == :closed ->
        if map_size(only_in_1) == 0 do
          # Bind tail1 to the extra fields as a closed row
          case bind_row_var(tail1, {:row, Map.to_list(only_in_2), :closed}, subst) do
            {:ok, subst} -> {:ok, subst, row_counter}
            error -> error
          end
        else
          {:error, "row field mismatch: extra fields #{inspect(Map.keys(only_in_1))} not in closed row"}
        end

      # tail2 is open - absorb only_in_1 fields
      tail1 == :closed and match?({:rvar, _}, tail2) ->
        if map_size(only_in_2) == 0 do
          case bind_row_var(tail2, {:row, Map.to_list(only_in_1), :closed}, subst) do
            {:ok, subst} -> {:ok, subst, row_counter}
            error -> error
          end
        else
          {:error, "row field mismatch: extra fields #{inspect(Map.keys(only_in_2))} not in closed row"}
        end

      # Both open - use the counter for fresh row variable
      match?({:rvar, _}, tail1) and match?({:rvar, _}, tail2) ->
        # Generate a fresh row variable using the counter
        fresh_tail = {:rvar, row_counter}
        new_counter = row_counter + 1

        # tail1 = only_in_2 ++ fresh_tail
        # tail2 = only_in_1 ++ fresh_tail
        with {:ok, subst} <- bind_row_var(tail1, {:row, Map.to_list(only_in_2), fresh_tail}, subst),
             {:ok, subst} <- bind_row_var(tail2, {:row, Map.to_list(only_in_1), fresh_tail}, subst) do
          {:ok, subst, new_counter}
        end

      true ->
        {:error, "cannot unify row tails"}
    end
  end

  @doc """
  Unifies a row type with a concrete record.
  The row must have at least the fields in the record.
  """
  def unify_row_with_record(row_fields, row_tail, record_fields, subst, row_counter \\ 0) do
    row_map = Map.new(row_fields)
    record_map = Map.new(record_fields)

    # All row fields must exist in record
    missing_in_record = Map.keys(row_map) -- Map.keys(record_map)
    unless missing_in_record == [] do
      {:error, "row requires fields #{inspect(missing_in_record)} not in record"}
    else
      # Unify common fields
      with {:ok, subst, row_counter} <- unify_common_fields(MapSet.new(Map.keys(row_map)), row_map, record_map, subst, row_counter) do
        # Extra fields in record go into the row tail
        extra_in_record = Map.drop(record_map, Map.keys(row_map))

        case row_tail do
          :closed ->
            if map_size(extra_in_record) == 0 do
              {:ok, subst, row_counter}
            else
              {:error, "closed row doesn't accept extra fields #{inspect(Map.keys(extra_in_record))}"}
            end

          {:rvar, _} ->
            # Bind row variable to extra fields as closed
            case bind_row_var(row_tail, {:row, Map.to_list(extra_in_record), :closed}, subst) do
              {:ok, subst} -> {:ok, subst, row_counter}
              error -> error
            end
        end
      end
    end
  end
end
