defmodule Vaisto.Elab.Types do
  @moduledoc """
  Types for the elaborator (liquid-types.md §3, §9): surface annotations to
  Core types, metavariables, and unification.

  A type is a Core type (liquid-core.md §3.2), possibly containing
  metavariables `{:meta, n}`, which stand for a type local inference has not
  chosen yet. Unification computes their most general solution, a
  coequalizer (§9). Metavariables never reach Core: the elaborator replaces
  them by their solution, and one that has none is an error, never `Dyn`.

  `[:tvar, a]` is a type variable of the signature being checked. Inside
  that definition it is rigid: it equals only itself.
  """

  @primitives %{int: :Int, float: :Float, string: :String, bool: [:Bool], atom: :Atom, unit: :Unit}

  @type t :: term()
  @type subst :: %{non_neg_integer() => t()}

  # --- Surface annotations ---------------------------------------------------------

  @doc """
  The Core type an annotation writes, given the declared types
  (`%{name => arity}`). Returns `{:ok, type}` or `{:error, message}`.

  `:a` is a type variable, `:Point` and `(Result :int :string)` are named
  types, and `(Fn :int :int :bool)` is a function of two Ints to a Bool.
  `:any` is no annotation: there is no `:any` type (§9).
  """
  @spec from_surface(term(), %{atom() => non_neg_integer()}) :: {:ok, t()} | {:error, String.t()}
  def from_surface(annotation, decls) do
    {:ok, surface(annotation, decls)}
  catch
    {:type_error, message} -> {:error, message}
  end

  defp surface({:atom, t}, decls), do: surface(t, decls)
  defp surface(:any, _decls), do: fail("no type is given")
  defp surface(:num, _decls), do: fail("`:num` is not a type: write `:int` or `:float`")
  defp surface(t, _decls) when is_map_key(@primitives, t), do: Map.fetch!(@primitives, t)

  defp surface(t, decls) when is_atom(t) do
    cond do
      lowercase?(t) -> [:tvar, t]
      Map.has_key?(decls, t) -> named(t, [], decls)
      true -> fail("the type `#{t}` is not declared")
    end
  end

  defp surface({:call, :List, [elem], _loc}, decls), do: [:List, surface(elem, decls)]
  defp surface({:call, :Tuple, [], _loc}, _decls), do: :Unit
  defp surface({:call, :Tuple, ts, _loc}, decls), do: [:Tuple | Enum.map(ts, &surface(&1, decls))]

  defp surface({:call, :Fn, [_ | _] = ts, _loc}, decls) do
    {params, [result]} = Enum.split(ts, -1)
    [:->, Enum.map(params, &surface(&1, decls)), [:eff, :closed], surface(result, decls)]
  end

  defp surface({:call, t, args, _loc}, decls) when is_atom(t) do
    if Map.has_key?(decls, t), do: named(t, Enum.map(args, &surface(&1, decls)), decls), else: fail("the type `#{t}` is not declared")
  end

  defp surface(other, _decls), do: fail("this is not a type: #{inspect(other, limit: 4)}")

  defp named(t, args, decls) do
    case Map.fetch!(decls, t) do
      n when n == length(args) -> [t | args]
      n -> fail("`#{t}` takes #{n} type argument#{if n == 1, do: "", else: "s"}, but #{length(args)} #{if length(args) == 1, do: "is", else: "are"} given")
    end
  end

  defp fail(message), do: throw({:type_error, message})

  defp lowercase?(t), do: String.match?(Atom.to_string(t), ~r/^[a-z]/)

  # --- Type variables and substitution -----------------------------------------------

  @doc "The type variables of a type, in order of first appearance."
  @spec tvars(t()) :: [atom()]
  def tvars(type), do: type |> collect_tvars() |> Enum.uniq()

  defp collect_tvars([:tvar, a]), do: [a]
  defp collect_tvars(list) when is_list(list), do: Enum.flat_map(list, &collect_tvars/1)
  defp collect_tvars(_), do: []

  @doc "Replace type variables by name."
  @spec subst_tvars(t(), %{atom() => t()}) :: t()
  def subst_tvars([:tvar, a] = t, map), do: Map.get(map, a, t)
  def subst_tvars(list, map) when is_list(list), do: Enum.map(list, &subst_tvars(&1, map))
  def subst_tvars(t, _map), do: t

  @doc "Close a type over its type variables: `(forall ((a Type)...) τ)`."
  @spec generalize(t()) :: t()
  def generalize(type) do
    case tvars(type) do
      [] -> type
      as -> [:forall, Enum.map(as, &[&1, :Type]), type]
    end
  end

  # --- Metavariables and unification (§9) -----------------------------------------------

  @doc "A type with every solved metavariable replaced by its solution."
  @spec zonk(t(), subst()) :: t()
  def zonk({:meta, n} = m, s) do
    case Map.fetch(s, n) do
      {:ok, t} -> zonk(t, s)
      :error -> m
    end
  end

  def zonk(list, s) when is_list(list), do: Enum.map(list, &zonk(&1, s))
  def zonk(t, _s), do: t

  @doc "Whether a (zonked) type still has an unsolved metavariable."
  @spec unsolved?(t()) :: boolean()
  def unsolved?({:meta, _}), do: true
  def unsolved?(list) when is_list(list), do: Enum.any?(list, &unsolved?/1)
  def unsolved?(_), do: false

  @doc """
  The most general unifier of two types, extending `s`: `{:ok, s}` or
  `:error`. Type variables are rigid; rows and effects are closed in version 0.
  """
  @spec unify(t(), t(), subst()) :: {:ok, subst()} | :error
  def unify(a, b, s) do
    {:ok, go(zonk(a, s), zonk(b, s), s)}
  catch
    :mismatch -> :error
  end

  defp go({:meta, n}, {:meta, n}, s), do: s
  defp go({:meta, n}, t, s), do: bind(n, t, s)
  defp go(t, {:meta, n}, s), do: bind(n, t, s)
  defp go(a, a, s) when is_atom(a), do: s

  defp go(as, bs, s) when is_list(as) and is_list(bs) and length(as) == length(bs) do
    Enum.zip(as, bs) |> Enum.reduce(s, fn {a, b}, s -> go(zonk(a, s), zonk(b, s), s) end)
  end

  defp go(_a, _b, _s), do: throw(:mismatch)

  defp bind(n, t, s) do
    if occurs?(n, t), do: throw(:mismatch), else: Map.put(s, n, t)
  end

  defp occurs?(n, {:meta, n}), do: true
  defp occurs?(n, list) when is_list(list), do: Enum.any?(list, &occurs?(n, &1))
  defp occurs?(_n, _t), do: false

  # --- Formatting ----------------------------------------------------------------------

  @doc "A type as a message shows it, in Vaisto's spelling."
  @spec format(t()) :: String.t()
  def format(:Int), do: "Int"
  def format(:Float), do: "Float"
  def format(:String), do: "String"
  def format(:Atom), do: "Atom"
  def format(:Unit), do: "Unit"
  def format(:Dyn), do: "Dyn"
  def format({:meta, _}), do: "_"
  def format([:tvar, a]), do: Atom.to_string(a)
  def format([:Bool]), do: "Bool"
  def format([:->, ps, _eff, r]), do: "(Fn " <> Enum.map_join(ps ++ [r], " ", &format/1) <> ")"
  def format([:forall, _binders, t]), do: format(t)
  def format([t]) when is_atom(t), do: Atom.to_string(t)
  def format([t | args]) when is_atom(t), do: "(#{t} " <> Enum.map_join(args, " ", &format/1) <> ")"
  def format(t), do: inspect(t)
end
