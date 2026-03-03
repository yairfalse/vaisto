defmodule Vaisto.TypeChecker.TcCtx do
  @moduledoc """
  Type-checking context that threads a substitution, row counter, and
  fresh type variable counter through the TypeChecker.

  Supports let-polymorphism via `generalize/2` and `instantiate/2`,
  ported from `Vaisto.TypeSystem.Context` (Algorithm W).
  """

  alias Vaisto.TypeSystem.Core
  alias Vaisto.TypeSystem.Unify

  defstruct [:env, :subst, row_counter: 0, counter: 10_000, constraints: [], constrained_tvars: MapSet.new()]

  @doc "Create a new context from a type environment."
  def new(env) do
    %__MODULE__{env: env, subst: Core.empty_subst(), row_counter: 0, counter: 10_000, constrained_tvars: MapSet.new()}
  end

  @doc "Unify two types within this context, updating the substitution and row counter."
  def unify(%__MODULE__{subst: subst, row_counter: rc} = ctx, t1, t2) do
    case Unify.unify(t1, t2, subst, rc) do
      {:ok, new_subst, new_rc} -> {:ok, %{ctx | subst: new_subst, row_counter: new_rc}}
      {:error, _} = err -> err
    end
  end

  @doc "Apply the current substitution to a type."
  def apply_subst(%__MODULE__{subst: subst}, type) do
    Core.apply_subst(subst, type)
  end

  @doc "Generate a fresh type variable, returning {tvar, updated_ctx}."
  def fresh_var(%__MODULE__{counter: n} = ctx) do
    {Core.tvar(n), %{ctx | counter: n + 1}}
  end

  @doc "Generate n fresh type variables, returning {[tvars], updated_ctx}."
  def fresh_vars(ctx, 0), do: {[], ctx}
  def fresh_vars(ctx, n) when n > 0 do
    {var, ctx} = fresh_var(ctx)
    {rest, ctx} = fresh_vars(ctx, n - 1)
    {[var | rest], ctx}
  end

  @doc """
  Instantiate a polymorphic type scheme with fresh type variables.

  `{:forall, [vars], type}` → monotype with fresh tvars.
  `{:forall, [vars], {:constrained, constraints, type}}` → monotype + constraints added to ctx.
  Bare monotypes pass through unchanged.
  """
  def instantiate(ctx, {:forall, vars, {:constrained, constraints, type}}) do
    {fresh, ctx} = fresh_vars(ctx, length(vars))
    subst = Enum.zip(vars, fresh) |> Map.new()
    inst_type = Core.apply_subst(subst, type)
    inst_constraints = Enum.map(constraints, fn {class, t} ->
      {class, Core.apply_subst(subst, t)}
    end)
    {inst_type, %{ctx | constraints: ctx.constraints ++ inst_constraints}}
  end

  def instantiate(ctx, {:forall, vars, type}) do
    {fresh, ctx} = fresh_vars(ctx, length(vars))
    subst = Enum.zip(vars, fresh) |> Map.new()
    {Core.apply_subst(subst, type), ctx}
  end

  def instantiate(ctx, type), do: {type, ctx}

  @doc """
  Generalize a type to a type scheme by quantifying over free variables
  not bound in the environment. Used for let-polymorphism.
  """
  def generalize(%__MODULE__{env: env, subst: subst, constraints: constraints}, type) do
    type = Core.apply_subst(subst, type)
    type_vars = Core.free_vars(type)
    env_vars = env_free_vars(env, subst)

    quantified = MapSet.difference(type_vars, env_vars) |> MapSet.to_list()

    relevant = Enum.filter(constraints, fn {_class, t} ->
      t = Core.apply_subst(subst, t)
      t_vars = Core.free_vars(t)
      not MapSet.disjoint?(t_vars, MapSet.new(quantified))
    end)

    case {quantified, relevant} do
      {[], _} -> type
      {_, []} -> {:forall, quantified, type}
      _ -> {:forall, quantified, {:constrained, relevant, type}}
    end
  end

  @doc "Mark a type variable as constrained (used by a typeclassed operation)."
  def mark_constrained(%__MODULE__{} = ctx, {:tvar, id}) do
    %{ctx | constrained_tvars: MapSet.put(ctx.constrained_tvars, id)}
  end
  def mark_constrained(ctx, _), do: ctx

  @doc """
  Generalize a type conservatively.

  `eligible_tvars` is the set of tvar IDs created by freshening — only these
  can be quantified. Constrained tvars (from typeclassed operations) are pinned
  to `:any`. All other tvars (e.g. from ADT definitions) are left as-is.
  """
  def generalize_conservative(%__MODULE__{subst: subst, constrained_tvars: constrained}, type, eligible_tvars) do
    # Pin constrained tvars to :any BEFORE applying the main subst
    pin_subst = constrained
      |> Enum.map(fn id -> {id, :any} end)
      |> Map.new()

    merged_subst = Map.merge(subst, pin_subst)
    type = Core.apply_subst(merged_subst, type)
    type_vars = Core.free_vars(type)

    # Only quantify tvars that are both free in the type AND eligible (from freshening)
    quantified = type_vars
      |> Enum.filter(&MapSet.member?(eligible_tvars, &1))
      |> Enum.sort()

    case quantified do
      [] -> type
      _ -> {:forall, quantified, type}
    end
  end

  # Collects all free variables in the environment
  defp env_free_vars(env, subst) do
    Enum.reduce(env, MapSet.new(), fn {_name, type}, acc ->
      type = Core.apply_subst(subst, type)
      case type do
        {:forall, vars, {:constrained, _constraints, inner}} ->
          inner_free = Core.free_vars(inner)
          bound = MapSet.new(vars)
          MapSet.union(acc, MapSet.difference(inner_free, bound))
        {:forall, vars, inner} ->
          inner_free = Core.free_vars(inner)
          bound = MapSet.new(vars)
          MapSet.union(acc, MapSet.difference(inner_free, bound))
        _ -> MapSet.union(acc, Core.free_vars(type))
      end
    end)
  end
end
