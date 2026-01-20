defmodule Vaisto.TypeSystem.Context do
  @moduledoc """
  Type inference context that tracks:
  - Fresh type variable counter
  - Current substitution
  - Type environment (variable -> type scheme mappings)

  This enables Algorithm W style inference where we generate fresh
  type variables and accumulate substitutions as we traverse the AST.
  """

  alias Vaisto.TypeSystem.Core
  alias Vaisto.TypeSystem.Unify

  defstruct [
    :counter,      # Next fresh type variable ID
    :row_counter,  # Next fresh row variable ID
    :subst,        # Current substitution map
    :env           # Type environment: var_name -> type or type_scheme
  ]

  @doc """
  Creates a new inference context with optional initial environment.
  """
  def new(env \\ %{}) do
    %__MODULE__{
      counter: 0,
      row_counter: 0,
      subst: Core.empty_subst(),
      env: env
    }
  end

  @doc """
  Generates a fresh type variable and returns {tvar, updated_context}.
  """
  def fresh_var(%__MODULE__{counter: n} = ctx) do
    {Core.tvar(n), %{ctx | counter: n + 1}}
  end

  @doc """
  Generates n fresh type variables.
  Returns {[tvars], updated_context}.
  """
  def fresh_vars(ctx, 0), do: {[], ctx}
  def fresh_vars(ctx, n) when n > 0 do
    {var, ctx} = fresh_var(ctx)
    {rest, ctx} = fresh_vars(ctx, n - 1)
    {[var | rest], ctx}
  end

  @doc """
  Generates a fresh row variable and returns {rvar, updated_context}.
  """
  def fresh_row_var(%__MODULE__{row_counter: n} = ctx) do
    {{:rvar, n}, %{ctx | row_counter: n + 1}}
  end

  @doc """
  Looks up a variable in the environment.
  Returns {:ok, type} or :error.
  """
  def lookup(%__MODULE__{env: env}, name) do
    Map.fetch(env, name)
  end

  @doc """
  Extends the environment with a new binding.
  """
  def extend(%__MODULE__{env: env} = ctx, name, type) do
    %{ctx | env: Map.put(env, name, type)}
  end

  @doc """
  Extends the environment with multiple bindings.
  """
  def extend_many(ctx, bindings) when is_list(bindings) do
    Enum.reduce(bindings, ctx, fn {name, type}, acc ->
      extend(acc, name, type)
    end)
  end

  @doc """
  Adds a substitution to the context, composing with existing substitutions.
  """
  def add_subst(%__MODULE__{subst: s1} = ctx, s2) do
    %{ctx | subst: Core.compose_subst(s1, s2)}
  end

  @doc """
  Unifies two types within the context, updating substitutions.
  Returns {:ok, updated_context} or {:error, reason}.
  """
  def unify_types(%__MODULE__{subst: subst, row_counter: row_counter} = ctx, t1, t2) do
    case Unify.unify(t1, t2, subst, row_counter) do
      {:ok, new_subst, new_row_counter} ->
        {:ok, %{ctx | subst: new_subst, row_counter: new_row_counter}}
      {:error, _} = err -> err
    end
  end

  @doc """
  Applies the current substitution to a type.
  """
  def apply(%__MODULE__{subst: subst}, type) do
    Core.apply_subst(subst, type)
  end

  @doc """
  Instantiates a polymorphic type scheme with fresh type variables.

  A type scheme {:forall, [vars], type} becomes a concrete type
  where each var is replaced with a fresh type variable.
  """
  def instantiate(ctx, {:forall, vars, type}) do
    {fresh, ctx} = fresh_vars(ctx, length(vars))
    subst = Enum.zip(vars, fresh) |> Map.new()
    {Core.apply_subst(subst, type), ctx}
  end

  def instantiate(ctx, type), do: {type, ctx}

  @doc """
  Generalizes a type to a type scheme by quantifying over
  free variables not in the environment.

  This is used for let-polymorphism: `(let [id (fn [x] x)] ...)`
  where `id` gets a polymorphic type.
  """
  def generalize(%__MODULE__{env: env, subst: subst}, type) do
    type = Core.apply_subst(subst, type)
    type_vars = Core.free_vars(type)
    env_vars = env_free_vars(env, subst)

    # Variables to quantify are those free in the type but not in the env
    quantified = MapSet.difference(type_vars, env_vars) |> MapSet.to_list()

    if quantified == [] do
      type
    else
      {:forall, quantified, type}
    end
  end

  # Collects all free variables in the environment
  defp env_free_vars(env, subst) do
    Enum.reduce(env, MapSet.new(), fn {_name, type}, acc ->
      type = Core.apply_subst(subst, type)
      case type do
        {:forall, vars, inner} ->
          # Exclude forall-bound variables from free vars
          inner_free = Core.free_vars(inner)
          bound = MapSet.new(vars)
          MapSet.union(acc, MapSet.difference(inner_free, bound))
        _ -> MapSet.union(acc, Core.free_vars(type))
      end
    end)
  end
end
