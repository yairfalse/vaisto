defmodule Vaisto.TypeChecker.TcCtx do
  @moduledoc """
  Type-checking context that threads a substitution through the TypeChecker.

  Unlike `Vaisto.TypeSystem.Context` (used by Infer/Algorithm W), this context
  does not carry a fresh variable counter — the TypeChecker uses
  `:erlang.unique_integer` for fresh tvars. This keeps TcCtx minimal.
  """

  alias Vaisto.TypeSystem.Core
  alias Vaisto.TypeSystem.Unify

  defstruct [:env, :subst]

  @doc "Create a new context from a type environment."
  def new(env), do: %__MODULE__{env: env, subst: Core.empty_subst()}

  @doc "Unify two types within this context, updating the substitution."
  def unify(%__MODULE__{subst: subst} = ctx, t1, t2) do
    case Unify.unify(t1, t2, subst) do
      {:ok, new_subst, _row_counter} -> {:ok, %{ctx | subst: new_subst}}
      {:error, _} = err -> err
    end
  end

  @doc "Apply the current substitution to a type."
  def apply_subst(%__MODULE__{subst: subst}, type) do
    Core.apply_subst(subst, type)
  end
end
