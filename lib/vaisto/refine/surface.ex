defmodule Vaisto.Refine.Surface do
  @moduledoc """
  Refinement syntax at the surface (RFC §4.4, §21.1).

  The parser reads `{v :int | p}` in a `defn`'s parameter or result slot as
  `{:refine, v, base_type, p}`. Phase 2.1 checks refinements through a bridge:
  HM sees the plain program, with every refined type replaced by its base type,
  and only the refinement checker reads the refined signatures. `split/1`
  produces both.

  HM must never see a refined type. If it did, an entry point that skips the
  refinement checker would accept the program and drop its refinements
  silently, so `refined?/1` lets the type checker refuse instead.
  """

  defmodule Sig do
    @moduledoc """
    A refined signature, as written. Each parameter is `{name, base_type, refinement}`
    and the result is `{base_type, refinement}`. A refinement is `nil` or
    `{binder, predicate}`, with the predicate as surface AST.
    """
    @enforce_keys [:name, :params, :result]
    defstruct [:name, :params, :result, :loc]

    @type refinement :: nil | {atom(), term()}
    @type t :: %__MODULE__{
            name: atom(),
            params: [{atom(), term(), refinement()}],
            result: {term(), refinement()},
            loc: term()
          }
  end

  @doc """
  Split a parsed program into the plain program and the refined signatures,
  keyed by function name. A `defn` with no refinement has no signature.
  """
  @spec split(term()) :: {term(), %{atom() => Sig.t()}}
  def split(forms) when is_list(forms), do: Enum.map_reduce(forms, %{}, &split_form/2)
  def split(form), do: split_form(form, %{})

  @doc "Whether any top-level `defn` still carries a refined type."
  @spec refined?(term()) :: boolean()
  def refined?(forms) when is_list(forms), do: Enum.any?(forms, &refined?/1)
  def refined?(form), do: match?({_, %Sig{}}, refined_sig(form))

  defp split_form(form, sigs) do
    case refined_sig(form) do
      {plain, %Sig{} = sig} -> {plain, Map.put(sigs, sig.name, sig)}
      nil -> {form, sigs}
    end
  end

  # {:defn, name, params, body, ret, loc} and the guarded {:defn, name, params, body, ret, guard, loc}
  defp refined_sig({:defn, name, params, body, ret, loc}) do
    with {plain_params, plain_ret, sig} <- refine_parts(name, params, ret, loc) do
      {{:defn, name, plain_params, body, plain_ret, loc}, sig}
    end
  end

  defp refined_sig({:defn, name, params, body, ret, guard, loc}) do
    with {plain_params, plain_ret, sig} <- refine_parts(name, params, ret, loc) do
      {{:defn, name, plain_params, body, plain_ret, guard, loc}, sig}
    end
  end

  defp refined_sig(_form), do: nil

  defp refine_parts(name, params, ret, loc) when is_list(params) do
    sig_params = Enum.map(params, fn {param, type} -> {param, base(type), refinement(type)} end)
    sig_result = {base(ret), refinement(ret)}

    if Enum.any?(sig_params, fn {_, _, r} -> r != nil end) or elem(sig_result, 1) != nil do
      plain_params = for {param, type, _} <- sig_params, do: {param, type}
      sig = %Sig{name: name, params: sig_params, result: sig_result, loc: loc}
      {plain_params, base(ret), sig}
    end
  end

  defp refine_parts(_name, _params, _ret, _loc), do: nil

  defp base({:refine, _binder, type, _predicate}), do: type
  defp base(type), do: type

  defp refinement({:refine, binder, _type, predicate}), do: {binder, predicate}
  defp refinement(_type), do: nil
end
