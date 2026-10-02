defmodule Vaisto.Elab.Shadow do
  @moduledoc """
  The elaborator in shadow mode (liquid-types.md §12, §13): how close it is
  to retiring Hindley–Milner.

  `run/1` takes a program with a `main` and elaborates it with `Vaisto.Elab`.
  Where a definition has no signature, the one HM infers is *suggested*, and
  the elaborator verifies it: that is the migration of §13. The elaborator's
  Core is re-checked by Core Lint, evaluated, and compared with what both
  backends do with today's pipeline. Verdicts:

    * `:agree` - the elaborated Core means what the backends run;
    * `{:disagree, backends}` - it does not;
    * `{:backend_error, backends}` - a backend cannot compile the program:
      a backend bug, not a difference in meaning;
    * `{:elab_rejects, message}` - HM accepts the program and the elaborator
      does not: an HM hole such as D17 or D22, where the suggested signature
      fails to check, or something the elaborator cannot do yet;
    * `{:elab_bug, problems}` - Core Lint rejects the elaborator's output.
      This is never acceptable;
    * `{:outside, reason}` - the program uses something outside slice 1;
    * `:hm_rejects` - there is nothing to compare;
    * `:no_main`.
  """

  alias Vaisto.Elab
  alias Vaisto.Liquid.{Adapter, Harness, Lint}

  @spec run(String.t()) :: map()
  def run(source) do
    with {:ok, ast} <- Vaisto.Compilation.parse(source),
         {:ok, _type, typed} <- typecheck(ast) do
      shadow(source, ast, suggestions(typed))
    else
      _ -> %{verdict: :hm_rejects}
    end
  end

  # The signatures HM infers, in Core, from the Phase 0 adapter. A type in
  # which HM left something unresolved (Dyn) is no suggestion.
  @doc false
  def suggestions(typed) do
    {:ok, core, _skipped} = Adapter.module(typed)
    for [:def, f, type, _] <- core, not dyn?(type), into: %{}, do: {f, type}
  end

  defp dyn?(:Dyn), do: true
  defp dyn?(list) when is_list(list), do: Enum.any?(list, &dyn?/1)
  defp dyn?(_), do: false

  defp shadow(source, ast, suggest) do
    case Elab.module(ast, suggest: suggest) do
      {:outside, reason} ->
        %{verdict: {:outside, reason}}

      {:error, [error | _]} ->
        %{verdict: {:elab_rejects, error.message <> if(error.note, do: " (#{error.note})", else: "")}}

      {:ok, core} ->
        defs = for [:def, f | _] <- core, do: f

        cond do
          :main not in defs ->
            %{verdict: :no_main, core: core}

          true ->
            case Lint.check_module(core) do
              {:error, problems} ->
                %{verdict: {:elab_bug, problems}, core: core}

              :ok ->
                eval = Harness.evaluate(core)
                beam = Harness.beam_outcomes(source)
                broken = for {backend, {:compile_error, _}} <- beam, do: backend
                differing = for {backend, outcome} <- beam, backend not in broken, not Harness.agrees?(eval, outcome), do: backend

                verdict =
                  cond do
                    differing != [] -> {:disagree, differing}
                    broken != [] -> {:backend_error, broken}
                    true -> :agree
                  end

                %{verdict: verdict, core: core, eval: eval, beam: beam}
            end
        end
    end
  end

  defp typecheck(ast) do
    Vaisto.TypeChecker.check(ast)
  rescue
    e -> {:error, Exception.message(e)}
  end
end
