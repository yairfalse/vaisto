defmodule Vaisto.Refine.Solver.Script do
  @moduledoc """
  A solver with scripted answers, for testing diagnostics without Z3
  (RFC §11.1).

  `opts[:answers]` maps a VC id to the result to give for it. A VC whose id
  is not scripted is `{:unknown, :unscripted}`, so a missing answer can
  never pass.
  """

  @behaviour Vaisto.Refine.Solver

  @impl true
  def check(vcs, opts \\ []) do
    answers = Keyword.get(opts, :answers, %{})
    for vc <- vcs, do: {vc.id, Map.get(answers, vc.id, {:unknown, :unscripted})}
  end

  @impl true
  def identity, do: {"script", "test"}
end
