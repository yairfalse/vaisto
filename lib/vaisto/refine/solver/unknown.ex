defmodule Vaisto.Refine.Solver.Unknown do
  @moduledoc """
  A solver that knows nothing: every VC is `{:unknown, :test}`.

  It exists to show that an unknown answer never passes (RFC §14, C13).
  """

  @behaviour Vaisto.Refine.Solver

  @impl true
  def check(vcs, _opts \\ []), do: for(vc <- vcs, do: {vc.id, {:unknown, :test}})

  @impl true
  def identity, do: {"unknown", "test"}
end
