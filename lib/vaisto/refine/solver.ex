defmodule Vaisto.Refine.Solver do
  @moduledoc """
  The solver interface for verification conditions (RFC §11.1).

  `check/2` answers each VC, in input order:

    * `:valid`: `∧hyps ∧ ¬goal` is unsatisfiable, so the VC holds;
    * `{:invalid, counterexample}`: the query is satisfiable. The
      counterexample maps IR variable names to values. It is advisory,
      since a model over uninterpreted measures can be spurious;
    * `{:unknown, reason}`: nothing is known. This covers a solver
      `unknown`, a resource limit, a timeout, a crash, unparsable output
      and a missing solver. It never passes (RFC §11.2, C13).

  Implementations: `Vaisto.Refine.Solver.Z3` (production),
  `Vaisto.Refine.Solver.Unknown` and `Vaisto.Refine.Solver.Script` (test
  doubles).
  """

  alias Vaisto.Refine.VC

  @type result :: :valid | {:invalid, counterexample :: %{atom() => term()}} | {:unknown, reason :: term()}

  @callback check([VC.t()], keyword()) :: [{vc_id :: term(), result()}]
  @callback identity() :: {name :: String.t(), version :: String.t()}
end
