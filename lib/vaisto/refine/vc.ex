defmodule Vaisto.Refine.VC do
  @moduledoc """
  A verification condition: ∀ vars. ∧hyps ⇒ goal (RFC §5.5).

  The query for a VC is `∧hyps ∧ ¬goal`; `unsat` means the VC is valid.
  A satisfiability question ("can the requirements ever be met") is a VC
  whose goal is `{:bool, false}`, so `:valid` there means "unsatisfiable".

  Fields:

    * `id`: any term the caller chooses, unique per `check/2` call;
    * `kind`: `:precondition | :postcondition | :primitive_safety | :vacuity | :contradiction`;
    * `vars`: `[{name, sort}]`. The lowering declares these plus every
      variable occurring in `hyps` and `goal`, so the list may be incomplete
      or empty;
    * `hyps`: `[pred]`;
    * `goal`: `pred`;
    * `meta`: opaque to the solver layer; the lowering never reads it.

  Terms are sorted (`t:sort/0`), and a predicate is a term of sort `:bool`.
  A list is an uninterpreted sort with one measure, `len`. `tdiv` and `trem`
  are Vaisto's truncating `div` and `rem` (RFC §4.3).
  """

  @enforce_keys [:id, :kind, :hyps, :goal]
  defstruct [:id, :kind, :hyps, :goal, vars: [], meta: %{}]

  @type sort :: :int | :bool | :list

  @type ir_term ::
          {:var, atom(), sort()}
          | {:int, integer()}
          | {:bool, boolean()}
          | {:arith, :add | :sub | :mul | :tdiv | :trem, [ir_term()]}
          | {:arith, :neg, [ir_term()]}
          | {:ite, ir_term(), ir_term(), ir_term()}
          | {:len, ir_term()}
          | {:eq, ir_term(), ir_term()}
          | {:lt, ir_term(), ir_term()}
          | {:le, ir_term(), ir_term()}
          | {:not, ir_term()}
          | {:and, [ir_term()]}
          | {:or, [ir_term()]}
          | {:implies, ir_term(), ir_term()}
          | {:iff, ir_term(), ir_term()}

  @type kind :: :precondition | :postcondition | :primitive_safety | :vacuity | :contradiction

  @type t :: %__MODULE__{
          id: term(),
          kind: kind(),
          vars: [{atom(), sort()}],
          hyps: [ir_term()],
          goal: ir_term(),
          meta: term()
        }
end
