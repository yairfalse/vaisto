defmodule Vaisto.Build.Context do
  @moduledoc """
  Lightweight build context for tracing multi-file builds.

  Tracks build ID and timing for correlating log messages across
  compilation of multiple files in a single build invocation.
  """

  @type t :: %__MODULE__{
          build_id: String.t(),
          started_at: integer()
        }

  defstruct [:build_id, :started_at]

  @spec new() :: t()
  def new do
    %__MODULE__{
      build_id: Base.encode16(:crypto.strong_rand_bytes(4), case: :lower),
      started_at: System.monotonic_time(:millisecond)
    }
  end

  @spec elapsed(t()) :: integer()
  def elapsed(%__MODULE__{started_at: s}) do
    System.monotonic_time(:millisecond) - s
  end
end
