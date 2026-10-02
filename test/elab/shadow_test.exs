defmodule Vaisto.Elab.ShadowTest do
  # The elaborator in shadow mode over every program in the test suite
  # (liquid-types.md §12: HM retires when the elaborator reaches parity on the
  # corpus). Signatures a program lacks are suggested from HM and verified
  # (§13), so a rejection is an HM hole (D17, D22), a signature with no syntax
  # yet, or a gap in the elaborator; the counts below are a ratchet.
  use ExUnit.Case, async: true

  alias Vaisto.Elab.Shadow
  alias Vaisto.Liquid.Harness

  test "the elaborator never produces Core that Lint rejects, and never disagrees with the backends" do
    corpus = Harness.corpus(Path.wildcard("test/**/*_test.exs") -- Path.wildcard("test/{liquid,refine,elab}/*_test.exs"))
    assert length(corpus) > 100

    results = for {file, source} <- corpus, do: {file, source, Shadow.run(source).verdict}

    bugs = for {file, source, {:elab_bug, problems}} <- results, do: {file, source, problems}
    assert bugs == [], "Core Lint rejects the elaborator's output:\n#{inspect(bugs, pretty: true)}"

    disagreeing = for {file, source, {:disagree, backends}} <- results, do: {file, source, backends}
    assert disagreeing == [], "the elaborated Core disagrees with the backends:\n#{inspect(disagreeing, pretty: true)}"

    agreeing = Enum.count(results, &match?({_, _, :agree}, &1))
    # 75 since the elaborator became pure: no Int-Float coercion, no default
    # for an undetermined type, exhaustive matches and irrefutable lets.
    assert agreeing >= 75, "only #{agreeing} programs agree"
  end
end
