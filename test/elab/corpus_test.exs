defmodule Vaisto.Elab.CorpusTest do
  # Signed fixtures exercise the real compiler without inferred signatures.
  # Keep the migration's agreement ratchet after retiring shadow mode.
  use ExUnit.Case, async: true

  alias Vaisto.Liquid.Harness

  test "the elaborator never produces Core that Lint rejects, and never disagrees with the backends" do
    corpus =
      Harness.corpus(
        Path.wildcard("test/**/*_test.exs") --
          Path.wildcard("test/{liquid,refine,elab}/*_test.exs")
      )

    assert length(corpus) > 100

    results = for {file, source} <- corpus, do: {file, source, Harness.run(source).verdict}

    bugs = for {file, source, {:lint_rejects, problems}} <- results, do: {file, source, problems}

    assert bugs == [],
           "Core Lint rejects the elaborator's output:\n#{inspect(bugs, pretty: true)}"

    disagreeing =
      for {file, source, {:disagree, backends}} <- results, do: {file, source, backends}

    assert disagreeing == [],
           "the elaborated Core disagrees with the backends:\n#{inspect(disagreeing, pretty: true)}"

    agreeing = Enum.count(results, &match?({_, _, :agree}, &1))
    # 75 since the elaborator became pure: no Int-Float coercion, no default
    # for an undetermined type, exhaustive matches and irrefutable lets.
    assert agreeing >= 75, "only #{agreeing} programs agree"
  end
end
