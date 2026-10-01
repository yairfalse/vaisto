defmodule Vaisto.Liquid.SoundnessTest do
  # docs/design/liquid-core.md §10.15: Core Lint and the evaluator agree.
  # If Lint gives a closed term a type, evaluating it never goes wrong.
  use ExUnit.Case, async: true

  alias Vaisto.Liquid.{Canonical, Eval, Gen, Lint}

  @terms 1000
  @mutants_per_term 20
  @depth 4

  setup_all do
    %{decls: Gen.decls()}
  end

  test "generated terms: Lint accepts every one at the type it was generated for", %{decls: decls} do
    failures =
      for seed <- 1..@terms, {term, type} = generated(seed), Lint.synth(term, types: decls) != {:ok, type} do
        {seed, type, term, Lint.synth(term, types: decls)}
      end

    assert failures == [], report("generator and Lint disagree", failures)
  end

  test "safety: no generated term goes wrong", %{decls: decls} do
    failures =
      for seed <- 1..@terms,
          {term, type} = generated(seed),
          problem = goes_wrong({:ok, type}, outcome(term, decls), decls) do
        {seed, problem, term}
      end

    assert failures == [], report("generated terms went wrong", failures)
  end

  test "mutants: Lint rejects each one, or it does not go wrong at the type Lint gives it", %{decls: decls} do
    results =
      for seed <- 1..@terms, m <- 1..@mutants_per_term do
        :rand.seed(:exsss, {seed, m, 99})
        {term, _type} = generated(seed)
        mutant = Gen.mutate(term)

        case lint(mutant, decls) do
          {:crashed, message} -> {:lint_crashed, message, {seed, m}, mutant}
          {:error, _} -> :rejected
          synth -> {:accepted, goes_wrong(synth, outcome(mutant, decls), decls), {seed, m}, mutant}
        end
      end

    # Lint is total: on any tree it answers, it never raises.
    crashes = for {:lint_crashed, message, at, mutant} <- results, do: {at, message, mutant}
    assert crashes == [], report("mutants made Lint raise instead of answering", crashes)

    failures = for {:accepted, problem, at, mutant} <- results, problem, do: {at, problem, mutant}
    assert failures == [], report("Lint accepted mutants that went wrong", failures)

    # A sanity check on the mutator itself: most corruptions must be type errors,
    # or the test above would be exercising almost nothing.
    rejected = Enum.count(results, &(&1 == :rejected))
    assert rejected / length(results) > 0.5, "only #{rejected} of #{length(results)} mutants were rejected"
  end

  defp lint(term, decls) do
    Lint.synth(term, types: decls)
  rescue
    e -> {:crashed, Exception.format(:error, e, __STACKTRACE__) |> String.slice(0, 600)}
  end

  defp generated(seed) do
    :rand.seed(:exsss, {seed, 2026, 10})
    type = Enum.random(Gen.types())
    {Gen.term(type, @depth), type}
  end

  # Evaluate in a separate process, bounded in time and memory: a mutant may
  # loop, and not ending is allowed (§10.15).
  defp outcome(term, decls) do
    parent = self()

    {pid, ref} =
      spawn_monitor(fn ->
        Process.flag(:max_heap_size, %{size: 4_000_000, kill: true, error_logger: false})

        result =
          try do
            Eval.eval(term, types: decls)
          rescue
            e -> {:went_wrong, Exception.format(:error, e) |> String.slice(0, 300)}
          end

        send(parent, {self(), result})
      end)

    receive do
      {^pid, result} ->
        Process.demonitor(ref, [:flush])
        result

      {:DOWN, ^ref, _, _, _} ->
        :did_not_end
    after
      2_000 ->
        Process.exit(pid, :kill)
        Process.demonitor(ref, [:flush])
        :did_not_end
    end
  end

  # nil when the outcome is one §10.15 allows; otherwise what went wrong.
  defp goes_wrong(synth, outcome, decls) do
    case {synth, outcome} do
      {_, {:went_wrong, message}} -> "the evaluator stopped: #{message}"
      {_, :did_not_end} -> nil
      {_, {:crash, reason}} -> if Eval.value_of?(reason, [:Reason]), do: nil, else: "crash reason #{inspect(reason)} is not a (Reason)"
      {{:ok, type}, {:ok, v}} -> if Eval.value_of?(v, type, types: decls), do: nil, else: "#{inspect(v, limit: 8)} is not a value of #{Canonical.render(type)}"
      {:none, {:ok, v}} -> "a term with no type returned #{inspect(v, limit: 8)}"
    end
  end

  defp report(what, failures) do
    shown =
      failures
      |> Enum.take(3)
      |> Enum.map_join("\n\n", fn f -> f |> Tuple.to_list() |> Enum.map_join("\n", &show/1) end)

    "#{length(failures)} #{what}. The first ones:\n\n#{shown}"
  end

  defp show(x) when is_list(x), do: Canonical.render(x)
  defp show(x), do: inspect(x, limit: 12)
end
