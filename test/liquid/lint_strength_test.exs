defmodule Vaisto.Liquid.LintStrengthTest do
  # Core Lint is the trusted check on everything that produces Core (RFC §9.2),
  # so it must hold on real Core, not only on generated terms (soundness_test.exs):
  #
  #   1. Lint is total: on any module, declarations included, and on any tree,
  #      it answers and never raises;
  #   2. fault injection on real Core (RFC K5): the elaborator's output for
  #      every program in the test suite is corrupted at random, and a mutant
  #      Lint accepts must still not go wrong (liquid-core.md §10.15).
  use ExUnit.Case, async: true

  alias Vaisto.{Elab, Parser, TypeChecker}
  alias Vaisto.Elab.Shadow
  alias Vaisto.Liquid.{Canonical, Eval, Gen, Harness, Lint}

  @mutants_per_module 300
  @random_trees 2_000

  setup_all do
    corpus = Harness.corpus(Path.wildcard("test/**/*_test.exs") -- Path.wildcard("test/{liquid,refine,elab}/*_test.exs"))

    modules =
      for {_file, source} <- corpus,
          {:ok, ast} <- [Vaisto.Compilation.parse(source)],
          {:ok, _, typed} <- [safe(fn -> TypeChecker.check(ast) end)],
          {:ok, core} <- [Elab.module(Parser.parse(source), suggest: Shadow.suggestions(typed))],
          Lint.check_module(core) == :ok,
          main_type(core) != nil,
          uniq: true,
          do: core

    %{modules: modules}
  end

  test "there is real Core to corrupt", %{modules: modules} do
    assert length(modules) >= 60
  end

  test "Lint is total on corrupted modules, declarations included", %{modules: modules} do
    crashes =
      for {module, i} <- Enum.with_index(modules), m <- 1..@mutants_per_module, reduce: [] do
        acc ->
          :rand.seed(:exsss, {i, m, 7})
          mutant = Gen.mutate(module)

          case lint(mutant) do
            {:crashed, message} -> [{message, mutant} | acc]
            _ -> acc
          end
      end

    assert crashes == [], report("corrupted modules made Lint raise", crashes)
  end

  test "Lint is total on arbitrary trees" do
    crashes =
      for seed <- 1..@random_trees, tree = random_tree(seed), match?({:crashed, _}, lint(tree)), do: {seed, tree}

    assert crashes == [], report("random trees made Lint raise", crashes)
  end

  test "K5: a corrupted module Lint accepts does not go wrong", %{modules: modules} do
    results =
      for {module, i} <- Enum.with_index(modules), m <- 1..@mutants_per_module do
        :rand.seed(:exsss, {i, m, 11})
        mutant = Gen.mutate(module)

        case lint(mutant) do
          :ok -> {:accepted, goes_wrong(mutant), mutant}
          _ -> :rejected
        end
      end

    failures = for {:accepted, problem, mutant} <- results, problem, do: {problem, mutant}
    assert failures == [], report("Lint accepted corrupted modules that went wrong", failures)

    rejected = Enum.count(results, &(&1 == :rejected))
    assert rejected / length(results) > 0.5, "only #{rejected} of #{length(results)} corruptions were rejected"
  end

  # nil when main's outcome is one §10.15 allows at main's declared type.
  defp goes_wrong(module) do
    case main_type(module) do
      nil ->
        nil

      type ->
        decls = for [:type | _] = d <- module, do: d

        case evaluate(module) do
          {:went_wrong, message} -> "the evaluator stopped: #{message}"
          :did_not_end -> nil
          {:crash, reason} -> unless Eval.value_of?(reason, [:Reason]), do: "crash reason #{inspect(reason)} is not a (Reason)"
          {:ok, v} -> unless Eval.value_of?(v, type, types: decls), do: "#{inspect(v, limit: 8)} is not a value of #{Canonical.render(type)}"
        end
    end
  end

  defp main_type([:module, _name, _version | items]) do
    Enum.find_value(items, fn
      [:def, :main, [:->, [], _eff, result], _fn] -> result
      _ -> nil
    end)
  end

  defp main_type(_), do: nil

  # Bounded, short: a corruption may loop.
  defp evaluate(module) do
    parent = self()

    {pid, ref} =
      spawn_monitor(fn ->
        Process.flag(:max_heap_size, %{size: 2_000_000, kill: true, error_logger: false})

        result =
          try do
            Eval.run(module, :main, [])
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
      300 ->
        Process.exit(pid, :kill)
        Process.demonitor(ref, [:flush])
        :did_not_end
    end
  end

  defp lint(tree) do
    Lint.check_module(tree)
  rescue
    e -> {:crashed, Exception.format(:error, e, __STACKTRACE__) |> String.slice(0, 600)}
  catch
    kind, value -> {:crashed, "#{kind}: #{inspect(value, limit: 6)}"}
  end

  # A tree of symbols, numbers, strings and lists, some shaped like a module.
  defp random_tree(seed) do
    :rand.seed(:exsss, {seed, 3, 3})
    body = for _ <- 1..Enum.random(0..4)//1, do: tree(4)
    Enum.random([[:module, :Main, [:"core-version", 0] | body], tree(5), [:module | body]])
  end

  defp tree(0), do: leaf()

  defp tree(depth) do
    case :rand.uniform(3) do
      1 -> leaf()
      _ -> for _ <- 1..Enum.random(0..4)//1, do: tree(depth - 1)
    end
  end

  defp leaf do
    Enum.random([:def, :type, :fn, :app, :match, :inj, :record, :sum, :forall, :->, :eff, :closed, :Int, :Bool, :x, :_, :when, :perform, :crash, 0, -1, 2.5, "s", [], [:unit]])
  end

  defp safe(fun) do
    fun.()
  rescue
    _ -> :error
  end

  defp report(what, failures) do
    shown = failures |> Enum.take(3) |> Enum.map_join("\n\n", fn f -> f |> Tuple.to_list() |> Enum.map_join("\n", &show/1) end)
    "#{length(failures)} #{what}. The first ones:\n\n#{shown}"
  end

  defp show(x) when is_list(x), do: Canonical.render(x)
  defp show(x), do: inspect(x, limit: 12)
end
