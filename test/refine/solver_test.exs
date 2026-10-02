defmodule Vaisto.Refine.SolverTest do
  # Expectations come from the solver contract of Phase 2.1 and from
  # liquid-vaisto-rfc.md §4.3, §11 (interface, trust boundary, vacuity),
  # §14 (C10, C13) and Appendix A (truncating division).
  #
  # Tests tagged :z3 run the real solver. They are not excluded by default:
  # without z3 on PATH they fail, as RFC §14 requires.
  use ExUnit.Case, async: true

  alias Vaisto.Refine.Solver
  alias Vaisto.Refine.Solver.{Script, Unknown, Z3}
  alias Vaisto.Refine.VC

  defp vc(id, goal, hyps \\ [], kind \\ :postcondition), do: %VC{id: id, kind: kind, hyps: hyps, goal: goal}

  defp var(name, sort \\ :int), do: {:var, name, sort}
  defp int(n), do: {:int, n}
  defp gt(a, b), do: {:lt, b, a}
  defp ge(a, b), do: {:le, b, a}
  defp len(l), do: {:len, l}
  defp sub(a, b), do: {:arith, :sub, [a, b]}
  defp mul(a, b), do: {:arith, :mul, [a, b]}

  # Fermat's last theorem for cubes: valid, and nonlinear far beyond what Z3 decides.
  defp fermat_cubes(id) do
    [x, y, z] = for n <- [:x, :y, :z], do: var(n)
    cube = fn v -> mul(mul(v, v), v) end
    vc(id, {:not, {:eq, {:arith, :add, [cube.(x), cube.(y)]}, cube.(z)}}, [gt(x, int(0)), gt(y, int(0)), gt(z, int(0))])
  end

  describe "Z3" do
    @describetag :z3

    setup do
      assert Z3.available?(), "refinement checking needs the z3 solver, which was not found on PATH"
      :ok
    end

    test "identity parses z3 --version" do
      assert {"z3", version} = Z3.identity()
      assert version =~ ~r/\A\d+\.\d+/
    end

    test "tdiv and trem truncate toward zero on all four sign combinations" do
      x = var(:x)
      y = var(:y)

      cases = [{7, 2, 3}, {-7, 2, -3}, {7, -2, -3}, {-7, -2, 3}]

      vcs =
        for {a, b, quotient} <- cases, kind <- [:tdiv, :trem] do
          expected = if kind == :tdiv, do: quotient, else: rem(a, b)
          vc({a, b, kind}, {:eq, {:arith, kind, [x, y]}, int(expected)}, [{:eq, x, int(a)}, {:eq, y, int(b)}])
        end

      for {id, result} <- Z3.check(vcs, []) do
        assert result == :valid, "#{inspect(id)}: #{inspect(result)}"
      end
    end

    test "tdiv is not SMT-LIB's Euclidean div" do
      # Appendix A: SMT-LIB gives (div -7 2) = -4; Vaisto gives -3.
      x = var(:x)
      assert [{:euclid, {:invalid, _}}] = Z3.check([vc(:euclid, {:eq, {:arith, :tdiv, [x, int(2)]}, int(-4)}, [{:eq, x, int(-7)}])], [])
    end

    test "C10: half-up holds and half-floor does not under truncating division" do
      x = var(:x)
      r = {:arith, :tdiv, [x, int(2)]}
      negative = [{:lt, x, int(0)}]

      assert [{:half_up, :valid}, {:half_floor, {:invalid, %{x: _}}}] =
               Z3.check([vc(:half_up, ge(mul(int(2), r), x), negative), vc(:half_floor, {:le, mul(int(2), r), x}, negative)], [])
    end

    test "a valid linear VC" do
      x = var(:x)
      y = var(:y)
      goal = {:and, [gt(y, int(0)), ge(sub(y, x), int(1))]}
      assert [{:linear, :valid}] = Z3.check([vc(:linear, goal, [gt(x, int(0)), gt(y, x)])], [])
    end

    test "an invalid VC gives a counterexample with an integer for the right variable" do
      n = var(:"total n")
      m = var(:m)

      assert [{:bad, {:invalid, model}}] = Z3.check([vc(:bad, gt(n, int(10)), [gt(n, int(0)), {:eq, m, n}])], [])
      assert is_integer(model[:"total n"])
      assert is_integer(model[:m])
    end

    test "negative integers and booleans in a counterexample" do
      # In pure linear arithmetic every model satisfies the hypotheses, so
      # these values are forced; the counterexample is otherwise advisory.
      x = var(:x)
      p = var(:p, :bool)

      assert [{:neg, {:invalid, %{x: value}}}, {:bool, {:invalid, %{p: false}}}] =
               Z3.check([vc(:neg, ge(x, int(-5)), [{:lt, x, int(-5)}]), vc(:bool, p)], [])

      assert is_integer(value) and value < -5
    end

    test "a list in a counterexample is its raw text" do
      xs = var(:xs, :list)
      assert [{:list, {:invalid, %{xs: value}}}] = Z3.check([vc(:list, {:eq, len(xs), int(0)})], [])
      assert is_binary(value)
    end

    test "len: from len(t) = len(xs) - 1 and len(xs) > 0 follows len(t) >= 0" do
      t = var(:t, :list)
      xs = var(:xs, :list)
      hyps = [{:eq, len(t), sub(len(xs), int(1))}, gt(len(xs), int(0))]
      assert [{:tail, :valid}] = Z3.check([vc(:tail, ge(len(t), int(0)), hyps)], [])
    end

    test "len(xs) >= 0 for any xs, which only the axiom gives" do
      xs = var(:xs, :list)
      assert [{:axiom, :valid}, {:nonempty, {:invalid, _}}] =
               Z3.check([vc(:axiom, ge(len(xs), int(0))), vc(:nonempty, gt(len(xs), int(0)))], [])
    end

    test "vacuity: contradictory requirements are unsatisfiable, so the false goal is valid" do
      x = var(:x)
      assert [{:never, :valid}, {:possible, {:invalid, _}}] =
               Z3.check(
                 [
                   vc(:never, {:bool, false}, [gt(x, int(0)), {:lt, x, int(0)}], :vacuity),
                   vc(:possible, {:bool, false}, [gt(x, int(0))], :vacuity)
                 ],
                 []
               )
    end

    test "several VCs in one call each get a fresh context, answered in input order" do
      p = var(:p, :bool)
      x = var(:x)

      # The first VC declares x_1 as a Bool and asserts a contradiction. If
      # either leaked, the second (x_1 as an Int, no hypotheses) would fail
      # to declare or would be vacuously valid.
      vcs = [
        vc(:contradiction, {:bool, false}, [p, {:not, p}], :contradiction),
        vc(:open, {:lt, x, int(0)}),
        vc(:closed, {:lt, x, {:arith, :add, [x, int(1)]}})
      ]

      assert [{:contradiction, :valid}, {:open, {:invalid, %{x: _}}}, {:closed, :valid}] = Z3.check(vcs, [])
    end

    test "a tiny rlimit on a hard nonlinear VC is unknown, not valid" do
      assert [{:fermat, {:unknown, reason}}] = Z3.check([fermat_cubes(:fermat)], rlimit: 1_000, timeout: 30_000)
      # the deterministic limit stopped it, not the wall clock
      refute reason == :timeout
    end

    test "an error from Z3 is unknown, even when Z3 also answers unsat" do
      # A negative rlimit is rejected by Z3, which then checks the trivially valid query anyway.
      assert [{:trivial, {:unknown, _reason}}] = Z3.check([vc(:trivial, {:bool, true})], rlimit: -1)
    end

    test "the wall-clock backstop gives :timeout, and later VCs are still answered" do
      x = var(:x)

      assert [{:fermat, {:unknown, :timeout}}, {:after, :valid}] =
               Z3.check([fermat_cubes(:fermat), vc(:after, {:eq, x, x})], rlimit: 0, timeout: 300)
    end
  end

  describe "Z3 without a solver binary" do
    test "every VC is {:unknown, :solver_missing}" do
      vcs = [vc(:a, {:bool, true}), vc(:b, {:bool, false})]
      assert Z3.check(vcs, z3: "/nonexistent/z3") == [a: {:unknown, :solver_missing}, b: {:unknown, :solver_missing}]
      refute Z3.available?(z3: "/nonexistent/z3")
    end

    test "a solver process that dies is unknown for every VC" do
      dies = System.find_executable("false")
      vcs = [vc(:a, {:bool, true}), vc(:b, {:bool, true})]
      assert [{:a, {:unknown, _}}, {:b, {:unknown, _}}] = Z3.check(vcs, z3: dies)
    end
  end

  describe "Unknown" do
    test "answers {:unknown, :test} to everything, in input order" do
      vcs = [vc(:trivial, {:bool, true}), vc(:false_goal, {:bool, false}), vc({:nested, 1}, {:lt, var(:x), int(0)})]

      assert Unknown.check(vcs, []) == [
               {:trivial, {:unknown, :test}},
               {:false_goal, {:unknown, :test}},
               {{:nested, 1}, {:unknown, :test}}
             ]

      assert Unknown.check([], []) == []
    end

    test "is a solver with an identity" do
      assert Solver in Unknown.module_info(:attributes)[:behaviour]
      identity = Unknown.identity()
      assert tuple_size(identity) == 2
      assert identity |> Tuple.to_list() |> Enum.all?(&is_binary/1)
    end
  end

  describe "Script" do
    test "answers from opts[:answers] by VC id, in input order" do
      vcs = [vc(:a, {:bool, false}), vc(:b, {:bool, true}), vc(:c, {:bool, true})]
      answers = %{a: :valid, b: {:invalid, %{x: 1}}, c: {:unknown, :resource_limit}}

      assert Script.check(vcs, answers: answers) == [a: :valid, b: {:invalid, %{x: 1}}, c: {:unknown, :resource_limit}]
    end

    test "an unscripted id is {:unknown, :unscripted}" do
      vcs = [vc(:a, {:bool, true}), vc(:missing, {:bool, true})]
      assert Script.check(vcs, answers: %{a: :valid}) == [a: :valid, missing: {:unknown, :unscripted}]
      assert Script.check(vcs, []) == [a: {:unknown, :unscripted}, missing: {:unknown, :unscripted}]
    end

    test "is a solver with an identity" do
      assert Solver in Script.module_info(:attributes)[:behaviour]
      identity = Script.identity()
      assert tuple_size(identity) == 2
      assert identity |> Tuple.to_list() |> Enum.all?(&is_binary/1)
    end
  end
end
