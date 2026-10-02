defmodule Vaisto.Refine.SMTLIBTest do
  # Expectations come from the Phase 2.1 lowering contract and from
  # liquid-vaisto-rfc.md §4.3 (admission table), §5.5, §11 and Appendix A.
  use ExUnit.Case, async: true

  alias Vaisto.Refine.{SMTLIB, VC}

  @tdiv """
  (define-fun tdiv ((x Int) (y Int)) Int
    (ite (>= x 0) (ite (> y 0) (div x y) (- (div x (- y))))
                  (ite (> y 0) (- (div (- x) y)) (div (- x) (- y)))))
  """

  @trem "(define-fun trem ((x Int) (y Int)) Int (- x (* y (tdiv x y))))\n"

  defp vc(goal, hyps \\ [], vars \\ []), do: %VC{id: :t, kind: :postcondition, hyps: hyps, goal: goal, vars: vars}

  defp x, do: {:var, :x, :int}
  defp y, do: {:var, :y, :int}
  defp p, do: {:var, :p, :bool}
  defp q, do: {:var, :q, :bool}
  defp xs, do: {:var, :xs, :list}
  defp int(n), do: {:int, n}

  defp text(vc), do: SMTLIB.lower(vc).text

  # The negated goal of a VC whose goal is `goal`, as a fragment of the query.
  defp negated(goal), do: "(assert (not #{goal}))\n"

  defp list_sort(text) do
    [_, sort] = Regex.run(~r/\(declare-sort (\S+) 0\)/, text)
    sort
  end

  defp count(text, fragment), do: length(String.split(text, fragment)) - 1

  describe "one lowering per IR node" do
    test "an Int variable is declared and renamed" do
      lowered = SMTLIB.lower(vc({:lt, x(), int(0)}))
      assert lowered.text =~ "(declare-const x_1 Int)\n"
      assert lowered.text =~ negated("(< x_1 0)")
      assert lowered.names == %{"x_1" => :x}
    end

    test "a Bool variable is declared and can be a predicate by itself" do
      t = text(vc(p()))
      assert t =~ "(declare-const x_1 Bool)\n"
      assert t =~ negated("x_1")
    end

    test "a List variable is declared at the list sort" do
      t = text(vc({:eq, xs(), xs()}))
      assert t =~ "(declare-const x_1 #{list_sort(t)})\n"
      assert t =~ negated("(= x_1 x_1)")
    end

    test "integer literals, exact and unbounded" do
      assert text(vc({:lt, x(), int(5)})) =~ negated("(< x_1 5)")
      assert text(vc({:lt, x(), int(0)})) =~ negated("(< x_1 0)")
      big = Integer.pow(2, 70)
      assert text(vc({:lt, x(), int(big)})) =~ negated("(< x_1 #{big})")
    end

    test "negative literals are (- n), never -n" do
      t = text(vc({:lt, x(), int(-5)}, [{:le, int(-123_456_789_012_345_678_901), x()}]))
      assert t =~ negated("(< x_1 (- 5))")
      assert t =~ "(assert (<= (- 123456789012345678901) x_1))\n"
      refute t =~ ~r/[ (]-[0-9]/
    end

    test "boolean literals" do
      assert text(vc({:bool, true})) =~ negated("true")
      assert text(vc({:bool, false})) =~ negated("false")
    end

    test "add, sub, mul and neg" do
      assert text(vc({:eq, {:arith, :add, [x(), y()]}, int(1)})) =~ negated("(= (+ x_1 x_2) 1)")
      assert text(vc({:eq, {:arith, :sub, [x(), y()]}, int(1)})) =~ negated("(= (- x_1 x_2) 1)")
      assert text(vc({:eq, {:arith, :mul, [x(), y()]}, int(1)})) =~ negated("(= (* x_1 x_2) 1)")
      assert text(vc({:eq, {:arith, :neg, [x()]}, int(1)})) =~ negated("(= (- x_1) 1)")
    end

    test "tdiv and trem lower to the truncating functions of Appendix A" do
      assert text(vc({:eq, {:arith, :tdiv, [x(), y()]}, int(1)})) =~ negated("(= (tdiv x_1 x_2) 1)")
      assert text(vc({:eq, {:arith, :trem, [x(), y()]}, int(1)})) =~ negated("(= (trem x_1 x_2) 1)")
    end

    test "ite, at Int and at Bool" do
      assert text(vc({:eq, {:ite, p(), x(), int(0)}, int(1)})) =~ negated("(= (ite x_1 x_2 0) 1)")
      assert text(vc({:ite, p(), q(), {:bool, false}})) =~ negated("(ite x_1 x_2 false)")
    end

    test "len" do
      assert text(vc({:lt, {:len, xs()}, int(3)})) =~ negated("(< (len x_1) 3)")
    end

    test "eq at each sort" do
      assert text(vc({:eq, x(), y()})) =~ negated("(= x_1 x_2)")
      assert text(vc({:eq, p(), q()})) =~ negated("(= x_1 x_2)")
      assert text(vc({:eq, xs(), {:var, :ys, :list}})) =~ negated("(= x_1 x_2)")
    end

    test "lt and le" do
      assert text(vc({:lt, x(), y()})) =~ negated("(< x_1 x_2)")
      assert text(vc({:le, x(), y()})) =~ negated("(<= x_1 x_2)")
    end

    test "not, implies and iff" do
      assert text(vc({:not, p()})) =~ negated("(not x_1)")
      assert text(vc({:implies, p(), q()})) =~ negated("(=> x_1 x_2)")
      assert text(vc({:iff, p(), q()})) =~ negated("(= x_1 x_2)")
    end

    test "and and or" do
      assert text(vc({:and, [p(), q(), {:lt, x(), int(1)}]})) =~ negated("(and x_1 x_2 (< x_3 1))")
      assert text(vc({:or, [p(), q()]})) =~ negated("(or x_1 x_2)")
    end

    test "the empty and is true and the empty or is false" do
      assert text(vc({:and, []})) =~ negated("true")
      assert text(vc({:or, []})) =~ negated("false")
    end
  end

  describe "the query" do
    test "asserts each hypothesis in order, then the negated goal, then check-sat" do
      h1 = {:lt, int(0), x()}
      h2 = {:lt, x(), y()}
      t = text(vc({:lt, int(0), y()}, [h1, h2]))

      assert t =~ "(assert (< 0 x_1))\n(assert (< x_1 x_2))\n(assert (not (< 0 x_2)))\n(check-sat)\n"
      assert String.ends_with?(t, "(check-sat)\n")
      assert count(t, "(check-sat)") == 1
    end

    test "the goal is negated whole, never weakened or split" do
      goal = {:and, [{:lt, int(0), x()}, {:implies, p(), {:le, x(), int(9)}}]}
      t = text(vc(goal, [{:lt, int(-3), x()}]))

      assert t =~ negated("(and (< 0 x_1) (=> x_2 (<= x_1 9)))")
      # one assert for the hypothesis, one for the negated goal; no conjunct dropped
      assert count(t, "(assert ") == 2
    end

    test "a VC with no hypotheses and no variables" do
      assert text(vc({:bool, true})) == "(assert (not true))\n(check-sat)\n"
    end
  end

  describe "naming" do
    test "variables are x_1, x_2, … by first occurrence: hypotheses in order, then the goal, then vars" do
      lowered =
        SMTLIB.lower(
          vc({:eq, {:var, :c, :int}, {:var, :a, :int}}, [{:lt, {:var, :b, :int}, {:var, :a, :int}}], [{:d, :bool}, {:a, :int}])
        )

      assert lowered.names == %{"x_1" => :b, "x_2" => :a, "x_3" => :c, "x_4" => :d}

      assert lowered.text =~
               "(declare-const x_1 Int)\n(declare-const x_2 Int)\n(declare-const x_3 Int)\n(declare-const x_4 Bool)\n"
    end

    test "names that are not SMT symbols are renamed and mapped back" do
      odd = [:"x'", :"total n", :"a|b", :"(weird)", :""]
      goal = {:and, for(name <- odd, do: {:lt, {:var, name, :int}, int(0)})}
      lowered = SMTLIB.lower(vc(goal))

      assert lowered.names |> Map.values() |> Enum.sort() == Enum.sort(odd)
      assert Map.keys(lowered.names) |> Enum.sort() == ["x_1", "x_2", "x_3", "x_4", "x_5"]
      refute lowered.text =~ "total n"
    end

    test "variables only in vars are declared too" do
      lowered = SMTLIB.lower(vc({:bool, true}, [], [{:n, :int}]))
      assert lowered.text =~ "(declare-const x_1 Int)\n"
      assert lowered.names == %{"x_1" => :n}
    end
  end

  describe "lists and len" do
    test "no list sort, no len and no axiom when no list occurs" do
      t = text(vc({:lt, x(), int(0)}))
      refute t =~ "declare-sort"
      refute t =~ "len"
    end

    test "the list sort and len are declared once when a list occurs" do
      t = text(vc({:lt, {:len, xs()}, {:len, {:var, :ys, :list}}}))
      sort = list_sort(t)

      assert count(t, "(declare-sort #{sort} 0)\n") == 1
      assert count(t, "(declare-fun len (#{sort}) Int)\n") == 1
    end

    test "len ≥ 0 is asserted once per list-sorted variable" do
      t = text(vc({:lt, {:len, xs()}, {:len, {:var, :ys, :list}}}, [{:lt, int(0), {:len, xs()}}]))

      assert count(t, "(assert (>= (len x_1) 0))\n") == 1
      assert count(t, "(assert (>= (len x_2) 0))\n") == 1
      assert count(t, "(>= (len") == 2
    end

    test "a list variable listed only in vars still gets the declarations and the axiom" do
      t = text(vc({:bool, true}, [], [{:xs, :list}]))
      assert t =~ "(declare-const x_1 #{list_sort(t)})\n"
      assert t =~ "(assert (>= (len x_1) 0))\n"
    end
  end

  describe "tdiv and trem definitions" do
    test "tdiv is defined exactly as in Appendix A, only when used" do
      t = text(vc({:eq, {:arith, :tdiv, [x(), int(2)]}, int(1)}))
      assert count(t, @tdiv) == 1
      refute t =~ "define-fun trem"
    end

    test "trem brings tdiv, which it is defined with" do
      t = text(vc({:eq, {:arith, :trem, [x(), int(2)]}, int(1)}))
      assert count(t, @tdiv) == 1
      assert count(t, @trem) == 1
      [before_trem, _] = String.split(t, @trem)
      assert before_trem =~ @tdiv
    end

    test "each definition appears once however often it is used" do
      d = {:arith, :tdiv, [x(), int(2)]}
      r = {:arith, :trem, [x(), int(3)]}
      t = text(vc({:eq, {:arith, :add, [d, r]}, d}, [{:lt, r, d}]))
      assert count(t, "(define-fun tdiv") == 1
      assert count(t, "(define-fun trem") == 1
    end

    test "nothing is defined when neither is used" do
      refute text(vc({:eq, {:arith, :mul, [x(), int(2)]}, int(1)})) =~ "define-fun"
    end
  end

  # A VC that uses every sort, a measure, trem, ite and a variable listed in vars.
  defp rich do
    k = {:var, :k, :int}
    b = {:var, :b, :bool}

    %VC{
      id: :rich,
      kind: :precondition,
      vars: [{:k, :int}, {:xs, :list}, {:unused, :int}],
      hyps: [
        {:le, int(0), k},
        {:lt, k, {:len, xs()}},
        {:iff, b, {:eq, {:arith, :trem, [k, int(-2)]}, int(0)}}
      ],
      goal: {:or, [b, {:lt, int(-1), {:ite, b, int(1), {:arith, :neg, [k]}}}]}
    }
  end

  describe "determinism and digests" do
    test "lowering the same VC twice is byte-identical" do
      assert SMTLIB.lower(rich()) == SMTLIB.lower(rich())
      assert SMTLIB.digest(rich()) == SMTLIB.digest(rich())
    end

    test "id, kind and meta are not part of the query" do
      other = %{rich() | id: {:other, 7}, kind: :vacuity, meta: %{span: {1, 2}}}
      assert SMTLIB.lower(other) == SMTLIB.lower(rich())
      assert SMTLIB.digest(other) == SMTLIB.digest(rich())
    end

    test "the digest is the SHA-256 of the text, as lowercase hex" do
      expected = :crypto.hash(:sha256, SMTLIB.lower(rich()).text) |> Base.encode16(case: :lower)
      assert SMTLIB.digest(rich()) == expected
      assert SMTLIB.digest(rich()) =~ ~r/\A[0-9a-f]{64}\z/
    end

    test "a different goal gives a different digest" do
      refute SMTLIB.digest(%{rich() | goal: {:bool, true}}) == SMTLIB.digest(rich())
    end
  end

  describe "ill-formed VCs raise ArgumentError" do
    test "unsupported nodes" do
      for bad <- [
            {:measure, :len, x()},
            {:int, 1.5},
            {:bool, :maybe},
            {:var, "x", :int},
            {:var, :x, :float},
            {:arith, :pow, [x(), y()]},
            {:arith, :add, [x()]},
            {:arith, :neg, [x(), y()]},
            {:and, :not_a_list},
            {:and, [p() | q()]},
            :x,
            nil
          ] do
        assert_raise ArgumentError, fn -> SMTLIB.lower(vc({:eq, {:ite, {:bool, true}, bad, bad}, bad})) end
        assert_raise ArgumentError, fn -> SMTLIB.lower(vc({:bool, true}, [{:eq, bad, bad}])) end
      end
    end

    test "ill-sorted nodes" do
      for bad <- [
            {:lt, p(), int(1)},
            {:le, int(1), xs()},
            {:arith, :add, [p(), int(1)]},
            {:len, x()},
            {:eq, x(), p()},
            {:eq, xs(), x()},
            {:ite, x(), p(), q()},
            {:ite, p(), x(), q()},
            {:not, x()},
            {:and, [p(), int(1)]},
            {:or, [xs()]},
            {:implies, p(), x()},
            {:iff, x(), p()},
            x(),
            int(1)
          ] do
        assert_raise ArgumentError, fn -> SMTLIB.lower(vc(bad)) end
        assert_raise ArgumentError, fn -> SMTLIB.lower(vc({:bool, true}, [bad])) end
      end
    end

    test "an unsupported node inside the goal is not dropped, so the goal is not weakened" do
      goal = {:and, [{:lt, x(), int(1)}, {:measure, :sum, xs()}]}
      assert_raise ArgumentError, fn -> SMTLIB.lower(vc(goal)) end
      assert_raise ArgumentError, fn -> SMTLIB.lower(vc({:or, [{:not, goal}, p()]})) end
    end

    test "one name at two sorts" do
      assert_raise ArgumentError, fn -> SMTLIB.lower(vc({:eq, {:var, :x, :int}, {:var, :x, :int}}, [{:var, :x, :bool}])) end
      assert_raise ArgumentError, fn -> SMTLIB.lower(vc({:lt, x(), int(0)}, [], [{:x, :bool}])) end
    end

    test "malformed vars and hyps" do
      assert_raise ArgumentError, fn -> SMTLIB.lower(vc({:bool, true}, [], [{:x, :real}])) end
      assert_raise ArgumentError, fn -> SMTLIB.lower(vc({:bool, true}, [], [:x])) end
      assert_raise ArgumentError, fn -> SMTLIB.lower(vc({:bool, true}, [], :x)) end
      assert_raise ArgumentError, fn -> SMTLIB.lower(vc({:bool, true}, {:lt, x(), y()})) end
    end
  end
end
