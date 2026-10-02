defmodule Vaisto.Refine.SMTLIB do
  @moduledoc """
  The SMT-LIB lowering of verification conditions (RFC §4.3, §11).

  `lower/1` is a pure function from a `Vaisto.Refine.VC` to the text of one
  query: `∧hyps ∧ ¬goal`, then `(check-sat)`. It is deterministic, so the
  same VC always yields byte-identical text and `digest/1` can key a cache
  of answers (RFC §11.1). The VC's `id`, `kind` and `meta` are not part of
  the query.

  The query is laid out in a fixed order:

    1. the list sort and its `len` measure, when a list occurs;
    2. `tdiv` and `trem` (RFC Appendix A), when used;
    3. one declaration per variable, named `x_1, x_2, …` in order of first
       occurrence in the hypotheses, then the goal, then `vars`;
    4. the axiom `(>= (len x) 0)` once per list-sorted variable (RFC §4.3);
    5. one assert per hypothesis, then `(assert (not goal))`, then `(check-sat)`.

  The lowering is total on well-formed VCs and exact. It never approximates:
  an unsupported or ill-sorted node raises `ArgumentError`, because dropping
  or weakening it could turn an unprovable goal into a provable one
  (RFC §11.2).
  """

  alias Vaisto.Refine.VC

  @type lowered :: %{text: String.t(), names: %{String.t() => atom()}}

  @sorts [:int, :bool, :list]

  # Z3 predeclares a parametric `List` datatype and rejects `(declare-sort List 0)`.
  @list_sort "VList"

  @list_preamble """
  (declare-sort #{@list_sort} 0)
  (declare-fun len (#{@list_sort}) Int)
  """

  # Verbatim from RFC Appendix A: SMT-LIB's `div` is Euclidean, Vaisto's truncates.
  @tdiv """
  (define-fun tdiv ((x Int) (y Int)) Int
    (ite (>= x 0) (ite (> y 0) (div x y) (- (div x (- y))))
                  (ite (> y 0) (- (div (- x) y)) (div (- x) (- y)))))
  """

  @trem """
  (define-fun trem ((x Int) (y Int)) Int (- x (* y (tdiv x y))))
  """

  @doc """
  Lower a VC to SMT-LIB text.

  `names` maps each SMT name back to the IR variable it stands for. Raises
  `ArgumentError` on an unsupported or ill-sorted node, or on a variable
  used at two sorts.
  """
  @spec lower(VC.t()) :: lowered()
  def lower(%VC{hyps: hyps, goal: goal, vars: vars}) do
    unless is_list(hyps), do: raise(ArgumentError, "VC hyps must be a list, got: #{inspect(hyps)}")
    unless is_list(vars), do: raise(ArgumentError, "VC vars must be a list, got: #{inspect(vars)}")

    st = %{vars: %{}, order: [], tdiv: false, trem: false}
    {hyps, st} = Enum.map_reduce(hyps, st, &expect(&1, :bool, &2))
    {goal, st} = expect(goal, :bool, st)
    st = Enum.reduce(vars, st, &declare_listed/2)

    declared = Enum.reverse(st.order)
    lists = for {smt, :list} <- declared, do: smt

    text =
      [
        if(lists != [], do: @list_preamble, else: []),
        if(st.tdiv or st.trem, do: @tdiv, else: []),
        if(st.trem, do: @trem, else: []),
        for({smt, sort} <- declared, do: ["(declare-const ", smt, " ", sort_name(sort), ")\n"]),
        for(smt <- lists, do: ["(assert (>= (len ", smt, ") 0))\n"]),
        for(h <- hyps, do: ["(assert ", h, ")\n"]),
        ["(assert (not ", goal, "))\n"],
        "(check-sat)\n"
      ]
      |> IO.iodata_to_binary()

    names = Map.new(st.vars, fn {ir, {smt, _sort}} -> {smt, ir} end)
    %{text: text, names: names}
  end

  @doc "SHA-256 of the lowered text, as lowercase hex."
  @spec digest(VC.t()) :: String.t()
  def digest(%VC{} = vc) do
    :crypto.hash(:sha256, lower(vc).text) |> Base.encode16(case: :lower)
  end

  # --- Terms ----------------------------------------------------------------

  defp expect(term, sort, st) do
    case lower_term(term, st) do
      {smt, ^sort, st} ->
        {smt, st}

      {_smt, other, _st} ->
        raise ArgumentError, "ill-sorted refinement term: expected #{sort}, got #{other} in #{inspect(term)}"
    end
  end

  defp lower_term({:var, name, sort}, st) when is_atom(name) and sort in @sorts do
    {smt, st} = bind(name, sort, st)
    {smt, sort, st}
  end

  defp lower_term({:int, n}, st) when is_integer(n), do: {int_literal(n), :int, st}
  defp lower_term({:bool, b}, st) when is_boolean(b), do: {Atom.to_string(b), :bool, st}

  defp lower_term({:arith, :neg, [a]}, st) do
    {a, st} = expect(a, :int, st)
    {["(- ", a, ")"], :int, st}
  end

  defp lower_term({:arith, op, [a, b]}, st) when op in [:add, :sub, :mul, :tdiv, :trem] do
    {a, st} = expect(a, :int, st)
    {b, st} = expect(b, :int, st)
    st = if op in [:tdiv, :trem], do: Map.put(st, op, true), else: st
    {["(", arith_op(op), " ", a, " ", b, ")"], :int, st}
  end

  defp lower_term({:ite, c, t, e}, st) do
    {c, st} = expect(c, :bool, st)
    {t, sort, st} = lower_term(t, st)
    {e, st} = expect(e, sort, st)
    {["(ite ", c, " ", t, " ", e, ")"], sort, st}
  end

  defp lower_term({:len, l}, st) do
    {l, st} = expect(l, :list, st)
    {["(len ", l, ")"], :int, st}
  end

  defp lower_term({:eq, a, b}, st) do
    {a, sort, st} = lower_term(a, st)
    {b, st} = expect(b, sort, st)
    {["(= ", a, " ", b, ")"], :bool, st}
  end

  defp lower_term({op, a, b}, st) when op in [:lt, :le] do
    {a, st} = expect(a, :int, st)
    {b, st} = expect(b, :int, st)
    {["(", if(op == :lt, do: "<", else: "<="), " ", a, " ", b, ")"], :bool, st}
  end

  defp lower_term({:not, p}, st) do
    {p, st} = expect(p, :bool, st)
    {["(not ", p, ")"], :bool, st}
  end

  defp lower_term({op, ps}, st) when op in [:and, :or] and is_list(ps) do
    {ps, st} = expect_all(ps, ps, st)

    smt =
      case ps do
        [] -> if op == :and, do: "true", else: "false"
        [p] -> p
        ps -> ["(", Atom.to_string(op), Enum.map(ps, &[" ", &1]), ")"]
      end

    {smt, :bool, st}
  end

  defp lower_term({:implies, p, q}, st) do
    {p, st} = expect(p, :bool, st)
    {q, st} = expect(q, :bool, st)
    {["(=> ", p, " ", q, ")"], :bool, st}
  end

  defp lower_term({:iff, p, q}, st) do
    {p, st} = expect(p, :bool, st)
    {q, st} = expect(q, :bool, st)
    {["(= ", p, " ", q, ")"], :bool, st}
  end

  defp lower_term(term, _st), do: raise(ArgumentError, "unsupported refinement term: #{inspect(term)}")

  defp expect_all([], _whole, st), do: {[], st}

  defp expect_all([p | rest], whole, st) do
    {p, st} = expect(p, :bool, st)
    {rest, st} = expect_all(rest, whole, st)
    {[p | rest], st}
  end

  defp expect_all(_improper, whole, _st), do: raise(ArgumentError, "improper list of predicates: #{inspect(whole)}")

  defp arith_op(:add), do: "+"
  defp arith_op(:sub), do: "-"
  defp arith_op(:mul), do: "*"
  defp arith_op(:tdiv), do: "tdiv"
  defp arith_op(:trem), do: "trem"

  # SMT-LIB has no negative numerals: -5 is the application (- 5).
  defp int_literal(n) when n < 0, do: ["(- ", Integer.to_string(-n), ")"]
  defp int_literal(n), do: Integer.to_string(n)

  # --- Variables ------------------------------------------------------------

  defp declare_listed({name, sort}, st) when is_atom(name) and sort in @sorts do
    {_smt, st} = bind(name, sort, st)
    st
  end

  defp declare_listed(entry, _st), do: raise(ArgumentError, "malformed VC var: #{inspect(entry)}")

  defp bind(name, sort, st) do
    case st.vars do
      %{^name => {smt, ^sort}} ->
        {smt, st}

      %{^name => {_smt, other}} ->
        raise ArgumentError, "variable #{inspect(name)} is used at sorts #{other} and #{sort}"

      vars ->
        smt = "x_" <> Integer.to_string(map_size(vars) + 1)
        {smt, %{st | vars: Map.put(vars, name, {smt, sort}), order: [{smt, sort} | st.order]}}
    end
  end

  defp sort_name(:int), do: "Int"
  defp sort_name(:bool), do: "Bool"
  defp sort_name(:list), do: @list_sort
end
