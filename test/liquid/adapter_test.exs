defmodule Vaisto.Liquid.AdapterTest do
  # The Phase 0 adapter (liquid-vaisto-rfc.md §9.3): today's typed AST to Core.
  # Its requirement is that what it produces is Core that Lint accepts
  # (liquid-core.md §10) and that means what the program means (§6): these
  # tests check both, the second by evaluating `main`.
  use ExUnit.Case, async: true

  alias Vaisto.Liquid.{Adapter, Canonical, Eval, Lint}

  defp core(source) do
    {:ok, _type, typed} = Vaisto.TypeChecker.check(Vaisto.Parser.parse(source))
    {:ok, core, skipped} = Adapter.module(typed)
    {core, skipped}
  end

  # The program's Core passes Lint, and `main` evaluates to `expected`.
  defp means(source, expected) do
    {core, skipped} = core(source)
    assert skipped == []
    assert Lint.check_module(core) == :ok, "Lint rejects:\n#{Canonical.render(core)}\n#{inspect(Lint.check_module(core))}"
    assert Eval.run(core, :main, []) == expected
    core
  end

  defp defn(core, name), do: Enum.find(core, &match?([:def, ^name | _], &1))

  describe "definitions and calls" do
    test "a monomorphic function and its call" do
      core = means("(defn add [x :int y :int] :int (+ x y))\n(defn main [] :int (add 1 2))", {:ok, 3})
      assert [:def, :add, [:->, [:Int, :Int], [:eff, :closed], :Int], [:fn, [[:x, :Int], [:y, :Int]], _]] = defn(core, :add)
    end

    test "a function is polymorphic in its parameters' type variables; a call instantiates it" do
      core = means("(defn id [x] x)\n(defn main [] :int (id 3))", {:ok, 3})
      assert [:def, :id, [:forall, [[_a, :Type]], _], _] = defn(core, :id)
      assert [:def, :main, _, [:fn, [], [:app, [:inst, :id, :Int], 3]]] = defn(core, :main)
    end

    test "a let-bound polymorphic lambda is instantiated at its use" do
      means("(defn main [] :int (let [id (fn [x] x)] (id 1)))", {:ok, 1})
    end

    test "lambdas and their application" do
      means("(defn main [] :int (let [f (fn [a] (* a 2))] (f 21)))", {:ok, 42})
    end

    test "a guard becomes a guarded clause; a failed guard crashes with no_match" do
      source = "(defn neg [x :int :when (< x 0)] :int (- 0 x))\n"
      means(source <> "(defn main [] :int (neg -5))", {:ok, 5})
      means(source <> "(defn main [] :int (neg 5))", {:crash, :no_match})
    end

    test "a multi-clause function becomes a match on its argument" do
      {core, []} = core("(defn f\n  [0 :zero]\n  [n :other])\n(defn main [] :atom (f 0))")
      assert [:def, :f, _, [:fn, [[:arg, _]], [:match, :arg, [0, [:atom, :zero]], [:n, [:atom, :other]]]]] = defn(core, :f)

      # HM types every multi-clause function `:any -> :any` today, so its
      # result is Dyn and Lint names it: Phase 1a work, not an adapter bug.
      assert {:error, [%{in: :f, message: "expected Dyn, found Atom"} | _]} = Lint.check_module(core)
    end
  end

  describe "data" do
    test "sums recover their type arguments from the declaration" do
      core = means("(deftype Result (Ok v) (Err e))\n(defn main [] :int (match (Ok 1) [(Ok v) v] [(Err e) 0]))", {:ok, 1})
      assert [:type, :Result, [_, _], [:sum, [:Ok, [:tvar, _]], [:Err, [:tvar, _]]]] = Enum.find(core, &match?([:type | _], &1))
    end

    test "records, construction by position, and field access" do
      means("(deftype Point [x :int y :int])\n(defn main [] :int (. (Point 1 2) :y))", {:ok, 2})
    end

    test "a let that binds by a pattern" do
      means("(deftype Point [x :int y :int])\n(defn main [] :int (let [(Point x y) (Point 3 7)] (+ x y)))", {:ok, 10})
    end

    test "nested and as patterns" do
      means("(deftype R (Ok :int) (Err :string))\n(deftype M (Some R) (None))\n(defn main [] :int (match (Some (Ok 7)) [(Some (Ok v)) v] [_ 0]))", {:ok, 7})
      means("(deftype R (Ok :int) (Err :string))\n(defn main [] :int (match (Ok 4) [(r @ (Ok v)) v] [_ 0]))", {:ok, 4})
    end

    test "lists, tuples and their patterns" do
      means("(defn main [] :int (let [xs [1 2 3] n (length xs)] (if (> n 2) (head xs) 0)))", {:ok, 1})
      means("(defn main [] :int (match (tuple 1 \"a\") [(tuple a b) a]))", {:ok, 1})
      means("(defn main [] :int (match [1 2] [[] 0] [[h | t] h]))", {:ok, 1})
    end
  end

  describe "operators" do
    test "Int and Float arithmetic, with an Int promoted beside a Float as Erlang does" do
      means("(defn main [] :float (+ 1.5 2))", {:ok, 3.5})
      means("(defn main [] :int (div -7 2))", {:ok, -3})
      means("(defn main [] :float (/ 7 2))", {:ok, 3.5})
    end

    test "and and or short-circuit (D5)" do
      means("(defn chk [x :int y :int] :bool (and (!= y 0) (> (div x y) 1)))\n(defn main [] :bool (chk 10 0))", {:ok, false})
    end

    test "try catches a crash with handle-crash" do
      means("(defn main [] :int (try (div 1 0) [catch [:error e 0]]))", {:ok, 0})
    end

    test "a match with no matching clause crashes with no_match" do
      means("(defn f [n :int] :int (match n [0 1]))\n(defn main [] :int (f 5))", {:crash, :no_match})
    end
  end

  describe "higher-order builtins come from a prelude written in Core" do
    test "map, filter, fold and flat_map" do
      core = means("(defn dbl [x :int] :int (* x 2))\n(defn main [] (List :int) (map dbl [1 2 3]))", {:ok, [2, 4, 6]})
      assert defn(core, :"prelude.map")
      refute defn(core, :"prelude.fold")

      means("(defn main [] (List :int) (filter (fn [x] (> x 1)) [1 2 3]))", {:ok, [2, 3]})
      means("(defn main [] :int (fold + 0 [1 2 3 4]))", {:ok, 10})
      means("(defn twice [x :int] (List :int) [x x])\n(defn main [] (List :int) (flat_map twice [1 2]))", {:ok, [1, 1, 2, 2]})
    end

    test "a lambda HM falls back on types its parameter Dyn, and Lint names it (RFC §15.4)" do
      {core, []} = core("(defn main [] (List :int) (flat_map (fn [x] [x x]) [1 2]))")
      assert {:error, [%{in: :main, message: message}]} = Lint.check_module(core)
      assert message =~ "a fn with parameters (Dyn)"
    end
  end

  describe "binders" do
    test "are renamed apart, as Lint requires (liquid-core.md §10.12)" do
      core = means("(defn main [] :int (let [x 1] (let [x (+ x 1)] x)))", {:ok, 2})
      assert [:def, :main, _, [:fn, [], [:let, [[:x, :Int, 1]], [:let, [[:"x'2", :Int, _]], :"x'2"]]]] = defn(core, :main)
    end
  end

  describe "what HM leaves untyped reaches Lint" do
    test "arithmetic on an unannotated parameter (D17)" do
      {core, []} = core("(defn double [x] (* x 2))\n(defn main [] (double 21))")
      assert {:error, [%{in: :double, message: message}]} = Lint.check_module(core)
      assert message =~ "expected Int, found (tvar"
    end

    test "a match on an unannotated parameter (D22)" do
      {core, []} = core("(deftype Result (Ok v) (Err e))\n(defn f [r] :int (match r [(Ok v) v] [(Err e) 0]))\n(defn main [] :int (f (Ok 1)))")
      assert {:error, [%{in: :f, message: message}]} = Lint.check_module(core)
      assert message =~ "pattern of (Result"
    end
  end

  describe "outside the pure fragment of Core version 0" do
    test "a definition is skipped with its reason, and so is every caller" do
      {core, skipped} = core("(defn s [] :string (str 1))\n(defn t [] :string (s))\n(defn main [] :int 1)")
      assert [{:s, "the builtin :str"}, {:t, "calls a definition outside the fragment"}] = skipped
      assert Lint.check_module(core) == :ok
      assert Eval.run(core, :main, []) == {:ok, 1}
    end
  end
end
