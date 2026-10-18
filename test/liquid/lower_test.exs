defmodule Vaisto.Liquid.LowerTest do
  use ExUnit.Case, async: true

  alias Vaisto.{Elab, Parser}
  alias Vaisto.Liquid.{Canonical, Eval, Gen, Harness, Lower, Runtime}

  defp core(text), do: Canonical.parse!(text, atoms: :create)

  defp native(core, opts \\ []) do
    name = :"LiquidNative#{System.unique_integer([:positive])}"
    assert {:ok, ^name, binary} = Lower.compile(core, name)
    assert is_binary(binary)

    try do
      Runtime.run(name, :main, [], opts)
    after
      :code.purge(name)
      :code.delete(name)
    end
  end

  defp agrees(source, opts \\ []) do
    assert {:ok, core} = source |> Parser.parse() |> Elab.module()
    expected = Eval.run(core, :main, [], opts)
    assert native(core, opts) === expected
    expected
  end

  test "native functions, recursion, closures and polymorphism need no HM typed AST" do
    assert {:ok, 43} =
             agrees("""
             (defn identity [x :a] :a x)
             (defn fact [n :int] :int (if (== n 0) 1 (* n (fact (- n 1)))))
             (defn main [] :int
               (let [offset 1 f (fn [x :int] (+ x offset))]
                 (identity (+ (f 18) (fact 4)))))
             """)

    assert {:ok, 25} =
             agrees("""
             (defn main [] :int
               (fold + 0 (map (fn [x] (* x x)) (filter (fn [x] (> x 2)) [1 2 3 4]))))
             """)
  end

  test "records, sums, fields, tuples and patterns have the specified layouts" do
    assert {:ok, {7, [1, 2]}} =
             agrees("""
             (deftype Point [x :int y :int])
             (deftype Shape (At :Point) (Empty))
             (defn main [] (Tuple :int (List :int))
               (match (At (Point 3 4))
                 [(At p) (tuple (+ (. p :x) (. p :y)) [1 2])]
                 [(Empty) (tuple 0 [])]))
             """)
  end

  test "normalized crashes and short circuiting agree with the evaluator" do
    for source <- [
          "(defn main [] :int (div 1 0))",
          "(defn main [] :int (head (the (List :int) [])))",
          "(defn main [] :int (try (div 1 0) [catch [:error e 7]]))",
          "(defn main [] :bool (and false (== (div 1 0) 0)))",
          "(defn main [] :bool (or true (== (head (the (List :int) [])) 0)))",
          "(defn main [] :float (/ 1.0 0.0))"
        ] do
      agrees(source)
    end
  end

  test "native guards fall through on a crash and do not evaluate later bodies early" do
    assert {:ok, 8} =
             agrees("""
             (defn guarded [n :int :when (> (div 1 n) 0)] :int 5)
             (defn main [] :int (try (guarded 0) [catch [:error e 8]]))
             """)

    mod =
      core("""
      (module Guard (core-version 0)
        (def main (-> () (eff closed) Int)
          (fn ()
            (match 0
              (n (when (prim gt (prim div 1 n) 0)) (perform crash (inj (Reason) badarg)))
              (_ 9)))))
      """)

    assert {:ok, 9} = native(mod)
    assert native(mod) == Eval.run(mod, :main, [])
  end

  test "local mutually recursive functions capture their lexical environment" do
    mod =
      core("""
      (module Rec (core-version 0)
        (def main (-> () (eff closed) Int)
          (fn ()
            (let ((offset Int 10))
              (letrec
                ((even (-> (Int) (eff closed) Int)
                   (fn ((n Int)) (if (prim eq n 0) offset (app odd (prim sub n 1)))))
                 (odd (-> (Int) (eff closed) Int)
                   (fn ((m Int)) (if (prim eq m 0) (prim add offset 1) (app even (prim sub m 1))))))
                (app odd 3))))))
      """)

    assert {:ok, 10} = native(mod)
    assert native(mod) == Eval.run(mod, :main, [])
  end

  test "effects execute left to right and foreign results are decoded in native code" do
    source = """
    (extern foreign:next [] :int)
    (extern foreign:pair [:int :int] (Tuple :int :int))
    (defn main [] (Tuple :int :int) (foreign:pair (foreign:next) (foreign:next)))
    """

    steps = [
      {:external, [:foreign, :next, []], {:ok, 1}},
      {:external, [:foreign, :next, []], {:ok, 2}},
      {:external, [:foreign, :pair, [1, 2]], {:ok, {1, 2}}}
    ]

    assert {:ok, {1, 2}} = agrees(source, handler: {:script, steps})
    bad = List.replace_at(steps, 2, {:external, [:foreign, :pair, [1, 2]], {:ok, {1, "2"}}})
    assert {:crash, :bad_decode} = agrees(source, handler: {:script, bad})
  end

  test "the native decoder validates nested nominal types and improper lists" do
    source = """
    (deftype Box [items (List :int)])
    (extern foreign:box [] :Box)
    (defn main [] :Box (foreign:box))
    """

    for value <- [{:Box, [1, 2]}, {:Box, [1 | 2]}, {:Box, [1, "2"]}, {:Wrong, [1, 2]}] do
      agrees(source, handler: {:script, [{:external, [:foreign, :box, []], {:ok, value}}]})
    end
  end

  test "record field evaluation uses source order, not declaration order" do
    mod =
      core("""
      (module Order (core-version 0)
        (type Point () (record (x Int) (y Int)))
        (def main (-> () (eff now closed) (Point))
          (fn () (record (Point) (y (perform now)) (x (perform now))))))
      """)

    opts = [handler: {:script, [{:now, [], {:ok, 3}}, {:now, [], {:ok, 4}}]}]
    assert {:ok, {:Point, 4, 3}} = native(mod, opts)
    assert native(mod, opts) == Eval.run(mod, :main, [], opts)
  end

  test "load false produces a real unloaded BEAM module; Lint rejection emits nothing" do
    assert {:ok, mod} = "(defn main [] :int 42)" |> Parser.parse() |> Elab.module()
    name = :"LiquidUnloaded#{System.unique_integer([:positive])}"
    assert {:ok, ^name, beam} = Lower.compile(mod, name, load: false)
    assert :code.is_loaded(name) == false
    assert {:ok, {^name, _}} = :beam_lib.chunks(beam, [:exports])

    invalid =
      core("(module Bad (core-version 0) (def main (-> () (eff closed) Int) (fn () 1.5)))")

    assert {:error, _} = Lower.compile(invalid, name)
    assert :code.is_loaded(name) == false
  end

  test "every strictly elaborated corpus program agrees with native BEAM" do
    files =
      Path.wildcard("test/**/*_test.exs") -- Path.wildcard("test/{liquid,refine,elab}/*_test.exs")

    accepted =
      for {_, source} <- Harness.corpus(files),
          {:ok, ast} <- [Vaisto.Compilation.parse(source)],
          {:ok, core} <- [Elab.module(ast)],
          do: {source, core}

    assert length(accepted) >= 31

    for {source, core} <- accepted do
      assert Harness.agrees?(
               native(core, handler: :observe),
               Eval.run(core, :main, [], handler: :observe)
             ),
             source
    end
  end

  test "observing an effect stops before OTP runs it and does not relax pure handlers" do
    source =
      "(extern erlang:put [:atom :int] :dyn) (defn main [] :dyn (erlang:put :observe_effect_test 7))"

    assert {:ok, core} = source |> Parser.parse() |> Elab.module()
    previous = Process.get(:observe_effect_test, :absent)
    expected = {:effect, :external, [:erlang, :put, [:observe_effect_test, 7]]}

    assert native(core, handler: :observe) == expected
    assert Eval.run(core, :main, [], handler: :observe) == expected
    assert Process.get(:observe_effect_test, :absent) == previous
    assert_raise ArgumentError, fn -> native(core) end
    assert_raise Eval.Stuck, fn -> Eval.run(core, :main, []) end
  end

  test "generated well-typed Core agrees with the evaluator, including applied closures" do
    for seed <- 1..500 do
      :rand.seed(:exsss, {seed, 2026, 11})
      type = Enum.random(Gen.types())
      term = Gen.term(type, 4)

      {term, type} =
        case type do
          [:->, [:Int], _, :Int] -> {[:app, term, 7], :Int}
          _ -> {term, type}
        end

      mod = [
        :module,
        :Generated,
        [:"core-version", 0]
        | Gen.decls() ++ [[:def, :main, [:->, [], [:eff, :closed], type], [:fn, [], term]]]
      ]

      assert native(mod) === Eval.run(mod, :main, []),
             "native disagreement at seed #{seed}: #{inspect(term)}"
    end
  end

  test "closure-conversion helper names cannot collide with source definitions" do
    mod =
      core("""
      (module Fresh (core-version 0)
        (def __liquid_rec_0 (-> () (eff closed) Int) (fn () 99))
        (def main (-> () (eff closed) Int)
          (fn ()
            (letrec ((f (-> () (eff closed) Int) (fn () 7)))
              (prim add (app f) (app __liquid_rec_0))))))
      """)

    assert {:ok, 106} = native(mod)
  end

  test "unused script events are rejected even after a program crash" do
    assert {:ok, core} = "(defn main [] :int (div 1 0))" |> Parser.parse() |> Elab.module()

    assert_raise ArgumentError, ~r/script was not used up/, fn ->
      native(core, handler: {:script, [{:now, [], {:ok, 1}}]})
    end

    assert {:crash, :badarith} = native(core)
  end
end
