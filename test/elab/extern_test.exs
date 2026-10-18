defmodule Vaisto.Elab.ExternTest do
  use ExUnit.Case, async: true

  alias Vaisto.{Elab, Parser}
  alias Vaisto.Liquid.{Eval, Lint}

  defp elab(source) do
    case Elab.module(Parser.parse(source)) do
      {:ok, core} = result ->
        assert :ok = Lint.check_module(core)
        result

      other ->
        other
    end
  end

  defp run(source, steps \\ []) do
    assert {:ok, core} = elab(source)
    Eval.run(core, :main, [], handler: {:script, steps})
  end

  defp error(source) do
    assert {:error, [error | _]} = elab(source)
    error
  end

  test "an extern checks its arguments and decodes its actual foreign result" do
    source = "(extern erlang:abs [:int] :int) (defn main [] :int (erlang:abs -42))"
    assert {:ok, 42} = run(source, [{:external, [:erlang, :abs, [-42]], {:ok, :erlang.abs(-42)}}])
  end

  test "a wrong declaration produces bad_decode, not a mistyped value" do
    source = "(extern foreign:value [] :int) (defn main [] :int (foreign:value))"

    for value <- [1.0, "1", nil, :one, [], {1}] do
      assert {:crash, :bad_decode} =
               run(source, [{:external, [:foreign, :value, []], {:ok, value}}])
    end
  end

  test "deep decoding checks list tails, nominal tags and nested fields" do
    source = """
    (deftype Point [x :int y (List :int)])
    (deftype Shape (At :Point) (Empty))
    (extern foreign:shape [] :Shape)
    (defn main [] :Shape (foreign:shape))
    """

    valid = {:At, {:Point, 2, [3, 4]}}
    assert {:ok, ^valid} = run(source, [{:external, [:foreign, :shape, []], {:ok, valid}}])

    for invalid <- [
          {:At, {:Other, 2, [3]}},
          {:At, {:Point, 2.0, [3]}},
          {:At, {:Point, 2, [3 | 4]}},
          {:At, {:Point, 2, ["3"]}},
          {:At, {:Point, 2}},
          {:Empty, 1},
          :Empty
        ] do
      assert {:crash, :bad_decode} =
               run(source, [{:external, [:foreign, :shape, []], {:ok, invalid}}])
    end
  end

  test "extern arguments run exactly once, from left to right" do
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

    assert {:ok, {1, 2}} = run(source, steps)
  end

  test "foreign exceptions retain their normalized reason" do
    source = "(extern foreign:value [] :int) (defn main [] :int (foreign:value))"

    assert {:crash, {:raised, :error, :badarg}} =
             run(source, [{:external, [:foreign, :value, []], {:raised, :error, :badarg}}])
  end

  test "argument and arity errors are located diagnostics" do
    prefix = "(extern erlang:abs [:int] :int)\n"
    e = error(prefix <> "(defn main [] :int (erlang:abs \"x\"))")
    assert e.message == "type mismatch"
    assert e.primary_span.line == 2

    assert error(prefix <> "(defn main [] :int (erlang:abs))").message ==
             "wrong number of arguments"
  end

  test "undeclared and duplicate externs are errors" do
    e = error("(defn main [] :int (erlang:abs -1))")
    assert e.message =~ "undeclared foreign call"
    assert e.hint =~ "extern erlang:abs"

    assert error("(extern erlang:abs [:int] :int) (extern erlang:abs [:int] :int)").message =~
             "declared twice"
  end

  test "ordinary functions at other arities do not replace fixed-arity typing forms" do
    source = """
    (defn the [x :int] :int x)
    (defn up [x :int] :int (+ x 1))
    (defn decode [x :int] :int (+ x 1))
    (defn main [] :int
      (match (decode :int (up :int (decode (the (up 40)))))
        [(Core.Ok n) n] [(Core.Err _) 0]))
    """

    assert {:ok, 42} = run(source)

    for name <- [:the, :up, :decode] do
      assert error("(defn #{name} [a :int b :int] :int a)").message =~ "reserved form"
    end
  end

  test "extern declarations distinguish arities without accepting undeclared overloads" do
    source = """
    (extern foreign:choose [:int] :int)
    (extern foreign:choose [:int :int] :int)
    (defn main [] :int (+ (foreign:choose 1) (foreign:choose 2 3)))
    """

    assert {:ok, 6} =
             run(source, [
               {:external, [:foreign, :choose, [1]], {:ok, 1}},
               {:external, [:foreign, :choose, [2, 3]], {:ok, 5}}
             ])

    assert error(String.replace(source, "(foreign:choose 1)", "(foreign:choose)")).message ==
             "wrong number of arguments"
  end

  test "extern types cannot smuggle functions, unresolved variables or any" do
    for signature <- [
          "[(Fn :int :int)] :int",
          "[] (List (Fn :int :int))",
          "[:any] :int",
          "[] :a",
          "[] {r :int | (> r 0)}"
        ] do
      assert {:error, _} = elab("(extern foreign:f #{signature})")
    end

    assert {:error, _} = elab("(deftype Box [f (Fn :int :int)]) (extern foreign:f [] :Box)")
  end

  test "external propagates through callers, including a recursive group" do
    source = """
    (extern foreign:value [] :int)
    (defn main [] :int (first 2))
    (defn first [n :int] :int (if (== n 0) (foreign:value) (second (- n 1))))
    (defn second [n :int] :int (first n))
    """

    assert {:ok, core} = elab(source)
    for [:def, _, [:->, _, eff, _], _] <- core, do: assert(:external in eff)

    assert {:ok, 9} =
             Eval.run(core, :main, [],
               handler: {:script, [{:external, [:foreign, :value, []], {:ok, 9}}]}
             )
  end

  test "a locally inferred lambda carries external; a pure callback cannot hide it" do
    prefix = "(extern foreign:value [] :int) "

    assert {:ok, 7} =
             run(
               prefix <> "(defn main [] :int (let [f (fn [] (foreign:value))] (f)))",
               [{:external, [:foreign, :value, []], {:ok, 7}}]
             )

    e =
      error(
        prefix <>
          "(defn pure-map [f (Fn :int :int) xs (List :int)] (List :int) (map f xs)) (defn main [] (List :int) (pure-map (fn [x] (foreign:value)) [1]))"
      )

    assert e.message =~ "pure function"
  end

  test "Dyn is explicit and decode branches carry the checked target type" do
    source = """
    (defn main [] :int
      (match (decode :int (up :int 42))
        [(Core.Ok n) (+ n 1)]
        [(Core.Err _) 0]))
    """

    assert {:ok, 43} = run(source)
    assert {:ok, 0} = run(String.replace(source, "(up :int 42)", "(up :string \"42\")"))
    assert error("(defn main [x :dyn] :int x)").message == "type mismatch"
    assert error("(defn main [] :int (decode :int 42))").message == "type mismatch"
  end

  test "raw extern results remain Dyn until explicitly decoded" do
    source = """
    (extern foreign:raw [] :dyn)
    (defn main [] :int
      (match (decode :int (foreign:raw)) [(Core.Ok n) n] [(Core.Err _) 0]))
    """

    assert {:ok, 3} = run(source, [{:external, [:foreign, :raw, []], {:ok, 3}}])
    assert {:ok, 0} = run(source, [{:external, [:foreign, :raw, []], {:ok, "three"}}])
  end

  test "up and decode reject functions even nested inside data" do
    for expression <- [
          "(up (Fn :int :int) (fn [x] x))",
          "(decode (List (Fn :int :int)) (up :int 1))"
        ] do
      assert error("(defn main [] :dyn (do #{expression} (up :int 1)))").message =~ "decodable"
    end
  end

  test "built-in decode constructors cannot be redeclared" do
    assert error("(deftype Forged (Core.Ok :int))").message =~ "unqualified"
  end

  test "foreign maps, references and untyped pids retain their precise data types" do
    for {type, value} <- [
          {"(Map :atom :int)", %{x: 3}},
          {":ref", make_ref()},
          {"(Pid :dyn)", self()}
        ] do
      source = "(extern foreign:value [] #{type}) (defn main [] #{type} (foreign:value))"
      assert {:ok, ^value} = run(source, [{:external, [:foreign, :value, []], {:ok, value}}])
    end

    source = """
    (extern foreign:map [] (Map :atom :int))
    (extern foreign:read [(Map :atom :int)] :int)
    (defn main [] :int (foreign:read (foreign:map)))
    """

    assert {:ok, 3} =
             run(source, [
               {:external, [:foreign, :map, []], {:ok, %{x: 3}}},
               {:external, [:foreign, :read, [%{x: 3}]], {:ok, 3}}
             ])

    assert error("(extern foreign:pid [] (Pid :int))").message =~ "decodable"
  end

  test "decode result types and constructors can be written explicitly" do
    assert {:ok, {:"Core.Ok", 2}} =
             run("(defn main [] (Core.Result :Core.DecodeError :int) (Core.Ok 2))")

    assert {:ok, {:"Core.Err", {:"Core.BadDecode"}}} =
             run(
               "(defn main [] (Core.Result :Core.DecodeError :int) (Core.Err (Core.BadDecode)))"
             )
  end

  test "the corpus harness never executes foreign effects without a script" do
    source = "(extern erlang:abs [:int] :int) (defn main [] :int (erlang:abs -42))"
    assert %{verdict: {:outside_fragment, reason}, core: _} = Vaisto.Liquid.Harness.run(source)
    assert reason =~ "scripted handler"
  end
end
