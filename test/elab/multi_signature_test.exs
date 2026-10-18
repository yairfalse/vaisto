defmodule Vaisto.Elab.MultiSignatureTest do
  use ExUnit.Case, async: true

  alias Vaisto.{Compilation, Elab, Parser}
  alias Vaisto.Liquid.{Eval, Lint}

  defp compile(source) do
    name = :"MultiSignature#{System.unique_integer([:positive])}"
    assert {:ok, core} = source |> Parser.parse() |> Elab.module(name: name)
    assert :ok = Lint.check_module(core)
    assert {:ok, ^name, _} = Compilation.compile_liquid(source, name, format_errors: false)

    on_exit(fn ->
      :code.purge(name)
      :code.delete(name)
    end)

    {name, core}
  end

  test "a recursive polymorphic list function needs no suggested signature" do
    source = """
    (defn len (Fn (List :a) :int)
      [[] 0]
      [[h | t] (+ 1 (len t))])
    (defn main [] :int (+ (len [1 2 3]) (len [true false])))
    """

    {name, core} = compile(source)
    assert apply(name, :main, []) == 5
    assert Eval.run(core, :main, []) == {:ok, 5}
    assert {:ok, [_, _]} = Vaisto.Compilation.parse(source, file: "signature.va")
  end

  test "the signature governs guards and branch results" do
    {name, core} =
      compile("""
      (defn classify (Fn :int :int)
        [n :when (< n 0) -1]
        [0 0]
        [_ 1])
      """)

    for {input, expected} <- [{-5, -1}, {0, 0}, {4, 1}] do
      assert apply(name, :classify, [input]) == expected
      assert Eval.run(core, :classify, [input]) == {:ok, expected}
    end
  end

  test "multi-clause effect contracts propagate callbacks and are never widened" do
    source = """
    (extern foreign:tick [:int] :int)
    (defn tick (Fn :int :int :effects [external])
      [0 0]
      [n (foreign:tick n)])
    (defn apply (Fn (Fn :int :int :effects :fx) :int :effects :fx)
      [f (f 42)])
    (defn main [] :int (apply tick))
    """

    {name, core} = compile(source)
    handler = {:script, [{:external, [:foreign, :tick, [42]], {:ok, 43}}]}
    assert Vaisto.Liquid.Runtime.run(name, :main, [], handler: handler) == {:ok, 43}
    assert Eval.run(core, :main, [], handler: handler) == {:ok, 43}

    pure =
      String.replace(source, "(Fn :int :int :effects [external])", "(Fn :int :int :effects [])")

    assert {:error, [_ | _]} = pure |> Parser.parse() |> Elab.module()
  end

  test "the signature is rigid, exhaustive and independently checked" do
    for source <- [
          "(defn f (Fn :a :a) [x 1])",
          "(defn f (Fn :int :int) [x false])",
          "(defn f (Fn :int :int) [0 1])",
          "(defn f (Fn :int :int) [x :when (+ x 1) 1] [_ 0])",
          "(defn f (Fn :int :int) [x :when (receive [:ping true]) 1] [_ 0])",
          "(defn f (Fn :Missing :int) [_ 0])"
        ] do
      assert {:error, [_ | _]} = source |> Parser.parse() |> Elab.module()
    end
  end

  test "obsolete migration options cannot override or supply signatures" do
    source = "(defn f (Fn :int :int) [n (+ n 1)])"
    suggestion = [:->, [[:Bool]], [:eff, :closed], [:Bool]]
    assert {:ok, core} = source |> Parser.parse() |> Elab.module(suggest: %{f: suggestion})
    assert Eval.run(core, :f, [1]) == {:ok, 2}

    for params <- [[], [:Int, :Int]] do
      assert {:error, [_]} =
               "(defn f [n n])"
               |> Parser.parse()
               |> Elab.module(suggest: %{f: [:->, params, [:eff, :closed], :Int]})
    end
  end

  test "annotated and unannotated malformed clauses return located errors" do
    for source <- [
          "(defn f (Fn :int :int))",
          "(defn f (Fn :int :int) [])",
          "(defn f (Fn :int :int) [x])",
          "(defn f (Fn :int :int) [x :when])",
          "(defn f (Fn :int :int) [x :when true])",
          "(defn f (Fn :int :int) 1)",
          "(defn f (Fn :int) [_ 0])",
          "(defn f (Fn :int :int :int) [_ 0])",
          "(defn f [])",
          "(defn f [x :when])"
        ] do
      assert {:error, [_ | _]} = source |> Parser.parse() |> Elab.module()
    end
  end

  test "body locations preserve the real source location" do
    source = "\n(defn f (Fn :int :int)\n  [x 1 missing])"

    assert {:defn_multi, :f, [{:x, nil, {:do, _, %Parser.Loc{line: 2}}}], _, %Parser.Loc{line: 2}} =
             Parser.parse(source)

    assert {:error, [error]} = source |> Parser.parse() |> Elab.module()
    assert error.primary_span.line == 2
  end
end
