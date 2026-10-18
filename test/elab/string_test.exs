defmodule Vaisto.Elab.StringTest do
  use ExUnit.Case, async: true
  alias Vaisto.{Compilation, Elab, Parser}
  alias Vaisto.Liquid.{Eval, Lint}

  test "string conversion and interpolation have the same native and Core meaning" do
    for {body, expected} <- [
          {"(str)", ""},
          {~s|(str "x=" 42 ", " true " " 3.14)|, "x=42, true 3.14"},
          {~S|"answer: #{(+ 40 2)}"|, "answer: 42"},
          {"(str (tuple :ok 1))", "{ok,1}"},
          {"(str (list 1 2))", "[1,2]"},
          {"(str (up :int 3))", "3"}
        ] do
      source = "(defn main [] :string #{body})"
      name = :"NativeString#{System.unique_integer([:positive])}"
      assert {:ok, core} = source |> Parser.parse() |> Elab.module(name: name)
      assert :ok = Lint.check_module(core)
      assert {:ok, ^name, _} = Compilation.compile_liquid(source, name, format_errors: false)

      try do
        assert apply(name, :main, []) == expected
        assert Eval.run(core, :main, []) == {:ok, expected}
      after
        :code.purge(name)
        :code.delete(name)
      end
    end
  end

  test "formatting does not erase abstract types or functions" do
    for source <- [
          "(defn f [x :a] :string (str x))",
          "(defn f [] :string (str (fn [x :int] x)))"
        ],
        do: assert({:error, [_ | _]} = source |> Parser.parse() |> Elab.module())

    assert {:error, _} = Lint.synth([:prim, :str, 1])
  end
end
