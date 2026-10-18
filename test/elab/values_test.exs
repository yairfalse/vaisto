defmodule Vaisto.Elab.ValuesTest do
  use ExUnit.Case, async: true
  alias Vaisto.{Compilation, Elab, Parser}
  alias Vaisto.Liquid.{Eval, Lint, Runtime}

  defp compile(source) do
    name = :"NativeValue#{System.unique_integer([:positive])}"
    assert {:ok, core} = source |> Parser.parse() |> Elab.module(name: name)
    assert :ok = Lint.check_module(core)
    assert {:ok, ^name, _} = Compilation.compile_liquid(source, name, format_errors: false)

    on_exit(fn ->
      :code.purge(name)
      :code.delete(name)
    end)

    {name, core}
  end

  test "top-level values carry explicit signatures and read through zero-arity accessors" do
    {name, core} = compile("(def answer :int 42) (+ answer answer)")
    assert apply(name, :answer, []) == 42
    assert apply(name, :main, []) == 84
    assert Eval.run(core, :main, []) == {:ok, 84}
    {name, core} = compile("(def answer (the :int 42)) answer")
    assert apply(name, :main, []) == 42
    assert Eval.run(core, :main, []) == {:ok, 42}
  end

  test "an explicitly polymorphic value is freshly instantiated at each read" do
    {name, core} =
      compile("""
      (def id (Fn :a :a) (fn [x] x))
      (if (id true) (id 42) 0)
      """)

    assert apply(name, :main, []) == 42
    assert Eval.run(core, :main, []) == {:ok, 42}
  end

  test "value effects are explicit and retain the existing recomputation semantics" do
    {name, core} = compile("(extern foreign:next [] :int) (def n :int (foreign:next)) (+ n n)")

    handler =
      {:script,
       [
         {:external, [:foreign, :next, []], {:ok, 1}},
         {:external, [:foreign, :next, []], {:ok, 2}}
       ]}

    assert Runtime.run(name, :main, [], handler: handler) == {:ok, 3}
    assert Eval.run(core, :main, [], handler: handler) == {:ok, 3}
  end

  test "missing signatures, wrong initializers and duplicate definitions fail before execution" do
    for source <- [
          "(def answer 42)",
          "(def answer :int false)",
          "(def answer :int 1) (def answer :int 2)",
          "(def answer :int 1) (defn answer [] :int 2)",
          "(def answer 1 2)"
        ],
        do: assert({:error, [_ | _]} = source |> Parser.parse() |> Elab.module())
  end
end
