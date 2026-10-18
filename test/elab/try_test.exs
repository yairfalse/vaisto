defmodule Vaisto.Elab.TryTest do
  use ExUnit.Case, async: true
  alias Vaisto.{Compilation, Elab, Parser}
  alias Vaisto.Liquid.{Eval, Lint, Runtime}

  defp compile(source) do
    name = :"NativeTry#{System.unique_integer([:positive])}"
    assert {:ok, core} = source |> Parser.parse() |> Elab.module(name: name)
    assert :ok = Lint.check_module(core)
    assert {:ok, ^name, _} = Compilation.compile_liquid(source, name, format_errors: false)

    on_exit(fn ->
      :code.purge(name)
      :code.delete(name)
    end)

    {name, core}
  end

  test "after preserves the original value and cannot leak its local bindings" do
    {name, core} = compile("(defn main [] :int (try 42 [after 99]))")
    assert Runtime.run(name, :main, []) == {:ok, 42}
    assert Eval.run(core, :main, []) == {:ok, 42}

    assert {:error, [_ | _]} =
             "(defn main [] :int (do (try 1 [after (let [n 2] n)]) n))"
             |> Parser.parse()
             |> Elab.module()
  end

  test "error catches normalized arithmetic crashes and Reasons can be inspected safely" do
    {name, core} =
      compile("(defn main [] :string (try (str (div 1 0)) [catch [:error e (str e)]] [after 2]))")

    assert Runtime.run(name, :main, []) == {:ok, "badarith"}
    assert Eval.run(core, :main, []) == {:ok, "badarith"}
  end

  test "explicit crashes check at the enclosing result type and preserve their Reason" do
    {name, core} =
      compile("""
      (defn main [] :string (try (crash :bad_decode) [catch [:error r (str r)]]))
      """)

    assert apply(name, :main, []) == "bad_decode"
    assert Eval.run(core, :main, []) == {:ok, "bad_decode"}
    assert {:error, [_ | _]} = "(defn main [] :int (crash 42))" |> Parser.parse() |> Elab.module()
  end

  test "foreign catch classes are selected exactly, not swallowed by error" do
    for class <- [:throw, :exit, :error] do
      source = """
      (extern erlang:#{class} [:atom] :int)
      (defn main [] :int
        (try (erlang:#{class} :boom)
          [catch [:error e 1] [:throw e 2] [:exit e 3]]))
      """

      {name, core} = compile(source)
      expected = %{error: 1, throw: 2, exit: 3}[class]
      assert Runtime.run(name, :main, [], handler: :native) == {:ok, expected}
      handler = {:script, [{:external, [:erlang, class, [:boom]], {:raised, class, :boom}}]}
      assert Eval.run(core, :main, [], handler: handler) == {:ok, expected}
    end

    {name, _} =
      compile(
        "(extern erlang:throw [:atom] :int) (defn main [] :int (try (erlang:throw :boom) [catch [:error e 0]]))"
      )

    assert Runtime.run(name, :main, [], handler: :native) == {:crash, {:raised, :throw, :boom}}
  end

  test "after executes once on success, failure and caught failure" do
    for {body, outcome, handled} <- [
          {"(foreign:body)", {:ok, 42}, {:ok, 42}},
          {"(foreign:body)", {:raised, :error, :boom}, {:crash, {:raised, :error, :boom}}},
          {"(try (foreign:body) [catch [:error e 7]])", {:raised, :error, :boom}, {:ok, 7}}
        ] do
      {name, core} =
        compile("""
        (extern foreign:body [] :int)
        (extern foreign:after [] :int)
        (defn main [] :int (try #{body} [after (foreign:after)]))
        """)

      handler =
        {:script,
         [
           {:external, [:foreign, :body, []], outcome},
           {:external, [:foreign, :after, []], {:ok, 99}}
         ]}

      assert Runtime.run(name, :main, [], handler: handler) == handled
      assert Eval.run(core, :main, [], handler: handler) == handled
    end
  end

  test "a failing after overrides the previous outcome but never runs twice" do
    {name, core} =
      compile("""
      (extern foreign:body [] :int)
      (extern foreign:after [] :int)
      (defn main [] :int (try (foreign:body) [after (foreign:after)]))
      """)

    for before <- [{:ok, 42}, {:raised, :error, :first}] do
      handler =
        {:script,
         [
           {:external, [:foreign, :body, []], before},
           {:external, [:foreign, :after, []], {:raised, :throw, :last}}
         ]}

      expected = {:crash, {:raised, :throw, :last}}
      assert Runtime.run(name, :main, [], handler: handler) == expected
      assert Eval.run(core, :main, [], handler: handler) == expected
    end
  end
end
