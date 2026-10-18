defmodule Vaisto.Elab.TaskTest do
  use ExUnit.Case, async: false
  alias Vaisto.{Compilation, Elab, Parser}
  alias Vaisto.Liquid.{Eval, Lint, Runtime}

  @source """
  (deftype Question [text :string])
  (deftype Answer [text :string])
  (defprompt summarize :input Question :output Answer :template "Summarize {text}")
  (pipeline answer :input Question :output Answer
    (generate :prompt summarize :extract Answer))
  """

  setup do
    previous_llm = Application.get_env(:vaisto, :llm)
    previous_mock = Application.get_env(:vaisto, Vaisto.LLM.Mock)
    Application.put_env(:vaisto, :llm, Vaisto.LLM.Mock)

    on_exit(fn ->
      for {key, value} <- [{:llm, previous_llm}, {Vaisto.LLM.Mock, previous_mock}] do
        if value == nil,
          do: Application.delete_env(:vaisto, key),
          else: Application.put_env(:vaisto, key, value)
      end
    end)
  end

  defp compile(source \\ @source) do
    name = :"NativeTask#{System.unique_integer([:positive])}"
    assert {:ok, core} = source |> Parser.parse() |> Elab.module(name: name)
    assert :ok = Lint.check_module(core)
    assert {:ok, ^name, _} = Compilation.compile_liquid(source, name, format_errors: false)

    on_exit(fn ->
      :code.purge(name)
      :code.delete(name)
    end)

    {name, core}
  end

  defp mock(fun), do: Application.put_env(:vaisto, Vaisto.LLM.Mock, call: fun)

  test "native pipelines use the existing LLM driver and validate their response" do
    parent = self()

    mock(fn text, input, output, opts ->
      send(parent, {:call, text, input, output, opts})
      {:ok, %{text: "short"}}
    end)

    {name, core} = compile()
    payload = {:Question, "a long passage {text}"}
    assert apply(name, :answer, [payload]) == {:ok, {:Answer, "short"}}

    assert_received {:call, "Summarize a long passage {text}", %{text: "a long passage {text}"},
                     {:record, :Answer, [text: :string]}, []}

    handler =
      {:script,
       [
         {:generate, [[:Question], [:Answer], "Summarize {text}", payload],
          {:ok, {:ok, {:Answer, "short"}}}}
       ]}

    assert Eval.run(core, :answer, [payload], handler: handler) ==
             {:ok, {:ok, {:Answer, "short"}}}

    assert Runtime.run(name, :answer, [payload], handler: handler) ==
             {:ok, {:ok, {:Answer, "short"}}}

    assert_raise ArgumentError, fn -> Runtime.run(name, :answer, [payload]) end

    bad =
      {:script,
       [
         {:generate, [[:Question], [:Answer], "Summarize {text}", payload],
          {:ok, {:ok, {:Answer, 1}}}}
       ]}

    assert Runtime.run(name, :answer, [payload], handler: bad) == {:crash, :bad_decode}
    assert Eval.run(core, :answer, [payload], handler: bad) == {:crash, :bad_decode}
  end

  test "invalid model fields are rejected rather than trusted as the extract type" do
    {name, _} = compile()

    for answer <- [%{}, %{text: 42}, %{text: nil}, false] do
      mock(fn _, _, _, _ -> {:ok, answer} end)

      assert Runtime.run(name, :answer, [{:Question, "input"}], handler: :native) ==
               {:crash, :bad_decode}
    end

    mock(fn _, _, _, _ -> {:ok, %{"text" => "from JSON"}} end)
    assert apply(name, :answer, [{:Question, "input"}]) == {:ok, {:Answer, "from JSON"}}
  end

  test "multi-stage pipelines stop at the first failure" do
    source = """
    (deftype A [text :string])
    (deftype B [n :int])
    (deftype C [done :bool])
    (defprompt first :input A :output B :template "first {text}")
    (defprompt second :input B :output C :template "second {n}")
    (pipeline pipeline :input A :output C
      (generate :prompt first :extract B)
      (generate :prompt second :extract C))
    """

    {name, core} = compile(source)
    parent = self()

    mock(fn text, _, _, _ ->
      send(parent, {:called, text})
      {:error, :rate_limit}
    end)

    assert apply(name, :pipeline, [{:A, "input"}]) == {:error, :rate_limit}
    assert_received {:called, "first input"}
    refute_received {:called, "second " <> _}

    handler =
      {:script,
       [{:generate, [[:A], [:B], "first {text}", {:A, "input"}], {:ok, {:error, :rate_limit}}}]}

    assert Eval.run(core, :pipeline, [{:A, "input"}], handler: handler) ==
             {:ok, {:error, :rate_limit}}

    mock(fn _, _, {:record, type, _}, _ ->
      case type do
        :B -> {:ok, %{n: 42}}
        :C -> {:ok, %{done: true}}
      end
    end)

    assert apply(name, :pipeline, [{:A, "input"}]) == {:ok, {:C, true}}
  end

  test "output projection reconstructs the declared record instead of lying about its tag" do
    source = """
    (deftype Small [x :int])
    (deftype Large [x :int y :string])
    (defprompt p :input Small :output Large :template "{x}")
    (pipeline project :input Small :output Small
      (generate :prompt p :extract Large))
    """

    mock(fn _, _, _, _ -> {:ok, %{x: 42, y: "extra"}} end)
    {name, _} = compile(source)
    assert apply(name, :project, [{:Small, 1}]) == {:ok, {:Small, 42}}
  end

  test "nested records and lists cross the boundary with deep validation" do
    source = """
    (deftype Item [n :int])
    (deftype Batch [items (List :Item)])
    (defprompt p :input Batch :output Batch :template "batch")
    (pipeline batch :input Batch :output Batch
      (generate :prompt p :extract Batch))
    """

    {name, _} = compile(source)
    payload = {:Batch, [{:Item, 1}]}

    mock(fn _, data, _, _ ->
      assert data == %{items: [%{n: 1}]}
      {:ok, %{items: [%{n: 42}]}}
    end)

    assert apply(name, :batch, [payload]) == {:ok, {:Batch, [{:Item, 42}]}}

    for bad <- [[%{n: "bad"}], [%{n: 1} | :improper]] do
      mock(fn _, _, _, _ -> {:ok, %{items: bad}} end)
      assert Runtime.run(name, :batch, [payload], handler: :native) == {:crash, :bad_decode}
    end
  end

  test "task results retain precise types when source matches the Erlang tuple syntax" do
    mock(fn _, _, _, _ -> {:ok, %{text: "ok"}} end)

    {name, _} =
      compile(
        @source <>
          """
          (defn main [] :string
            (match (answer (Question "input"))
              [{:ok result} (. result :text)]
              [{:error reason} "failed"]))
          """
      )

    assert apply(name, :main, []) == "ok"
  end

  test "prompts, templates, projections and JSON shapes are checked before runtime" do
    for source <- [
          "(defprompt p :input :int :output :int) (pipeline f :input :int :output :int (generate :prompt p :extract :int))",
          "(deftype A [x :int]) (defprompt p :input A :output A :template \"{missing}\") (pipeline f :input A :output A (generate :prompt p :extract A))",
          "(defprompt p :input :int :output :int :template \"x\") (pipeline f :input :int :output :string (generate :prompt p :extract :string))",
          "(pipeline f :input :int :output :int (generate :prompt missing :extract :int))",
          "(deftype A [f (Fn :int :int)]) (defprompt p :input A :output A)",
          "(deftype Tree [children (List :Tree)]) (defprompt p :input Tree :output Tree)"
        ] do
      assert {:error, [_ | _]} = source |> Parser.parse() |> Elab.module()
    end
  end

  test "Core Lint independently checks generate schemas, inputs and effects" do
    for {effect, input, output, payload} <- [
          {[:eff, :closed], :Int, :Int, 1},
          {[:eff, :external, :closed], :Int, :Int, "wrong"},
          {[:eff, :external, :closed], :Int, [:->, [], [:eff, :closed], :Int], 1}
        ] do
      core = [
        :module,
        :Bad,
        [:"core-version", 0],
        [
          :def,
          :main,
          [:->, [], effect, [:"Core.TaskResult", output]],
          [:fn, [], [:perform, :generate, input, output, "literal", payload]]
        ]
      ]

      assert {:error, _} = Lint.check_module(core)
    end
  end
end
