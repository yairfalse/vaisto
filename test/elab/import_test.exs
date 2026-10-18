defmodule Vaisto.Elab.ImportTest do
  use ExUnit.Case, async: true
  alias Vaisto.{Compilation, Elab, Interface, Parser}
  alias Vaisto.Liquid.{Canonical, Eval, Lint, Runtime}

  defp provider(source) do
    name = :"NativeProvider#{System.unique_integer([:positive])}"
    assert {:ok, core} = source |> Parser.parse() |> Elab.module(name: name)
    assert {:ok, tree} = Interface.from_core(core)
    assert {:ok, ^name, _} = Compilation.compile_liquid(source, name, format_errors: false)

    on_exit(fn ->
      :code.purge(name)
      :code.delete(name)
    end)

    {name, core, tree}
  end

  defp consumer(source, interfaces) do
    name = :"NativeConsumer#{System.unique_integer([:positive])}"

    assert {:ok, core} =
             source |> Parser.parse() |> Elab.module(name: name, interfaces: interfaces)

    assert :ok = Lint.check_module(core)

    assert {:ok, ^name, _} =
             Compilation.compile_liquid(source, name,
               interfaces: interfaces,
               format_errors: false
             )

    on_exit(fn ->
      :code.purge(name)
      :code.delete(name)
    end)

    {name, core}
  end

  test "polymorphic imports and aliases execute as typed native remote functions" do
    {lib, provider, tree} = provider("(defn id [x :a] :a x)")

    {name, core} =
      consumer(
        """
        (import #{lib} :as Lib)
        (defn main [] :int (if (Lib/id true) (Lib/id 42) 0))
        """,
        %{lib => tree}
      )

    assert apply(name, :main, []) == 42
    assert Eval.run(core, :main, [], imports: %{lib => provider}) == {:ok, 42}
    assert [:import, ^lib, digest, _] = Enum.find(core, &match?([:import | _], &1))
    assert digest == Canonical.digest(tree)
    assert_raise Eval.Stuck, fn -> Eval.run(core, :main, []) end
  end

  test "imported numeric constraints retain implicit dictionaries and effects" do
    {lib, provider, tree} =
      provider("""
      (extern erlang:abs [:int] :int)
      (defn twice [x :a] :a where [(Num a)] (+ x x))
      (defn positive [x :a n :int] :a where [(Num a)]
        (do (erlang:abs n) (+ x 1)))
      """)

    {name, core} =
      consumer(
        """
        (import #{lib} :as Lib)
        (defn main [] (Tuple :int :float)
          {(Lib/twice 21) (Lib/positive 2.5 -1)})
        """,
        %{lib => tree}
      )

    script = {:script, [{:external, [:erlang, :abs, [-1]], {:ok, 1}}]}
    assert Runtime.run(name, :main, [], handler: script) == {:ok, {42, 3.5}}

    assert Eval.run(core, :main, [], imports: %{lib => provider}, handler: script) ==
             {:ok, {42, 3.5}}
  end

  test "constraint metadata is checked against its exported dictionary ABI" do
    {lib, _, tree} = provider("(defn twice [x :a] :a where [(Num a)] (+ x x))")
    [:constraints, [f, [c, t, dictionary]]] = Enum.at(tree, 9)

    for metadata <- [
          [:constraints, [f, [c, t, :Int]]],
          [:constraints, [:missing, [c, t, dictionary]]],
          [:constraints, [f, [c, [:tvar, :unbound], dictionary]]],
          [:constraints, [f, [c, t, dictionary]], [f, [c, t, dictionary]]]
        ] do
      assert {:error, _} = Interface.validate_core(List.replace_at(tree, 9, metadata))
    end

    forged = List.replace_at(tree, 9, [:constraints, [f, [:Eq, t, dictionary]]])

    assert {:error, _} =
             "(import #{lib}) (defn main [] :int (#{lib}/twice 21))"
             |> Parser.parse()
             |> Elab.module(interfaces: %{lib => forged})
  end

  test "user classes, generic instances and defaults survive chained imports" do
    {lib, provider, tree} =
      provider("""
      (defclass Printable [a]
        (text [x :a] :string)
        (wrapped [x :a] :string (str "[" (text x) "]")))
      (instance Printable :int (text [x] (str x)))
      (defn render [x :a] :string where [(Printable a)] (wrapped x))
      """)

    source = """
    (import #{lib} :as Base)
    (deftype Box (Box v))
    (instance Printable (Box a) where [(Printable a)]
      (text [x] (match x [(Box v) (text v)])))
    (defn render-box [n :int] :string (Base/render (Box n)))
    (defn main [] :string (render-box 42))
    """

    {middle, middle_core} = consumer(source, %{lib => tree})
    assert apply(middle, :main, []) == "[42]"
    assert Eval.run(middle_core, :main, [], imports: %{lib => provider}) == {:ok, "[42]"}
    assert {:ok, middle_tree} = Interface.from_core(middle_core)

    {last, last_core} =
      consumer(
        """
        (import #{middle} :as Middle)
        (defn main [] :string (wrapped (Middle/Box 7)))
        """,
        %{middle => middle_tree}
      )

    assert apply(last, :main, []) == "[7]"

    assert Eval.run(last_core, :main, [], imports: %{lib => provider, middle => middle_core}) ==
             {:ok, "[7]"}
  end

  test "interface class and instance signatures cannot contradict their Core exports" do
    {_, _, tree} =
      provider("""
      (defclass Label [a] (label [x :a] :string))
      (instance Label :int (label [x] (str x)))
      """)

    for invalid <- [
          List.replace_at(tree, 10, [
            :classes,
            [:Label, :a, [[:label, [:->, [[:tvar, :a]], [:eff, :closed], :Int], :none]]]
          ]),
          List.replace_at(tree, 10, [
            :classes,
            [:Label, :a, [[:label, [:->, [[:tvar, :a]], [:eff, :closed], :String], :missing]]]
          ]),
          List.replace_at(tree, 11, [:instances, [:Label, :Float, [], :"Instance.Label.Int"]]),
          List.replace_at(tree, 11, [
            :instances,
            [:Label, :Int, [[:Unknown, :Int]], :"Instance.Label.Int"]
          ]),
          List.replace_at(tree, 11, [:instances, [:Label, :Int, [], :missing]]),
          List.replace_at(tree, 11, [
            :instances,
            [:Label, [:List, :Int], [], :"Instance.Label.Int"]
          ])
        ] do
      assert {:error, _} = Interface.validate_core(invalid)
    end
  end

  test "imported named types, constructors, patterns and field layouts stay checked" do
    {lib, provider, tree} =
      provider("""
      (deftype Point [y :int x :int])
      (deftype Maybe (Just v) (Nothing))
      (defn x [p :Point] :int (. p :x))
      (defn wrap [p :Point] (Maybe :Point) (Just p))
      """)

    {name, core} =
      consumer(
        """
        (import #{lib} :as Lib)
        (defn read [p (Lib.Point)] :int (Lib/x p))
        (defn main [] :int
          (match (Lib/wrap (Lib/Point 7 42))
            [(Lib/Just p) (+ (. p :y) (read p))]
            [(Lib/Nothing) 0]))
        """,
        %{lib => tree}
      )

    assert apply(name, :main, []) == 49
    assert Eval.run(core, :main, [], imports: %{lib => provider}) == {:ok, 49}
  end

  test "imported effects propagate and use the same explicit handler" do
    {lib, provider, tree} =
      provider("(extern erlang:abs [:int] :int) (defn abs [x :int] :int (erlang:abs x))")

    {name, core} =
      consumer("(import #{lib} :as Lib) (defn main [] :int (Lib/abs -42))", %{lib => tree})

    script = {:script, [{:external, [:erlang, :abs, [-42]], {:ok, 42}}]}
    assert Runtime.run(name, :main, [], handler: script) == {:ok, 42}
    assert Eval.run(core, :main, [], handler: script, imports: %{lib => provider}) == {:ok, 42}
    assert [:def, :main, [:->, [], eff, :Int], _] = List.last(core)
    assert :external in eff
  end

  test "missing exports, wrong types, alias collisions and conflicting declarations fail closed" do
    {lib, _, tree} = provider("(deftype Point [x :int]) (defn f [x :int] :int x)")

    for source <- [
          "(import #{lib} :as Lib) (defn main [] :int (Lib/missing 1))",
          "(import #{lib} :as Lib) (defn main [] :int (Lib/f false))",
          "(import #{lib} :as Lib) (import #{lib} :as Lib)",
          "(deftype Point [x :string]) (import #{lib})"
        ] do
      assert {:error, [_ | _]} =
               source |> Parser.parse() |> Elab.module(interfaces: %{lib => tree})
    end

    assert {:error, [_ | _]} = "(import MissingInterface)" |> Parser.parse() |> Elab.module()
  end

  test "canonical interfaces round-trip exactly and reject the old Erlang-term format" do
    {lib, _, tree} = provider("(defn f [x :int] :int x)")
    dir = Path.join(System.tmp_dir!(), "vaisto-interface-#{System.unique_integer([:positive])}")
    on_exit(fn -> File.rm_rf!(dir) end)
    assert :ok = Interface.save_core(tree, dir)
    assert {:ok, interface} = Interface.load_core(lib, [dir])
    assert interface.tree == tree
    path = Path.join(dir, "#{lib}.vsi")

    File.write!(
      path,
      :erlang.term_to_binary(%{module: lib, version: 2, exports: %{}, types: %{}})
    )

    assert {:error, _} = Interface.load_core(lib, [dir])
  end

  test "implementation changes alter interface digests even when signatures stay identical" do
    name = :DigestProvider

    cores =
      for n <- [1, 2] do
        assert {:ok, core} =
                 "(defn value [] :int #{n})" |> Parser.parse() |> Elab.module(name: name)

        assert {:ok, tree} = Interface.from_core(core)
        tree
      end

    assert Enum.uniq(Enum.map(cores, &Canonical.digest/1)) |> length() == 2
  end

  test "interfaces cannot certify invalid Core or erase refined imports" do
    bad = [
      :module,
      :Bad,
      [:"core-version", 0],
      [:def, :f, [:->, [], [:eff, :closed], :Int], [:fn, [], "wrong"]]
    ]

    assert {:error, _} = Interface.from_core(bad)
    {lib, _, tree} = provider("(defn f [] :int 1)")

    tree =
      List.update_at(tree, 4, fn [:exports | exports] ->
        [:exports | Enum.map(exports, fn [f, t, _] -> [f, t, :refined] end)]
      end)

    source = "(import #{lib}) (defn main [] :int (#{lib}/f))"

    assert {:error, [error | _]} =
             source |> Parser.parse() |> Elab.module(interfaces: %{lib => tree})

    assert error.message =~ "refinements"

    for invalid <- [
          [],
          [:interface],
          List.replace_at(tree, 3, [:"ir-version", 99]),
          List.replace_at(tree, 7, [:status, :unchecked])
        ] do
      assert {:error, _} = Interface.validate_core(invalid)
    end
  end

  test "independent Lint rejects undeclared and malformed remote signatures" do
    for items <- [
          [],
          [[:import, :Lib, String.duplicate("0", 64), [[:f, :Int, :plain]]]],
          [[:import, :Lib, "not-a-digest", [[:f, [:->, [], [:eff, :closed], :Int], :plain]]]]
        ] do
      core = [
        :module,
        :Bad,
        [:"core-version", 0]
        | items ++
            [
              [
                :def,
                :main,
                [:->, [], [:eff, :closed], :Int],
                [:fn, [], [:app, [:remote, :Lib, :f]]]
              ]
            ]
      ]

      assert {:error, _} = Lint.check_module(core)
    end
  end
end
