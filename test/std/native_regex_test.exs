defmodule Vaisto.Std.NativeRegexTest do
  use ExUnit.Case, async: true
  alias Vaisto.{Compilation, Elab, Parser}
  alias Vaisto.Liquid.{Eval, Lint, Runtime}

  setup do
    source =
      File.read!("std/prelude.va") <>
        "\n" <>
        File.read!("std/Regex.va")

    name = :"NativeRegex#{System.unique_integer([:positive])}"
    assert {:ok, core} = source |> Parser.parse() |> Elab.module(name: name)
    assert :ok = Lint.check_module(core)
    assert {:ok, ^name, _} = Compilation.compile_liquid(source, name, format_errors: false)

    on_exit(fn ->
      :code.purge(name)
      :code.delete(name)
    end)

    %{name: name, core: core}
  end

  test "real regex matching, captures and replacements preserve their typed payloads", %{
    name: name
  } do
    assert {:Ok, compiled} = apply(name, :compile, ["[0-9]+"])
    assert apply(name, :matches?, ["a12", compiled])
    refute apply(name, :matches?, ["abc", compiled])
    assert apply(name, :find, ["a12b", compiled]) == {:Some, "12"}
    assert apply(name, :"find-all", ["a1b22", compiled]) == ["1", "22"]

    assert apply(name, :"find-pos", ["abc12z", compiled]) ==
             {:Some, %{start: 3, len: 2, match: "12"}}

    assert apply(name, :replace, ["a1b22", compiled, "X"]) == "aXb22"
    assert apply(name, :"replace-all", ["a1b22", compiled, "X"]) == "aXbX"
    assert apply(name, :split, ["a,b,,c", ","]) == ["a", "b", "", "c"]
    assert apply(name, :"split-n", ["a,b,c", ",", 2]) == ["a", "b,c"]
    assert {:Err, _} = apply(name, :compile, ["["])
  end

  test "lexer helpers retain heterogeneous maps and position tracking", %{name: name} do
    assert apply(name, :"match-at", ["123abc", "[0-9]+"]) == {:Some, %{match: "123", rest: "abc"}}

    assert apply(name, :"match-groups", ["123abc rest", "([0-9]+)([a-z]+)"]) ==
             {:Some, %{full: "123abc", groups: ["123", "abc"], rest: " rest"}}

    assert apply(name, :"match-first", ["123abc", [{:number, "[0-9]+"}, {:ident, "[a-z]+"}]]) ==
             {:Some, %{type: :number, match: "123", rest: "abc"}}

    assert apply(name, :"match-first-groups", ["123abc!", [{:number, "([0-9]+)([a-z]+)"}]]) ==
             {:Some, %{type: :number, full: "123abc", groups: ["123", "abc"], rest: "!"}}

    assert apply(name, :"skip-whitespace", [" \n  abc"]) == %{
             skipped: " \n  ",
             rest: "abc",
             lines: 1,
             cols: 2
           }

    assert apply(name, :"skip-whitespace", ["abc"]) == %{
             skipped: "",
             rest: "abc",
             lines: 0,
             cols: 0
           }

    assert apply(name, :peek, ["abc", 2]) == "ab"
    assert apply(name, :"starts-with?", ["abc", "ab"])
  end

  test "scripted and native paths independently reject malformed deep captures", %{
    name: name,
    core: core
  } do
    options = [{:capture, :first, :binary}]
    args = ["a12", "[0-9]+", options]

    for response <- [{:match, [42]}, {:match, ["12" | :bad_tail]}, {:wrong, ["12"]}, :unexpected] do
      script = {:script, [{:external, [:re, :run, args], {:ok, response}}]}

      assert Runtime.run(name, :find, Enum.take(args, 2), handler: script) ==
               {:crash, :bad_decode}

      assert Eval.run(core, :find, Enum.take(args, 2), handler: script) == {:crash, :bad_decode}
    end

    script = {:script, [{:external, [:re, :run, args], {:ok, {:match, ["12"]}}}]}
    assert Runtime.run(name, :find, Enum.take(args, 2), handler: script) == {:ok, {:Some, "12"}}
    assert Eval.run(core, :find, Enum.take(args, 2), handler: script) == {:ok, {:Some, "12"}}
  end
end
