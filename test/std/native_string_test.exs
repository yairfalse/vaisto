defmodule Vaisto.Std.NativeStringTest do
  use ExUnit.Case, async: true
  alias Vaisto.{Compilation, Elab, Parser}
  alias Vaisto.Liquid.{Eval, Lint, Runtime}

  test "the real String library compiles and runs through the native compiler" do
    source =
      File.read!("std/String.va") <>
        """
        (defn main [] (Tuple :string :string :int :bool :bool :bool :bool :string)
          {(repeat "ab" 5) (rest "hello") (length "hé")
           (starts-with? "hello" "hel") (contains? "hello" "zzz")
           (char-upper? 65) (char-digit? 57) (trim-leading "  hello")})
        """

    name = :"NativeString#{System.unique_integer([:positive])}"
    assert {:ok, core} = source |> Parser.parse() |> Elab.module(name: name)
    assert :ok = Lint.check_module(core)
    assert {:ok, ^name, _} = Compilation.compile_liquid(source, name, format_errors: false)

    on_exit(fn ->
      :code.purge(name)
      :code.delete(name)
    end)

    expected = {"ababababab", "ello", 3, true, false, true, true, "hello"}
    assert apply(name, :main, []) == expected

    events =
      for(
        xs <- [["", ""], ["ab", ""], ["ab", "ab"], ["abab", "abab"], ["ab", "abababab"]],
        do: {:external, [:erlang, :iolist_to_binary, [xs]], {:ok, Enum.join(xs)}}
      ) ++
        [
          {:external, [:erlang, :byte_size, ["hello"]], {:ok, 5}},
          {:external, [:binary, :part, ["hello", 1, 4]], {:ok, "ello"}},
          {:external, [:erlang, :byte_size, ["hé"]], {:ok, 3}},
          {:external, [:string, :prefix, ["hello", "hel"]], {:ok, "lo"}},
          {:external, [:string, :find, ["hello", "zzz"]], {:ok, :nomatch}},
          {:external, [:string, :trim, ["  hello", :leading]], {:ok, "hello"}}
        ]

    assert Runtime.run(name, :main, [], handler: {:script, events}) == {:ok, expected}
    assert Eval.run(core, :main, [], handler: {:script, events}) == {:ok, expected}
  end
end
