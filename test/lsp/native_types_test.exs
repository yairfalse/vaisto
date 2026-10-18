defmodule Vaisto.LSP.NativeTypesTest do
  use ExUnit.Case, async: true
  alias Vaisto.{Compilation, Parser}
  alias Vaisto.LSP.{Handler, Hover, InlayHints, Server}

  defp hover_last(source, line, name) do
    text = source |> String.split("\n") |> Enum.at(line - 1)
    {byte, _} = :binary.matches(text, name) |> List.last()
    col = text |> binary_part(0, byte) |> String.length() |> Kernel.+(1)
    assert {:ok, hover} = Hover.get_hover(source, line, col)
    hover.contents
  end

  test "hover asks the native checker at the actual shadowed occurrence" do
    source = """
    (let [x 42]
      (do
        (let [x "inner"] x)
        x))
    """

    assert hover_last(source, 3, "x") =~ "String"
    assert hover_last(source, 4, "x") =~ "Int"
  end

  test "sequential bindings and destructured variables retain their real types" do
    source = """
    (let [x 42
          y x
          x "last"]
      {y x})
    """

    assert hover_last(source, 2, "x") =~ "Int"
    assert hover_last(source, 4, "x") =~ "String"
    assert hover_last("(let [{x y} {1 \"text\"}] y)", 1, "y") =~ "String"
    assert Enum.map(InlayHints.collect(source), & &1["label"]) == [": Int", ": Int", ": String"]
  end

  test "queries inside guards preserve guard safety" do
    source = "(defn f [x :int :when (> x 0)] :int x) (f 1)"
    {byte, _} = :binary.match(source, "x 0")
    assert {:ok, hover} = Hover.get_hover(source, 1, byte + 1)
    assert hover.contents =~ "Int"

    unsafe = """
    (extern foreign:value [] :bool)
    (defn f [x :int :when (__vaisto_type_probe__ 0 (foreign:value))] :int x)
    """

    assert {:error, [error | _]} =
             Compilation.check_liquid(unsafe, :UnsafeQuery,
               type_probe: true,
               format_errors: false
             )

    assert error.message =~ "guard cannot"
  end

  test "native declarations show generic constraints without dictionary implementation types" do
    source = "(defn render [x :a] :string where [(Show a)] (show x))"
    assert {:ok, hover} = Hover.get_hover(source, 1, 7)
    assert hover.contents =~ "Show a"
    assert hover.contents =~ "-> String"
    refute hover.contents =~ "Tuple"
  end

  test "inlay hints query typed local values and lambda parameters in one native check" do
    source = "(let [x 42 y \"hi\" f (fn [n] (+ n 1))] (f x))"
    hints = InlayHints.collect(source)
    assert Enum.map(hints, & &1["label"]) == [": Int", ": String", ": (Int) -> Int", ": Int"]
    assert Enum.map(InlayHints.collect("(map (fn [x] x) [1 2])"), & &1["label"]) == [": Int"]
  end

  test "hint positions use UTF-16 units and never match names in strings or comments" do
    source = "(let [s \"😀\" x ; x is only a comment here\n 42] x)"
    hints = InlayHints.collect(source)
    assert Enum.map(hints, & &1["label"]) == [": String", ": Int"]
    assert List.last(hints)["position"] == %{"line" => 0, "character" => 14}
    assert Enum.map(InlayHints.collect("(let [s \"( ] }\"] s)"), & &1["label"]) == [": String"]
  end

  test "editor identities are disabled in normal compilation and cannot hide type errors" do
    source = "(defn main [] :int (__vaisto_type_probe__ 0 42))"

    assert {:error, [_ | _]} =
             Compilation.check_liquid(source, :NormalQuery, format_errors: false)

    assert {:ok, _} =
             Compilation.check_liquid(source, :TypedQuery, type_probe: true, format_errors: false)

    assert {:error, [_ | _]} =
             Compilation.check_liquid(String.replace(source, "0 42", "0 true"), :BadQuery,
               type_probe: true,
               format_errors: false
             )

    assert InlayHints.collect("(let [x 42] missing)") == []
  end

  test "source ranges come from the real tokenizer, including multiline expressions" do
    source = "x ; comment\n(fn [n] (+ n 1)) \"[not a delimiter]\""

    assert [{:x, "x", x}, {{:fn, _, _, _}, function, fn_loc}, {{:string, _}, _, _}] =
             Parser.parse_with_source(source, locations: true)

    assert x.length == 1
    assert function == "(fn [n] (+ n 1))"
    assert fn_loc.line == 2
    assert fn_loc.length == String.length(function)
  end

  test "the real hover protocol converts both incoming and outgoing UTF-16 positions" do
    source = "(let [s \"😀\" x 42] x)"
    uri = "file:///tmp/native-hover-unicode.va"

    request = %{
      method: "textDocument/hover",
      id: 1,
      params: %{
        "textDocument" => %{"uri" => uri},
        "position" => %{"line" => 0, "character" => 19}
      }
    }

    {response, _} = Handler.handle(request, %Server{documents: %{uri => source}})
    assert response.result["contents"]["value"] =~ "Int"

    assert response.result["range"] == %{
             "start" => %{"line" => 0, "character" => 19},
             "end" => %{"line" => 0, "character" => 20}
           }
  end
end
