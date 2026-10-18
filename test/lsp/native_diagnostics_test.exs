defmodule Vaisto.LSP.NativeDiagnosticsTest do
  use ExUnit.Case, async: true
  alias Vaisto.LSP.{Handler, Server}

  defp diagnostics(source) do
    uri = "file:///tmp/native-diagnostics.va"

    request = %{
      method: "textDocument/didOpen",
      id: nil,
      params: %{"textDocument" => %{"uri" => uri, "text" => source}}
    }

    {notification, state} = Handler.handle(request, %Server{})
    assert state.documents[uri] == source
    assert notification.method == "textDocument/publishDiagnostics"
    notification.params["diagnostics"]
  end

  test "diagnostics follow required signatures and actual Core type checking" do
    assert [] == diagnostics("(defn double [x :int] :int (* 2 x)) (double 21)")
    assert [missing] = diagnostics("(defn double [x] (* 2 x))")
    assert missing["message"] =~ "needs a signature"
    assert [wrong] = diagnostics("(defn double [x :int] :int (* 2 false))")
    assert wrong["message"] =~ "type mismatch"
  end

  test "diagnostics check refinements and syntax rather than dropping either" do
    assert [] == diagnostics("(defn f [x :int] {r :int | (>= r x)} (+ x 1))")
    assert [wrong] = diagnostics("(defn f [x :int] {r :int | (> r x)} x)")
    assert wrong["message"] =~ "result does not satisfy"
    assert [_ | _] = diagnostics("(defn broken [")
  end
end
