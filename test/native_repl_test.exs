defmodule Vaisto.NativeREPLTest do
  use ExUnit.Case
  import ExUnit.CaptureIO
  alias Vaisto.REPL

  defp session(input), do: capture_io(input, fn -> assert REPL.start() == :ok end)

  test "actual interactive loop checks signatures, evaluates and transactionally redefines" do
    output =
      session("""
      (defn double [x :int] :int (* x 2))
      (double 21)
      (defn double [x :int] :int (+ x 1))
      (double 21)
      :env
      :q
      """)

    assert output =~ "Liquid Core"
    refute output =~ "Algorithm W"
    assert output =~ "=> 42"
    assert output =~ "=> 22"
    assert output =~ "double : (Fn Int Int)"
    assert :code.is_loaded(:VaistoREPL_0) == false
    assert :code.is_loaded(:VaistoREPL_1) == false
  end

  test "failed redefinitions leave both the original function and its callers intact" do
    output =
      session("""
      (defn f [x :int] :int (+ x 1))
      (defn g [x :int] :int (f x))
      (defn f [x :string] :string x)
      (g 41)
      :q
      """)

    assert output =~ "error: type mismatch"
    assert output =~ "=> 42"
  end

  test "unsigned definitions fail without installing a fallback" do
    output =
      session("""
      (defn f [x] (+ x 1))
      :env
      (defn f [x :int] :int (+ x 1))
      (f 41)
      :q
      """)

    assert output =~ "needs a signature"
    assert output =~ "No user-defined functions yet."
    assert output =~ "=> 42"
  end

  test "user main, generated-looking names and error-valued tuples do not collide" do
    output =
      session("""
      (defn main [] :int 99)
      (defn __vaisto_repl_entry_0 [] :int 5)
      (+ 1 2)
      {:error 42}
      (main)
      :q
      """)

    assert output =~ "=> 3"
    assert output =~ "=> {:error, 42}"
    assert output =~ "=> 99"
    refute output =~ "error:"
  end

  test "real load/save retains multiline Unicode source and ignores file expressions" do
    dir = Path.join(System.tmp_dir!(), "native-repl-#{System.unique_integer([:positive])}")
    File.mkdir_p!(dir)
    on_exit(fn -> File.rm_rf!(dir) end)
    input = Path.join(dir, "input.va")
    saved = Path.join(dir, "saved.va")

    File.write!(input, """
    ; source before the first declaration
    (defn greeting [] :string
      "héllo ) ; still a string")
    42
    (defn twice [x :int] :int
      (* x 2))
    """)

    output =
      session(
        ":load #{input}\n:save #{saved}\n:clear\n:load #{saved}\n(greeting)\n(twice 21)\n:q\n"
      )

    assert output =~ "Loaded definitions"
    assert output =~ "héllo ) ; still a string"
    assert output =~ "=> 42"
    refute output =~ "Error loading"
    text = File.read!(saved)
    assert text =~ "(defn greeting [] :string"
    assert text =~ "(defn twice [x :int] :int"
    refute text =~ "(defn ...)"
    refute text =~ "\n42\n"
    assert {:ok, _} = Vaisto.Compilation.check_liquid(text, :SavedREPL, format_errors: false)
  end

  test "invalid loads are atomic and explicit crashes clean up their native module" do
    path =
      Path.join(System.tmp_dir!(), "native-repl-invalid-#{System.unique_integer([:positive])}.va")

    on_exit(fn -> File.rm(path) end)
    File.write!(path, "(defn added [] :int 7) (defn broken [] :int true)")

    output =
      session(
        "(defn kept [] :int 42)\n:load #{path}\n:env\n(kept)\n(the :int (crash :bad_decode))\n(kept)\n:q\n"
      )

    assert output =~ "Error loading: type mismatch"
    refute output =~ "added :"
    assert output =~ "runtime error: :bad_decode"
    assert length(Regex.scan(~r/=> 42/, output)) == 2
    assert :code.is_loaded(:VaistoREPL_1) == false
  end
end
