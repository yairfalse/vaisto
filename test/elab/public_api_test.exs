defmodule Vaisto.Elab.PublicAPITest do
  use ExUnit.Case, async: true
  alias Vaisto.Liquid.{Eval, Lint}

  test "the removed compiler engines are not available as fallback modules" do
    for module <- [
          Vaisto.TypeChecker,
          Vaisto.TypeChecker.TcCtx,
          Vaisto.TypeSystem.Infer,
          Vaisto.TypeSystem.Context,
          Vaisto.TypeSystem.Core,
          Vaisto.TypeSystem.Unify,
          Vaisto.TypeEnv,
          Vaisto.Liquid.Adapter,
          Vaisto.Elab.Shadow,
          Vaisto.Emitter,
          Vaisto.CoreEmitter,
          Vaisto.Backend,
          Vaisto.Backend.Core,
          Vaisto.Backend.Elixir,
          Vaisto.Backend.Shared,
          Vaisto.LocationStripper
        ] do
      refute Code.ensure_loaded?(module), "retired module still available: #{inspect(module)}"
    end
  end

  test "backend options cannot silently select a retired compiler" do
    for backend <- [:core, :elixir, :unknown] do
      assert {:error, [%{message: "backend selection has been removed"}]} =
               Vaisto.Compilation.compile("42", :NoBackendFallback,
                 backend: backend,
                 load: false,
                 format_errors: false
               )

      assert :code.is_loaded(:NoBackendFallback) == false
    end
  end

  test "the def function alias shares explicit signatures, guards and native lowering" do
    source = "(def double [x :int :when (>= x 0)] :int (* x 2)) (def main [] :int (double 21))"
    assert {:ok, core} = Vaisto.check(source)
    assert :ok = Lint.check_module(core)
    assert {:ok, 42} = Eval.run(core, :main, [])
    assert {:ok, 42} = Vaisto.Compilation.run(source)
    assert {:error, [%{message: "`f` needs a signature"}]} = Vaisto.check("(def f [x] x)")

    # A bracket value remains a value, not an empty function definition.
    assert {:defval, :xs, {:bracket, [1, 2]}, _} = Vaisto.Parser.parse("(def xs [1 2])")
  end

  test "the public API checks and natively executes ordinary expression programs" do
    for {source, value} <- [
          {"(+ 40 2)", 42},
          {"(let [x 6] (* x 7))", 42},
          {"(defn double [n :int] :int (* 2 n)) (double 21)", 42},
          {"(str (show 42) (neq 1 2))", "42true"}
        ] do
      name = :"PublicNative#{System.unique_integer([:positive])}"
      assert {:ok, core} = Vaisto.check(source)
      assert :ok = Lint.check_module(core)
      assert Eval.run(core, :main, []) == {:ok, value}
      assert {:ok, ^name, beam} = Vaisto.compile_string(source, name)

      try do
        assert is_binary(beam)
        assert apply(name, :main, []) == value
      after
        :code.purge(name)
        :code.delete(name)
      end
    end
  end

  test "top-level effects keep their permissions and unsafe callbacks still fail" do
    source = "(extern erlang:abs [:int] :int) (erlang:abs -42)"
    assert {:ok, [:module, _, _ | items]} = Vaisto.check(source)
    assert [:def, :main, [:->, [], eff, :Int], _] = List.last(items)
    assert :external in eff
    assert {:error, [_ | _]} = Vaisto.check("(defn f [x] (+ x 1)) (f 1)")
    assert {:error, [_ | _]} = Vaisto.check("(defn main [] :int 0) 1")

    assert {:error, [_ | _]} =
             Vaisto.check(
               "(extern erlang:abs [:int] :int) (defn pure-map [f (Fn :int :int) xs (List :int)] (List :int) (map f xs)) (pure-map (fn [x] (erlang:abs x)) [1])"
             )
  end

  test "file compilation writes actual BEAM and leaves existing output intact on failure" do
    dir = Path.join(System.tmp_dir!(), "public-api-#{System.unique_integer([:positive])}.va")
    File.mkdir_p!(dir)
    on_exit(fn -> File.rm_rf!(dir) end)
    source = Path.join(dir, "main.va")
    output = Path.join(dir, "main.beam")
    File.write!(source, "(+ 40 2)")
    assert {:ok, ^output} = Vaisto.compile(source)
    assert {:ok, <<"FOR1", _::binary>> = beam} = File.read(output)
    File.write!(source, "(+ 1 false)")
    assert {:error, _} = Vaisto.compile(source)
    assert File.read!(output) == beam
    :code.purge(:VaistoModule)
    :code.delete(:VaistoModule)
  end
end
