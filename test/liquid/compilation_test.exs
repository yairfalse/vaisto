defmodule Vaisto.Liquid.CompilationTest do
  use ExUnit.Case, async: true

  alias Vaisto.{Compilation, Elab, Parser, Refine}
  alias Vaisto.Liquid.{Canonical, Runtime}
  alias Vaisto.Refine.{Solver, Surface}

  defp compile(source, opts \\ []) do
    name = :"LiquidPipeline#{System.unique_integer([:positive])}"
    {name, Compilation.compile_liquid(source, name, Keyword.put_new(opts, :format_errors, false))}
  end

  test "the native source pipeline compiles and executes a real BEAM module" do
    {name, result} = compile("(defn main [] :int (+ 40 2))")
    assert {:ok, ^name, beam} = result
    assert {:ok, {^name, _}} = :beam_lib.chunks(beam, [:exports])

    try do
      assert apply(name, :main, []) == 42
    after
      :code.purge(name)
      :code.delete(name)
    end
  end

  test "the pipeline never falls back to inferred signatures or undeclared foreign calls" do
    {name, result} = compile("(defn main [] (+ 40 2))", load: false)
    assert {:error, [error]} = result
    assert error.message == "`main` needs a signature"
    assert :code.is_loaded(name) == false

    {name, result} = compile("(defn main [] :int (erlang:abs -1))", load: false)
    assert {:error, [error]} = result
    assert error.message =~ "undeclared foreign call"
    assert :code.is_loaded(name) == false
  end

  test "diagnostic options preserve explicit line offsets and source filenames" do
    source = "; generated header\n; generated header\n(+ 1 true)"

    assert {:error, formatted} =
             Compilation.check_liquid(source, :Offset, line_offset: 2, file: "offset.va")

    assert formatted =~ "at offset.va:1\n"
    assert formatted =~ "(+ 1 true)"
    refute formatted =~ "generated header"

    assert {:error, [error]} =
             Compilation.check_liquid("(defn unsigned [x] x)", :Filename,
               file: "signature.va",
               format_errors: false
             )

    assert error.file == "signature.va"
  end

  test "an ordinary native program does not start or consult a solver" do
    {name, result} = compile("(defn main [] :int 42)", load: false, solver: MissingSolver)
    assert {:ok, ^name, _} = result
    assert :code.is_loaded(name) == false
  end

  @tag :z3
  test "refinements are checked on new Core before native code can be loaded" do
    safe = "(defn safe-div [x :int y {d :int | (!= d 0)}] :int (div x y))\n"
    {name, result} = compile(safe <> "(defn main [] :int (safe-div 12 3))")
    assert {:ok, ^name, _} = result

    try do
      assert apply(name, :main, []) == 4
    after
      :code.purge(name)
      :code.delete(name)
    end

    {name, result} = compile(safe <> "(defn main [] :int (safe-div 12 0))")
    assert {:error, errors} = result
    assert Enum.any?(errors, &(&1.message == "requirement not met"))
    assert :code.is_loaded(name) == false
  end

  test "unknown solver answers cannot produce native bytecode" do
    source = "(defn positive [x :int] {r :int | (> r 0)} (+ x 1))"
    {name, result} = compile(source, solver: Solver.Unknown)
    assert {:error, errors} = result
    assert errors != []
    assert :code.is_loaded(name) == false
  end

  test "refinement signatures cannot be dropped or attached to the wrong Core definition" do
    source = "(defn positive [x :int] {r :int | (> r 0)} (+ x 1))"
    {plain, sigs} = source |> Parser.parse() |> Surface.split()
    assert {:ok, core} = Elab.module(plain)

    assert {:error, [error]} =
             Refine.check_core(core, %{missing: sigs.positive}, solver: MissingSolver)

    assert error.message =~ "no definition"

    wrong =
      Canonical.parse!(
        "(module Wrong (core-version 0) (def positive (-> (Int) (eff closed) String) (fn ((x Int)) \"bad\")))",
        atoms: :create
      )

    assert {:error, [error]} = Refine.check_core(wrong, sigs, solver: MissingSolver)
    assert error.message =~ "does not match"
  end

  test "Lint gates even an unrefined module in the native refinement path" do
    invalid =
      Canonical.parse!(
        "(module Wrong (core-version 0) (def main (-> () (eff closed) Int) (fn () 1.5)))",
        atoms: :create
      )

    assert {:error, [_ | _]} = Refine.check_core(invalid, %{}, solver: MissingSolver)
  end

  test "scripted foreign boundaries cannot evade refinement verification" do
    source = "(extern foreign:value [] :int) (defn main [] :int (foreign:value))"
    {name, result} = compile(source)
    assert {:ok, ^name, _} = result

    try do
      assert {:crash, :bad_decode} =
               Runtime.run(name, :main, [],
                 handler: {:script, [{:external, [:foreign, :value, []], {:ok, "wrong"}}]}
               )
    after
      :code.purge(name)
      :code.delete(name)
    end

    {name, result} =
      compile("(extern foreign:value [] :int) (defn main [] {r :int | (> r 0)} (foreign:value))",
        solver: MissingSolver
      )

    assert {:error, errors} = result
    assert Enum.any?(errors, &(&1.message =~ "does not admit"))
    assert :code.is_loaded(name) == false
  end

  test "native foreign calls execute only at runtime and retain result decoding and exception normalization" do
    for {source, expected} <- [
          {"(extern erlang:abs [:int] :int) (defn main [] :int (erlang:abs -42))", {:ok, 42}},
          {~s|(extern String/length [:string] :int) (defn main [] :int (String/length "hé"))|,
           {:ok, 2}},
          {"(extern erlang:abs [:int] :string) (defn main [] :string (erlang:abs -42))",
           {:crash, :bad_decode}},
          {"(extern erlang:error [:atom] :dyn) (defn main [] :dyn (erlang:error :boom))",
           {:crash, {:raised, :error, :boom}}}
        ] do
      {name, result} = compile(source)
      assert {:ok, ^name, _} = result

      try do
        assert Runtime.run(name, :main, [], handler: :native) == expected
        assert_raise ArgumentError, fn -> Runtime.run(name, :main, []) end
      after
        :code.purge(name)
        :code.delete(name)
      end
    end
  end

  test "native errors retain formatted source context when requested" do
    {_, result} = compile("(defn main [] :int \"wrong\")", load: false, format_errors: true)
    assert {:error, message} = result
    assert is_binary(message)
    assert message =~ "type mismatch"
    assert message =~ "wrong"
  end
end
