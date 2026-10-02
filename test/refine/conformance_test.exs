defmodule Vaisto.Refine.ConformanceTest do
  # The conformance suite of liquid-vaisto-rfc.md §14, for Phase 2.1.
  #
  # Each program under test/refine/conformance/ states its expectation in a
  # header:
  #
  #   ; expect: compiles      and  ; main: <value>, run on both backends
  #   ; expect: errors        and one  ; error: message | label part | caret text
  #                           per expected error
  #
  # A diagnostic is asserted only on its normative parts (§14): the message
  # line, the failing conjunct (in the label), and the caret target.
  #
  # The suite needs z3 and fails without it, never skips (§14).
  use ExUnit.Case, async: true

  alias Vaisto.Compilation
  alias Vaisto.Refine.Solver

  @moduletag :z3

  @dir Path.join(__DIR__, "conformance")
  @programs @dir |> Path.join("*.va") |> Path.wildcard() |> Enum.sort()

  for path <- @programs do
    @program path
    test Path.basename(path, ".va") do
      check_program(@program)
    end
  end

  test "the suite covers the Phase 2.1 conformance programs" do
    names = Enum.map(@programs, &Path.basename/1)

    for prefix <- ~w(c01 c02 c03 c04 c05 c06 c07 c09 c10 c18 c20) do
      assert Enum.any?(names, &String.starts_with?(&1, prefix)), "no program for #{prefix}"
    end
  end

  # C11: erasing the refinements changes nothing the backends produce.
  test "C11: a refined program and its refinement-free text compile to the same bytes on both backends" do
    for path <- @programs, source = File.read!(path), header(source).expect == "compiles", backend <- [:core, :elixir] do
      stripped = strip_refinements(source)
      refute stripped == source

      assert {:ok, _, refined} = Compilation.compile(source, :ErasureProbe, backend: backend, load: false)
      assert {:ok, _, plain} = Compilation.compile(stripped, :ErasureProbe, backend: backend, load: false)
      assert refined == plain, "#{Path.basename(path)} on #{backend}"
    end
  end

  # C13: an unknown answer never counts as proved.
  test "C13: with a solver that answers unknown, C1 fails with could not verify" do
    source = File.read!(Path.join(@dir, "c01_safe_div_guarded.va"))
    assert {:error, errors} = Compilation.compile(source, :UnknownProbe, load: false, format_errors: false, solver: Solver.Unknown)
    assert Enum.any?(errors, &(&1.message == "could not verify this requirement within its resource limit"))
  end

  # --- Running one program -----------------------------------------------------------

  defp check_program(path) do
    source = File.read!(path)
    header = header(source)

    case header.expect do
      "compiles" ->
        {expected, _} = Code.eval_string(header.main)

        for backend <- [:core, :elixir] do
          module = :"Conformance_#{:erlang.unique_integer([:positive])}"
          assert {:ok, ^module, _} = Compilation.compile(source, module, backend: backend, format_errors: false)

          try do
            assert apply(module, :main, []) == expected, "#{Path.basename(path)} on #{backend}"
          after
            :code.purge(module)
            :code.delete(module)
          end
        end

      "errors" ->
        assert {:error, errors} = Compilation.compile(source, :ConformanceError, load: false, format_errors: false)
        assert length(errors) == length(header.errors), "expected #{length(header.errors)} errors, got:\n#{describe(errors)}"

        for {message, label, caret} <- header.errors do
          assert Enum.any?(errors, &matches?(&1, message, label, caret, source)),
                 "no error `#{message}` with label containing #{inspect(label)} at #{inspect(caret)}, in:\n#{describe(errors)}"
        end
    end
  end

  defp matches?(error, message, label, caret, source) do
    span = error.primary_span

    error.message == message and span != nil and String.contains?(span.label || "", label) and
      caret_text(source, span) == caret
  end

  defp caret_text(source, %{line: line, col: col, length: length}) do
    source |> String.split("\n") |> Enum.at(line - 1, "") |> String.slice(col - 1, length)
  end

  defp describe(errors), do: Enum.map_join(errors, "\n", &"  #{&1.message} (#{inspect(&1.primary_span)})")

  defp header(source) do
    lines = for "; " <> line <- String.split(source, "\n"), do: line

    %{
      expect: Enum.find_value(lines, fn "expect: " <> e -> String.trim(e); _ -> nil end),
      main: Enum.find_value(lines, fn "main: " <> m -> m; _ -> nil end),
      errors:
        for "error: " <> e <- lines do
          [message, label, caret] = e |> String.split("|") |> Enum.map(&String.trim/1)
          {message, label, caret}
        end
    }
  end

  # The program as it would be written without refinements: {v :int | p}
  # becomes :int, and {v (List :int) | p} becomes (List :int).
  defp strip_refinements(source) do
    case :binary.match(source, "{") do
      :nomatch ->
        source

      {at, 1} ->
        {inside, rest} = until_close(binary_part(source, at + 1, byte_size(source) - at - 1), 0, "")
        [left, _predicate] = String.split(inside, " | ", parts: 2)
        [_binder, base] = String.split(left, " ", parts: 2)
        binary_part(source, 0, at) <> base <> strip_refinements(rest)
    end
  end

  defp until_close("}" <> rest, 0, acc), do: {acc, rest}
  defp until_close("}" <> rest, depth, acc), do: until_close(rest, depth - 1, acc <> "}")
  defp until_close("{" <> rest, depth, acc), do: until_close(rest, depth + 1, acc <> "{")
  defp until_close(<<c::utf8, rest::binary>>, depth, acc), do: until_close(rest, depth, acc <> <<c::utf8>>)
end
