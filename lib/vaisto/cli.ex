defmodule Vaisto.CLI do
  @moduledoc """
  Command-line interface for the Vaisto compiler.

  Usage:
    vaistoc file.va              # Compile to file.beam
    vaistoc file.va -o out.beam  # Compile to specific output
    vaistoc --eval "(+ 1 2)"     # Evaluate expression
  """

  alias Vaisto.{Parser, TypeChecker, Emitter}

  def main(args) do
    case parse_args(args) do
      {:compile, input, output} ->
        compile_file(input, output)

      {:eval, code} ->
        eval_code(code)

      {:error, msg} ->
        IO.puts(:stderr, "error: #{msg}")
        System.halt(1)

      :help ->
        print_help()
    end
  end

  defp parse_args([]), do: :help
  defp parse_args(["--help"]), do: :help
  defp parse_args(["-h"]), do: :help

  defp parse_args(["--eval", code]), do: {:eval, code}
  defp parse_args(["-e", code]), do: {:eval, code}

  defp parse_args([input]) do
    # Output filename must match module name (capitalized)
    dir = Path.dirname(input)
    base = input |> Path.basename(".va") |> camelize() |> Atom.to_string()
    output = Path.join(dir, base <> ".beam")
    {:compile, input, output}
  end

  defp parse_args([input, "-o", output]), do: {:compile, input, output}
  defp parse_args(_), do: {:error, "invalid arguments. Use --help for usage."}

  defp compile_file(input, output) do
    case File.read(input) do
      {:ok, source} ->
        module_name = input |> Path.basename(".va") |> camelize()

        case compile(source, module_name) do
          {:ok, _module, bytecode} ->
            File.write!(output, bytecode)
            IO.puts("Compiled #{input} → #{output}")

          {:error, reason} ->
            IO.puts(:stderr, format_error(input, reason))
            System.halt(1)
        end

      {:error, reason} ->
        IO.puts(:stderr, "error: cannot read #{input}: #{reason}")
        System.halt(1)
    end
  end

  defp eval_code(code) do
    case compile(code, VaistoEval) do
      {:ok, module, _bytecode} ->
        result = module.main()
        IO.puts(inspect(result))

      {:error, reason} ->
        IO.puts(:stderr, format_error("<eval>", reason))
        System.halt(1)
    end
  end

  defp compile(source, module_name) do
    with ast <- Parser.parse(source),
         {:ok, _type, typed_ast} <- TypeChecker.check(ast) do
      Emitter.compile(typed_ast, module_name)
    end
  end

  defp format_error(file, reason) when is_binary(reason) do
    "#{file}: #{reason}"
  end

  defp format_error(file, reason) do
    "#{file}: #{inspect(reason)}"
  end

  defp print_help do
    IO.puts("""
    vaistoc - Vaisto compiler

    Usage:
      vaistoc <file.va>              Compile to <file.beam>
      vaistoc <file.va> -o <output>  Compile to specific output
      vaistoc --eval "<code>"        Evaluate expression
      vaistoc --help                 Show this help

    Examples:
      vaistoc counter.va
      vaistoc counter.va -o build/counter.beam
      vaistoc --eval "(+ 1 2)"
    """)
  end

  defp camelize(string) do
    name =
      string
      |> String.split(~r/[_\-]/)
      |> Enum.map(&String.capitalize/1)
      |> Enum.join()

    # Elixir modules need Elixir. prefix to be proper atoms
    Module.concat([String.to_atom(name)])
  end
end
