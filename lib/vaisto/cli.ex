defmodule Vaisto.CLI do
  @moduledoc """
  Command-line interface for the Vaisto compiler.

  Usage:
    vaistoc file.va                     # Compile to file.beam (Core Erlang backend)
    vaistoc file.va -o out.beam         # Compile to specific output
    vaistoc file.va --backend elixir    # Use Elixir backend instead
    vaistoc --eval "(+ 1 2)"            # Evaluate expression
    vaistoc repl                        # Start interactive REPL

  The standard prelude (Result, Option types) is automatically included.
  """

  alias Vaisto.{Parser, TypeChecker, Emitter, CoreEmitter}

  # Embed the prelude at compile time
  @external_resource Path.join(__DIR__, "../../std/prelude.va")
  @prelude File.read!(Path.join(__DIR__, "../../std/prelude.va"))

  def main(args) do
    case parse_args(args) do
      {:compile, input, output, backend} ->
        compile_file(input, output, backend)

      {:build, source_dir, output_dir, backend} ->
        build_project(source_dir, output_dir, backend)

      {:eval, code, backend} ->
        eval_code(code, backend)

      :repl ->
        Vaisto.REPL.start()

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
  defp parse_args(["repl"]), do: :repl

  # Build project (all .va files in directory)
  defp parse_args(["build"]), do: {:build, ".", ".", :core}
  defp parse_args(["build", dir]), do: {:build, dir, dir, :core}
  defp parse_args(["build", dir, "-o", out_dir]), do: {:build, dir, out_dir, :core}
  defp parse_args(["build", dir, "--backend", backend]), do: {:build, dir, dir, parse_backend(backend)}
  defp parse_args(["build", dir, "-o", out_dir, "--backend", backend]), do: {:build, dir, out_dir, parse_backend(backend)}

  defp parse_args(["--eval", code]), do: {:eval, code, :core}
  defp parse_args(["-e", code]), do: {:eval, code, :core}
  defp parse_args(["--eval", code, "--backend", backend]), do: {:eval, code, parse_backend(backend)}
  defp parse_args(["-e", code, "--backend", backend]), do: {:eval, code, parse_backend(backend)}

  defp parse_args([input]) do
    {:compile, input, default_output(input), :core}
  end

  defp parse_args([input, "-o", output]) do
    {:compile, input, output, :core}
  end

  defp parse_args([input, "--backend", backend]) do
    {:compile, input, default_output(input), parse_backend(backend)}
  end

  defp parse_args([input, "-o", output, "--backend", backend]) do
    {:compile, input, output, parse_backend(backend)}
  end

  defp parse_args([input, "--backend", backend, "-o", output]) do
    {:compile, input, output, parse_backend(backend)}
  end

  defp parse_args(_), do: {:error, "invalid arguments. Use --help for usage."}

  defp parse_backend("core"), do: :core
  defp parse_backend("elixir"), do: :elixir
  defp parse_backend(other), do: {:error, "unknown backend: #{other}. Use 'core' or 'elixir'."}

  defp default_output(input) do
    dir = Path.dirname(input)
    module_name = input |> Path.basename(".va") |> camelize()
    # BEAM files must match the module name exactly
    Path.join(dir, Atom.to_string(module_name) <> ".beam")
  end

  defp compile_file(input, output, backend) do
    case File.read(input) do
      {:ok, source} ->
        module_name = input |> Path.basename(".va") |> camelize()

        case compile(source, module_name, backend) do
          {:ok, _module, bytecode} when is_binary(bytecode) ->
            # Ensure output directory exists
            output |> Path.dirname() |> File.mkdir_p!()
            File.write!(output, bytecode)
            IO.puts("✓ Compiled #{input} → #{output}")

          {:ok, _module, [{_mod, bytecode} | _rest]} when is_binary(bytecode) ->
            # Elixir emitter returns list of {module, bytecode} tuples
            output |> Path.dirname() |> File.mkdir_p!()
            File.write!(output, bytecode)
            IO.puts("✓ Compiled #{input} → #{output}")

          {:error, reason} ->
            IO.puts(:stderr, format_error(input, reason))
            System.halt(1)
        end

      {:error, reason} ->
        IO.puts(:stderr, "error: cannot read #{input}: #{reason}")
        System.halt(1)
    end
  end

  defp eval_code(code, backend) do
    case compile(code, VaistoEval, backend) do
      {:ok, module, bytecode} ->
        # Load the module into the VM
        :code.load_binary(module, ~c"vaisto_eval", bytecode)
        result = module.main()
        IO.puts(inspect(result))
        # Clean up
        :code.purge(module)
        :code.delete(module)

      {:error, reason} ->
        IO.puts(:stderr, format_error("<eval>", reason))
        System.halt(1)
    end
  end

  defp build_project(source_dir, output_dir, backend) do
    IO.puts("Building project in #{source_dir}...")

    case Vaisto.Build.build(source_dir, output_dir: output_dir, backend: backend, prelude: @prelude) do
      {:ok, results} ->
        Enum.each(results, fn result ->
          IO.puts("✓ Compiled #{result.module} → #{result.beam}")
        end)
        IO.puts("\n#{length(results)} module(s) compiled successfully.")

      {:error, errors} when is_list(errors) ->
        Enum.each(errors, fn {:error, msg} ->
          IO.puts(:stderr, "error: #{msg}")
        end)
        System.halt(1)

      {:error, reason} ->
        IO.puts(:stderr, "error: #{reason}")
        System.halt(1)
    end
  end

  defp compile(source, module_name, backend) do
    # Prepend prelude to get standard types (Result, Option) and helpers
    full_source = @prelude <> "\n\n" <> source

    with {:ok, ast} <- parse(full_source),
         {:ok, _type, typed_ast} <- TypeChecker.check(ast) do
      case backend do
        :core -> CoreEmitter.compile(typed_ast, module_name)
        :elixir -> Emitter.compile(typed_ast, module_name)
        {:error, msg} -> {:error, msg}
      end
    end
  end

  defp parse(source) do
    try do
      {:ok, Parser.parse(source)}
    rescue
      e -> {:error, Exception.message(e)}
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
    vaistoc - The Vaisto Compiler

    Usage:
      vaistoc <file.va>                     Compile to <File>.beam
      vaistoc <file.va> -o <output>         Compile to specific output
      vaistoc <file.va> --backend <backend> Use specific backend (core|elixir)
      vaistoc build [dir]                   Build all .va files in directory
      vaistoc build [dir] -o <output_dir>   Build with custom output directory
      vaistoc --eval "<code>"               Evaluate expression
      vaistoc repl                          Start interactive REPL
      vaistoc --help                        Show this help

    Backends:
      core    - Direct Core Erlang compilation (default, smaller output)
      elixir  - Compile via Elixir AST (more features, larger output)

    Module System:
      (ns MyModule)                         Declare module name
      (import Std.List)                     Import another module
      (import Std.List :as L)               Import with alias
      (Std.List/fold xs 0 +)                Call imported function

    Examples:
      vaistoc counter.va
      vaistoc counter.va -o build/Counter.beam
      vaistoc main.va --backend elixir
      vaistoc build src/ -o build/
      vaistoc --eval "(+ 1 2)"
      vaistoc --eval "(deftype Result (Ok v) (Error e)) (Ok 42)"
    """)
  end

  defp camelize(string) do
    name =
      string
      |> String.split(~r/[_\-]/)
      |> Enum.map(&String.capitalize/1)
      |> Enum.join()

    Module.concat([String.to_atom(name)])
  end
end
