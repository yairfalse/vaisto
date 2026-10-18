defmodule Vaisto.CLI do
  @moduledoc """
  Command-line interface for the Vaisto compiler.

  Usage:
    vaistoc file.va                     # Compile verified Core to native BEAM
    vaistoc file.va -o out.beam         # Compile to specific output
    vaistoc --eval "(+ 1 2)"            # Evaluate expression
    vaistoc repl                        # Start interactive REPL

  The standard prelude (Result, Option types) is automatically included.
  """

  alias Vaisto.{Compilation, Errors}
  alias Vaisto.Package.{Manifest, Namespace}

  # Embed the prelude at compile time
  @external_resource Path.join(__DIR__, "../../std/prelude.va")
  @prelude File.read!(Path.join(__DIR__, "../../std/prelude.va"))

  def main(args) do
    case parse_args(args) do
      {:compile, input, output} ->
        compile_file(input, output)

      {:build, source_dir, output_dir, src_roots} ->
        build_dir(source_dir, output_dir, src_roots)

      :build_manifest ->
        build_manifest(".")

      {:init, name} ->
        init_project(name)

      {:add, path} ->
        add_dependency(path)

      {:eval, code} ->
        eval_code(code)

      :repl ->
        Vaisto.REPL.start()

      :lsp ->
        start_lsp()

      {:error, msg} ->
        IO.puts(:stderr, "error: #{msg}")
        System.halt(1)

      :help ->
        print_help()
    end
  end

  defp parse_args([]), do: :help
  defp parse_args(["repl" | _]), do: :repl
  defp parse_args(["lsp" | _]), do: :lsp
  defp parse_args(["init" | rest]), do: parse_init(rest)
  defp parse_args(["add" | rest]), do: parse_add(rest)
  defp parse_args(["build" | rest]), do: parse_build(rest)
  defp parse_args(args), do: parse_compile_or_eval(args)

  defp parse_compile_or_eval(args) do
    {opts, positional, invalid} =
      OptionParser.parse(args,
        strict: [output: :string, eval: :string, help: :boolean],
        aliases: [o: :output, e: :eval, h: :help]
      )

    case invalid do
      [{flag, _} | _] -> {:error, Errors.cli_error("unknown option: #{flag}", hint: "Run vaistoc --help to see available options")}
      [] ->
        cond do
          opts[:help] ->
            :help

          eval = opts[:eval] ->
            {:eval, eval}

          length(positional) == 1 ->
            [input] = positional
            output = opts[:output] || default_output(input)
            {:compile, input, output}

          true ->
            {:error, Errors.cli_error("invalid arguments", hint: "Run vaistoc --help to see available options")}
        end
    end
  end

  defp parse_build(args) do
    {opts, positional, invalid} =
      OptionParser.parse(args,
        strict: [output: :string, src: :keep],
        aliases: [o: :output]
      )

    case invalid do
      [{flag, _} | _] ->
        {:error, Errors.cli_error("unknown build option: #{flag}", hint: "Run vaistoc --help to see available options")}

      [] ->
        # No positional args = try manifest mode.
        if positional == [] and Manifest.exists?(".") do
          :build_manifest
        else
          dir = List.first(positional) || "."
          output = opts[:output] || dir

          src_roots =
            opts
            |> Keyword.get_values(:src)
            |> Enum.map(fn root ->
              case String.split(root, ":", parts: 2) do
                [path, prefix] -> {path, prefix}
                [path] -> {path, ""}
              end
            end)

          {:build, dir, output, src_roots}
        end
    end
  end

  defp parse_init([name | _]) when byte_size(name) > 0, do: {:init, name}
  defp parse_init([]), do: {:init, Path.basename(File.cwd!())}

  defp parse_add([path | _]) when byte_size(path) > 0, do: {:add, path}
  defp parse_add([]), do: {:error, Errors.cli_error("add requires a path argument", hint: "Usage: vaistoc add ../my-dep")}

  defp default_output(input) do
    dir = Path.dirname(input)
    # Use path-based module naming
    module_name = Vaisto.Build.infer_module_name(input)
    # BEAM files must match the module name exactly
    Path.join(dir, Atom.to_string(module_name) <> ".beam")
  end

  defp compile_file(input, output) do
    unless String.ends_with?(input, ".va") do
      IO.puts(:stderr, "error: expected a .va file, got #{Path.extname(input) |> then(fn "" -> "no extension"; ext -> ext end)}: #{input}")
      System.halt(1)
    end

    case File.read(input) do
      {:ok, source} ->
        # Use path-based module naming
        module_name = Vaisto.Build.infer_module_name(input)

        case Compilation.compile(source, module_name, prelude: @prelude) do
          {:ok, _module, bytecode} when is_binary(bytecode) ->
            write_beam(input, output, bytecode)

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
    if String.trim(code) == "" do
      IO.puts(:stderr, "error: empty expression")
      System.halt(1)
    end

    case Compilation.run(code, prelude: @prelude) do
      {:ok, result} ->
        IO.puts(inspect(result))

      {:error, reason} ->
        IO.puts(:stderr, format_error("<eval>", reason))
        System.halt(1)
    end
  end

  defp build_dir(source_dir, output_dir, src_roots) do
    IO.puts("  Building #{source_dir} ...")

    # Use custom source roots if provided, otherwise use defaults
    opts = [output_dir: output_dir, prelude: @prelude]
    opts = if src_roots != [], do: Keyword.put(opts, :source_roots, src_roots), else: opts

    case Vaisto.Build.build(source_dir, opts) do
      {:ok, results} ->
        print_build_results(results)

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

  defp build_manifest(dir) do
    start = System.monotonic_time(:millisecond)

    case Vaisto.Build.build_project(dir, prelude: @prelude) do
      {:ok, results} ->
        elapsed = System.monotonic_time(:millisecond) - start
        print_build_results(results, elapsed)

      {:error, reason} ->
        IO.puts(:stderr, "error: #{reason}")
        System.halt(1)
    end
  end

  defp init_project(name) do
    case Manifest.validate_name(name) do
      {:error, msg} ->
        IO.puts(:stderr, "error: #{msg}")
        System.halt(1)

      :ok -> :ok
    end

    prefix = Namespace.to_prefix(name)

    # Create directory structure
    dir = name
    src_dir = Path.join(dir, "src")

    if File.exists?(dir) do
      IO.puts(:stderr, "error: directory `#{dir}` already exists")
      System.halt(1)
    end

    File.mkdir_p!(src_dir)

    # Write vaisto.toml
    manifest_path = Path.join(dir, "vaisto.toml")
    File.write!(manifest_path, Manifest.generate(name))

    # Write root module
    module_path = Path.join(src_dir, "#{prefix}.va")

    File.write!(module_path, """
    (ns #{prefix})

    (defn hello [] :string
      "hello from #{name}")
    """)

    IO.puts("  Created #{manifest_path}")
    IO.puts("  Created #{module_path}")
    IO.puts("")
    IO.puts("  cd #{name} && vaistoc build")
  end

  defp add_dependency(path) do
    manifest_path = Path.join(".", "vaisto.toml")

    unless File.exists?(manifest_path) do
      IO.puts(:stderr, "error: no vaisto.toml in current directory")
      IO.puts(:stderr, "  hint: run `vaistoc init` to create a project first")
      System.halt(1)
    end

    # Resolve the dependency name from its manifest or directory name
    abs_path = Path.expand(path)
    dep_name = resolve_dep_name(abs_path, path)

    # Read existing manifest, add dependency
    content = File.read!(manifest_path)

    escaped_path = String.replace(path, "\"", "\\\"")

    if String.contains?(content, "[dependencies]") do
      # Append after existing [dependencies] header
      updated =
        String.replace(content, "[dependencies]\n", "[dependencies]\n#{dep_name} = { path = \"#{escaped_path}\" }\n", global: false)

      File.write!(manifest_path, updated)
    else
      # Add [dependencies] section at end
      updated = String.trim_trailing(content) <> "\n\n[dependencies]\n#{dep_name} = { path = \"#{escaped_path}\" }\n"
      File.write!(manifest_path, updated)
    end

    prefix = Namespace.to_prefix(dep_name)
    IO.puts("  Added #{dep_name} (path: #{path}) -> #{prefix}.*")
  end

  defp resolve_dep_name(abs_path, _rel_path) do
    # Try to read the dep's manifest for its canonical name
    case Manifest.load(abs_path) do
      {:ok, manifest} -> manifest.name
      _ -> abs_path |> Path.basename() |> String.downcase() |> String.replace("_", "-")
    end
  end

  defp print_build_results(results, elapsed \\ nil) do
    Enum.each(results, fn result ->
      module_str = result.module |> to_string() |> String.replace_prefix("Elixir.", "")
      IO.puts("  Compiling #{module_str}")
    end)

    count = length(results)
    time_str = if elapsed, do: " in #{elapsed}ms", else: ""
    IO.puts("  Built #{count} module(s)#{time_str}")
  end

  defp write_beam(input, output, bytecode) do
    with :ok <- output |> Path.dirname() |> File.mkdir_p(),
         :ok <- File.write(output, bytecode) do
      IO.puts("✓ Compiled #{input} → #{output}")
    else
      {:error, reason} ->
        IO.puts(:stderr, "error: cannot write #{output}: #{:file.format_error(reason)}")
        System.halt(1)
    end
  end

  defp start_lsp do
    # stdout belongs exclusively to LSP frames, even with debug logging on.
    for %{id: id, module: :logger_std_h, config: %{type: :standard_io}} = handler <- :logger.get_handler_config() do
      config = Map.drop(handler, [:id, :module])
      :ok = :logger.remove_handler(id)
      case :logger.add_handler(id, :logger_std_h, put_in(config, [:config, :type], :standard_error)) do
        :ok -> :ok
        {:error, reason} ->
          :ok = :logger.add_handler(id, :logger_std_h, config)
          raise "cannot route LSP logs to stderr: #{inspect(reason)}"
      end
    end
    # Start LSP server on stdio
    {:ok, _pid} = Vaisto.LSP.Server.start()
    # Keep the process alive
    Process.sleep(:infinity)
  end

  defp format_error(_file, reason) when is_binary(reason) do
    # Already formatted (from check_with_source)
    reason
  end

  defp format_error(file, %Vaisto.Error{} = error) do
    "#{file}: #{Vaisto.Error.to_string(error)}"
  end

  defp format_error(file, reason) do
    "#{file}: #{inspect(reason)}"
  end

  defp print_help do
    IO.puts("""
    vaistoc - The Vaisto Compiler

    Usage:
      vaistoc <file.va>                     Compile a single file
      vaistoc <file.va> -o <output>         Compile to specific output
      vaistoc build                         Build project (reads vaisto.toml)
      vaistoc build [dir]                   Build all .va files in directory
      vaistoc build [dir] -o <output_dir>   Build with custom output directory
      vaistoc build [dir] --src <root:pfx>  Add source root for module naming
      vaistoc init [name]                   Create a new package
      vaistoc add <path>                    Add a local dependency
      vaistoc --eval "<code>"               Evaluate expression
      vaistoc repl                          Start interactive REPL
      vaistoc lsp                           Start Language Server Protocol server
      vaistoc --help                        Show this help

    Packages:
      vaistoc init my-lib                   Creates my-lib/ with vaisto.toml + src/
      vaistoc add ../my-dep                 Adds path dependency to vaisto.toml
      vaistoc build                         Builds from vaisto.toml (if present)

      Package names are kebab-case. The name determines the module namespace:
        my-lib → MyLib.*    json-parser → JsonParser.*

    Build Modes:
      With vaisto.toml:  vaistoc build       Reads manifest, resolves deps
      Without:           vaistoc build src/   Builds directory directly (no packages)

    Compilation:
      Source → bidirectional elaboration → verified Liquid Core → native BEAM

    Module Naming:
      Module names are inferred from file paths:
        src/Vaisto/Lexer.va → Elixir.Vaisto.Lexer
        std/List.va         → Elixir.Std.List

    Module System:
      (ns Vaisto.Lexer)                     Optional: validates against path
      (import Vaisto.Lexer.Types)           Import another module
      (import Std.List :as L)               Import with alias
      (Std.List/fold + 0 [1 2 3])           Call imported function

    Examples:
      vaistoc counter.va
      vaistoc init my-app && cd my-app && vaistoc build
      vaistoc add ../json-parser
      vaistoc build src/ -o build/
      vaistoc --eval "(+ 1 2)"
    """)
  end
end
