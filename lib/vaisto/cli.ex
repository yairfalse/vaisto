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

  alias Vaisto.Compilation
  alias Vaisto.Package.{Manifest, Namespace}

  # Embed the prelude at compile time
  @external_resource Path.join(__DIR__, "../../std/prelude.va")
  @prelude File.read!(Path.join(__DIR__, "../../std/prelude.va"))

  def main(args) do
    case parse_args(args) do
      {:compile, input, output, backend} ->
        compile_file(input, output, backend)

      {:build, source_dir, output_dir, backend, src_roots} ->
        build_dir(source_dir, output_dir, backend, src_roots)

      :build_manifest ->
        build_manifest(".")

      {:init, name} ->
        init_project(name)

      {:add, path} ->
        add_dependency(path)

      {:eval, code, backend} ->
        eval_code(code, backend)

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
        strict: [output: :string, backend: :string, eval: :string, help: :boolean],
        aliases: [o: :output, e: :eval, h: :help]
      )

    case invalid do
      [{flag, _} | _] -> {:error, "unknown option: #{flag}. Use --help for usage."}
      [] ->
        cond do
          opts[:help] ->
            :help

          eval = opts[:eval] ->
            backend = parse_backend(opts[:backend] || "core")
            {:eval, eval, backend}

          length(positional) == 1 ->
            [input] = positional
            backend = parse_backend(opts[:backend] || "core")
            output = opts[:output] || default_output(input)
            {:compile, input, output, backend}

          true ->
            {:error, "invalid arguments. Use --help for usage."}
        end
    end
  end

  defp parse_build(args) do
    {opts, positional, invalid} =
      OptionParser.parse(args,
        strict: [output: :string, backend: :string, src: :keep],
        aliases: [o: :output]
      )

    case invalid do
      [{flag, _} | _] ->
        {:error, "unknown build option: #{flag}. Use --help for usage."}

      [] ->
        # No positional args + no explicit flags = try manifest mode
        if positional == [] and opts == [] and Manifest.exists?(".") do
          :build_manifest
        else
          dir = List.first(positional) || "."
          output = opts[:output] || dir
          backend = parse_backend(opts[:backend] || "core")

          src_roots =
            opts
            |> Keyword.get_values(:src)
            |> Enum.map(fn root ->
              case String.split(root, ":", parts: 2) do
                [path, prefix] -> {path, prefix}
                [path] -> {path, ""}
              end
            end)

          {:build, dir, output, backend, src_roots}
        end
    end
  end

  defp parse_init([name | _]) when byte_size(name) > 0, do: {:init, name}
  defp parse_init([]), do: {:init, Path.basename(File.cwd!())}

  defp parse_add([path | _]) when byte_size(path) > 0, do: {:add, path}
  defp parse_add([]), do: {:error, "add requires a path argument. Usage: vaistoc add ../my-dep"}

  defp parse_backend("core"), do: :core
  defp parse_backend("elixir"), do: :elixir
  defp parse_backend(other), do: {:error, "unknown backend: #{other}. Use 'core' or 'elixir'."}

  defp default_output(input) do
    dir = Path.dirname(input)
    # Use path-based module naming
    module_name = Vaisto.Build.infer_module_name(input)
    # BEAM files must match the module name exactly
    Path.join(dir, Atom.to_string(module_name) <> ".beam")
  end

  defp compile_file(input, output, backend) do
    case File.read(input) do
      {:ok, source} ->
        # Use path-based module naming
        module_name = Vaisto.Build.infer_module_name(input)

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

  defp build_dir(source_dir, output_dir, backend, src_roots) do
    IO.puts("  Building #{source_dir} ...")

    # Use custom source roots if provided, otherwise use defaults
    opts = [output_dir: output_dir, backend: backend, prelude: @prelude]
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

    if String.contains?(content, "[dependencies]") do
      # Append to existing [dependencies] section
      updated =
        String.replace(content, "[dependencies]", "[dependencies]\n#{dep_name} = { path = \"#{path}\" }", global: false)

      File.write!(manifest_path, updated)
    else
      # Add [dependencies] section at end
      updated = String.trim_trailing(content) <> "\n\n[dependencies]\n#{dep_name} = { path = \"#{path}\" }\n"
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

  defp compile(source, module_name, backend) do
    Compilation.compile(source, module_name,
      prelude: @prelude,
      backend: backend
    )
  end

  defp start_lsp do
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
    # Structured error - format it
    # This shouldn't normally happen since check_with_source formats them
    "#{file}: #{error.message}"
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

    Backends:
      core    - Direct Core Erlang compilation (default, smaller output)
      elixir  - Compile via Elixir AST (more features, larger output)

    Module Naming:
      Module names are inferred from file paths:
        src/Vaisto/Lexer.va → Elixir.Vaisto.Lexer
        std/List.va         → Elixir.Std.List

    Module System:
      (ns Vaisto.Lexer)                     Optional: validates against path
      (import Vaisto.Lexer.Types)           Import another module
      (import Std.List :as L)               Import with alias
      (Std.List/fold xs 0 +)                Call imported function

    Examples:
      vaistoc counter.va
      vaistoc init my-app && cd my-app && vaistoc build
      vaistoc add ../json-parser
      vaistoc build src/ -o build/
      vaistoc --eval "(+ 1 2)"
    """)
  end
end
