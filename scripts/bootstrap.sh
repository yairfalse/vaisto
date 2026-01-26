#!/bin/bash
# Bootstrap the self-hosted Vaisto compiler
#
# This script compiles all self-hosted components using the Elixir-based compiler,
# producing BEAM files that can then compile Vaisto code without Elixir.
#
# Module names are now inferred from file paths:
# - src/Vaisto/Lexer.va → Elixir.Vaisto.Lexer
# - std/State.va → Elixir.Std.State
#
# Compile order is determined by dependencies:
# 1. Leaf modules (no Vaisto imports)
# 2. Modules that depend on (1)
# 3. And so on...

set -e

cd "$(dirname "$0")/.."

OUTPUT_DIR="build/bootstrap"
mkdir -p "$OUTPUT_DIR"

echo "Bootstrapping self-hosted Vaisto compiler..."
echo "Output: $OUTPUT_DIR"
echo ""

# Compile function - module name inferred from path
# For src/Vaisto/Foo/Bar.va → Vaisto.Foo.Bar → Elixir.Vaisto.Foo.Bar.beam
# For std/Foo.va → Std.Foo → Elixir.Std.Foo.beam
compile() {
    local src="$1"
    local module_name

    # Infer module name from path
    if [[ "$src" == src/* ]]; then
        # src/Vaisto/Foo/Bar.va → Vaisto.Foo.Bar
        module_name=$(echo "$src" | sed 's|^src/||' | sed 's|\.va$||' | sed 's|/|.|g')
    elif [[ "$src" == std/* ]]; then
        # std/Foo.va → Std.Foo
        module_name=$(echo "$src" | sed 's|^std/|Std.|' | sed 's|\.va$||' | sed 's|/|.|g')
    else
        # Fallback: capitalize basename
        local name=$(basename "$src" .va)
        local first_char="$(echo ${name:0:1} | tr '[:lower:]' '[:upper:]')"
        local rest="${name:1}"
        module_name="${first_char}${rest}"
    fi

    local output_name="Elixir.${module_name}.beam"
    echo -n "  Compiling $src -> $output_name... "
    ./vaistoc "$src" -o "$OUTPUT_DIR/$output_name" 2>&1 && echo "ok" || echo "FAILED"
}

# Phase 0: Standard library modules (no deps)
echo "Phase 0: Standard library"
compile std/State.va
compile std/Regex.va
compile std/String.va

# Phase 1: Leaf modules (no Vaisto dependencies)
echo ""
echo "Phase 1: Leaf modules"
compile src/Vaisto/Lexer/Types.va
compile src/Vaisto/Parser/AST.va
compile src/Vaisto/Compiler/CoreEmitter.va

# Phase 2: TypeChecker support modules
echo ""
echo "Phase 2: TypeChecker support"
compile src/Vaisto/TypeChecker/Types.va
compile src/Vaisto/TypeChecker/Core.va

# Phase 3: TypeChecker modules with internal deps
echo ""
echo "Phase 3: TypeChecker internal"
compile src/Vaisto/TypeChecker/Unify.va
compile src/Vaisto/TypeChecker/Context.va
compile src/Vaisto/TypeChecker/Errors.va

# Phase 4: Main modules
echo ""
echo "Phase 4: Main modules"
compile src/Vaisto/Lexer.va
compile src/Vaisto/Parser.va
compile src/Vaisto/TypeChecker.va

# Phase 5: Compiler and CLI
echo ""
echo "Phase 5: Compiler and CLI"
compile src/Vaisto/Compiler.va
compile src/Vaisto/CLI.va

echo ""
echo "Bootstrap complete!"
echo ""
echo "Compiled modules:"
ls -la "$OUTPUT_DIR"/*.beam 2>/dev/null | awk '{print "  " $NF}'

echo ""
echo "To test the self-hosted CLI:"
echo "  cd $OUTPUT_DIR && erl -pa . -noshell -eval \"'Elixir.Vaisto.CLI':main()\" -s init stop"
