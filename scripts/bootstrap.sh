#!/bin/bash
# Bootstrap the self-hosted Vaisto compiler
#
# This script compiles all self-hosted components using the Elixir-based compiler,
# producing BEAM files that can then compile Vaisto code without Elixir.
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

# Compile function
compile() {
    local src="$1"
    local name=$(basename "$src" .va)
    echo -n "  Compiling $src... "
    ./vaistoc "$src" -o "$OUTPUT_DIR/$name.beam" 2>&1 && echo "ok" || echo "FAILED"
}

# Phase 1: Leaf modules (no Vaisto dependencies)
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
echo "  cd $OUTPUT_DIR && erl -pa . -noshell -eval \"'Elixir.Cli':main()\" -s init stop"
