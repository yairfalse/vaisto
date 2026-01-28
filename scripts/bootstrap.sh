#!/bin/bash
# Bootstrap the self-hosted Vaisto compiler
#
# This script compiles all self-hosted components using the Elixir-based compiler,
# producing BEAM files that can then compile Vaisto code without Elixir.
#
# Uses `vaistoc build` which properly infers module names from file paths:
# - src/Vaisto/Lexer.va → Vaisto.Lexer
# - std/State.va → Std.State

set -e

cd "$(dirname "$0")/.."

OUTPUT_DIR="build/bootstrap"
mkdir -p "$OUTPUT_DIR"

echo "Bootstrapping self-hosted Vaisto compiler..."
echo "Output: $OUTPUT_DIR"
echo ""

# Phase 0: Generate Prelude module with embedded content
echo "Phase 0: Generating embedded prelude"
./scripts/generate-prelude.sh
echo ""

# Use vaistoc build which handles module naming correctly
# Build standard library first
echo "Phase 1: Standard library (std/)"
./vaistoc build std -o "$OUTPUT_DIR"
echo ""

# Build compiler modules
echo "Phase 2: Compiler modules (src/)"
./vaistoc build src -o "$OUTPUT_DIR"
echo ""

echo "Bootstrap complete!"
echo ""
echo "Compiled modules:"
ls -la "$OUTPUT_DIR"/*.beam 2>/dev/null | awk '{print "  " $NF}'

echo ""
echo "To test the self-hosted CLI:"
echo "  cd $OUTPUT_DIR && erl -pa . -noshell -eval \"'Vaisto.CLI':main()\" -s init stop"
