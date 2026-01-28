#!/bin/bash
# Generate src/Vaisto/Prelude.va with embedded prelude content
#
# This script reads std/prelude.va and encodes it as a list of byte values
# that can be converted back to a string at runtime using erlang:list_to_binary/1

set -e

cd "$(dirname "$0")/.."

PRELUDE_FILE="std/prelude.va"
OUTPUT_FILE="src/Vaisto/Prelude.va"

if [ ! -f "$PRELUDE_FILE" ]; then
    echo "Error: $PRELUDE_FILE not found"
    exit 1
fi

# Read prelude and convert to space-separated byte values
# Using od to get decimal byte values, then format for Vaisto list syntax
BYTES=$(od -An -tu1 "$PRELUDE_FILE" | tr -s ' \n' ' ' | sed 's/^ *//' | sed 's/ *$//')

# Generate the Vaisto module
cat > "$OUTPUT_FILE" << 'HEADER'
; Vaisto.Prelude - Embedded prelude source
; This file is generated during bootstrap - DO NOT EDIT MANUALLY
;
; The prelude source is encoded as a list of byte values to avoid
; escaping issues with quotes, newlines, and special characters.
;
; To regenerate: run ./scripts/generate-prelude.sh

(ns Vaisto.Prelude)

(extern erlang/list_to_binary [(List :int)] :string)

; Returns the prelude source code as a string
(defn source []
  (erlang/list_to_binary (prelude-bytes)))

; GENERATED: Prelude content as byte list
(defn prelude-bytes []
  (list
HEADER

# Add the bytes, formatted nicely (about 20 numbers per line)
echo "$BYTES" | tr ' ' '\n' | while read -r byte; do
    [ -n "$byte" ] && echo -n " $byte"
done >> "$OUTPUT_FILE"

# Close the list and function
echo "))" >> "$OUTPUT_FILE"

echo "Generated $OUTPUT_FILE with $(echo "$BYTES" | wc -w | tr -d ' ') bytes from $PRELUDE_FILE"
