#!/usr/bin/env bash
# Black-box test runner for vaistoc CLI
set -uo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_DIR="$(cd "$SCRIPT_DIR/../.." && pwd)"
VAISTOC="$PROJECT_DIR/vaistoc"

# Colors
RED=$'\033[0;31m'
GREEN=$'\033[0;32m'
YELLOW=$'\033[0;33m'
CYAN=$'\033[0;36m'
NC=$'\033[0m'
BOLD=$'\033[1m'

PASS=0
FAIL=0
BUGS=0
declare -a RESULTS=()

log_pass() { ((PASS++)); RESULTS+=("${GREEN}PASS${NC} $1: $2"); }
log_fail() { ((FAIL++)); RESULTS+=("${RED}FAIL${NC} $1: $2 ${YELLOW}→ $3${NC}"); }
log_bug()  { ((BUGS++)); RESULTS+=("${RED}BUG!${NC} $1: $2 ${YELLOW}→ $3${NC}"); }

# Temp files for capturing output
_OUT=$(mktemp)
_TIME=$(mktemp)

run_cmd() {
  # Run vaistoc, capture combined output, exit code, and duration
  local start_ms end_ms
  start_ms=$(python3 -c 'import time; print(int(time.time()*1000))')
  "$VAISTOC" "$@" > "$_OUT" 2>&1
  BB_EXIT=$?
  end_ms=$(python3 -c 'import time; print(int(time.time()*1000))')
  BB_DURATION=$((end_ms - start_ms))
  BB_OUTPUT=$(cat "$_OUT")
}

has_stacktrace() {
  echo "$1" | grep -qE '(\*\* \(|\.ex:[0-9]+:|\.erl:[0-9]+:|stacktrace|CRASH)' 2>/dev/null
}

check_no_stacktrace() {
  local id="$1" desc="$2"
  if has_stacktrace "$BB_OUTPUT"; then
    log_bug "$id" "$desc — leaks stacktrace" "Internal error exposed to user"
    return 1
  fi
  return 0
}

cd "$PROJECT_DIR"

echo ""
echo "${BOLD}╔══════════════════════════════════════════════════════╗${NC}"
echo "${BOLD}║       vaistoc CLI — Black-Box Test Suite             ║${NC}"
echo "${BOLD}╚══════════════════════════════════════════════════════╝${NC}"
echo ""

# ── POSITIVE TESTS ──────────────────────────────────────────────────
echo "${BOLD}── Positive Tests ──${NC}"

run_cmd --eval "(+ 1 2)"
if [[ $BB_EXIT -eq 0 ]] && echo "$BB_OUTPUT" | grep -q "3"; then
  log_pass "POS-001" "Eval (+ 1 2) = 3"
else
  log_fail "POS-001" "Eval (+ 1 2)" "exit=$BB_EXIT"
fi

run_cmd --eval "(* 6 7)"
if [[ $BB_EXIT -eq 0 ]] && echo "$BB_OUTPUT" | grep -q "42"; then
  log_pass "POS-002" "Eval (* 6 7) = 42"
else
  log_fail "POS-002" "Eval (* 6 7)" "exit=$BB_EXIT"
fi

run_cmd --eval "(* (+ 1 2) (- 10 3))"
if [[ $BB_EXIT -eq 0 ]] && echo "$BB_OUTPUT" | grep -q "21"; then
  log_pass "POS-003" "Nested arithmetic = 21"
else
  log_fail "POS-003" "Nested arithmetic" "exit=$BB_EXIT"
fi

run_cmd --eval "(if true 1 0)"
if [[ $BB_EXIT -eq 0 ]] && echo "$BB_OUTPUT" | grep -q "1"; then
  log_pass "POS-004" "If true branch = 1"
else
  log_fail "POS-004" "If true branch" "exit=$BB_EXIT"
fi

run_cmd --eval "(if false 1 0)"
if [[ $BB_EXIT -eq 0 ]] && echo "$BB_OUTPUT" | grep -q "0"; then
  log_pass "POS-005" "If false branch = 0"
else
  log_fail "POS-005" "If false branch" "exit=$BB_EXIT"
fi

run_cmd --eval "(let [x 10] (+ x 5))"
if [[ $BB_EXIT -eq 0 ]] && echo "$BB_OUTPUT" | grep -q "15"; then
  log_pass "POS-006" "Let binding = 15"
else
  log_fail "POS-006" "Let binding" "exit=$BB_EXIT"
fi

run_cmd --eval '(str "hello" " " "world")'
if [[ $BB_EXIT -eq 0 ]] && echo "$BB_OUTPUT" | grep -q "hello world"; then
  log_pass "POS-007" "String concat"
else
  log_fail "POS-007" "String concat" "exit=$BB_EXIT"
fi

run_cmd --eval "(defn f [x :int] :int (+ x 1)) (f 5)"
if [[ $BB_EXIT -eq 0 ]] && echo "$BB_OUTPUT" | grep -q "6"; then
  log_pass "POS-008" "Defn + call = 6"
else
  log_fail "POS-008" "Defn + call" "exit=$BB_EXIT"
fi

run_cmd --eval "(do (+ 1 2) (* 3 4))"
if [[ $BB_EXIT -eq 0 ]] && echo "$BB_OUTPUT" | grep -q "12"; then
  log_pass "POS-009" "Do block = 12"
else
  log_fail "POS-009" "Do block" "exit=$BB_EXIT"
fi

run_cmd --eval "true"
if [[ $BB_EXIT -eq 0 ]] && echo "$BB_OUTPUT" | grep -q "true"; then
  log_pass "POS-010" "Bool true"
else
  log_fail "POS-010" "Bool true" "exit=$BB_EXIT"
fi

run_cmd --eval "false"
if [[ $BB_EXIT -eq 0 ]] && echo "$BB_OUTPUT" | grep -q "false"; then
  log_pass "POS-011" "Bool false"
else
  log_fail "POS-011" "Bool false" "exit=$BB_EXIT"
fi

run_cmd --eval '"hello"'
if [[ $BB_EXIT -eq 0 ]] && echo "$BB_OUTPUT" | grep -q "hello"; then
  log_pass "POS-012" "String literal"
else
  log_fail "POS-012" "String literal" "exit=$BB_EXIT"
fi

run_cmd --eval "42"
if [[ $BB_EXIT -eq 0 ]] && echo "$BB_OUTPUT" | grep -q "42"; then
  log_pass "POS-013" "Integer literal = 42"
else
  log_fail "POS-013" "Integer literal" "exit=$BB_EXIT"
fi

run_cmd --eval "[1 2 3]"
if [[ $BB_EXIT -eq 0 ]]; then
  log_pass "POS-014" "List literal"
else
  log_fail "POS-014" "List literal" "exit=$BB_EXIT"
fi

run_cmd --eval "(deftype Color (Red) (Green) (Blue)) (match (Red) [(Red) 1] [(Green) 2] [(Blue) 3])"
if [[ $BB_EXIT -eq 0 ]] && echo "$BB_OUTPUT" | grep -q "1"; then
  log_pass "POS-015" "Exhaustive match on ADT"
else
  log_fail "POS-015" "Exhaustive match" "exit=$BB_EXIT"
fi

run_cmd --eval "(deftype Maybe (Just v) (Nothing)) (match (Just 42) [(Just v) v] [(Nothing) 0])"
if [[ $BB_EXIT -eq 0 ]] && echo "$BB_OUTPUT" | grep -q "42"; then
  log_pass "POS-016" "ADT with data + match = 42"
else
  log_fail "POS-016" "ADT with data" "exit=$BB_EXIT"
fi

run_cmd --eval "(- 100 58)"
if [[ $BB_EXIT -eq 0 ]] && echo "$BB_OUTPUT" | grep -q "42"; then
  log_pass "POS-017" "Subtraction = 42"
else
  log_fail "POS-017" "Subtraction" "exit=$BB_EXIT"
fi

run_cmd --eval "(/ 10.0 2.0)"
if [[ $BB_EXIT -eq 0 ]] && echo "$BB_OUTPUT" | grep -q "5"; then
  log_pass "POS-018" "Division = 5"
else
  log_fail "POS-018" "Division" "exit=$BB_EXIT"
fi

run_cmd --eval "(> 5 3)"
if [[ $BB_EXIT -eq 0 ]] && echo "$BB_OUTPUT" | grep -q "true"; then
  log_pass "POS-019" "Comparison (> 5 3) = true"
else
  log_fail "POS-019" "Comparison" "exit=$BB_EXIT"
fi

run_cmd --eval "(== 5 5)"
if [[ $BB_EXIT -eq 0 ]] && echo "$BB_OUTPUT" | grep -q "true"; then
  log_pass "POS-020" "Equality (== 5 5) = true"
else
  log_fail "POS-020" "Equality" "exit=$BB_EXIT"
fi

mkdir -p /tmp/bb_pos021
run_cmd examples/math.va -o /tmp/bb_pos021/Math.beam
if [[ $BB_EXIT -eq 0 ]] && echo "$BB_OUTPUT" | grep -q "Compiled"; then
  log_pass "POS-021" "Compile math.va"
else
  log_fail "POS-021" "Compile math.va" "exit=$BB_EXIT out=$BB_OUTPUT"
fi
rm -rf /tmp/bb_pos021

mkdir -p /tmp/bb_pos022
run_cmd examples/math.va -o /tmp/bb_pos022/Math.beam
if [[ $BB_EXIT -eq 0 ]] && [[ -f /tmp/bb_pos022/Math.beam ]]; then
  log_pass "POS-022" "Compile with -o creates file"
else
  log_fail "POS-022" "Compile with -o" "exit=$BB_EXIT"
fi
rm -rf /tmp/bb_pos022

mkdir -p /tmp/bb_pos023
run_cmd examples/maybe.va -o /tmp/bb_pos023/Maybe.beam
if [[ $BB_EXIT -eq 0 ]] && echo "$BB_OUTPUT" | grep -q "Compiled"; then
  log_pass "POS-023" "Compile maybe.va (ADT)"
else
  log_fail "POS-023" "Compile maybe.va" "exit=$BB_EXIT"
fi
rm -rf /tmp/bb_pos023

run_cmd --help
if [[ $BB_EXIT -eq 0 ]] && echo "$BB_OUTPUT" | grep -q "Usage:"; then
  log_pass "POS-024" "--help shows usage"
else
  log_fail "POS-024" "--help" "exit=$BB_EXIT"
fi

BB_OUTPUT=$("$VAISTOC" 2>&1) || true; BB_EXIT=$?
if [[ $BB_EXIT -eq 0 ]] && echo "$BB_OUTPUT" | grep -q "Usage:"; then
  log_pass "POS-025" "No args shows help"
else
  log_fail "POS-025" "No args" "exit=$BB_EXIT"
fi

rm -rf bb-test-pkg-026
run_cmd init bb-test-pkg-026
if [[ $BB_EXIT -eq 0 ]] && echo "$BB_OUTPUT" | grep -q "Created"; then
  log_pass "POS-026" "Init creates package"
else
  log_fail "POS-026" "Init package" "exit=$BB_EXIT"
fi
rm -rf bb-test-pkg-026

run_cmd --eval "(let [f (fn [x] (+ x 1))] (f 10))"
if [[ $BB_EXIT -eq 0 ]] && echo "$BB_OUTPUT" | grep -q "11"; then
  log_pass "POS-027" "Lambda + apply = 11"
else
  log_fail "POS-027" "Lambda + apply" "exit=$BB_EXIT out=$BB_OUTPUT"
fi

echo ""

# ── NEGATIVE TESTS ──────────────────────────────────────────────────
echo "${BOLD}── Negative Tests ──${NC}"

run_cmd --eval "((("
if [[ $BB_EXIT -ne 0 ]] && ! has_stacktrace "$BB_OUTPUT"; then
  log_pass "NEG-001" "Syntax error: clean error message"
elif has_stacktrace "$BB_OUTPUT"; then
  log_bug "NEG-001" "Syntax error leaks stacktrace" "Internal error exposed"
else
  log_fail "NEG-001" "Syntax error" "exit=$BB_EXIT"
fi

run_cmd --eval '(+ 1 "hello")'
if [[ $BB_EXIT -ne 0 ]] && ! has_stacktrace "$BB_OUTPUT"; then
  log_pass "NEG-002" "Type error: clean error message"
elif has_stacktrace "$BB_OUTPUT"; then
  log_bug "NEG-002" "Type error leaks stacktrace" "Internal error exposed"
else
  log_fail "NEG-002" "Type error" "exit=$BB_EXIT"
fi

run_cmd --eval "(unknown-func 1 2)"
if [[ $BB_EXIT -ne 0 ]] && ! has_stacktrace "$BB_OUTPUT"; then
  log_pass "NEG-003" "Undefined function: clean error"
elif has_stacktrace "$BB_OUTPUT"; then
  log_bug "NEG-003" "Undefined function leaks stacktrace" "Internal error exposed"
else
  log_fail "NEG-003" "Undefined function" "exit=$BB_EXIT"
fi

run_cmd nonexistent.va
if [[ $BB_EXIT -ne 0 ]] && ! has_stacktrace "$BB_OUTPUT"; then
  log_pass "NEG-004" "Nonexistent file: clean error"
elif has_stacktrace "$BB_OUTPUT"; then
  log_bug "NEG-004" "Nonexistent file leaks stacktrace" "Internal error exposed"
else
  log_fail "NEG-004" "Nonexistent file" "exit=$BB_EXIT"
fi

run_cmd build /nonexistent/dir
if [[ $BB_EXIT -ne 0 ]] && ! has_stacktrace "$BB_OUTPUT"; then
  log_pass "NEG-005" "Nonexistent build dir: clean error"
elif has_stacktrace "$BB_OUTPUT"; then
  log_bug "NEG-005" "Build nonexistent dir leaks stacktrace" "Internal error exposed"
else
  log_fail "NEG-005" "Build nonexistent dir" "exit=$BB_EXIT"
fi

run_cmd --bogus-flag
if [[ $BB_EXIT -ne 0 ]] && ! has_stacktrace "$BB_OUTPUT"; then
  log_pass "NEG-006" "Unknown flag: clean error"
elif has_stacktrace "$BB_OUTPUT"; then
  log_bug "NEG-006" "Unknown flag leaks stacktrace" "Internal error exposed"
else
  log_fail "NEG-006" "Unknown flag" "exit=$BB_EXIT"
fi

run_cmd --eval "(deftype Color (Red) (Green) (Blue)) (match (Red) [(Red) 1] [(Green) 2])"
if [[ $BB_EXIT -ne 0 ]] && echo "$BB_OUTPUT" | grep -qi "not exhaustive"; then
  log_pass "NEG-007" "Non-exhaustive match detected"
else
  log_fail "NEG-007" "Non-exhaustive match" "exit=$BB_EXIT out=$BB_OUTPUT"
fi

run_cmd init "My Package"
if [[ $BB_EXIT -ne 0 ]] && ! has_stacktrace "$BB_OUTPUT"; then
  log_pass "NEG-008" "Init with spaces: rejected"
elif has_stacktrace "$BB_OUTPUT"; then
  log_bug "NEG-008" "Init with spaces leaks stacktrace" "Internal error exposed"
else
  log_fail "NEG-008" "Init with spaces" "exit=$BB_EXIT"
fi

run_cmd init UPPER
if [[ $BB_EXIT -ne 0 ]] && ! has_stacktrace "$BB_OUTPUT"; then
  log_pass "NEG-009" "Init with UPPER: rejected"
elif has_stacktrace "$BB_OUTPUT"; then
  log_bug "NEG-009" "Init UPPER leaks stacktrace" "Internal error exposed"
else
  log_fail "NEG-009" "Init UPPER" "exit=$BB_EXIT"
fi

run_cmd --eval ""
if [[ $BB_EXIT -ne 0 ]] && ! has_stacktrace "$BB_OUTPUT"; then
  log_pass "NEG-010" "Eval empty string: clean error"
elif has_stacktrace "$BB_OUTPUT"; then
  log_bug "NEG-010" "Eval empty string leaks stacktrace" "UndefinedFunctionError exposed to user"
else
  log_fail "NEG-010" "Eval empty string" "exit=$BB_EXIT"
fi

run_cmd --eval ")"
if [[ $BB_EXIT -ne 0 ]] && ! has_stacktrace "$BB_OUTPUT"; then
  log_pass "NEG-011" "Eval ')': clean error"
elif has_stacktrace "$BB_OUTPUT"; then
  log_bug "NEG-011" "Eval ')' leaks stacktrace" "Internal error exposed"
else
  log_fail "NEG-011" "Eval ')'" "exit=$BB_EXIT"
fi

run_cmd --eval '"hello'
if [[ $BB_EXIT -ne 0 ]] && ! has_stacktrace "$BB_OUTPUT"; then
  log_pass "NEG-012" "Unclosed string: clean error"
elif has_stacktrace "$BB_OUTPUT"; then
  log_bug "NEG-012" "Unclosed string leaks stacktrace" "Internal error exposed"
else
  log_fail "NEG-012" "Unclosed string" "exit=$BB_EXIT"
fi

echo "hello" > /tmp/bb_vaisto_notva.txt
run_cmd /tmp/bb_vaisto_notva.txt
if [[ $BB_EXIT -ne 0 ]] && ! has_stacktrace "$BB_OUTPUT"; then
  log_pass "NEG-013" "Non-.va file: rejected"
else
  if [[ $BB_EXIT -eq 0 ]]; then
    log_fail "NEG-013" "Non-.va file compiled without error" "Should reject .txt files"
  elif has_stacktrace "$BB_OUTPUT"; then
    log_bug "NEG-013" "Non-.va file leaks stacktrace" "Internal error exposed"
  fi
fi
rm -f /tmp/bb_vaisto_notva.txt

echo ""

# ── SYSTEM TESTS ────────────────────────────────────────────────────
echo "${BOLD}── System Tests ──${NC}"

# SYS-001: Init then build
rm -rf /tmp/bb-sys-001
(cd /tmp && "$VAISTOC" init bb-sys-001 >/dev/null 2>&1)
if [[ -d /tmp/bb-sys-001 ]]; then
  BB_OUTPUT=$(cd /tmp/bb-sys-001 && "$VAISTOC" build 2>&1); BB_EXIT=$?
  if [[ $BB_EXIT -eq 0 ]] && echo "$BB_OUTPUT" | grep -q "Built"; then
    log_pass "SYS-001" "Init then build succeeds"
  else
    log_fail "SYS-001" "Init then build" "build exit=$BB_EXIT out=$BB_OUTPUT"
  fi
else
  log_fail "SYS-001" "Init then build" "init failed"
fi
rm -rf /tmp/bb-sys-001

# SYS-002: Output is valid BEAM file
mkdir -p /tmp/bb_sys002
run_cmd examples/math.va -o /tmp/bb_sys002/Elixir.Math.beam
if [[ $BB_EXIT -eq 0 ]] && [[ -f /tmp/bb_sys002/Elixir.Math.beam ]]; then
  magic=$(head -c4 /tmp/bb_sys002/Elixir.Math.beam)
  if [[ "$magic" == "FOR1" ]]; then
    log_pass "SYS-002" "Output is valid BEAM file (FOR1 magic)"
  else
    log_fail "SYS-002" ".beam validity" "File missing FOR1 magic bytes"
  fi
else
  log_fail "SYS-002" ".beam validity" "exit=$BB_EXIT or file missing"
fi
rm -rf /tmp/bb_sys002

# SYS-003: Init creates correct structure
rm -rf /tmp/bb-sys-003
(cd /tmp && "$VAISTOC" init bb-sys-003 >/dev/null 2>&1)
if [[ -f /tmp/bb-sys-003/vaisto.toml ]] && [[ -d /tmp/bb-sys-003/src ]]; then
  log_pass "SYS-003" "Init creates vaisto.toml + src/"
else
  log_fail "SYS-003" "Init structure" "toml=$(test -f /tmp/bb-sys-003/vaisto.toml && echo yes || echo no) src=$(test -d /tmp/bb-sys-003/src && echo yes || echo no)"
fi
rm -rf /tmp/bb-sys-003

# SYS-004: Compile idempotent
mkdir -p /tmp/bb_sys004
run_cmd examples/math.va -o /tmp/bb_sys004/Math.beam
e1=$BB_EXIT
run_cmd examples/math.va -o /tmp/bb_sys004/Math.beam
e2=$BB_EXIT
if [[ $e1 -eq 0 ]] && [[ $e2 -eq 0 ]]; then
  log_pass "SYS-004" "Compile twice is idempotent"
else
  log_fail "SYS-004" "Compile idempotent" "first=$e1 second=$e2"
fi
rm -rf /tmp/bb_sys004

echo ""

# ── INTEGRATION TESTS ──────────────────────────────────────────────
echo "${BOLD}── Integration Tests ──${NC}"

run_cmd --eval "(+ 2147483647 1)"
if [[ $BB_EXIT -eq 0 ]] && echo "$BB_OUTPUT" | grep -q "2147483648"; then
  log_pass "INT-001" "BEAM big integer (2^31 + 1)"
else
  log_fail "INT-001" "Big integer" "exit=$BB_EXIT out=$BB_OUTPUT"
fi

run_cmd --eval '(str "abc" "def" "ghi")'
if [[ $BB_EXIT -eq 0 ]] && echo "$BB_OUTPUT" | grep -q "abcdefghi"; then
  log_pass "INT-002" "BEAM string binary concat"
else
  log_fail "INT-002" "String binary" "exit=$BB_EXIT"
fi

mkdir -p /tmp/bb_int003
run_cmd examples/maybe.va -o /tmp/bb_int003/Elixir.Maybe.beam
if [[ $BB_EXIT -eq 0 ]]; then
  load_result=$(elixir -e 'Code.prepend_path("/tmp/bb_int003"); IO.inspect(:code.load_file(:"Elixir.Maybe"))' 2>&1) || true
  if echo "$load_result" | grep -qE "(module|loaded)"; then
    log_pass "INT-003" "Compiled .beam loadable by Elixir"
  else
    log_fail "INT-003" ".beam loadable" "load=$load_result"
  fi
else
  log_fail "INT-003" ".beam loadable" "compile exit=$BB_EXIT"
fi
rm -rf /tmp/bb_int003

echo ""

# ── PERFORMANCE TESTS ──────────────────────────────────────────────
echo "${BOLD}── Performance Tests ──${NC}"

run_cmd --eval "(+ 1 2)"
if [[ $BB_EXIT -eq 0 ]] && [[ $BB_DURATION -lt 5000 ]]; then
  log_pass "PERF-001" "Eval simple: ${BB_DURATION}ms (< 5000ms)"
else
  log_fail "PERF-001" "Eval perf" "${BB_DURATION}ms"
fi

mkdir -p /tmp/bb_perf002
run_cmd examples/math.va -o /tmp/bb_perf002/Math.beam
if [[ $BB_EXIT -eq 0 ]] && [[ $BB_DURATION -lt 10000 ]]; then
  log_pass "PERF-002" "Compile file: ${BB_DURATION}ms (< 10000ms)"
else
  log_fail "PERF-002" "Compile perf" "${BB_DURATION}ms"
fi
rm -rf /tmp/bb_perf002

run_cmd --help
if [[ $BB_EXIT -eq 0 ]] && [[ $BB_DURATION -lt 2000 ]]; then
  log_pass "PERF-003" "Help: ${BB_DURATION}ms (< 2000ms)"
else
  log_fail "PERF-003" "Help perf" "${BB_DURATION}ms"
fi

echo ""

# ── LOAD TESTS ──────────────────────────────────────────────────────
echo "${BOLD}── Load Tests ──${NC}"

mkdir -p /tmp/bb_load001 /tmp/bb_load001_out
for i in $(seq 1 10); do
  echo "(defn f_${i} [x :int] :int (+ x ${i}))" > "/tmp/bb_load001/File${i}.va"
done
run_cmd build /tmp/bb_load001 -o /tmp/bb_load001_out
if [[ $BB_EXIT -eq 0 ]] && [[ $BB_DURATION -lt 30000 ]]; then
  log_pass "LOAD-001" "Build 10 files: ${BB_DURATION}ms"
else
  log_fail "LOAD-001" "Build 10 files" "exit=$BB_EXIT dur=${BB_DURATION}ms"
fi
rm -rf /tmp/bb_load001 /tmp/bb_load001_out

run_cmd --eval "(do (+ 1 1) (+ 2 2) (+ 3 3) (+ 4 4) (+ 5 5) (+ 6 6) (+ 7 7) (+ 8 8) (+ 9 9) (+ 10 10))"
if [[ $BB_EXIT -eq 0 ]] && [[ $BB_DURATION -lt 10000 ]]; then
  log_pass "LOAD-002" "Eval 10-expr do: ${BB_DURATION}ms"
else
  log_fail "LOAD-002" "Eval do block" "exit=$BB_EXIT dur=${BB_DURATION}ms"
fi

echo ""

# ── ABNORMAL TESTS ──────────────────────────────────────────────────
echo "${BOLD}── Abnormal Tests ──${NC}"

run_cmd --eval "(div 1 0)"
if [[ $BB_EXIT -ne 0 ]] && ! has_stacktrace "$BB_OUTPUT"; then
  log_pass "ABN-001" "Division by zero: clean error"
elif has_stacktrace "$BB_OUTPUT"; then
  log_bug "ABN-001" "Division by zero leaks stacktrace" "ArithmeticError exposed to user"
else
  log_fail "ABN-001" "Division by zero" "exit=$BB_EXIT"
fi

long_id=$(python3 -c "print('a' * 10000)")
run_cmd --eval "($long_id 1 2)"
if ! has_stacktrace "$BB_OUTPUT"; then
  log_pass "ABN-002" "10,000-char identifier: handled"
else
  log_bug "ABN-002" "Long identifier leaks stacktrace" "Internal error exposed"
fi

deep_expr=$(python3 -c "print('(+ ' * 200 + '1' + ' 1)' * 200)")
run_cmd --eval "$deep_expr"
if ! has_stacktrace "$BB_OUTPUT"; then
  log_pass "ABN-003" "200-level deep nesting: handled"
else
  log_bug "ABN-003" "Deep nesting leaks stacktrace" "Internal error exposed"
fi

run_cmd --eval '(let [über 42] über)'
if ! has_stacktrace "$BB_OUTPUT"; then
  log_pass "ABN-004" "Unicode identifiers: handled"
else
  log_bug "ABN-004" "Unicode identifiers leak stacktrace" "Internal error exposed"
fi

echo "(+ 1 2)" > /tmp/bb_noperm.va
chmod 000 /tmp/bb_noperm.va
run_cmd /tmp/bb_noperm.va
chmod 644 /tmp/bb_noperm.va
if [[ $BB_EXIT -ne 0 ]] && ! has_stacktrace "$BB_OUTPUT"; then
  log_pass "ABN-005" "No-read-perm file: clean error"
elif has_stacktrace "$BB_OUTPUT"; then
  log_bug "ABN-005" "No-read-perm leaks stacktrace" "Internal error exposed"
else
  log_fail "ABN-005" "No-read-perm file" "exit=$BB_EXIT"
fi
rm -f /tmp/bb_noperm.va

mkdir -p /tmp/bb_readonly
chmod 555 /tmp/bb_readonly
run_cmd examples/math.va -o /tmp/bb_readonly/Math.beam
chmod 755 /tmp/bb_readonly
if [[ $BB_EXIT -ne 0 ]] && ! has_stacktrace "$BB_OUTPUT"; then
  log_pass "ABN-006" "Read-only output dir: clean error"
elif has_stacktrace "$BB_OUTPUT"; then
  log_bug "ABN-006" "Read-only output dir leaks stacktrace" "Internal error exposed"
else
  log_fail "ABN-006" "Read-only output dir" "exit=$BB_EXIT out=$BB_OUTPUT"
fi
rm -rf /tmp/bb_readonly

python3 -c "import os; open('/tmp/bb_binary.va','wb').write(os.urandom(1024))"
run_cmd /tmp/bb_binary.va
if ! has_stacktrace "$BB_OUTPUT"; then
  log_pass "ABN-007" "Binary garbage .va: handled"
else
  log_bug "ABN-007" "Binary garbage leaks stacktrace" "Internal error exposed"
fi
rm -f /tmp/bb_binary.va /tmp/Elixir.Bb_binary.beam

run_cmd --eval '(str "hello " "🌍")'
if [[ $BB_EXIT -eq 0 ]] && ! has_stacktrace "$BB_OUTPUT"; then
  log_pass "ABN-008" "Emoji in strings: works"
elif has_stacktrace "$BB_OUTPUT"; then
  log_bug "ABN-008" "Emoji leaks stacktrace" "Internal error exposed"
else
  log_fail "ABN-008" "Emoji in strings" "exit=$BB_EXIT"
fi

rm -rf /tmp/bb-abn-009
(cd /tmp && "$VAISTOC" init bb-abn-009 >/dev/null 2>&1)
BB_OUTPUT=$(cd /tmp && "$VAISTOC" init bb-abn-009 2>&1); BB_EXIT=$?
if [[ $BB_EXIT -ne 0 ]] && ! has_stacktrace "$BB_OUTPUT"; then
  log_pass "ABN-009" "Init duplicate package: rejected"
elif [[ $BB_EXIT -eq 0 ]]; then
  log_fail "ABN-009" "Init duplicate" "Silently overwrites existing package"
elif has_stacktrace "$BB_OUTPUT"; then
  log_bug "ABN-009" "Init duplicate leaks stacktrace" "Internal error exposed"
fi
rm -rf /tmp/bb-abn-009

run_cmd --eval "   "
if ! has_stacktrace "$BB_OUTPUT"; then
  log_pass "ABN-010" "Eval whitespace-only: handled"
else
  log_bug "ABN-010" "Eval whitespace leaks stacktrace" "Internal error exposed"
fi

echo ""

# ── RESULTS SUMMARY ────────────────────────────────────────────────
echo "${BOLD}╔══════════════════════════════════════════════════════╗${NC}"
echo "${BOLD}║                    RESULTS                          ║${NC}"
echo "${BOLD}╚══════════════════════════════════════════════════════╝${NC}"
echo ""

for r in "${RESULTS[@]}"; do
  echo "  $r"
done

echo ""
echo "${BOLD}────────────────────────────────────────────────────────${NC}"
TOTAL=$((PASS + FAIL + BUGS))
echo "  ${GREEN}PASS:${NC} $PASS    ${RED}FAIL:${NC} $FAIL    ${RED}BUGS:${NC} $BUGS    TOTAL: $TOTAL"
echo "${BOLD}────────────────────────────────────────────────────────${NC}"

if [[ $BUGS -gt 0 ]]; then
  echo ""
  echo "${RED}${BOLD}Confirmed Bugs:${NC}"
  for r in "${RESULTS[@]}"; do
    if echo "$r" | grep -q "BUG!"; then
      echo "  $r"
    fi
  done
fi

if [[ $FAIL -gt 0 ]]; then
  echo ""
  echo "${YELLOW}${BOLD}Failures:${NC}"
  for r in "${RESULTS[@]}"; do
    if echo "$r" | grep -q "FAIL"; then
      echo "  $r"
    fi
  done
fi

echo ""
rm -f "$_OUT" "$_TIME"

[[ $((FAIL + BUGS)) -gt 0 ]] && exit 1 || exit 0
