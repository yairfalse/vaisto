#!/usr/bin/env bash
# Black-box test runner for vaistoc REPL
# Tests interactive session behavior via stdin piping
set -uo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_DIR="$(cd "$SCRIPT_DIR/../.." && pwd)"
VAISTOC="$PROJECT_DIR/vaistoc"

RED=$'\033[0;31m'
GREEN=$'\033[0;32m'
YELLOW=$'\033[0;33m'
NC=$'\033[0m'
BOLD=$'\033[1m'

PASS=0
FAIL=0
BUGS=0
declare -a RESULTS=()

log_pass() { ((PASS++)); RESULTS+=("${GREEN}PASS${NC} $1: $2"); }
log_fail() { ((FAIL++)); RESULTS+=("${RED}FAIL${NC} $1: $2 ${YELLOW}→ $3${NC}"); }
log_bug()  { ((BUGS++)); RESULTS+=("${RED}BUG!${NC} $1: $2 ${YELLOW}→ $3${NC}"); }

_OUT=$(mktemp)

# Run REPL with given input lines, capture combined output
# Usage: repl_run "line1\nline2\n:quit"
repl_run() {
  echo -e "$1" | "$VAISTOC" repl > "$_OUT" 2>&1
  BB_EXIT=$?
  BB_OUTPUT=$(cat "$_OUT")
}

has_stacktrace() {
  echo "$1" | grep -qE '(\*\* \(|\.ex:[0-9]+:|\.erl:[0-9]+:|stacktrace|CRASH)' 2>/dev/null
}

# Extract REPL output lines (skip banner, prompts)
# Returns lines containing "=>" (results) or "error:" (errors)
repl_results() {
  echo "$BB_OUTPUT" | grep -E '^λ =>' | sed 's/^λ => //'
}

cd "$PROJECT_DIR"

echo ""
echo "${BOLD}╔══════════════════════════════════════════════════════╗${NC}"
echo "${BOLD}║       vaistoc REPL — Black-Box Test Suite            ║${NC}"
echo "${BOLD}╚══════════════════════════════════════════════════════╝${NC}"
echo ""

# ── POSITIVE TESTS ──────────────────────────────────────────────────
echo "${BOLD}── Positive Tests ──${NC}"

repl_run "(+ 1 2)\n:quit"
if [[ $BB_EXIT -eq 0 ]] && echo "$BB_OUTPUT" | grep -q "=> 3"; then
  log_pass "POS-001" "Eval (+ 1 2) = 3"
else
  log_fail "POS-001" "Eval (+ 1 2)" "exit=$BB_EXIT"
fi

repl_run "(* 6 7)\n:quit"
if [[ $BB_EXIT -eq 0 ]] && echo "$BB_OUTPUT" | grep -q "=> 42"; then
  log_pass "POS-002" "Eval (* 6 7) = 42"
else
  log_fail "POS-002" "Eval (* 6 7)" "exit=$BB_EXIT"
fi

repl_run "(if true 1 0)\n:quit"
if [[ $BB_EXIT -eq 0 ]] && echo "$BB_OUTPUT" | grep -q "=> 1"; then
  log_pass "POS-003" "If expression = 1"
else
  log_fail "POS-003" "If expression" "exit=$BB_EXIT"
fi

repl_run "(let [x 10] (+ x 5))\n:quit"
if [[ $BB_EXIT -eq 0 ]] && echo "$BB_OUTPUT" | grep -q "=> 15"; then
  log_pass "POS-004" "Let binding = 15"
else
  log_fail "POS-004" "Let binding" "exit=$BB_EXIT"
fi

repl_run '(str "hello" " " "world")\n:quit'
if [[ $BB_EXIT -eq 0 ]] && echo "$BB_OUTPUT" | grep -q "hello world"; then
  log_pass "POS-005" "String concat"
else
  log_fail "POS-005" "String concat" "exit=$BB_EXIT"
fi

repl_run "true\n:quit"
if [[ $BB_EXIT -eq 0 ]] && echo "$BB_OUTPUT" | grep -q "=> true"; then
  log_pass "POS-006" "Bool literal true"
else
  log_fail "POS-006" "Bool true" "exit=$BB_EXIT"
fi

repl_run "42\n:quit"
if [[ $BB_EXIT -eq 0 ]] && echo "$BB_OUTPUT" | grep -q "=> 42"; then
  log_pass "POS-007" "Integer literal 42"
else
  log_fail "POS-007" "Integer 42" "exit=$BB_EXIT"
fi

repl_run '"hello"\n:quit'
if [[ $BB_EXIT -eq 0 ]] && echo "$BB_OUTPUT" | grep -q "hello"; then
  log_pass "POS-008" "String literal"
else
  log_fail "POS-008" "String literal" "exit=$BB_EXIT"
fi

repl_run "(+ 1 2)\n(+ 3 4)\n(* 5 6)\n:quit"
if [[ $BB_EXIT -eq 0 ]] && echo "$BB_OUTPUT" | grep -q "=> 3" && echo "$BB_OUTPUT" | grep -q "=> 7" && echo "$BB_OUTPUT" | grep -q "=> 30"; then
  log_pass "POS-009" "Multiple expressions in sequence"
else
  log_fail "POS-009" "Multiple expressions" "exit=$BB_EXIT"
fi

repl_run "(do (+ 1 2) (* 3 4))\n:quit"
if [[ $BB_EXIT -eq 0 ]] && echo "$BB_OUTPUT" | grep -q "=> 12"; then
  log_pass "POS-010" "Do block = 12"
else
  log_fail "POS-010" "Do block" "exit=$BB_EXIT"
fi

repl_run "(- 100 58)\n:quit"
if [[ $BB_EXIT -eq 0 ]] && echo "$BB_OUTPUT" | grep -q "=> 42"; then
  log_pass "POS-011" "Subtraction = 42"
else
  log_fail "POS-011" "Subtraction" "exit=$BB_EXIT"
fi

repl_run "(> 5 3)\n:quit"
if [[ $BB_EXIT -eq 0 ]] && echo "$BB_OUTPUT" | grep -q "=> true"; then
  log_pass "POS-012" "Comparison (> 5 3)"
else
  log_fail "POS-012" "Comparison" "exit=$BB_EXIT"
fi

repl_run "(== 5 5)\n:quit"
if [[ $BB_EXIT -eq 0 ]] && echo "$BB_OUTPUT" | grep -q "=> true"; then
  log_pass "POS-013" "Equality (== 5 5)"
else
  log_fail "POS-013" "Equality" "exit=$BB_EXIT"
fi

repl_run "(let [f (fn [x] (+ x 1))] (f 10))\n:quit"
if [[ $BB_EXIT -eq 0 ]] && echo "$BB_OUTPUT" | grep -q "=> 11"; then
  log_pass "POS-014" "Lambda + apply = 11"
else
  log_fail "POS-014" "Lambda" "exit=$BB_EXIT out=$BB_OUTPUT"
fi

# Stateful: defn then call
repl_run "(defn double [x] (* x 2))\n(double 21)\n:quit"
if [[ $BB_EXIT -eq 0 ]] && echo "$BB_OUTPUT" | grep -q "=> 42"; then
  log_pass "POS-015" "Defn then call = 42"
elif has_stacktrace "$BB_OUTPUT"; then
  log_bug "POS-015" "Defn crashes REPL" "UndefinedFunctionError — defn not working"
else
  log_fail "POS-015" "Defn then call" "exit=$BB_EXIT out=$BB_OUTPUT"
fi

# Stateful: deftype then construct
repl_run "(deftype Maybe (Just v) (Nothing))\n(Just 42)\n:quit"
if [[ $BB_EXIT -eq 0 ]] && echo "$BB_OUTPUT" | grep -q "42"; then
  log_pass "POS-016" "Deftype then construct"
elif has_stacktrace "$BB_OUTPUT"; then
  log_bug "POS-016" "Deftype crashes REPL" "Enumerable protocol error on sum type"
else
  log_fail "POS-016" "Deftype then construct" "exit=$BB_EXIT out=$BB_OUTPUT"
fi

echo ""

# ── POSITIVE: REPL COMMANDS ─────────────────────────────────────────
echo "${BOLD}── REPL Command Tests ──${NC}"

repl_run ":help\n:quit"
if [[ $BB_EXIT -eq 0 ]] && echo "$BB_OUTPUT" | grep -q "Commands:"; then
  log_pass "CMD-001" ":help shows commands"
else
  log_fail "CMD-001" ":help" "exit=$BB_EXIT"
fi

repl_run ":env\n:quit"
if [[ $BB_EXIT -eq 0 ]] && echo "$BB_OUTPUT" | grep -qi "no user-defined\|defined"; then
  log_pass "CMD-002" ":env shows env (empty)"
else
  log_fail "CMD-002" ":env" "exit=$BB_EXIT out=$BB_OUTPUT"
fi

repl_run ":clear\n:quit"
if [[ $BB_EXIT -eq 0 ]] && echo "$BB_OUTPUT" | grep -qi "cleared"; then
  log_pass "CMD-003" ":clear clears definitions"
else
  log_fail "CMD-003" ":clear" "exit=$BB_EXIT out=$BB_OUTPUT"
fi

repl_run ":save /tmp/bb_repl_cmd_save.va\n:quit"
if [[ $BB_EXIT -eq 0 ]] && echo "$BB_OUTPUT" | grep -qi "saved"; then
  log_pass "CMD-004" ":save writes file"
else
  log_fail "CMD-004" ":save" "exit=$BB_EXIT out=$BB_OUTPUT"
fi
rm -f /tmp/bb_repl_cmd_save.va

repl_run ":save /tmp/bb_repl_roundtrip.va\n:load /tmp/bb_repl_roundtrip.va\n:quit"
if [[ $BB_EXIT -eq 0 ]] && echo "$BB_OUTPUT" | grep -qi "loaded"; then
  log_pass "CMD-005" ":save then :load round-trip"
else
  log_fail "CMD-005" ":save/:load" "exit=$BB_EXIT out=$BB_OUTPUT"
fi
rm -f /tmp/bb_repl_roundtrip.va

repl_run ":q"
if [[ $BB_EXIT -eq 0 ]] && echo "$BB_OUTPUT" | grep -q "Goodbye"; then
  log_pass "CMD-006" ":q shorthand exits"
else
  log_fail "CMD-006" ":q" "exit=$BB_EXIT"
fi

repl_run ":quit"
if [[ $BB_EXIT -eq 0 ]] && echo "$BB_OUTPUT" | grep -q "Goodbye"; then
  log_pass "CMD-007" ":quit exits"
else
  log_fail "CMD-007" ":quit" "exit=$BB_EXIT"
fi

echo ""

# ── NEGATIVE TESTS ──────────────────────────────────────────────────
echo "${BOLD}── Negative Tests ──${NC}"

repl_run "(((\n:quit"
if [[ $BB_EXIT -eq 0 ]] && echo "$BB_OUTPUT" | grep -qi "error" && ! has_stacktrace "$BB_OUTPUT"; then
  log_pass "NEG-001" "Parse error: clean recovery"
elif has_stacktrace "$BB_OUTPUT"; then
  log_bug "NEG-001" "Parse error crashes REPL" "Stacktrace leaked"
else
  log_fail "NEG-001" "Parse error" "exit=$BB_EXIT"
fi

repl_run '(+ 1 "hello")\n:quit'
if [[ $BB_EXIT -eq 0 ]] && echo "$BB_OUTPUT" | grep -qi "error" && ! has_stacktrace "$BB_OUTPUT"; then
  log_pass "NEG-002" "Type error: clean recovery"
elif has_stacktrace "$BB_OUTPUT"; then
  log_bug "NEG-002" "Type error crashes REPL" "Stacktrace leaked"
else
  log_fail "NEG-002" "Type error" "exit=$BB_EXIT"
fi

repl_run "(unknown-func 1 2)\n:quit"
if [[ $BB_EXIT -eq 0 ]] && echo "$BB_OUTPUT" | grep -qi "error" && ! has_stacktrace "$BB_OUTPUT"; then
  log_pass "NEG-003" "Undefined function: clean recovery"
elif has_stacktrace "$BB_OUTPUT"; then
  log_bug "NEG-003" "Undefined function crashes REPL" "Stacktrace leaked"
else
  log_fail "NEG-003" "Undefined function" "exit=$BB_EXIT"
fi

repl_run ":bogus\n:quit"
if [[ $BB_EXIT -eq 0 ]] && echo "$BB_OUTPUT" | grep -qi "unknown command"; then
  log_pass "NEG-004" "Unknown command: helpful error"
else
  log_fail "NEG-004" "Unknown command" "exit=$BB_EXIT"
fi

repl_run ":load /nonexistent/file.va\n:quit"
if [[ $BB_EXIT -eq 0 ]] && echo "$BB_OUTPUT" | grep -qi "error\|enoent"; then
  log_pass "NEG-005" ":load nonexistent: error"
elif has_stacktrace "$BB_OUTPUT"; then
  log_bug "NEG-005" ":load nonexistent crashes REPL" "Stacktrace leaked"
else
  log_fail "NEG-005" ":load nonexistent" "exit=$BB_EXIT"
fi

repl_run ':save /readonly_dir/test.va\n:quit'
if [[ $BB_EXIT -eq 0 ]] && echo "$BB_OUTPUT" | grep -qi "error\|enoent"; then
  log_pass "NEG-006" ":save to bad path: error"
elif has_stacktrace "$BB_OUTPUT"; then
  log_bug "NEG-006" ":save bad path crashes REPL" "Stacktrace leaked"
else
  log_fail "NEG-006" ":save bad path" "exit=$BB_EXIT"
fi

repl_run '"\n:quit'
if [[ $BB_EXIT -eq 0 ]] && ! has_stacktrace "$BB_OUTPUT"; then
  log_pass "NEG-007" "Unclosed string: clean recovery"
elif has_stacktrace "$BB_OUTPUT"; then
  log_bug "NEG-007" "Unclosed string crashes REPL" "Stacktrace leaked"
else
  log_fail "NEG-007" "Unclosed string" "exit=$BB_EXIT"
fi

repl_run "(deftype Color (Red) (Green) (Blue))\n(match (Red) [(Red) 1] [(Green) 2])\n:quit"
if echo "$BB_OUTPUT" | grep -qi "non-exhaustive\|error" && ! has_stacktrace "$BB_OUTPUT"; then
  log_pass "NEG-008" "Non-exhaustive match detected"
elif has_stacktrace "$BB_OUTPUT"; then
  log_bug "NEG-008" "Non-exhaustive match crashes REPL" "Stacktrace leaked"
else
  log_fail "NEG-008" "Non-exhaustive match" "exit=$BB_EXIT out=$BB_OUTPUT"
fi

echo ""

# ── SYSTEM TESTS ────────────────────────────────────────────────────
echo "${BOLD}── System Tests ──${NC}"

# Banner on startup
repl_run ":quit"
if echo "$BB_OUTPUT" | grep -q "Vaisto REPL" && echo "$BB_OUTPUT" | grep -q "Goodbye"; then
  log_pass "SYS-001" "Banner + goodbye displayed"
else
  log_fail "SYS-001" "Banner/goodbye" "out=$BB_OUTPUT"
fi

# EOF handling (no :quit)
BB_OUTPUT=$(echo "(+ 1 2)" | "$VAISTOC" repl 2>&1); BB_EXIT=$?
if [[ $BB_EXIT -eq 0 ]] && echo "$BB_OUTPUT" | grep -q "=> 3" && echo "$BB_OUTPUT" | grep -q "Goodbye"; then
  log_pass "SYS-002" "EOF exits gracefully"
else
  log_fail "SYS-002" "EOF handling" "exit=$BB_EXIT"
fi

# Recovery: error then success
repl_run "(((\n(+ 1 2)\n:quit"
if [[ $BB_EXIT -eq 0 ]] && echo "$BB_OUTPUT" | grep -q "error" && echo "$BB_OUTPUT" | grep -q "=> 3"; then
  log_pass "SYS-003" "Recovery after parse error"
else
  log_fail "SYS-003" "Error recovery" "exit=$BB_EXIT"
fi

# Multiple error recovery
repl_run '(((\n(+ 1 "x")\n(+ 1 2)\n:quit'
if [[ $BB_EXIT -eq 0 ]] && echo "$BB_OUTPUT" | grep -q "=> 3"; then
  log_pass "SYS-004" "Recovery after multiple errors"
else
  log_fail "SYS-004" "Multi-error recovery" "exit=$BB_EXIT"
fi

# Empty lines don't break session
repl_run "\n\n\n(+ 1 2)\n:quit"
if [[ $BB_EXIT -eq 0 ]] && echo "$BB_OUTPUT" | grep -q "=> 3"; then
  log_pass "SYS-005" "Empty lines handled"
else
  log_fail "SYS-005" "Empty lines" "exit=$BB_EXIT"
fi

# :clear resets state
repl_run "(+ 1 2)\n:clear\n:env\n:quit"
if [[ $BB_EXIT -eq 0 ]] && echo "$BB_OUTPUT" | grep -qi "cleared" && echo "$BB_OUTPUT" | grep -qi "no user-defined"; then
  log_pass "SYS-006" ":clear resets to clean state"
else
  log_fail "SYS-006" ":clear reset" "exit=$BB_EXIT out=$BB_OUTPUT"
fi

echo ""

# ── ABNORMAL TESTS ──────────────────────────────────────────────────
echo "${BOLD}── Abnormal Tests ──${NC}"

# Division by zero
repl_run "(/ 1 0)\n:quit"
if [[ $BB_EXIT -eq 0 ]] && ! has_stacktrace "$BB_OUTPUT"; then
  log_pass "ABN-001" "Division by zero: clean recovery"
elif has_stacktrace "$BB_OUTPUT"; then
  log_bug "ABN-001" "Division by zero crashes REPL" "ArithmeticError leaked to user"
else
  log_fail "ABN-001" "Division by zero" "exit=$BB_EXIT"
fi

# Division by zero then continue
repl_run "(/ 1 0)\n(+ 1 2)\n:quit"
if [[ $BB_EXIT -eq 0 ]] && echo "$BB_OUTPUT" | grep -q "=> 3"; then
  log_pass "ABN-002" "Recovery after div/0"
elif has_stacktrace "$BB_OUTPUT"; then
  log_bug "ABN-002" "Div/0 kills session — no recovery" "REPL terminates on runtime error"
else
  log_fail "ABN-002" "Div/0 recovery" "exit=$BB_EXIT"
fi

# Very long expression
long_expr=$(python3 -c "print('(+ ' * 100 + '1' + ' 1)' * 100)")
repl_run "$long_expr\n:quit"
if ! has_stacktrace "$BB_OUTPUT"; then
  log_pass "ABN-003" "100-level deep nesting: handled"
else
  log_bug "ABN-003" "Deep nesting crashes REPL" "Stacktrace leaked"
fi

# Unicode in expressions
repl_run '(str "hello" " 🌍")\n:quit'
if [[ $BB_EXIT -eq 0 ]] && ! has_stacktrace "$BB_OUTPUT"; then
  log_pass "ABN-004" "Emoji in strings: works"
elif has_stacktrace "$BB_OUTPUT"; then
  log_bug "ABN-004" "Emoji crashes REPL" "Stacktrace leaked"
else
  log_fail "ABN-004" "Emoji" "exit=$BB_EXIT"
fi

# Whitespace-only input
repl_run "   \n:quit"
if [[ $BB_EXIT -eq 0 ]] && ! has_stacktrace "$BB_OUTPUT"; then
  log_pass "ABN-005" "Whitespace-only: handled"
else
  log_bug "ABN-005" "Whitespace crashes REPL" "Stacktrace leaked"
fi

# Very long string
long_str=$(python3 -c "print('a' * 10000)")
repl_run "(str \"$long_str\")\n:quit"
if [[ $BB_EXIT -eq 0 ]] && ! has_stacktrace "$BB_OUTPUT"; then
  log_pass "ABN-006" "10,000-char string: handled"
else
  log_bug "ABN-006" "Long string crashes REPL" "Stacktrace leaked"
fi

# Command with extra spaces
repl_run ":help  \n:quit"
if [[ $BB_EXIT -eq 0 ]] && ! has_stacktrace "$BB_OUTPUT"; then
  log_pass "ABN-007" "Command with trailing spaces: handled"
else
  log_fail "ABN-007" "Command with spaces" "exit=$BB_EXIT"
fi

# Rapid sequential expressions
repl_run "(+ 1 1)\n(+ 2 2)\n(+ 3 3)\n(+ 4 4)\n(+ 5 5)\n(+ 6 6)\n(+ 7 7)\n(+ 8 8)\n(+ 9 9)\n(+ 10 10)\n:quit"
if [[ $BB_EXIT -eq 0 ]] && echo "$BB_OUTPUT" | grep -q "=> 20"; then
  log_pass "ABN-008" "10 rapid expressions: all processed"
else
  log_fail "ABN-008" "Rapid expressions" "exit=$BB_EXIT"
fi

# Expression after :clear
repl_run ":clear\n(+ 1 2)\n:quit"
if [[ $BB_EXIT -eq 0 ]] && echo "$BB_OUTPUT" | grep -q "=> 3"; then
  log_pass "ABN-009" "Expression after :clear works"
else
  log_fail "ABN-009" "After :clear" "exit=$BB_EXIT"
fi

# :load with non-.va file
echo "not vaisto code" > /tmp/bb_repl_bad.txt
repl_run ":load /tmp/bb_repl_bad.txt\n:quit"
if [[ $BB_EXIT -eq 0 ]] && ! has_stacktrace "$BB_OUTPUT"; then
  log_pass "ABN-010" ":load non-.va file: handled"
elif has_stacktrace "$BB_OUTPUT"; then
  log_bug "ABN-010" ":load non-.va crashes REPL" "Stacktrace leaked"
else
  log_fail "ABN-010" ":load non-.va" "exit=$BB_EXIT"
fi
rm -f /tmp/bb_repl_bad.txt

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
rm -f "$_OUT"

[[ $((FAIL + BUGS)) -gt 0 ]] && exit 1 || exit 0
