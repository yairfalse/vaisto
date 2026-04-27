# vaistoc CLI — Black-Box Test Results

**Date:** 2026-04-28
**Version:** 0.1.0
**Total:** 62 cases | **Pass:** 62 | **Fail:** 0 | **Bugs:** 0

All seven categories pass. Suite is fully green.

## Coverage Summary

| Category | Cases | Pass | Fail |
|----------|-------|------|------|
| Positive | 27 | 27 | 0 |
| Negative | 13 | 13 | 0 |
| System | 4 | 4 | 0 |
| Integration | 3 | 3 | 0 |
| Performance | 3 | 3 | 0 |
| Load | 2 | 2 | 0 |
| Abnormal | 10 | 10 | 0 |

## Running the Suite

```bash
# From project root:
test/blackbox/runner.sh

# Single case:
test/blackbox/runner.sh POS-008
```

## History

### 2026-04-28 — POS-008 fixed

**POS-008 (`vaistoc --eval "(defn f [x :int] :int (+ x 1)) (f 5)"`)** — multi-form `--eval` input combining a `defn` with a top-level expression returned exit=1 with `<eval>: BEAM compilation failed`.

Root cause: the type checker misclassified `:f` as a local variable (env leak of `__local_vars__` across top-level forms when prelude is loaded), emitting `{:apply, {:var, :f, ...}, args, ret}` instead of `{:call, :f, args, ret}`. The Core Erlang emitter's `:apply` clause then unconditionally emitted `c_apply(c_var(:f), ...)`, which the cerl `:core_lint` step rejected with `{:unbound_var, :f, {:main, 0}}`.

Fix: `lib/vaisto/core_emitter.ex` `:apply` clause now checks `user_fns` and emits `c_fname` for top-level user-defined functions, mirroring the existing `:call` clause's `cond`. Defensive against further typechecker classification drift; the underlying `__local_vars__` leak in `check_module_forms` is a separate concern still worth fixing.

### 2026-02-09 — initial run, all 4 confirmed bugs and 3 design issues now resolved

The February audit identified 4 bugs and 3 design issues in the `--eval` and `compile` paths:

- **BUG-1, BUG-2** — `--eval ""` and `--eval "   "` leaked `UndefinedFunctionError` stacktraces. Fixed.
- **BUG-3** — `--eval "(/ 1 0)"` leaked `ArithmeticError` stacktrace. Fixed (try/rescue around eval execution).
- **BUG-4** — Compile to a read-only directory leaked `File.Error` stacktrace. Fixed (`File.write/3` with structured error).
- **ISSUE-1** — `--eval ")"` parsed `)` as an atom. Fixed (parser tightened).
- **ISSUE-2** — Non-`.va` files compiled silently. Fixed (extension check).
- **ISSUE-3** — Package names with digits rejected. Fixed (validator relaxed).
