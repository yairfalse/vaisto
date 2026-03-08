# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What is Vaisto?

Vaisto ("Finnish for intuition") is a statically-typed Scheme-like language that compiles to BEAM bytecode. It combines:
- S-expression syntax (minimal, parseable)
- Hindley-Milner type inference (ML/Rust-style safety without annotation tax)
- BEAM runtime (Erlang/Elixir ecosystem, fault tolerance, distribution)

The key insight: BEAM's process isolation makes Rust-style ownership unnecessary—you get safety through the runtime.

## Build Commands

```bash
mix deps.get              # Install dependencies
mix test                  # Run all tests (1211 tests)
mix test test/parser_test.exs       # Run a single test file
mix test test/parser_test.exs:12    # Run a specific test by line number
mix escript.build         # Build the CLI compiler (escript)
./vaistoc file.va                   # Compile a .va file to BEAM
./vaistoc file.va -o build/File.beam  # Compile with specific output
./vaistoc --eval "(+ 1 2)"         # Evaluate an expression
./vaistoc build src/ -o build/      # Build all .va files in directory
./vaistoc repl                      # Start REPL
./vaistoc lsp                       # Start LSP server
```

Dependencies: `jason ~> 1.4` (JSON for LSP), `toml ~> 0.7` (manifest parsing). No CI, no formatter config, no Credo/Dialyzer.

## Architecture

### Compilation Pipeline

```
Source (.va) → Parser → AST → TypeChecker → Typed AST → Backend → BEAM bytecode
                                                         ├─ CoreEmitter (Core Erlang, default for :core)
                                                         └─ Emitter (Elixir AST, default for :elixir)
```

Orchestrated by `Vaisto.Compilation.compile/3`. The `Vaisto.Backend` behaviour dispatches to `Backend.Core` or `Backend.Elixir`.

### Key Modules

| Module | Purpose |
|--------|---------|
| `Vaisto.Parser` | S-expression parser. AST nodes are tuples with `%Loc{}` as final element. **Raises** on syntax errors (not `{:error, ...}`). |
| `Vaisto.TypeChecker` | HM-style bidirectional type inference. Two-pass: collect signatures, then check bodies via `check_s`/`check_impl_s` (ctx-threaded). Returns `{:ok, type, typed_ast}`. |
| `Vaisto.TypeChecker.TcCtx` | Type checking context. Threads substitution, tvar counter, constraints, constrained_tvars, field_tvars through inference. |
| `Vaisto.TypeSystem.Infer` | Algorithm W for anonymous functions. **Separate** from main TypeChecker—used as fallback for `{:fn, ...}` nodes. Has its own context (`TypeSystem.Context`). |
| `Vaisto.TypeSystem.Core` | Type primitives: `{:tvar, id}`, `{:rvar, id}`, substitutions, `apply_subst/2` |
| `Vaisto.TypeSystem.Unify` | Unification with occurs check. Handles row polymorphism. `:any` unifies with everything. |
| `Vaisto.CoreEmitter` | Typed AST → Core Erlang via `:cerl` module → BEAM via `:compile.forms/2` |
| `Vaisto.Emitter` | Typed AST → Elixir quoted AST → compiled via `Code.compile_quoted/1` |
| `Vaisto.Build` | Multi-file builds: dependency graph, topological sort, `.vsi` interface files |
| `Vaisto.Interface` | Module interface serialization (Erlang `term_to_binary`) for separate compilation |
| `Vaisto.Errors` | Structured error constructors with Jaro-distance "did you mean?" hints |
| `Vaisto.ErrorFormatter` | Rust-style error rendering with source context and ANSI colors |

### Two Type Checkers — Important

The codebase has two separate type inference engines:
1. **`Vaisto.TypeChecker`** — main bidirectional checker, uses `TcCtx` for context threading
2. **`Vaisto.TypeSystem.Infer`** — Algorithm W, used as fallback for anonymous functions (`{:fn, params, body}`)

They use different context structs (`TcCtx` vs `TypeSystem.Context`). Both now return structured `%Error{}` values via `Vaisto.Errors` constructors. When `Infer` is invoked from inside `TypeChecker.check_impl_s`, the `TcCtx` context is **not** propagated through the inference.

### Lambda Fallback Mechanism

When the TypeChecker encounters `{:fn, params, body}`, it first tries Infer (Algorithm W). If Infer fails, the error is classified by `infer_should_fallback?/1`:
- **Fallback errors** (`unknown expression`, `unknown function`, `undefined variable`, `cannot call non-function`) → re-check with `:any`-typed params via `fallback_lambda/3`. These indicate Infer's limitations (limited env, unsupported forms).
- **Genuine errors** (type mismatch, arity mismatch, predicate not bool, etc.) → propagated as-is. The code is actually wrong.

### ctx-threaded vs legacy check

The TypeChecker has two calling conventions:
- `check(form, env)` — legacy, creates fresh `TcCtx` per call, returns `{:ok, type, ast}`
- `check_s(form, ctx)` — ctx-threaded, returns `{:ok, type, ast, new_ctx}`

Functions with `_s` suffix thread `TcCtx`. The `check_module_forms` pipeline uses `check_s` to thread ctx between top-level forms, resetting substitutions between forms while preserving the tvar counter.

### Type Representations

```elixir
:int, :float, :string, :bool, :any, :atom, :unit  # Primitives
{:tvar, id}                    # Type variable (inference)
{:rvar, id}                    # Row variable (row polymorphism)
{:fn, [arg_types], ret_type}   # Function type
{:tuple, [elem_types]}         # Typed tuple
{:list, elem_type}             # Homogeneous list
{:record, name, [{field, type}]}  # Product type
{:sum, name, [{ctor, [field_types]}]}  # ADT
{:row, [{field, type}], tail}  # Row type (tail is :closed or {:rvar, id})
{:pid, process_name, accepted_msgs}  # Typed PID
{:process, state_type, msg_types}    # Process type
{:forall, [tvar_ids], type}    # Polymorphic scheme (from generalization)
{:forall, [tvar_ids], {:constrained, [{class, type}], type}}  # Constrained scheme
```

### AST Conventions

Parser output always includes location as final tuple element:
```elixir
{:call, func, args, %Loc{}}
{:if, cond, then, else, %Loc{}}
{:defn, name, params, body, ret_type, %Loc{}}
```

Typed AST annotates with types (no location):
```elixir
{:lit, :int, 42}               # Typed literal
{:var, name, type}             # Typed variable
{:call, func, typed_args, ret_type}  # Typed call
{:class_call, class, method, instance_key, args, ret_type}  # Typeclass dispatch
```

### Typeclass System

Dictionary-passing implementation:
- `defclass` defines method signatures
- `instance` provides implementations (stored in env under `{:__instances__, class, type}`)
- `class_call` typed AST nodes → emitters generate dictionary lookup + method call
- Constrained instances (`where` clause) pass inner dictionaries as arguments
- Deriving: `deftype ... deriving [Eq Show]` auto-synthesizes instances

### Module System

```scheme
(ns MyModule)                  ; Declare module name
(import Std.List)              ; Import module
(import Std.List :as L)        ; Import with alias
(Std.List/fold xs 0 +)         ; Qualified call
```

Files in `std/` contain standard library modules with `.vsi` interface files for separate compilation.

## Language Features

```scheme
; Function definition with type annotations (mixed typed/untyped supported)
(defn add [x :int y :int] :int (+ x y))
(defn take [n :int xs] ...)          ; n is :int, xs is :any

; Multi-clause functions (pattern matching) — currently arity-1 only
(defn len
  [[] 0]
  [[h | t] (+ 1 (len t))])

; Algebraic data types
(deftype Result (Ok v) (Err e))
(deftype Point [x :int y :int])      ; Record
(deftype Color (Red) (Green) deriving [Eq Show])

; Type classes
(defclass Eq [a] (eq [x :a y :a] :bool))
(instance Eq :int (eq [x y] (== x y)))
(instance Show (Maybe a) where [(Show a)]
  (show [x] (match x [(Just v) (str "Just(" (show v) ")")] [(Nothing) "Nothing"])))

; Pattern matching
(match result [(Ok v) v] [(Err e) default])

; Process definition (typed, compiles to GenServer or raw spawn loop)
(process counter 0
  :increment (+ state 1)
  :get state)

; Supervision
(supervise :one_for_one (counter 0))

; Erlang interop
(extern erlang:hd [(List :any)] :any)
```

### Design Decisions

- **No macros** — keeps type checking tractable and tooling possible
- **Typed PIDs** — `spawn` returns `(Pid ProcessName)`, `!` validates message types
- **Row polymorphism** — functions can require records with *at least* certain fields
- **Exhaustiveness checking** — match on sum types and booleans must cover all variants
- **Two backends** — Core Erlang (`:core`) and Elixir (`:elixir`), tested for parity via `core_backend_parity_test.exs`

## Known Limitations & Gaps

### Type System
- `:any` unifies with everything — acts as escape hatch that bypasses type safety
- Extern argument types are best-effort checked (fall back to declared ret_type on mismatch)
- Unknown qualified calls silently return `:any` instead of erroring
- Multi-clause functions (`defn_multi`) hardcode arity=1
- No try/catch/after, no receive-with-timeout, no binary/bitstring syntax

### Error Handling
- Parser **raises** on syntax errors (wrap with `Compilation.parse/2` for `{:error, ...}`)
- `CoreEmitter` wraps all exceptions in broad `rescue` — emitter bugs produce generic messages

### Infrastructure
- No CI pipeline, no `.formatter.exs`, no Credo/Dialyzer
- `Vaisto.Interface` has zero tests
- CLI (`vaistoc`) has zero tests

## Error Messages

Errors follow Rust's style: short, exact, with source context and actionable hints. Structured errors use `Vaisto.Error` with spans for rich formatting via `Vaisto.ErrorFormatter`. There is also a legacy `Vaisto.Diagnostic` struct that overlaps with `Error` and should eventually be unified.

## Testing

1211 tests across 42 files. Key test files:
- `typeclass_test.exs` (120+) — typeclasses, constraints, deriving, both backends
- `type_system/infer_test.exs` (122) — Algorithm W
- `core_backend_parity_test.exs` (81) — runs same code through both backends, compares results
- `emitter_test.exs` (64) — Elixir backend end-to-end
- `type_checker_test.exs` (66) — type checking, error messages, HM inference, lambda fallback
- `tuple_types_test.exs` — `(Tuple ...)` annotations and inference

Test helpers in `test/support/test_helpers.ex`: `parse!`, `check_type`, `assert_type`, `compile_and_run!`, `eval!`.
