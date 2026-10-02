# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What is Vaisto?

Vaisto ("Finnish for intuition") is a typed substrate for structurally accountable LLM systems. Prompts, contracts, and pipelines are typed artifacts checked at compile time — the headline compile error is `prompt_output_mismatch`, where a downstream `(generate :extract T)` rejects a prompt whose `:output` type lacks required fields. See `README.md` for the framing and `docs/design/task-contracts-manifesto.md` for the "why."

The implementation vehicle is a statically-typed Scheme-like language compiling to BEAM bytecode:
- S-expression syntax (minimal, parseable, no macros)
- Hindley-Milner type inference (ML/Rust-style safety without annotation tax)
- BEAM runtime — process isolation makes Rust-style ownership unnecessary; pipeline steps get fault isolation for free
- A real compiler/LSP loop, so prompts behave like typed code with prose inside

Most of the codebase today is the language and compiler; task contracts (`defprompt`, `pipeline`, `generate`) are a small but load-bearing surface on top. Treat both layers as first-class when scoping work.

## Companion docs

- `AGENTS.md` — working contract for AI agents in this repo (conventions, constraints, workflow). Read alongside this file.
- `DESIGN.md` — long-form design rationale for the language and compiler.
- `README.md` — the public framing: prompts as accountable, typed artifacts.
- `docs/adr/` — accepted architecture decisions (e.g. compiler implementation choice).
- `docs/design/` — spec drafts (`task-contracts-spec.md`, `task-contracts-manifesto.md`, `vaisto-bpf.md`); `task-contracts-spec.md` covers operators not yet wired in code.
- `docs/design/liquid-vaisto-rfc.md` — the Liquid Vaisto RFC (refinement types over a small semantic core). §1 maps the current compiler, and §1.12 lists 24 reproduced defects with reproductions in Appendix A. Check it before assuming a language feature works end to end.

## Build Commands

```bash
mix deps.get              # Install dependencies (also needed in every fresh git worktree)
mix test                  # Run all tests (~1560); excludes the :live OpenAI test; test/refine needs z3 on PATH
mix test --include live   # Also run the live OpenAI test (needs OPENAI_API_KEY)
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

Blackbox tests run against the built CLI: `mix escript.build`, then `test/blackbox/runner.sh`. The runner's cases are hardcoded; `test/blackbox/dataset.json` and `lsp_dataset.json` record each case's `spec_source` and are not read by the runner. `test/blackbox/lsp_runner.exs` talks to the LSP server directly, bypassing stdio.

Requires Elixir `~> 1.15`. Dependencies: `jason ~> 1.4` (JSON for LSP), `toml ~> 0.7` (manifest parsing). No CI, no formatter config, no Credo/Dialyzer.

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
| `Vaisto.Parser` | S-expression parser. AST nodes are tuples with `%Loc{}` as final element. **Raises** on syntax errors. |
| `Vaisto.TypeChecker` (+ `TypeChecker.TcCtx`) | HM-style bidirectional inference. Two-pass: collect signatures, then check bodies via `check_s`/`check_impl_s` (ctx-threaded; `TcCtx` carries substitution, tvar counter, constraints, field_tvars). Returns `{:ok, type, typed_ast}`. |
| `Vaisto.TypeSystem.Infer` | Algorithm W for anonymous functions. **Separate** from main TypeChecker — used as fallback for `{:fn, ...}` nodes. Has its own context (`TypeSystem.Context`). |
| `Vaisto.TypeSystem.Unify` | Unification with occurs check, row polymorphism, `:any` unifying with everything. **Always use this — don't write bespoke field/type comparison.** |
| `Vaisto.CoreEmitter` | Typed AST → Core Erlang via `:cerl` → BEAM via `:compile.forms/2`. Default for `:core`. |
| `Vaisto.Emitter` | Typed AST → Elixir quoted AST → `Code.compile_quoted/1`. Default for `:elixir`. **Only this emitter handles `defprompt`/`pipeline`/`generate`.** |
| `Vaisto.Error` (+ `Errors`, `ErrorFormatter`) | Structured error struct with spans, expected/actual, hints, notes. `Errors` exposes ~55 constructors with Jaro-distance "did you mean?" hints; `ErrorFormatter` renders Rust-style with source context. |
| `Vaisto.Compilation` | Pipeline orchestration. `compile/3` runs the full pipeline, `run/2` is eval (compile+execute+cleanup), `parse/2` wraps Parser raises into `{:error, Error}`. |
| `Vaisto.Runner` | Bridge to Elixir: `compile_and_load/3`, `run/2`, `call/3`, `spawn_process/2`. Used in e2e tests via `Runner.run(code, backend: :core)`. |
| `Vaisto.Build` (+ `Interface`) | Multi-file builds: dependency graph, topological sort, `.vsi` interface files. `Interface` serializes module interfaces (Erlang `term_to_binary`). |
| `Vaisto.Package.Manifest` / `Package.Namespace` | Parses `vaisto.toml`; resolves module names. Used by `Build` for multi-file compilation. |
| `Vaisto.Elab` (+ `Types`, `Shadow`) | The Phase 1a bidirectional elaborator: surface AST straight to Liquid Core (liquid-types.md §8, §9). Signatures required (a lowercase keyword such as `:a` is a type variable; `(Fn :a :b)` a function type; `(the τ e)` ascribes), local unification only, no `:any`, no coercion, no default types, exhaustive matches. Shadow mode: HM still drives compilation; `Shadow.run/1` compares the elaborated Core with the backends, suggesting HM's types as signatures. Its output must always pass Core Lint. |
| `Vaisto.Refine` (+ `Surface`, `Predicate`, `VCGen`, `SMTLIB`, `Solver.Z3`) | Refinement types, RFC Phase 2.1. `Surface.split/1` takes `{v :int \| p}` out of the parsed program so HM sees base types; `Refine.check/3` adapts HM's output to Liquid Core, gates it (§21.1), generates VCs and asks `z3` over a Port. The type checker refuses a refined type that skipped this, so every compile path must call `Compilation.check_refinements/5`. |
| `Vaisto.Lsp.Server` / `Lsp.Handler` | JSON-RPC over stdio. `Server` reads frames; `Handler` dispatches to feature modules: `Completion`, `Hover`, `SignatureHelp`, `References`, `InlayHints`. `AstAnalyzer` and `Position` are shared utilities. |
| `Vaisto.LLM` (+ `LLM.Mock`, `LLM.OpenAI`) | Behaviour + dispatcher. `call/4` looks up provider via `Application.get_env(:vaisto, :llm, Vaisto.LLM.Mock)`. `Mock` returns canned responses for tests; `OpenAI` uses `:httpc` with structured outputs (Responses API). |

### Two Type Checkers — Important

The codebase has two separate type inference engines:
1. **`Vaisto.TypeChecker`** — main bidirectional checker, uses `TcCtx` for context threading
2. **`Vaisto.TypeSystem.Infer`** — Algorithm W, used as fallback for anonymous functions (`{:fn, params, body}`)

They use different context structs (`TcCtx` vs `TypeSystem.Context`) but both return structured `%Error{}`. When `Infer` is invoked from inside `TypeChecker.check_impl_s`, the `TcCtx` context is **not** propagated through the inference.

`Infer` also keeps its own `@primitives` table (`infer.ex:26`), separate from `TypeEnv`. The two disagree today: `/` is `Int -> Int -> Int` there and `Float` in the main checker, so a function declared `:int` can return `3.5` through a lambda. Change primitive types in both places, or better, make `Infer` read `TypeEnv`.

### Lambda Fallback Mechanism

When the TypeChecker encounters `{:fn, params, body}`, it first tries Infer (Algorithm W). If Infer fails, the error is classified by `infer_should_fallback?/1`:
- **Fallback errors** (`unknown expression`, `unknown function`, `undefined variable`, `cannot call non-function`) → re-check with `:any`-typed params via `fallback_lambda/3`. These indicate Infer's limitations (limited env, unsupported forms).
- **Genuine errors** (type mismatch, arity mismatch, predicate not bool, etc.) → propagated as-is. The code is actually wrong.

### ctx-threaded vs legacy check

The TypeChecker has two calling conventions:
- `check(form, env)` — legacy, creates fresh `TcCtx` per call, returns `{:ok, type, ast}`
- `check_s(form, ctx)` — ctx-threaded, returns `{:ok, type, ast, new_ctx}`

Functions with `_s` suffix thread `TcCtx`. The `check_module_forms` pipeline uses `check_s` to thread ctx between top-level forms, resetting substitutions between forms while preserving the tvar counter.

### Structured Error System

All errors use `%Vaisto.Error{}`:
```elixir
%Error{
  message: "type mismatch",           # Required
  primary_span: %{line: 3, col: 8, length: 7, label: "expected `Int`"},
  expected: :int,                      # For type errors
  actual: :string,                     # For type errors
  hint: "use (string:to_int ...)",     # Actionable suggestion
  note: "additional context"           # Extra info
}
```

Error constructors live in `Vaisto.Errors` (e.g., `Errors.type_mismatch/3`, `Errors.undefined_variable/2`). `ErrorFormatter.format/3` renders them with source context, pointer lines, and ANSI colors.

Pattern for returning errors: `{:error, Errors.some_error(args)}`. Pattern for matching in tests: `assert {:error, %Error{message: "type mismatch"}} = ...` or match on the message string.

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
{:try, body, catch_clauses, after_body, %Loc{}}
```

Typed AST annotates with types (no location):
```elixir
{:lit, :int, 42}               # Typed literal
{:var, name, type}             # Typed variable
{:call, func, typed_args, ret_type}  # Typed call
{:class_call, class, method, instance_key, args, ret_type}  # Typeclass dispatch
{:try, typed_body, [{class, var, handler, handler_type}], typed_after, result_type}  # Try/catch
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

`std/` contains only `.va` source (plus `prelude.va`, which is prepended to every compile). `.vsi` interface files are generated by `vaistoc build` and are gitignored; `scripts/bootstrap.sh` builds `std` into `build/bootstrap`.

### Task Contracts

Task contracts (`defprompt`, `pipeline`, `generate`) are typed surfaces over LLM operations. The type checker enforces structural compatibility between a prompt's `:output` type and a downstream `(generate :extract T)` — fields missing from the prompt's output but required by the extract target produce `prompt_output_mismatch` errors with field-level diffs (this is the headline compile error in the README).

Compilation is asymmetric across backends: `defprompt` is metadata-only, and `pipeline` / `generate` emit runtime calls into `Vaisto.LLM.call/4` via **`Vaisto.Emitter` only**. `Vaisto.CoreEmitter` does not handle these forms — pipelines containing `generate` cannot target `:core` today. The provider is selected at runtime via `Application.put_env(:vaisto, :llm, Vaisto.LLM.OpenAI)` (default: `Vaisto.LLM.Mock`).

**Implementation status.** Only `defprompt`, `pipeline`, and `generate` are wired through parser → type checker → emitter today. The full design — 11 operators (`retrieve`, `rerank`, `extract`, `verify`, `tool`, `branch`, `map`, `parallel`, `fold`, `escalate`) and a `Ctx a` primitive with `payload`/`trace`/`budget`/`prov` fields — is specified in `docs/design/task-contracts-spec.md` but not yet bound in code. The manifesto (`docs/design/task-contracts-manifesto.md`) is the "why."

Typed AST shapes:
```elixir
{:defprompt, name, input_type, output_type, template, :unit}
{:pipeline, name, input_type, output_type, typed_ops, :unit}
{:generate, prompt_name, extract_type, type}
```

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

; Task contracts — typed prompts and pipelines (Elixir backend only)
(deftype Question [text :String])
(deftype Answer [text :String])
(defprompt qa
  :input  Question
  :output Answer
  :template """
  Answer concisely.
  Question: {text}
  """)
(pipeline simple-qa
  :input  Question
  :output Answer
  (generate :prompt qa :extract Answer))

; Process definition (typed, compiles to GenServer or raw spawn loop)
(process counter 0
  :increment (+ state 1)
  :get state)

; Supervision
(supervise :one_for_one (counter 0))

; Error handling
(try (/ 1 0) [catch [:error e (Err e)]])
(try (risky) [catch [:error e (Err e)]] [after (cleanup)])
(try (risky) [after (cleanup)])  ; after-only, exception propagates

; Erlang interop
(extern erlang:hd [(List :any)] :any)
```

### Design Decisions

- **No macros** — keeps type checking tractable and tooling possible
- **Typed PIDs** — `spawn` returns `(Pid ProcessName)`, `!` validates message types
- **Row polymorphism** — functions can require records with *at least* certain fields
- **Exhaustiveness checking** — match on sum types and booleans must cover all variants
- **Two backends** — Core Erlang (`:core`) and Elixir (`:elixir`), tested for parity via `core_backend_parity_test.exs`. The suite misses known divergences:
  - `and`/`or` are strict on `:core` and short-circuit on `:elixir`;
  - guarded `defn` fails to compile on `:core`;
  - record field access crashes on `:elixir`;
  - `let` bindings leak on `:elixir`.

  When touching an emitter, run end-to-end tests on both backends.

## Known Limitations & Gaps

### Type System
- `:any` unifies with everything — acts as escape hatch that bypasses type safety
- Extern argument types are best-effort checked (fall back to declared ret_type on mismatch)
- Unknown qualified calls silently return `:any` instead of erroring
- Multi-clause functions (`defn_multi`) hardcode arity=1
- No receive-with-timeout, no binary/bitstring syntax
- Record/sum annotations on `defn` parameters do not resolve at call sites: `[p :Point]` gives "expected `Point`, found `Point`". Existing tests only type-check such definitions and never call them.
- `match` and `receive` clauses have no guards: `[x :when g body]` is a parse error (D27). Multi-clause `defn` takes guards.
- Lambda parameters cannot be annotated: `(fn [x :int] x)` is a two-parameter lambda.
- `(deftype opaque ...)` and `(deftype T p [...])` are silently misparsed as records.
- Row-polymorphic field access type-checks but crashes at runtime on both backends when given a record.
- The checker accepts some ill-typed programs with no `:any` in the source (leaked `let` scope, a declared return type checked without the body's substitution, ignored sum-constructor field types). The full list with reproductions is in the Liquid Vaisto RFC, §1.12 and Appendix A.

### Error Handling
- Parser **raises** on syntax errors (wrap with `Compilation.parse/2` for `{:error, ...}`)
- `CoreEmitter` wraps all exceptions in broad `rescue` — emitter bugs produce generic messages

### Infrastructure
- No CI pipeline, no `.formatter.exs`, no Credo/Dialyzer
- `Vaisto.Interface` has zero tests
- CLI (`vaistoc`) has zero tests

## Error Messages

Errors follow Rust's style: short, exact, with source context and actionable hints. Structured errors use `Vaisto.Error` with spans for rich formatting via `Vaisto.ErrorFormatter`.

## When in Doubt — Navigation Guide

Orienting heuristics for common change types:

- **Adding a new top-level form** (alongside `defn` / `deftype` / `defprompt`): mirror `parse_defn` in `lib/vaisto/parser.ex`, wire through `TypeChecker.check_module_forms`, then **both emitters** (CoreEmitter and Emitter) — unless the form is task-contract-related, in which case Elixir-only is the documented asymmetry.
- **Adding a new typed AST node**: every emitter that pattern-matches typed AST nodes needs a clause. Skipping `CoreEmitter` is fine for `defprompt`/`pipeline`/`generate` (already asymmetric); not fine for general language constructs.
- **Adding a new error**: write a constructor in `Vaisto.Errors` returning `%Vaisto.Error{}` with a `%Span{}`. Use Jaro-distance hints when "did you mean?" applies — see `Errors.undefined_variable` for the pattern. In tests, match on the message string or the `%Error{message: ...}` shape.
- **Touching unification or type comparison**: route through `Vaisto.TypeSystem.Unify`. Bespoke field-walking is the wrong answer.
- **Lambda type-inference fails unexpectedly**: check `infer_should_fallback?/1` — fallback errors trigger `:any`-typed re-check, genuine errors propagate. Misclassifying turns real bugs into silent `:any` calls.
- **Parser errors during compilation**: remember the parser **raises**. Use `Vaisto.Compilation.parse/2` to get `{:error, Error}` instead of an exception.
- **Adding/changing types in typed AST**: `TypeFormatter` may need an entry so errors and LSP hover render correctly.
- **LSP feature work**: feature modules under `lib/vaisto/lsp/` are dispatched from `Lsp.Handler`. `AstAnalyzer` + `Position` are the shared utilities for translating LSP positions to AST nodes.
- **Multi-file changes**: `Vaisto.Build` writes `.vsi` files via `Vaisto.Interface`, but cross-module typing does not work today:
  - interfaces are keyed `Elixir.A:f` while lookups use `A:f`, so imported calls are typed `:any`;
  - loading an interface wipes the built-in typeclass registry;
  - `DependencyResolver` never matches imports to graph keys, so build order follows atom-creation order. This is the cause of the intermittent failure at `test/build/integration_test.exs:124`.

  Do not rely on imported types being checked.

## Testing

About 1560 tests. `test/build/integration_test.exs:124` fails intermittently (see Multi-file changes above). Key test files:
- `typeclass_test.exs` — typeclasses, constraints, deriving, both backends
- `type_system/infer_test.exs` — Algorithm W
- `core_backend_parity_test.exs` — runs same code through both backends, compares results
- `emitter_test.exs` — Elixir backend end-to-end
- `type_checker_test.exs` — type checking, error messages, HM inference, lambda fallback
- `tuple_types_test.exs` — `(Tuple ...)` annotations and inference
- `try_catch_test.exs` — parser, type checker, and e2e tests for try/catch/after (both backends)
- `task_contract_parser_test.exs`, `task_contract_typecheck_test.exs`, `emitter_task_contract_test.exs` — `defprompt`/`pipeline`/`generate` (parser, type checker, Elixir-backend e2e)

### Test Helpers

`import Vaisto.TestHelpers` in tests. Key functions:

| Helper | Returns | Purpose |
|--------|---------|---------|
| `parse!(code)` | AST | Parse source, raises on failure |
| `parse_clean(code)` | AST (no locs) | Parse and strip `%Loc{}` for structural comparison |
| `check_type(code)` | `{:ok, type}` or `{:error, reason}` | Parse + type check, return inferred type |
| `assert_type(code, type)` | `true` | Assert code type-checks to expected type |
| `assert_type_error(code, pattern)` | `true` | Assert type error matches string or regex |
| `compile_and_run!(code)` | result | Compile via Core Erlang, load, call `main/0` |
| `eval!(expr)` | result | Wrap expr in `(defn main [] :any ...)`, compile and run |

For e2e tests that need backend selection, use `Runner.run(code, backend: :core)` or `Runner.run(code)` (defaults to `:elixir`).
