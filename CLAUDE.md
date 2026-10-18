# CLAUDE.md

Architecture reference for Vaisto. Read `AGENTS.md` for the working contract
and `README.md` for language examples and motivation.

## Scope and stack

Vaisto is a Scheme-like, statically typed BEAM language, with typed processes
and a small prompt/pipeline surface. Implementation: Elixir. Dependencies:
`jason`, `toml`. Z3 is an executable reached through a Port, started only for
refinements. Do not add dependencies or another implementation language.

The production compiler is under `lib/`. The non-building `src/Vaisto/`
self-hosting experiment is historical and outside the type-system migration
(`docs/design/liquid-types.md` §13; ADR-002).

## Compiler pipeline

```text
source → Parser → located surface AST → Elab → Liquid Core
       → Core Lint → Refine.check_core → Liquid.Lower
       → Erlang abstract format → OTP compiler → native BEAM
```

There is one compiler. HM, Algorithm W, inference contexts, the adapter,
shadow mode, both old emitters and backend dispatch have been removed. Do not
reintroduce a fallback, unchecked inferred signature or legacy `.vsi` reader.

`Compilation.compile/3` delegates to `compile_liquid/3`.
`Compilation.check_liquid/3` performs the same checking without lowering.
`Vaisto.compile_string/2`, file compilation, Runner, Build, REPL and LSP type
queries use this pipeline. The old `:backend` option is rejected.
`Compilation.parse/2` wraps parser exceptions in structured errors.

## Native type checking

`Vaisto.Elab` is a pure, bidirectional elaborator. Introduction forms check
against an expected type; elimination forms synthesize one. Local context
threads substitution, fresh metavariable counter, names and effect checks.

- Every top-level function and value needs a written signature. Expression
  programs synthesize only their generated `main/0`.
- A lowercase signature keyword (`:a`) is universally quantified and rigid
  in the body. Calls instantiate it with fresh local metavariables.
- Local `let` bindings are monomorphic; no automatic let-generalization.
- `:any` and `:num` are not source types. Use explicit `:dyn` and `up`/`decode`
  boundaries, or a quantified parameter with an explicit Num/Ord constraint.
- Int and Float are distinct. `/` takes Float; `div` takes Int. Conversion is
  explicit with `int-to-float`; unresolved numeric choices are errors.
- Match coverage is checked, let patterns must be irrefutable and guards
  must be guard-safe. Guard failure is an explicit `no_match` crash.
- Use `Elab.Types.unify/4`; thread its returned counter as well as its
  substitution. Row/effect unification can allocate a shared fresh tail.
- Type, Row and Eff variables have different kinds. Generalization is over
  written signature variables, never over unresolved local metavariables.

Examples:

```scheme
(defn identity [x :a] :a x)
(defn apply [f (Fn :a :b :effects :fx) x :a] :b :effects :fx (f x))
(defn len (Fn (List :a) :int) [[] 0] [[h | t] (+ 1 (len t))])
(defn name [p (Record [name :string] :r)] :string (. p :name))
(def answer :int 42)
```

Closed effects use `:effects [external]`; open tails use `:effects :fx`.
`(Effects [external] :fx)` combines a prefix with an open tail. Neither
substitution nor unification may introduce duplicate effect labels.

## Representations and independent checks

Surface AST nodes have a final `%Vaisto.Parser.Loc{}`. Core is canonical
S-expression data represented by Elixir lists, not the deleted typed AST.

| Meaning | Core type |
|---|---|
| Integer / float / string | `:Int`, `:Float`, `:String` |
| Boolean / dynamic data | `[:Bool]`, `:Dyn` |
| List / tuple | `[:List, t]`, `[:Tuple, t1, t2]` |
| Function | `[:->, params, [:eff, labels..., tail], result]` |
| Named type / typed PID | `[:Name, args...]`, `[:Pid, protocol]` |
| Anonymous record | `[:Record, [:row, [field, type]..., tail]]` |
| Signature variable | `[:tvar, name]`, `[:rvar, name]`, `[:evar, name]` |
| Local metavariable | `{:meta, id}` — must not reach emitted Core |

Core Lint rechecks every definition independently: kinds, types, effects,
guard safety, coverage, ground equality, and globally unique binders within
each definition. A Lint failure after successful elaboration is a compiler
bug. Lowering gates emission with Lint too. Never weaken a rule to make an
elaborator output pass.

The reference evaluator is independent of native lowering/runtime. Strict
scripts require exact operations/arguments and complete consumption. Pure
handlers reject effects. Observer handlers return the first pending request
without executing it; this is not resumable or general deterministic replay.

## Runtime features

- Functions, closures, recursive groups, data, pattern matching and control
  flow lower to native BEAM, not evaluator calls.
- Extern results and selected mailbox data are deep-decoded against ground
  types. A wrong declaration/response becomes `bad_decode`.
- Source processes have typed state transitions and nominal protocols.
  Unexpected actor messages are dropped. General selective receive retains
  unmatched messages and retries after selected malformed data.
- Safe `!` checks the protocol; unchecked `!!` uses explicit dynamic data
  transport but still requires a PID. Both return Unit (`nil`).
- Native actors support GenServer calls and request/reply message interop.
  `start_link/1` is provided for a lone actor unless explicitly occupied;
  supervision provides linked OTP startup and restart ordering.
- Classes/defaults/instances/deriving lower to ordinary typed dictionaries.
  Constrained methods receive dictionaries; generic defaults are checked even
  if no instance uses them. No global mutable class registry is needed.
- `try` preserves error/throw/exit classes and exactly-once `after`. Handlers
  bind a checked Reason, not an implicitly untyped exception payload.
- Typed top-level values remain zero-arity accessors, recomputed per read.

## Interfaces, refinements and tasks

Build resolves dependencies first and passes canonical interfaces directly.
`.vsi` files contain checked Core signatures, datatypes, import digests,
effect rows, class schemas and dictionary factories. `Interface.from_core/3`
checks Core/refinements before exporting. `validate_core/1` independently
checks the interface schema and types. Legacy Erlang term environments are
not accepted. Digests are content identity, not authentication or proof that
an independently supplied BEAM file implements an interface.

`Refine.Surface.split/1` separates base annotations and refined signatures.
`Refine.check_core/3` Lints the whole module, checks refinement erasure and
generates VCs. Z3 verifies Int/Bool/List refinements on single-clause
functions. Unknown answers and unsupported verification fail closed. Imported
refinements remain refused; unrefined programs never start the solver.

`defprompt`/`pipeline`/`generate` use a typed external driver boundary. Output
projection reconstructs the requested record tag; finite schemas and deep
validation guard model responses. `LLM.Mock` is the default provider;
`LLM.OpenAI` uses `:httpc`. The additional task-contract operators, provenance,
budgets, sealing, higher-rank types and richer refinements remain design work.

## Interactive tools and diagnostics

REPL rechecks accumulated source transactionally. Replacing a definition or
loading a file must not leave callers invalid or partially mutate state.
LSP diagnostics, hover and inlay types use verified Core. Scoped expression
probes are tooling-only, preserve guard safety, and must not accept invalid
source. Handler positions use UTF-16 conversions through `LSP.Position`.

Use `Vaisto.Errors`/`Vaisto.Error` with source spans, filenames, hints and
notes; render via `ErrorFormatter`. `TypeFormatter` understands Core types
and the separate runtime descriptors used by LLM schema APIs. "Did you mean?"
hints reuse Jaro distance in `Errors`, not a separate guessing algorithm.

## Navigation and verification

- Syntax: `parser.ex`; new forms should mirror defn/deftype parsing.
- Source rules: `elab.ex`, `elab/types.ex`, `classes.ex`, `exhaustive.ex`.
- New Core forms must update the specification, Lint, evaluator and lowering;
  preserve independent implementations.
- Native execution: `liquid/lower.ex`, `runtime.ex`.
- Imports/builds: `interface.ex`, `build/compiler.ex`, dependency resolver.
- Refinements: `refine.ex`, `refine/vc_gen.ex`, predicate and solver modules.
- Editors: `lsp/handler.ex`, diagnostics, hover, inlay hints and position.

Run `mix test` (Z3 must be on PATH) and `mix escript.build`. There is no CI or
formatter configuration. For explicitly selected changed files, run
`mix format --check-formatted path...`. Exercise the real binary before
claiming completion.

Key suites: `test/elab/`, `test/liquid/` (generation, mutation, native parity),
`test/refine/`, `native_checker_test.exs`, `native_pipeline_test.exs`,
`native_cli_test.exs`, `native_examples_test.exs`, `test/std/` and `test/lsp/`.
The former backend-parity file now compares the native compiler with Eval.
TestHelpers returns Core types and executes native code; it never reconstructs
HM types or invents signatures.
