# Vaisto

**Finnish for "intuition"**: a typed language on the BEAM, and a substrate for
structurally accountable LLM systems.

Vaisto has three layers, each built on the one below:

1. **A statically typed Scheme-like language** that compiles to BEAM bytecode:
   S-expression syntax, Hindley-Milner type inference, algebraic data types,
   type classes, typed processes, and Erlang interop.
2. **Task contracts**: prompts and LLM pipelines as typed artifacts. A
   pipeline that extracts a type the prompt does not promise does not compile.
3. **Liquid Vaisto**: a small semantic core that says what every Vaisto
   program means, with a reference evaluator, a trusted checker and a
   differential harness. BEAM becomes one implementation of it. Its first
   refinement types, checked with the Z3 solver, work today.

> Prompts are prose. Vaisto makes them structurally accountable.

Vaisto is early. The language, both backends, the LSP server and the prompt
checks work today; the semantic core has its foundation and its first
refinement types in place. Every known gap is listed under
[Status and known gaps](#status-and-known-gaps).

## Contents

- [Why Vaisto](#why-vaisto)
- [Accountable prompts, today](#accountable-prompts-today)
- [The language](#the-language)
- [Task contracts](#task-contracts)
- [Liquid Vaisto: a semantic core](#liquid-vaisto-a-semantic-core)
- [Architecture](#architecture)
- [Getting started](#getting-started)
- [Editor support](#editor-support)
- [Development](#development)
- [Status and known gaps](#status-and-known-gaps)
- [Documentation](#documentation)
- [Related work](#related-work)

## Why Vaisto

LLM systems drift in ways ordinary software tools do not catch.

A prompt changes but the downstream parser still expects the old shape. A model
is swapped and starts omitting fields. A retrieval step is "improved" and the
answerer no longer receives the evidence the product depends on. A pipeline
written against one provider's API fossilizes when the ecosystem moves.

These failures usually appear late: in production, in eval dashboards, or in a
customer-visible malformed response. The code did not necessarily rot. The
abstractions did.

Durable software survives churn by separating **what must hold** from **how it
is executed**. SQL separates a query from the plan the database chooses. POSIX
separates a program's contract with the operating system from kernel
internals. LLM systems need the same separation, so Vaisto is built around two
languages:

1. **Contracts**: the durable obligation. Input type, output type, budget,
   quality target, policy, and failure semantics.
2. **Pipelines**: one executable strategy for meeting that obligation, by
   retrieving, generating, extracting, verifying, branching, escalating and
   calling tools.

The contract should outlive model churn; pipelines, prompts, model bindings,
retrievers and tools evolve underneath it.

```text
Contract   -> typed obligation
Pipeline   -> typed implementation
Compiler   -> structural satisfaction
Optimizer  -> binding choice
Runtime    -> stochastic agreement
```

The SQL analogy is useful but not exact. A SQL optimizer relies on algebraic
equivalences; Vaisto cannot honestly claim that two prompts or two models are
equivalent. It replaces algebraic equivalence with static types, structural
prompt checks, declared contracts, typed failures, runtime provenance, and a
history of empirical agreement.

## Accountable prompts, today

A prompt is not an anonymous string. It declares the input it may reference and
the output it promises:

```scheme
(deftype DocId [value :String])
(deftype Question [text :String])
(deftype CitedAnswer [text :String evidence (List DocId)])
(deftype Answer [text :String evidence (List DocId)])

(defprompt answer-with-citations
  :input  Question
  :output CitedAnswer
  :template """
  Answer with citations.
  Question: {text}
  """)

(pipeline legal-qa
  :input  Question
  :output Answer
  (generate :prompt answer-with-citations :extract Answer))
```

If someone changes the prompt's output type and drops `evidence`:

```scheme
(deftype CitedAnswer [text :String])
```

the pipeline no longer compiles:

```text
$ vaistoc qa.va
error: prompt output type mismatch
  at line 17
      (generate :prompt answer-with-citations :extract Answer))
      ^ expected `Answer`, found `CitedAnswer`
  note: prompt `answer-with-citations` output CitedAnswer does not satisfy extract target Answer
  note: missing field: evidence : List(:DocId)
```

A silent prompt/schema failure has become a compile-time error.

## The language

Vaisto is a Lisp in syntax and an ML in its types. Programs are S-expressions,
types are inferred, and the output is ordinary BEAM bytecode. There are no
macros: the syntax stays small enough that types, tools and the editor can see
everything. Every example below compiles and runs on both backends unless it
says otherwise.

### Functions and inference

```scheme
(defn square [x :int] :int (* x x))
(defn twice [f x] (f (f x)))

(defn main [] :int (twice square 3))   ; 81
```

Annotations are optional and can be mixed: `twice` is inferred as
`(a -> a) -> a -> a`.

### Algebraic data types and pattern matching

```scheme
(deftype Shape (Circle :float) (Rect :float :float))

(defn area [s]
  (match s
    [(Circle r) (* 3.0 (* r r))]
    [(Rect w h) (* w h)]))

(defn main [] :float (+ (area (Circle 1.0)) (area (Rect 2.0 3.0))))   ; 9.0
```

A `match` on a sum type must cover every constructor:

```text
error: non-exhaustive pattern match
  at line 4
      (match (Circle s)
      ^
  note: match on `Shape` does not cover all variants
  help: missing variants: Rect
```

### Records

```scheme
(deftype Point [x :int y :int])

(defn main [] :int
  (match (Point 3 4)
    [(Point x y) (+ (* x x) (* y y))]))   ; 25
```

Fields can also be read with `(. p :x)`.

### Lists, higher-order functions, and multi-clause definitions

```scheme
(defn main [] :int
  (fold + 0 (map (fn [x] (* x x)) (filter (fn [x] (> x 2)) [1 2 3 4]))))   ; 25

(defn len
  [[] 0]
  [[h | t] (+ 1 (len t))])
```

### Type classes

Classes compile to dictionary passing. Instances can be written by hand or
derived:

```scheme
(defclass Describe [a] (describe [x :a] :string))
(instance Describe :int (describe [x] "an int"))
(instance Describe :bool (describe [x] "a bool"))

(deftype Color (Red) (Green) deriving [Eq Show])

(defn main [] :string (++ (describe 42) (show (Red))))   ; "an intRed"
```

### Errors

```scheme
(defn safe-div [a :int b :int] :int
  (try (div a b) [catch [:error e 0]]))

(defn main [] :int (+ (safe-div 10 2) (safe-div 1 0)))   ; 5
```

`try` also takes an `[after ...]` clause.

### Processes and typed PIDs

A process declares its state and the messages it accepts. `spawn` returns a
PID typed by the process, so sending it a message it does not accept is a
compile error:

```scheme
(process counter 0
  :increment (+ state 1)
  :get state)

(defn bump []
  (let [pid (spawn counter 0)]
    (! pid :log)))
```

```text
error: invalid message type
  at line 7
        (! pid :log)))
        ^
  note: process `counter` does not accept `:log`
  help: accepted messages: :increment, :get
```

`!!` sends without the check. Supervision is syntax as well:
`(supervise :one_for_one (counter 0))`. From Elixir, `Vaisto.Runner` loads,
spawns and talks to processes:

```elixir
{:ok, mod} = Vaisto.Runner.compile_and_load(source, :CounterApp)
{:ok, pid} = Vaisto.Runner.spawn_process(mod, 0)
Vaisto.Runner.send_msg(pid, :increment)   # 1
```

### Erlang interop

```scheme
(extern erlang:abs [:int] :int)

(defn main [] :int (erlang:abs -42))   ; 42
```

### Modules and packages

```scheme
(ns Geometry)                 ; optional; checked against the file path
(import Std.List)
(import Std.List :as L)
(Std.List/fold xs 0 +)        ; a qualified call
```

Module names come from file paths (`src/Vaisto/Lexer.va` is
`Vaisto.Lexer`). `vaistoc build` compiles a directory in dependency order and
writes an interface file (`.vsi`) for each module. A package is a directory
with a `vaisto.toml`:

```bash
vaistoc init hello-world && cd hello-world && vaistoc build
vaistoc add ../json-parser          # a local dependency
```

The standard library lives in `std/`: `List`, `Map`, `String`, `Math`, `IO`,
`File`, `Json`, `Regex`, `Binary`, `State`, and a prelude.

## Task contracts

What is implemented today:

- `defprompt`, a typed prompt with an input type, an output type and a
  template;
- `pipeline` and `generate`, which compose prompts into a typed pipeline;
- the check above: a pipeline cannot extract a type its prompt's output does
  not satisfy, reported with the missing fields;
- two LLM providers, selected at runtime. `Vaisto.LLM.Mock`, the default,
  returns canned responses for deterministic tests. `Vaisto.LLM.OpenAI` uses
  structured outputs over `:httpc`, with no extra dependency:

  ```elixir
  Application.put_env(:vaisto, :llm, Vaisto.LLM.OpenAI)
  System.put_env("OPENAI_API_KEY", "sk-...")
  ```

Task-contract forms compile on the Elixir backend only.

The design goes further, and none of what follows is implemented yet.
Contracts become first-class, and several pipelines can satisfy one contract
with different strategies:

```scheme
(defcontract legal-qa
  :input Question
  :output CitedAnswer
  :quality {:min-conf 0.90}
  :budget {:cost 0.10 :latency 8s}
  :failure {:timeout retry :malformed-extract retry :low-confidence escalate})

(pipeline legal-qa-careful
  :satisfies legal-qa
  (retrieve :from legal-corpus :k 20)
  (rerank :model auto :keep-top 5)
  (generate :prompt careful-answer :extract CitedAnswer)
  (verify :rule citation-check)
  (branch (< conf 0.90)
    (escalate :human-review)
    pass))
```

Prompt lint makes writing a prompt feel like writing typed code with prose
inside it:
- placeholders autocomplete from the input type;
- unknown placeholders are errors;
- unused input fields and unmentioned output fields are warnings.

Lint is deliberately modest: it shows that a prompt is structurally aligned
with its types and contract, not that it is good or truthful.

Each run produces an **agreement record**:
- the contract, pipeline and binding;
- whether extraction and verification passed;
- cost, latency and confidence;
- the provenance of the prompt, model and tool versions.

That is the empirical counterpart to SQL's algebraic equivalence. Vaisto cannot
prove two bindings equivalent, but it can record whether a binding meets its
contract often enough, cheaply enough and safely enough.

The full argument is in the
[manifesto](docs/design/task-contracts-manifesto.md), and the eleven planned
operators and the `Ctx` type are specified in the
[task-contracts spec](docs/design/task-contracts-spec.md).

## Liquid Vaisto: a semantic core

Today the meaning of a Vaisto program is whatever its two backends do with it,
and they do not always agree. Liquid Vaisto turns that around: Vaisto has a
semantics, and BEAM implements it.

The semantics is **Liquid Core**, a small typed language with four primitives:
- values;
- refinements (types with predicates, such as "a non-zero integer");
- algebraic effects (sending, receiving and crashing are operations a handler
  interprets);
- evidence origin (where each fact a checker relies on came from).

Everything else is library code over those four, among it type classes, rows,
processes, contracts and authority. A program elaborates to Core:
- a reference evaluator defines what it means;
- a trusted checker, Core Lint, re-checks every elaboration;
- a differential harness holds BEAM to the evaluator.

The design, its algebra and its roadmap are in the
[Liquid Vaisto RFC](docs/design/liquid-vaisto-rfc.md).

### What exists

Phase 0, the foundation, is in place:

| Piece | Where | What it does |
|---|---|---|
| The Core specification | [`docs/design/liquid-core.md`](docs/design/liquid-core.md) | Normative: types, terms, evaluation, primitives, representations on BEAM, and the typing rules |
| Canonical trees | `Vaisto.Liquid.Canonical` | Canonical S-expression bytes, SHA-256 digests that ignore metadata, and a readable form |
| Reference evaluator | `Vaisto.Liquid.Eval` | The executable form of the specification |
| Core Lint | `Vaisto.Liquid.Lint` | Checks Core and infers nothing: kinds, rows, exhaustiveness, guards, ground equality, unique binders |
| Soundness tests | `test/liquid/soundness_test.exs` | On every run: 1,000 generated well-typed terms and 20,000 mutants. If Lint gives a term a type, it never goes wrong when evaluated |
| Adapter | `Vaisto.Liquid.Adapter` | Today's typed AST to Core. Where HM left a type unknown it writes `Dyn`, so Lint names the place |
| Differential harness | `Vaisto.Liquid.Harness` | Runs a program through the evaluator and both backends and compares outcomes |

Run over the 135 programs in the test suite, the harness gave these verdicts
when this was written:

| Verdict | Programs | What it means |
|---|---|---|
| agree | 84 | the evaluator and both backends give the same result |
| disagree | 0 | a backend differs from the evaluator: a backend bug |
| Lint rejects | 25 | programs HM accepted but left ill-typed, such as arithmetic on an unannotated parameter. This is the to-do list for the type checker |
| outside the core so far | 20 | processes, `str`, and calls across modules |
| rejected by HM | 6 | tests that expect a type error |

### Refinement types

A refined type is a type with a predicate, written in the type slot of a
parameter or a result:

```scheme
(defn safe-div [x :int y {d :int | (!= d 0)}] :int (div x y))
(defn clamp0 [x :int] {r :int | (>= r 0)} (if (< x 0) 0 x))

(defn avg [total :int n :int] :int
  (if (> n 0) (safe-div total n) 0))      ; compiles: n > 0 here
(defn avg-bad [total :int n :int] :int
  (safe-div total n))                     ; does not compile
```

```text
error: requirement not met
  at line 7
      (safe-div total n))                     ; does not compile
                      ^ `safe-div` requires (!= n 0) for its argument `y`
  note: nothing is known about `n` here
```

The compiler proves, with the Z3 solver, that every call to a refined function
meets its requirements and that every refined function returns what it
promises. Inside a refined function it also proves that `div`, `rem`, `head`
and `tail` cannot crash. Branch conditions, `and`/`or`, guards, `let`
bindings, list patterns and the results of other refined functions all count
as facts. Calls are checked in every function, refined or not. A program
without refinements never starts the solver.

The modules are `Vaisto.Refine` (the check and its diagnostics),
`Vaisto.Refine.VCGen` (verification conditions over Core),
`Vaisto.Refine.Predicate` (predicates to the solver's logic) and
`Vaisto.Refine.SMTLIB` and `Vaisto.Refine.Solver.Z3` (the solver, reached over
a port). The conformance programs of RFC §14 are in `test/refine/conformance/`.

This is the RFC's Phase 2.1, and it is deliberately narrow:
- refinements over `Int`, `Bool` and `List` (with `len`), on the parameters and
  results of single-clause functions;
- predicates use comparisons, `+ - *`, `and or not => iff`, `if` and `len`,
  but not `div` or function calls;
- the program still runs on today's backends, through the gated bridge of RFC
  §21.1. Every definition that has or uses refinements must be in the Core
  fragment, pass Core Lint, and avoid constructs the backends disagree on.
  Otherwise the build fails; it never runs unchecked.

### What comes next

The bidirectional elaborator that replaces HM (Phase 1a,
[`docs/design/liquid-types.md`](docs/design/liquid-types.md)), then
refinements over records, sums and measures (Phases 2.2 and 2.3). Later phases
bring effect rows and deterministic replay, sealed types and capabilities, and
contracts as a library. The RFC's §21 has the order and the dependencies.

## Architecture

```text
Source (.va) -> Parser -> AST -> TypeChecker (HM) -> typed AST -+-> CoreEmitter -> Core Erlang -> BEAM  (default)
                                                                +-> Emitter     -> Elixir AST  -> BEAM
                                                                |
                                                                +-> Liquid Adapter -> Liquid Core
                                                                       -> Core Lint (checks)
                                                                       -> reference evaluator (means)
                                                                       -> harness: evaluator vs both backends
```

| Path | What lives there |
|---|---|
| `lib/vaisto/parser.ex` | S-expression parser; every AST node carries its location |
| `lib/vaisto/type_checker.ex`, `type_system/` | Bidirectional HM checker, unification with row polymorphism, and an Algorithm W engine for lambdas |
| `lib/vaisto/core_emitter.ex` | The Core Erlang backend, the default for the CLI and builds |
| `lib/vaisto/emitter.ex` | The Elixir backend, the only one that compiles task contracts |
| `lib/vaisto/errors.ex`, `error_formatter.ex` | Structured errors with spans and hints, rendered in Rust's style |
| `lib/vaisto/build/`, `package/` | Multi-file builds, dependency order, `.vsi` interfaces, `vaisto.toml` |
| `lib/vaisto/lsp/` | The language server |
| `lib/vaisto/llm/` | LLM providers |
| `lib/vaisto/liquid/` | Liquid Core: codec, evaluator, Lint, adapter, harness |
| `std/` | The standard library, in Vaisto |
| `src/Vaisto/` | A proof-of-concept compiler written in Vaisto ([ADR-002](docs/adr/002-compiler-implementation.md)); not the production compiler, and out of date |

## Getting started

You need Elixir `~> 1.15` and a matching Erlang/OTP. Vaisto depends only on
`jason` and `toml`. Programs with refinement types also need the
[Z3](https://github.com/Z3Prover/z3) solver on your `PATH`
(`brew install z3`, `apt install z3`).

```bash
git clone https://github.com/yairfalse/vaisto.git
cd vaisto
mix deps.get
mix test
mix escript.build        # builds ./vaistoc
```

The CLI:

```bash
./vaistoc file.va                     # compile to BEAM (Core Erlang backend)
./vaistoc file.va -o build/File.beam  # choose the output
./vaistoc file.va --backend elixir    # use the Elixir backend
./vaistoc --eval "(+ 1 2)"            # evaluate an expression
./vaistoc build src/ -o build/        # build a directory
./vaistoc init my-lib                 # start a package
./vaistoc repl                        # interactive REPL
./vaistoc lsp                         # language server on stdio
```

From Elixir, `Vaisto.Runner.run(source)` compiles and runs a program's `main`;
pass `backend: :core` or `backend: :elixir`.

## Editor support

`vaistoc lsp` provides diagnostics, hover types, completion, signature help,
references and inlay hints. A VS Code extension lives in `editors/vscode`:
install the packaged `vaisto-0.1.0.vsix`, or run it from source:

```bash
mix escript.build
cd editors/vscode
npm install
```

Then open `editors/vscode` in VS Code and press **F5** to start an Extension
Development Host. If `vaistoc` is not on your `PATH`, point the extension at
it:

```json
{ "vaisto.serverPath": "/path/to/vaisto/vaistoc" }
```

## Development

```bash
mix test                              # the whole suite; the live OpenAI test is excluded
mix test --include live               # also call OpenAI (needs OPENAI_API_KEY)
mix test test/parser_test.exs         # one file
mix test test/parser_test.exs:12      # one test
mix test test/liquid/                 # Liquid Core, including the soundness fuzzing and the harness
mix test test/refine/                 # refinement types; needs z3, and fails without it
```

Black-box tests run against the built CLI: `mix escript.build`, then
`test/blackbox/runner.sh`. `test/blackbox/lsp_runner.exs` drives the language
server.

To see what the harness says about one program:

```bash
mix run -e 'IO.inspect(Vaisto.Liquid.Harness.run(File.read!("program.va")).verdict)'
```

`test/liquid/harness_test.exs` runs it over every program in the test suite on
each `mix test`, and fails on any disagreement that is not a known defect.

`AGENTS.md` is the working contract for contributors, human or AI: the
conventions, the constraints and the expected workflow. `CLAUDE.md` is the
architecture reference. There is no CI or formatter configuration yet; CI is
planned on [SYKLI](#related-work).

## Status and known gaps

Each gap below has been reproduced against the current code. The numbers are
the defect IDs of the [Liquid Vaisto RFC](docs/design/liquid-vaisto-rfc.md)
(§1.12, with reproductions in Appendix A) and of
[`liquid-core.md`](docs/design/liquid-core.md) §12.

**The backends disagree** on some programs:
- `==` is exact equality on one backend and numeric equality on the other
  (D25);
- a failed match crashes with a different reason on each backend (D26).

**The type checker accepts some ill-typed programs.** Among them:
- a function declared `:int` can return a float or a string (D4, D16);
- arithmetic on an unannotated parameter is generalized to any type (D17);
- a `match` on an unannotated parameter is not tied to its patterns' type
  (D22);
- `:any` unifies with everything;
- unknown qualified calls are typed `Any`;
- multi-clause functions are typed `Any -> Any`.

**Modules.** Cross-module calls are not type-checked yet. `.vsi` interfaces are
written but not used for typing imports (D9).

**Records.** A function parameter annotated with a record or sum type does not
match values of that type at call sites (D1, D2). Row-polymorphic field access
on a record crashes at run time (D24).

**Guards.** `match` and `receive` clauses have no guards; `[x :when g body]`
there is a parse error (D27). Multi-clause functions take guards.

**Not there yet:** receive with a timeout, binary syntax, decoding values
from `Dyn`, refinements beyond `Int`, `Bool` and `List` on single-clause
functions, effect rows, CI.

## Documentation

| Document | What it is |
|---|---|
| [`docs/design/liquid-vaisto-rfc.md`](docs/design/liquid-vaisto-rfc.md) | Liquid Vaisto: the semantic core, refinements, effects, evidence, and the roadmap |
| [`docs/design/liquid-core.md`](docs/design/liquid-core.md) | The normative definition of Liquid Core, version 0 |
| [`docs/design/task-contracts-manifesto.md`](docs/design/task-contracts-manifesto.md) | Why task contracts |
| [`docs/design/task-contracts-spec.md`](docs/design/task-contracts-spec.md) | The planned contract operators and `Ctx` |
| [`docs/design/vaisto-bpf.md`](docs/design/vaisto-bpf.md) | Compiling a Vaisto subset to eBPF |
| [`DESIGN.md`](DESIGN.md) | Language design decisions |
| [`docs/adr/`](docs/adr/) | Architecture decisions: [compiler implementation](docs/adr/002-compiler-implementation.md); [built-in telemetry](docs/adr/001-builtin-telemetry.md), accepted but not built |
| [`AGENTS.md`](AGENTS.md), [`CLAUDE.md`](CLAUDE.md) | Contributor contract and architecture reference |

## Related work

- **DSPy**: a Python optimizer for prompt tuning. Vaisto focuses on typed
  closure, structural accountability and runtime supervision.
- **LangChain, LlamaIndex**: composition by framework convention. Vaisto moves
  composition checks into the language.
- **Liquid Haskell**: refinement types checked by an SMT solver. Liquid Vaisto
  takes the idea to a BEAM language with algebraic effects.
- **Z3**: the SMT solver the Liquid Vaisto RFC uses for refinement checks. It
  is not a substitute for evaluating a model at run time.
- **Gleam**: a typed BEAM language and a close architectural cousin.
- **LFE**: a Lisp on the BEAM, untyped.

Vaisto is meant to sit in a larger stack: Vaisto is the typed substrate for
contracts, prompts and pipelines; BEAM provides isolation, supervision and
distribution; AHTI correlates causality across operations; SYKLI runs CI for
systems built on contracts.

## Origin

Conceived January 2026, 3am Berlin, while waiting for family to fly home.
Started as "learn Elixir methodically" and became a language design by
following intuition.

## License

MIT
