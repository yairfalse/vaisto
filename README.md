# Vaisto

**Finnish for "intuition"**: a typed language on the BEAM, and a substrate for
structurally accountable LLM systems.

Vaisto has three layers, each built on the one below:

1. **A statically typed Scheme-like language** that compiles to BEAM bytecode:
   S-expression syntax, signatures at definitions with local inference, algebraic data types,
   type classes, typed processes, and Erlang interop.
2. **Task contracts**: prompts and LLM pipelines as typed artifacts. A
   pipeline that extracts a type the prompt does not promise does not compile.
3. **Liquid Vaisto**: a small semantic core that says what every Vaisto
   program means, with a reference evaluator, a trusted checker and a
   differential harness. BEAM becomes one implementation of it. Its first
   refinement types, checked with the Z3 solver, work today.

> Prompts are prose. Vaisto makes them structurally accountable.

Vaisto is early. The native compiler, LSP server, prompt checks and first
refinement types work today. HM and the two old compiler backends have been
removed; every compilation goes through verified Liquid Core. Every known gap is listed under
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
(deftype DocId [value :string])
(deftype Question [text :string])
(deftype CitedAnswer [text :string evidence (List DocId)])
(deftype Answer [text :string evidence (List DocId)])

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
(deftype CitedAnswer [text :string])
```

the pipeline no longer compiles:

```text
$ vaistoc qa.va
error: task projection is missing field `evidence`
```

A silent prompt/schema failure has become a compile-time error.

## The language

Vaisto has Lisp syntax, explicit polymorphic signatures and local inference.
Programs are S-expressions; the output is ordinary BEAM bytecode. There are no
macros: the syntax stays small enough that types, tools and the editor can see
everything. The examples below target the native compiler; examples marked
as errors or future design are intentionally not runnable.

### Functions and inference

```scheme
(defn square [x :int] :int (* x x))
(defn twice [f (Fn :a :a :effects :fx) x :a] :a :effects :fx (f (f x)))

(defn main [] :int (twice square 3))   ; 81
```

Signatures are required on definitions; calls and local expressions infer
their types. `:a` is universally quantified in the signature and rigid in its
body. `:fx` carries the callback's effects. Local `let` bindings are not
generalized. Int and Float are distinct; use `(int-to-float n)` explicitly.

### Algebraic data types and pattern matching

```scheme
(deftype Shape (Circle :float) (Rect :float :float))

(defn area [s :Shape] :float
  (match s
    [(Circle r) (* 3.0 (* r r))]
    [(Rect w h) (* w h)]))

(defn main [] :float (+ (area (Circle 1.0)) (area (Rect 2.0 3.0))))   ; 9.0
```

A `match` on a sum type must cover every constructor:

```text
error: this match is not exhaustive
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

(defn len (Fn (List :a) :int)
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

(defn bump [] :unit
  (let [pid (spawn counter 0)]
    (! pid :log)))
```

```text
error: process does not accept `:log`
  note: accepted messages: :increment, :get
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
(Std.List/fold + 0 [1 2 3])        ; a qualified call
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

Task-contract forms compile through checked Core to native BEAM. Foreign and
model responses are decoded before being exposed as typed values.

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

Every production entry point uses the same pipeline:

```text
Source → Parser → bidirectional elaborator → Liquid Core
       → independent Core Lint → refinement verification
       → Erlang abstract forms → OTP compiler → native BEAM
```

HM, its contexts, the Phase 0 adapter and the old emitters are deleted. There
is no fallback for unsigned definitions or unsupported constructs.

| Piece | Purpose |
|---|---|
| `Vaisto.Elab`, `Elab.Types`, `Elab.Classes` | Required signatures, local unification, kinded rows and explicit dictionaries |
| `Vaisto.Liquid.Lint` | Independently checks types, effects, guards, exhaustiveness, ground data and binder uniqueness |
| `Vaisto.Liquid.Lower`, `Runtime` | Native functions, closures, recursion, processes, supervision, decoding and checked effect boundaries |
| `Vaisto.Liquid.Eval`, `Harness` | Independent reference execution, bounded native comparison, generated terms and mutation tests |
| `Vaisto.Interface`, `Build` | Canonical `.vsi` signatures, types, effects and class dictionaries, compiled in dependency order |
| `Vaisto.Compilation`, `Runner`, `REPL`, `LSP` | Shared verified pipeline; transactional REPL definitions and scoped editor type queries |

The corpus tests reject any evaluator/native disagreement. Scripted handlers
check effect ordering; observer handlers stop before the first effect without
executing it. Generation and mutation tests exercise the independent checker.
These tests are evidence, not a formal proof of the compiler.

### Refinement types

```scheme
(defn safe-div [x :int y {d :int | (!= d 0)}] :int (div x y))
(defn avg [total :int n :int] :int
  (if (> n 0) (safe-div total n) 0))
```

Z3 verifies requirements at calls and guarantees at returns, using path,
guard and list-length facts. A call `(safe-div 1 0)` fails compilation.
Refinements currently cover Int, Bool and List on single-clause functions;
unsupported verification fails closed. A program without refinements never
starts Z3. Imported refined functions remain refused rather than silently
erasing their requirements.

### Explicit dynamic and effect boundaries

```scheme
(extern erlang:abs [:int] :int)
(defn absolute [x :int] :int (erlang:abs x))

(defn decoded [] :int
  (match (decode :int (up :int 42))
    [(Core.Ok n) n]
    [(Core.Err _) 0]))

(defn apply [f (Fn :a :b :effects :fx) x :a] :b :effects :fx (f x))
```

`:dyn` is explicit dynamic data, not an implicit cast. Extern results are
deep-decoded against their declared ground data type; a false declaration
crashes with `bad_decode`. Pure `(Fn …)` callbacks cannot conceal effects.
Use `:effects [external]` for a closed row, `:effects :fx` for a quantified
row, or `:effects (Effects [external] :fx)` for a prefix and open tail.

Anonymous records can quantify their remaining fields:

```scheme
(defn name [p (Record [name :string] :r)] :string (. p :name))
```

Named records keep their own representation; there is no implicit conversion
between a tagged record and an anonymous map.

### Processes, classes and imports

```scheme
(process counter 0 :inc (+ state 1) :get state)
(defn start [] (Pid :Process.counter.Message) (spawn counter 0))
(defn bump [p (Pid :Process.counter.Message)] :unit (! p :inc))
```

Typed sends return Unit (`nil`). Actors drop unexpected messages and expose
OTP `start_link`/GenServer interoperability. Selective `receive` retains
unmatched messages and decodes selected data. Supervision supports
one-for-one, one-for-all and rest-for-one restart ordering.

Classes, defaults, constrained instances and deriving become ordinary typed
Core dictionaries. Canonical imports carry quantified signatures, exact
digests and those dictionaries, including transitive imports. Legacy HM
interfaces are rejected. Digests identify content; they do not authenticate
an arbitrary interface/BEAM pair or establish module sealing.

Top-level values use `(def answer :int 42)` and remain recomputed accessors.
Expression programs such as `(+ 40 2)` infer their generated `main/0`.

## Architecture

| Path | Responsibility |
|---|---|
| `lib/vaisto/parser.ex`, `elab.ex`, `elab/` | Located syntax, bidirectional checking and local unification |
| `lib/vaisto/liquid/` | Core codec, independent Lint/evaluator, native lowering/runtime and differential harness |
| `lib/vaisto/refine/` | Verification conditions, predicates and Z3 protocol |
| `lib/vaisto/compilation.ex`, `build/`, `interface.ex` | Verified compilation, package builds and canonical interfaces |
| `lib/vaisto/repl.ex`, `lsp/` | Interactive execution and editor tooling on checked Core |
| `lib/vaisto/errors.ex`, `error_formatter.ex` | Structured, located diagnostics |
| `lib/vaisto/llm/` | Model providers and schema conversion |
| `std/`, `examples/` | Vaisto source with explicit contracts |
| `src/Vaisto/` | Historical, non-building self-hosting prototype; outside this migration ([ADR-002](docs/adr/002-compiler-implementation.md)) |

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
./vaistoc file.va                     # compile verified Core to native BEAM
./vaistoc file.va -o build/File.beam  # choose the output
./vaistoc --eval "(+ 1 2)"            # evaluate an expression
./vaistoc build src/ -o build/        # build a directory
./vaistoc init my-lib                 # start a package
./vaistoc repl                        # interactive REPL
./vaistoc lsp                         # language server on stdio
```

From Elixir, `Vaisto.Runner.run(source)` compiles and runs a program's `main`.
There is one native compiler; the old `--backend` option is removed.

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
each `mix test`, and fails on any evaluator/native disagreement.

`AGENTS.md` is the working contract for contributors, human or AI: the
conventions, the constraints and the expected workflow. `CLAUDE.md` is the
architecture reference. There is no CI or formatter configuration yet; CI is
planned on [SYKLI](#related-work).

## Status and known gaps

The language, standard library, builds, REPL, diagnostics, hover and inlay
types use checked native Core. Process messages, imported types and class
dictionaries are checked rather than typed `Any`.

Not implemented: receive timeouts, binary syntax, higher-rank signatures,
module sealing/brands, row evidence, refined decode targets and refinements
over records, sums or user measures. Imported refinements remain a compile
error. General deterministic replay, capabilities and the full task-contract
operator library remain design work. There is no CI configuration yet.

Match/receive clause guards remain unsupported; guarded function clauses
are supported. Top-level signatures are mandatory, locals monomorphic, and
Int/Float conversion explicit. These are language rules, not HM fallbacks.

The design and roadmap are in [liquid-types.md](docs/design/liquid-types.md)
and the [Liquid Vaisto RFC](docs/design/liquid-vaisto-rfc.md). Historical
defect reproductions there describe the retired compiler, not current APIs.

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
