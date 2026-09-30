# Vaisto

**Finnish for "intuition"** — a typed substrate for structurally accountable
LLM systems.

Vaisto treats prompts, task contracts, and LLM pipelines as typed artifacts.
The goal is not to prove that a model will behave deterministically. It is to
make the pieces around that model accountable: the prompt text, the input
schema, the output schema, the contract, the pipeline, and the runtime evidence
produced by each execution.

> Prompts are prose. Vaisto makes them structurally accountable.

## The Problem

LLM systems drift in ways ordinary software tools do not catch.

A prompt changes but the downstream parser still expects the old shape. A model
is swapped and starts omitting fields. A retrieval step is "improved" and the
answerer no longer receives the evidence the product depends on. A pipeline
written against one provider API becomes fossilized when the ecosystem changes.

Today these failures usually appear late: in production, in eval dashboards, or
in a customer-visible malformed response. The code did not necessarily rot. The
abstractions did.

Durable software usually survives churn by separating **what must hold** from
**how it is executed**:

- SQL separates a query from the physical plan chosen by the database.
- POSIX separates a program's operating-system contract from kernel internals.
- Network APIs separate communication intent from the hardware below.

LLM systems need the same kind of separation.

## The Vaisto Bet

Vaisto is built around a double DSL:

1. **Contract DSL** — the durable obligation: input type, output type, budget,
   quality target, policy, and failure semantics.
2. **Pipeline DSL** — one executable strategy for satisfying that obligation:
   retrieve, generate, extract, verify, branch, escalate, and call tools.

The contract should outlive model churn. Pipelines, prompts, model bindings,
retrievers, and tools can evolve underneath it.

```text
Contract DSL  -> typed obligation
Pipeline DSL  -> typed implementation
Compiler      -> structural satisfaction
Optimizer     -> binding choice
Runtime       -> stochastic agreement
```

The SQL metaphor is useful but not exact. SQL optimizers can rely on many
algebraic equivalences. Vaisto cannot honestly say that two prompts or two
models are semantically equivalent. Vaisto replaces algebraic equivalence with:

- static types
- structural prompt accountability
- declared contracts
- typed failures
- runtime provenance
- eval and agreement history

In short:

> SQL made data work durable by separating declarative intent from physical
> execution. Vaisto aims to do the same for LLM work, but replaces algebraic
> equivalence with typed contracts plus empirical agreement.

## What This Looks Like

A prompt is not an anonymous string. It declares the input it may reference and
the output it promises to produce:

```scheme
(deftype DocId [value :String])
(deftype Question [text :String])
(deftype CitedAnswer [text :String evidence (List DocId)])

(defprompt answer-with-citations
  :input  Question
  :output CitedAnswer
  :template """
  Answer the question with citations.

  Question: {text}

  Return:
  - text
  - evidence
  """)
```

That gives the compiler and editor something real to check:

```text
template placeholders must exist on the input type
declared output fields can be checked against downstream consumers
prompt/schema drift can become a compiler diagnostic
```

Today Vaisto already checks the important structural case: a pipeline cannot
extract a type that the prompt's declared output does not satisfy.

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

If someone changes the prompt output type and drops `evidence`:

```scheme
(deftype CitedAnswer [text :String])
```

Vaisto refuses to compile the pipeline:

```text
error: prompt output type mismatch
  at line 6
      (generate :prompt answer-with-citations :extract Answer)
      ^ expected `Answer`, found `CitedAnswer`
  note: prompt `answer-with-citations` output CitedAnswer
        does not satisfy extract target Answer
  note: missing field: evidence : (List DocId)
```

A silent prompt/schema failure becomes a compile-time error.

## Contracts and Pipelines

The design direction is to make contracts first-class:

```scheme
(defcontract legal-qa
  :input Question
  :output CitedAnswer
  :quality {:min-conf 0.90}
  :budget {:cost 0.10 :latency 8s}
  :failure {
    :timeout retry
    :malformed-extract retry
    :low-confidence escalate
  })
```

A pipeline then claims to satisfy the contract:

```scheme
(pipeline legal-qa-fast
  :satisfies legal-qa
  (retrieve :from legal-corpus :k 8)
  (rerank :model auto :keep-top 3)
  (generate :prompt answer-with-citations :extract CitedAnswer)
  (verify :rule citation-check))
```

Another pipeline can satisfy the same contract with a different strategy:

```scheme
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

The contract is the durable artifact. Pipelines are implementations. Bindings
choose concrete models, tools, retrievers, and prompts. Runs produce evidence.

## Prompt Accountability

Vaisto should make prompt writing feel like writing typed code with prose inside
it.

The useful compiler and LSP loop is:

```text
autocomplete for placeholders from the input type
hover types inside prompt placeholders
error on unknown placeholders
warning on unused input fields
warning on output fields not mentioned by the prompt
warning when prompt text does not appear to account for contract requirements
```

This is intentionally modest. Vaisto should not claim that lint proves a prompt
is good, truthful, or semantically complete. Prompt lint means:

> The prompt is structurally aligned with the input type, output type, and
> contract it claims to serve.

That leaves the hard stochastic questions to runtime verification, evals,
calibration, and human escalation.

## Runtime Accountability

Each execution should produce an agreement record:

```text
contract: legal-qa
pipeline: legal-qa-fast
binding: selected model + retriever + verifier
input shape: short legal question
result:
  extraction: ok
  verification: passed
  cost: 0.04
  latency: 4.2s
  confidence: 0.91
  provenance: prompt version, model version, tool versions
```

This is the empirical counterpart to SQL's algebraic equivalence. Vaisto cannot
prove that a model binding is semantically identical to another. It can record
whether a binding satisfies a contract often enough, cheaply enough, and safely
enough for the deployment's policy.

## Current Status

Vaisto is early, but the core language and compiler are real.

Implemented today:

- S-expression parser with multi-line heredocs
- Hindley-Milner-style type checker
- Algebraic data types, records, pattern matching, and type classes
- `defprompt`, `pipeline`, and `generate`
- Prompt-output compatibility checks for downstream extraction
- Elixir backend for runnable task pipelines
- Core Erlang backend for the general language subset
- LSP server and VS Code extension
- Mock LLM provider for deterministic tests
- OpenAI provider via `:httpc` with structured outputs
- Multi-file builds; `.vsi` interface files are written but not yet used to
  type-check imports (see below)

Known gaps, each reproduced against the current code (details and
reproductions in the [Liquid Vaisto RFC](docs/design/liquid-vaisto-rfc.md),
§1.12):

- Cross-module calls are not type-checked yet: imported calls are typed `Any`.
- The type checker accepts some ill-typed programs, for example a `let`
  binding that leaks out of its scope, or a function declared `:int` that
  returns a float.
- The two backends disagree on a few constructs (`and`/`or` evaluation,
  guarded `defn`, record field access, `let` scoping), and row-polymorphic
  field access on a record crashes at runtime.
- Record and sum-type annotations on function parameters do not resolve at
  call sites.

Design direction:

- **Liquid Vaisto**: a small semantic core with refinement types, algebraic
  effects and evidence origin, a reference evaluator as the executable
  specification, and BEAM as its production implementation. See
  [docs/design/liquid-vaisto-rfc.md](docs/design/liquid-vaisto-rfc.md).
- first-class `defcontract`
- `pipeline :satisfies contract`
- prompt placeholder lint
- contract-aware prompt lint
- `Ctx` with payload, trace, budget, and provenance
- typed failures and supervision semantics
- `retrieve`, `rerank`, `extract`, `verify`, `tool`, `branch`, `map`,
  `parallel`, `fold`, and `escalate`
- model/tool catalog
- `:model auto` binding
- runtime agreement records
- refinement types for logical contract checks, discharged with Z3

For the full design argument, see
[docs/design/task-contracts-manifesto.md](docs/design/task-contracts-manifesto.md),
[docs/design/task-contracts-spec.md](docs/design/task-contracts-spec.md),
and [docs/design/liquid-vaisto-rfc.md](docs/design/liquid-vaisto-rfc.md).

## Quickstart

```bash
git clone https://github.com/yairfalse/vaisto.git
cd vaisto
mix deps.get
mix test
```

Build the CLI:

```bash
mix escript.build
./vaistoc --eval "(+ 1 2)"
./vaistoc build src/ -o build/
./vaistoc repl
```

Run a pipeline against the OpenAI provider:

```elixir
# In iex -S mix
iex> Application.put_env(:vaisto, :llm, Vaisto.LLM.OpenAI)
iex> System.put_env("OPENAI_API_KEY", "sk-...")
# then compile and call your pipeline as usual
```

## Editor Support

Vaisto has an LSP server and VS Code extension for real-time type feedback.

```bash
mix escript.build
cd editors/vscode
npm install
```

Open `editors/vscode` in VS Code and press **F5** to launch the Extension
Development Host. Open any `.va` file to get hover types, diagnostics, and
symbol navigation.

If `vaistoc` is not in your PATH, add this to VS Code settings:

```json
{
  "vaisto.serverPath": "/path/to/vaisto/vaistoc"
}
```

## Stack

| Layer | Tool | Purpose |
|-------|------|---------|
| Substrate | Vaisto | Typed IR for contracts, prompts, and pipelines |
| Runtime | BEAM | Process isolation, supervision, distribution |
| Observability | AHTI | Causality correlation across operations |
| Deployment | SYKLI | CI for systems built on contracts |

## Related Work

- **DSPy** — Python optimizer for prompt-tuning. Vaisto is focused on typed
  closure, structural accountability, and runtime supervision.
- **LangChain / LlamaIndex** — composition by framework convention. Vaisto
  moves composition checks into the language.
- **Z3** — SMT solver from Microsoft Research. The solver the Liquid Vaisto
  RFC proposes for refinement checks; not a substitute for runtime LLM
  evaluation.
- **Gleam** — typed BEAM language and close architectural cousin.
- **LFE** — Lisp on BEAM, untyped.

## Origin

Conceived January 2026, 3am Berlin, while waiting for family to fly home.
Started as "learn Elixir methodically," became a language design through
following intuition.

## License

MIT
