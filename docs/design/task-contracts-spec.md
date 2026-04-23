# Task Contracts — Companion Spec

Companion to `task-contracts-manifesto.md`. The manifesto is the prose argument; this file is the design record — the operator table, the Ctx primitive, the term discipline, and the design defenses that the manifesto compresses into beats.

Status: design sketch, not implementation. No code in the repo binds these names yet.

## Term discipline

The conceptual split that everything else depends on:

- **Contract** — the declared specification. The durable artifact. What outlives the model. Names what good enough looks like (quality, cost, latency, failure semantics). Does not name a model.
- **Pipeline** — the composed program. The runtime artifact. What the compiler type-checks and the supervisor runs. *Satisfies* (or *implements*, or *attempts*) a contract.
- **Operator** — a single step inside a pipeline. `generate`, `extract`, etc.

A pipeline satisfies a contract. A contract is binding on any pipeline that claims to fulfill it. The relationship is like SQL's logical plan vs physical plan, or interface vs implementation — except both sides are first-class in the language.

When in doubt: the thing the developer writes and the compiler type-checks is a *pipeline*. The thing that survives across model generations is a *contract*.

## The Ctx primitive

```
Ctx a = { payload : a
        , trace   : [Step]        ; ordered history; caching and replay
        , budget  : Budget        ; cost/latency/tokens, hierarchical
        , prov    : [Provenance]  ; source, artifact, and self-asserted
                                  ; confidence per step
        }
```

Four metadata fields, claimed minimal-and-complete. Each earns its seat:

| Field | Capability |
|-------|------------|
| `trace`  | caching, attribution, replay |
| `budget` | optimizer constraint, termination bound |
| `prov`   | audit trail, debugging, *confidence stack* |
| `payload` | the working data |

Removing any one kills a core capability. Adding a fifth tends to be a field of one of these (e.g. "tokens consumed" is in `budget`; "which prompt version" is in `prov`).

### Confidence representation

`conf` is **not a separate field**. It is a derived view over `prov`. Each `Provenance` entry carries `(source, artifact, conf-delta)`. The convenience view `conf` returns "the most recently asserted confidence by any operator." Other views are queries:

```
(conf-at :step retrieve)
(conf-min-across :steps)
(conf-product)
```

The default is a *convention*, not a *semantics*. The algebra does not impose a confidence combination rule because there isn't a right one. Users who need a specific aggregation rule express it as a query at the call site.

This is genuinely soft. A pipeline whose `branch (< conf 0.85)` behavior depends silently on which operator most recently touched conf has spooky-action-at-a-distance properties. The honest position: the canonical view is a convenience for the common case; rely on explicit queries when the answer matters.

### Budget is hierarchical

`Budget` is a tree, not a scalar. The pipeline declares a total. Operators declare their share. `map` distributes across elements. `parallel` splits across siblings. Depletion is monotone along any path.

### Future: distributions

`conf : Float` forces an arbitrary-rule camp for aggregation. The right representation is probably `Ctx (Distribution a)` with proper probabilistic propagation. We are *not* parameterizing now. Ship the concrete thing, isolate the commitment to the `conf` field, accept that the day distributions become first-class will be a breaking change. SQL-1 shipped without storage-engine abstraction; the same discipline applies.

## Operator set

Eleven operators. Every operator has type `(Ctx a) -> (Ctx b)` (closure).

| Op | Type | Role |
|----|------|------|
| `retrieve`  | `(Ctx Q) -> (Ctx (Q, [Doc]))`             | external data in |
| `rerank`    | `(Ctx (Q, [Doc])) -> (Ctx (Q, [Doc]))`    | reorder |
| `generate`  | `(Ctx a) -> (Ctx b)`                      | LLM call + structured extract |
| `extract`   | `(Ctx Text) -> (Ctx Struct)`              | deterministic projection |
| `verify`    | `(Ctx a) -> (Ctx a)`                      | deterministic conf update from external signal |
| `tool`      | `(Ctx Request) -> (Ctx Response)`         | external effect |
| `branch`    | `Pred -> Op -> Op -> Op`                  | conditional |
| `map`       | `Op -> (Ctx [a]) -> (Ctx [b])`            | per-element |
| `parallel`  | `[Op] -> (Ctx a) -> (Ctx [b])`            | fan-out same input |
| `fold`      | `Op -> (Ctx [a]) -> (Ctx b)`              | reduce |
| `escalate`  | `Reason -> (Ctx a) -> (Ctx (Escalated a))` | typed pipeline failure, returns sum |

### Design defenses

**Why `generate` and `extract` are split.** Different cost, different failure modes, different optimizer affordances. `generate` chooses among models with different cost/quality profiles. `extract` chooses among parsers (regex, JSON mode, grammar-constrained decoding). DSPy treats the LM call as a monolith, which is why its optimizer is restricted to prompt-tuning. The split gives the optimizer real reordering room: cache extracts, push them earlier, batch them across siblings of a `parallel`.

**Why no `judge`.** `judge` was in early sketches as "writes-to-conf, payload unchanged." But every operator can in principle write to any Ctx field; `judge` is just `generate` where the user discards payload-output and keeps only the conf-update. That makes it a usage pattern, not an operator. The seat is taken instead by `verify` — deterministic confidence updates from external signals (test-passed, regex-matched, tool-returned-exact-match) — which is categorically distinct from probabilistic LLM-based assessment.

**Why `parallel` is a primitive, not `(fan-out >> fold)` sugar.** The optimizer needs to *see* fan-out structure to make sensible decisions: batching across siblings, allocating shared latency budget, recognizing when 5 parallel small-model calls beat 1 large-model call. If `parallel` desugars before the optimizer sees it, that information is gone. Primitives the optimizer reasons about should be primitives. (Same reason SQL has `UNION ALL` as a primitive instead of two queries joined by the client.)

**Why `tool` is separate from `generate`.** Different failure modes (tool rate limits ≠ LLM rate limits), different cost shape (latency-dominated, not token-dominated), different security/permission model (auth, sandboxing, side effects). Conflating them costs the optimizer too much.

**Why `rerank` earns its seat.** Of all retrieval-adjacent operators, it's the only one where the optimizer has a meaningful model choice (cross-encoder vs LLM-as-judge vs cheap heuristic). Summarize is a `generate`. Chunk is an `extract`. Deduplicate is a `verify`. Rerank is the odd one out and earns primitive status for that reason.

**Why `escalate` returns `Escalated a` instead of raising.** Closure is the whole pitch. Breaking closure for the most operationally-important pattern inverts the manifesto's claim. `escalate : Reason -> (Ctx a) -> (Ctx (Escalated a))`, supervisor as pattern match on the sum, BEAM's exception mechanism as the *implementation*, not the *semantics*.

## Worked pipeline

Retrieve → rerank → answer → verify-then-escalate-if-low-confidence:

```scheme
(ns LegalQA)

(deftype Question [text :String])
(deftype Answered [question :String
                   answer   :String
                   evidence [DocId]])

(pipeline answer-legal-question
  :input  Question
  :output Answered
  :budget {:cost 0.05 :latency 5s :min-conf 0.85}

  (retrieve :from corpus.legal_2024 :k 10)
  (rerank   :model auto :keep-top 3)
  (generate :model auto
            :prompt prompts/answer-with-citations
            :extract Answered)
  (verify   :rule evals/legal-citation-grammar)
  (branch (< conf 0.85)
    (generate :model claude-opus
              :prompt prompts/answer-careful
              :extract Answered)
    pass))

(supervise :one_for_one
  (answer-legal-question
    :restart-on  [:rate-limit :timeout :malformed-extract]
    :max-restarts 3
    :backoff      exponential
    :escalate-to  (answer-legal-question
                    :budget {:cost 0.50 :min-conf 0.95})))
```

`:model auto` is the optimizer hook. At compile time, given the catalog (model → cost/latency/quality on benchmark slices) plus observed `(operator, payload-shape, outcome)` history from the runtime's correlation engine, the optimizer binds each `auto` to a concrete model. The chosen plan is a typed artifact — inspectable, cacheable, regression-testable.

## Open questions

The manifesto's "What is not solved" section names three. Recording them here in the form they need to take when implementation starts:

1. **Calibration.** Per-model, per-task calibration curves stored in the catalog. Updated from observed outcomes. Open: who derives the initial curves before observation history exists.

2. **Optimizer.** Rule-based first (route short prompts to small models, prefer cached plans, monotone budget consumption). Learned cost model on top, trained from observed pipeline outcomes — Bao/Neo lineage, not Selinger. Open: bootstrap problem before observation data exists.

3. **Catalog.** Three layers: provider-published specs, community benchmarks, per-deployment observations. Weighted by recency and relevance. Open: trust model, dispute resolution, refresh cadence.

4. **Sessions and memory.** Not addressed in v1. Probably: `Ctx` is per-invocation; a session is a process holding a sequence of Ctxs; pipelines can be opened against a session. Worth sketching before sessions become a v2 retrofit.

5. **Streaming outputs.** Token-by-token generation. Punted to v2. The Ctx parameterization does not foreclose `Ctx (Stream a)` later.

6. **Tools-as-`generate` vs `tool`-as-primitive.** Decided in favor of `tool` as separate operator (above). Worth re-litigating once the first real tool-heavy pipeline is built — the answer might change.
