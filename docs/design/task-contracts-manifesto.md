# What Outlives the Model

LangChain pipelines written in 2023 don't run in 2026. Not because the code rotted — because the abstractions did. The model API changed. The token-counting changed. The recommended retrieval pattern changed. The library that wrapped them rewrote its surface twice. The pipeline you wrote is a fossil of when its dependencies were stable, which was never.

This is not a library quality problem. It is the absence of a layer. What 2023 pipelines lacked was a **task contract** separable from its implementation — a description of the work the pipeline performs that does not name the model performing it.

Every domain that built durable software found one. Operating systems found POSIX. Networks found sockets. Databases found relational algebra. The domain layer didn't eliminate churn beneath it — operating systems still rewrite their schedulers, networks still change their TCP stacks, databases still rebuild their storage engines — but it kept the things written *on top* alive across that churn. A 1995 SQL query runs against 2026 PostgreSQL. A 2023 LangChain pipeline does not run against 2026 anything.

The missing layer for LLM operations is not another library. It is a typed, declarative algebra in which task contracts can be expressed, while the runtime decides which concrete model, retriever, or tool fulfills the contract. The model is the implementation detail. The contract is the durable artifact.

This essay sketches what that algebra looks like, why it requires both a small set of typed operators and a real runtime to land, and what it would take to be honest about the parts that are still unsolved.

## What SQL actually is

SQL's longevity is not an accident of marketing. Four properties did the work.

**Closure.** Every relational operator takes relations and returns relations. You compose freely without leaving the algebra, which is what makes optimizer rewrites possible — the optimizer can replace a subexpression with an equivalent one because the type and the laws hold.

**Decidable equivalence.** `A JOIN B = B JOIN A`. The optimizer can prove plans equivalent and choose between them on cost. Rewrites are safe.

**Schema typing.** Queries reference columns and tables that the catalog knows. Mismatches are caught before any data touches them.

**Cost-based binding.** The query says what to compute. The optimizer chooses how — which index, which join algorithm, which order — based on the catalog's statistics.

Three of these four transfer cleanly to LLM operations. The fourth — decidable equivalence — does not, and the manifesto for a task-contract algebra has to be honest about it from the first page.

LLM operators do close. A typed Context can flow through retrieve, generate, extract, verify, and out the other end as a typed Context. Schema typing applies — a contract can declare what each step expects and produces, with the catalog tracking the available models and tools. Cost-based binding applies — given declared budgets and a catalog of cost/quality profiles, the optimizer can pick concrete models for abstract requests. But you cannot prove that two prompts are semantically equivalent, or that swapping one model class for another preserves output distribution. The optimizer's rewrite space is fundamentally heuristic.

Losing decidable equivalence means rewrites are justified empirically, not algebraically — the optimizer learns that swapping a small model for a large one preserves quality within a task class, rather than proving it. This shifts the optimizer from Selinger's tradition toward the learned-optimizer tradition (Bao, Neo), and it means the catalog and the observation log are part of the compiler, not external to it. The algebra is *SQL-shaped* — declarative, typed, optimizer-bound — but its equivalences are empirical, learned from the catalog and observed traffic, not algebraic. This is a real weakening, and the design has to absorb it rather than hide it.

## A typed context, and the operations on it

The primitive that flows through the algebra is a typed Context, parameterized by its payload:

```
Ctx a = { payload : a
        , trace   : [Step]        ; ordered history; caching and replay
        , budget  : Budget        ; cost/latency/tokens, hierarchical
        , prov    : [Provenance]  ; source, artifact, and self-asserted
                                  ; confidence per step
        }
```

Four metadata fields, each earning its seat. `prov` is the confidence stack as well as the audit trail; the convenience view `conf` is "the most recently asserted confidence by any operator," with non-default views available as queries (`(conf-at :step retrieve)`, `(conf-min-across)`). This is a convention, not a semantics. The algebra does not impose a confidence combination rule because there isn't a right one — users who want one express it as a query.

Every operator has type `(Ctx a) -> (Ctx b)`. Closure is structural.

The core operator set is small — around ten — and the most consequential design call inside it is the split between the stochastic call and the structured projection of its output. `generate` makes the LLM call. `extract` projects the result into a typed structure. They have different cost, different failure modes, and — most importantly — different optimizer affordances: `generate` chooses among models with different cost/quality profiles; `extract` chooses among parsers (regex, JSON mode, grammar-constrained decoding). Tools like DSPy treat the LM call as a monolith, which is why their optimizers are restricted to prompt-tuning. With the split, the optimizer has real reordering room.

The other operators round out the algebra: `retrieve`, `rerank`, `verify`, `tool`, `branch`, `map`, `parallel`, `fold`, `escalate`. `rerank` earns its seat for one specific reason — it is the only retrieval-adjacent operator where the optimizer has a meaningful model choice (cross-encoder vs LLM-as-judge vs cheap heuristic). Summarize, chunk, and deduplicate are specializations of `generate`, `extract`, and `verify` respectively, and do not deserve primitive status. `escalate` returns a typed sum, `Escalated a`, rather than raising — preserving closure, which is the whole point. The runtime's exception mechanism is the implementation; the sum return is the semantics.

## Why the runtime earns its seat

The algebra closes. The contract is typed. But LLM operations live in a world where the typed operations themselves are stochastic and externally fragile — rate limits, timeouts, malformed extracts, hallucinated tool calls, model deprecations mid-run. An algebra without a runtime to handle these is a query language without a database engine.

The operationally honest answer is supervision. Each operator runs in a supervised process. Failures are typed (`:rate-limit`, `:timeout`, `:malformed-extract`, `:budget-exceeded`) and the supervisor's restart strategy is a first-class part of the contract:

```
(supervise :one_for_one
  (answer-legal-question
    :restart-on  [:rate-limit :timeout :malformed-extract]
    :max-restarts 3
    :backoff      exponential
    :escalate-to  (answer-legal-question
                    :budget {:cost 0.50 :min-conf 0.95})))
```

This is not an analogy. It is the actual OTP semantic, applied over typed failure tags emitted by typed operators. The runtime that already gives you process isolation, supervision trees, hot code reload, and transparent distribution is BEAM. The argument for building on it is not that BEAM is the obvious choice. It is that the properties an LLM-ops runtime requires — process isolation, typed failure, supervision, hot reload, transparent distribution — are exactly the properties BEAM has shipped for thirty years, and the alternative is reinventing them as conventions on top of Python or TypeScript. The current ecosystem has been doing that for three years, with predictable results.

Hot code reload deserves separate mention. Production LLM systems must swap models, prompts, and tools without downtime. A new prompt template loads into the running pipeline; in-flight requests finish on the old; new requests bind to the new. The runtime does this natively.

## What a compile error looks like

A prompt template declares an output schema:

```
(defprompt answer-with-citations
  :input  {:question :String :docs [Doc]}
  :output {:answer :String :evidence [DocId]})
```

A pipeline binds it:

```
(generate :prompt answer-with-citations :extract Answered)
```

The compiler unifies the prompt's declared output with the pipeline's expected `Answered` type. Today they match.

Someone "improves" the prompt to drop citations:

```
(defprompt answer-with-citations
  :input  {:question :String :docs [Doc]}
  :output {:answer :String})       ; ← evidence field removed
```

A contract somewhere else in the codebase imports this prompt. Three days later, in code review, the compiler catches the field drop before the PR merges. Today, this class of change fails at 3am when the `evidence` field is `None` and the downstream citation formatter throws:

```
error[E0418]: prompt output type mismatch
  ┌─ pipelines/legal_qa:14:13
14 │   (generate :prompt answer-with-citations :extract Answered)
   │             ^^^^^^^ prompt produces {:answer :String}
   │                     but pipeline expects Answered =
   │                       {:question :String :answer :String
   │                        :evidence [DocId]}
   │
   │   missing field: evidence : [DocId]
   │   = note: introduced by pipeline answer-legal-question at line 8
```

This is the moment that justifies the language. The cost is real — prompts must declare output schemas, which is authoring burden today's prompts don't carry. The benefit is that a class of failure that reaches production silently today becomes a compile error.

The type system catches pipeline-consumer mismatches against the declared schema, not mismatches between the schema and what the prompt actually elicits from the model — that second class is caught at runtime by the supervisor's `:malformed-extract` clause. Two layers, compile time for composition and runtime for stochastic reality, with structured-output modes (JSON mode, grammar-constrained decoding) thinning the runtime layer as they mature.

## Task contracts, not models

A contract written today does not specify which model to use. It specifies what good enough looks like. Any model that meets the declared quality, cost, and latency contract is a legal binding. As models improve, more models become legal bindings, and the optimizer picks the best available. The contract never needs to change because the contract was never about a model. It was about a task.

This is the SQL longevity argument made exact. SQL queries do not change across hardware generations because SQL queries were never about hardware — they were about data. Task contracts should not change across model generations because task contracts should never have been about models. They should be about the task: what is required, with what confidence, under what budget, with what failure semantics.

Everything else in the design — the closure of the algebra, the typed failures, the schema-checked prompts, the cost-based optimizer — exists in service of this. Without the contract, you have another framework. With the contract, the pipeline outlives the model that ran it the first time, and the model after that, and the framework that hosted it. That is the bet that justifies the cost of a new layer, and the only bet that does.

## What is not solved

Three things are not solved, and the manifesto's credibility depends on saying which.

**Calibration.** A confidence of 0.85 from one model on one task is not comparable to 0.85 from another model on another task without calibration. This is empirical and addressable — temperature scaling, eval-derived calibration curves stored in the catalog — but the runtime has to do the work, and today no runtime does. Confidence representation itself is currently a single Float; the right representation is probably a distribution over the payload, and the day distributions become first-class will be a breaking change. We will ship the concrete thing and let the abstraction emerge from experience, the way SQL-1 shipped without storage-engine abstraction.

**The optimizer.** Cost-based optimization over an algebra without decidable equivalence is closer to learned database optimization (Bao, Neo) than to Selinger. The starting point is rule-based — route short prompts to small models, prefer cached plans — with learned refinement on top, trained from observed pipeline outcomes. This is research. v1 needs to be defensible without it.

**The catalog.** Provider-published specs, community benchmarks, per-deployment observations: three sources, weighted by recency and relevance. The coordination problem of getting providers to populate the catalog honestly is real and not technical. It is a standards problem, and standards problems take a decade.

The bet is that the algebra is load-bearing enough to hold while these gaps get filled. Calibration is empirical and the catalog is the place to fix it. The optimizer is research, and v1 ships rule-based. The standards problem takes a decade, and that is exactly why the layer needs to be defined now rather than later.
