# RFC: Liquid Vaisto

- **Status:** Draft 2 for discussion. Design only; nothing in this document is implemented.
- **Date:** 2026-09-30
- **Supersedes:** draft 1 of this file, written earlier the same day. Draft 1 layered refinements on top of today's compiler and treated BEAM behaviour as the definition of the primitives. Review of that draft produced the central idea of this one: **Vaisto has a semantics; BEAM implements it.** Appendix B lists every change.
- **Baseline:** `origin/main` at `0ec7a82`. Test baseline: 1268 passing, 1 excluded, 1 intermittent failure (D10).
- **Scope:** the fourteen items of the brief's "Immediate assignment", and the parts of the brief that constrain them.
- **Notation:** `{v:B | p}` is a refined type: base type `B`, predicate `p` over the value `v`. Snippets that use refinements use **provisional syntax** (§4.3); the surface syntax is an open question (Q1).

---

## 0. Summary and verdict

**Question.** Can Liquid Vaisto be made sound enough, small enough, and native enough to the existing compiler to become Vaisto's next type-system layer?

**Answer: yes, if Vaisto first gets a semantics of its own.** Today the meaning of a Vaisto program is whatever one of its two backends happens to produce, and the two disagree (D5, D11, D12). A refinement proof has to be a proof *about something*. This draft therefore puts a small mathematical core at the centre, **Liquid Core**, and demotes BEAM from "the definition" to "the production implementation".

### 0.1 The core: four primitives

| Primitive | What it is | Why it has to be in the core |
|---|---|---|
| **Values** | a deterministic ML-style language: literals, functions, application, `let`, constructors, records, `match`, primitive operations | everything else is about values; primitive operations get *Vaisto* semantics |
| **Refinements** | subsets of and relations between values, `{v:B \| p}`, checked statically and erased | only the core can say which values are admissible and what a function guarantees |
| **Effects** | a closed algebra of requests: a computation is `Pure a` or `Perform e k` | purity typing and record/replay are the same mechanism; the world enters only here |
| **Evidence origin** | every fact the logic may use is `Static`, `Imported` or `Observed` | "a claim is not a fact" must be enforced by the language, not by convention |

Everything else is **library**: authority, capabilities, budgets, workflow phases, transitions, jobs, credentials, leases, contracts, receipts, accepted states, pipelines. The test for any proposed feature is: **does it need new theory in Liquid Core?** The expected answer is no. A new external effect extends the effect algebra; a new logical operation extends the refinement logic; almost everything else is a type, a refined function and a law in the standard library (§4.8, §16).

### 0.2 Architecture

```text
            S-expression source
                     │
                     ▼
        HM elaboration (untrusted)  ──►  Core Lint (trusted, no inference)
                     │
                     ▼
     ┌──────────────── LIQUID CORE ────────────────┐
     │ values · refinements · effects · evidence   │
     └──────┬──────────────┬──────────────┬────────┘
            │              │              │
            ▼              ▼              ▼
   reference evaluator   VC → Z3        Lean model
   (executable spec)                    (later: metatheory)
            │
            │ differential conformance: evaluator(P, I, T) = BEAM(P, I, T)
            ▼
   erase refinements → lower effects → Erlang abstract format → OTP compiler → BEAM
                                                                                 │
                                                                   effect handlers, world,
                                                                   observation receipts
```

### 0.3 Why it fits Vaisto

1. **BEAM integers are unbounded**, so the core's `Int` is exactly SMT integer arithmetic. No overflow to model.
2. **Values are immutable and terms acyclic.** A fact about a variable stays true wherever the name is in scope, including in closures, and structural recursion always terminates.
3. **S-expressions make every semantic artifact a canonical tree.** Core terms, interfaces, contracts, predicates, effect traces, verification conditions and admission records can share one tree format with one canonical byte encoding. That gives digests, diffs, and agent-readable context without a separate serialization layer (§5.1).
4. **OTP itself points at the right backend.** OTP 29's `compile` documentation recommends the Erlang abstract format or Erlang source to language implementors. It warns that generated Core Erlang can reach cases the backend "has never encountered before", with "no guarantees that the final BEAM code will be safe" (§7.1). Vaisto's default backend generates Core Erlang today.

### 0.4 Conditions

1. **The core and its reference evaluator come before refinements.** They are the oracle. The 23 defects reproduced in §1.12 stop being a hand-maintained bug list and become the seed corpus of a differential harness that finds the next ones automatically (§6.3).
2. **`:any` becomes `Dyn`**, a real dynamic type with no implicit conversion in either direction. Values cross into the typed world only through checked `decode` (§4.6). A silent top-and-bottom type makes any refinement soundness claim false.
3. **Construction privacy (`opaque`) exists.** Workflow phases are named types, relations are refinements, and "only admission can construct `Accepted`" is constructor privacy. These are three different mechanisms, and the third is not refinement typing (§4.7, §16.3).
4. **One backend.** A single lowering to the abstract format replaces the two emitters, once the harness shows it agrees with the evaluator (§7).

### 0.5 Central decisions

| Decision | Choice | Evidence or reason |
|---|---|---|
| Semantic authority | Liquid Core, executed by the reference evaluator; a backend is correct if and only if it agrees | the backends disagree today (D5, D11, D12) |
| Type inference | HM stays, as an **untrusted elaborator**; its output is re-checked by **Core Lint**, which does no inference | HM accepts ill-typed programs (D15–D22) |
| Refinements | live in Core types; HM never sees them; erased Core-to-Core before lowering | emitters and `Unify` pattern-match type terms structurally (§1.9) |
| Effects | algebraic requests; handlers are the BEAM runtime or a trace replayer | replay determinism becomes a property of the semantics (§4.4) |
| `:any` | replaced by `Dyn` with checked `decode` | §4.6, §15 |
| Backend | one lowering to the Erlang abstract format | §7.1 |
| Artifacts | one canonical S-expression format; digests over canonical bytes, excluding spans | §5.1 |
| Solver | Z3 as an external process over SMT-LIB; `unknown` is never `proved` | §11 |
| `.beam` metadata | a `VSTY` chunk is a *typed artifact admission record*, not a certificate; only the Vaisto loader enforces it | verified: `code:load_binary` ignores it and `beam_lib:strip` removes it (§8.4) |

The long-term target is the statement from the design review: **any execution admitted by the Liquid Vaisto semantics satisfies the properties encoded in its verified core, assuming the explicitly enumerated trusted observation and backend boundaries.** §17.4 lists the component theorems.

### 0.6 Tensions with the brief, stated plainly

- **"Not a rewrite of Vaisto."** The surface language and HM stay, and programs keep their meaning except where today's meaning is a bug. The compiler's back half *is* replaced: two emitters become one lowering. This is staged: the new lowering runs beside the old emitters, and each old emitter is retired only when the harness shows parity (§7.3).
- **"Full effect system" is a first-implementation non-goal.** The *semantics* of effects is in the core from the start, because determinism needs it. The *effect type system* starts minimal and arrives in Phase 4, after refinements (Phase 2).
- **"Proof-carrying BEAM bytecode" is a non-goal.** `VSTY` carries hashes and a signature, not proofs, and nothing is proved at load time (§8.4).
- **The first refinement target** (`Nat`, `safe-div`, `clamp`, bounded index) still comes first among refinement work. It now comes after Phase 0 builds the oracle and Phase 1 removes `:any`, because a refinement checker running on today's HM output would verify programs that crash (D15, D16).

### 0.7 Where each item is answered

| Assignment item | Topic | Section |
|---|---|---|
| 1 | Map of the existing type-checking architecture | §1 |
| 2 | Minimum refinement calculus | §4.3 (inside Liquid Core, §4) |
| 3 | Representations: refined types, predicates, environments, VCs | §5 |
| 4 | Erasure and the backends | §7 |
| 5 | Refinements in module interfaces | §8 |
| 6 | The two inference engines | §9 |
| 7 | Solver abstraction | §11.1 |
| 8 | Trust boundary for solver results | §11.2, §11.3 |
| 9 | Branch refinement rules | §10 |
| 10 | Measure discipline | §12 |
| 11 | Error-message examples | §13 |
| 12 | Conformance suite | §14 |
| 13 | Migration risks (`:any`, externs, unknown calls, lambda fallback) | §15 |
| 14 | Path to the first authority/capability example | §16 |
| — | Contracts, operators, intent, Lean | §17 |
| — | Trust model | §18 |
| — | Mechanism ledger | §19 |
| — | Open questions | §20 |
| — | Roadmap | §21 |

| Brief section | Topic | Section |
|---|---|---|
| §1, §37 Phase 0 | architecture map, test baseline | §1 |
| §5, §18 | small logic, admission rule per primitive, predicate IR, SMT as a tool | §4.2, §5.2, §11 |
| §7, §20 | kinds of truth; where each fact came from | §4.5 |
| §8, §31 | trusted observations; the worker cannot mint evidence | §4.5 |
| §9, §10, §23 | state transitions, workflow phases, candidate vs accepted | §16.3 |
| §11, §30 | capabilities and authority | §16.2, §16.4 |
| §12 | linear/affine resources | §16.6, §17.5 |
| §13, §32, §33 | `Ctx`, contracts, operators | §17.1, §17.3 |
| §14, §15 | user intent; frozen specifications | §17.2 |
| §16 | vacuous specifications | §11.3 |
| §17 | modular checking | §8 |
| §19, §21 | liquid inference, Lean | §17.4, §17.5 |
| §27 | Bool vs proposition | §10.1, §16.4 |
| §34 | diagnostics | §13 |
| §35, §36 | trust model; who creates, establishes, checks | §18 |
| §38 | non-goals | §0.6, §2 |

---

## 1. The existing implementation (assignment item 1)

### 1.1 Pipeline and entry points

```text
source ─► Parser.parse/2 ─► AST (tuples, %Loc{} last)
        ─► TypeChecker.check/2 ─► {:ok, type, typed_ast}      (type is discarded by callers)
        ─► Compilation.emit/4 ─► Backend.Core | Backend.Elixir ─► BEAM
```

- `Compilation.compile/3` (`compilation.ex:58-78`) parses, type-checks (`typecheck/5`, 178-197), and calls `emit/4` (127-138). The backend receives **only** the typed AST, a module name and `[load: bool]`: no env, no result type. Every type fact a backend uses must be inside typed-AST nodes.
- There are **five independent compile paths**, and the new pipeline (§3) must replace all of them, or some path will silently bypass it:
  1. `Compilation.compile/3` (CLI, `Runner`).
  2. `Build.Compiler.compile/3`, which calls `Compilation.typecheck/2` and `Compilation.emit/4` separately (`build/compiler.ex:149,164`) and writes `.vsi` from the typed AST in between.
  3. The REPL: `Compilation.emit(typed_ast, name, :core)` (`repl.ex:274-275`).
  4. `Vaisto.compile_string/2`: `TypeChecker.check!` then `Backend.Core.compile` directly (`vaisto.ex:24-31`).
  5. LSP hover, which re-runs `TypeChecker.check` on the source (`lsp/hover.ex:274`).
- Default backends differ by path: CLI and `Build.Compiler` default to `:core`; `Runner` defaults to `:elixir` (`runner.ex:64,170`). The test suite therefore exercises mostly the Elixir backend for end-to-end behaviour.

### 1.2 Type representations

Declared in `@type vaisto_type` (`type_checker.ex:19-37`) and used more widely than declared:

| Term | Meaning | Notes relevant to refinements |
|---|---|---|
| `:int :float :string :bool :atom :unit :any` | primitives | `:int` is an unbounded BEAM integer. |
| `:num` | int-or-float | Produced by numeric fallback (`unify_types_s`, 3202-3216). No single logical semantics. |
| `{:atom, a}` | singleton atom | Two *different* singleton atoms unify (`unify.ex:136-139`), so `(if c :read :write)` has type `:atom`. HM gives no discrimination between atom literals. |
| `{:tvar, id}` / `{:rvar, id}` | type / row variables | `TcCtx` counters start at 10 000 (`tc_ctx.ex:13`). |
| `{:fn, [args], ret}` | function | No parameter names. Dependent refinements need names, which is one reason refinements live outside this term. |
| `{:list, t}` `{:tuple, [t]}` | collections | |
| `{:record, name, [{f, t}]}` | nominal product | Field order is the runtime tuple layout (`core_emitter.ex:1180-1189`). |
| `{:sum, name, [{ctor, [t]}]}` | nominal sum | Parameters are raw `{:tvar, 0..n}` without a `forall` (`type_checker.ex:881-895`). |
| `{:variant, sum, ctor, [t]}` | variant | Appears in `Core.apply_subst`; not in `@type`. |
| `{:row, [{f, t}], :closed \| {:rvar, id}}` | row type | Row polymorphism for field access. |
| `{:pid, proc, msgs}` / `{:process, state, msgs}` | typed PIDs | Unify compares only the process name (`unify.ex:142-149`); message lists are ignored. |
| `{:forall, ids, t}` / `{:forall, ids, {:constrained, cs, t}}` | schemes | |
| `:supervisor`, `:ns`, `:import`, `:extern`, `:defclass` | pseudo-types for declarations | Returned by `check_impl_s` for non-expression forms. |

### 1.3 Engine A: `Vaisto.TypeChecker`

- **Two-pass module checking** (`check_module`, 3391-3490): pass 1 collects signatures for `defn`, `defn_multi`, `deftype`, `defprompt`, `process`, `extern`, `defval`, `defclass`, `instance`, and deriving into a flat env map; pass 2 checks each form with `check_s/2` threading a `TcCtx`.
- **Between top-level forms the substitution is reset** (`check_module_forms`, 3704-3714); the counter and constrained tvars carry forward. Each form's type is fully resolved before the next form is checked. This is convenient for refinements: a function's HM type is final before its refinement is checked.
- **`TcCtx`** (`tc_ctx.ex`) fields: `env`, `subst`, `row_counter`, `counter`, `constraints`, `constrained_tvars`, `field_tvars`. There is no place for logical facts; in the new design, facts live in the refinement checker's own environment over Liquid Core (§5.4), not in `TcCtx`.
- **Function definitions** (`check_impl_s({:defn, ...})`, 935-1016): parameter annotations go through `parse_type_expr/1` (1451-1464), whose fallback returns the annotation unchanged. `:any` parameters are freshened into tvars (`freshen_any_params`, 3932) and generalized conservatively (`TcCtx.generalize_conservative`). The declared return type is compared to the body type with a **stateless** unify (`types_unifiable?`, 3193), not the threaded substitution.
- **Guards** (`(defn f [x :int :when (> x 0)] ...)`) exist: the parser splits at `:when` (`parser.ex:991`) and the checker requires a `Bool` guard (956-957). Guards are runtime-only today; `(pos -5)` compiles.
- **Calls** go through one choke point, `unify_call_poly_s` (3289-3340), which unifies each argument against the parameter type. `:any` on either side is accepted without binding (3308-3312). In the new design HM only elaborates; precondition obligations attach to calls in Liquid Core (§4.3), not here.
- **`if`** (660-668): the condition must be `Bool`; branches are checked in the same env; no path information is recorded.
- **`match`** (671-676, 2704-2753): each clause extends the env with pattern bindings and restores it afterwards. A clause knows nothing about the earlier clauses having failed.
- **`let`** (695-705, 2636-2701): bindings are generalized for let-polymorphism; no equation `x = e` is recorded.
- **Lambdas** (1033-1054) are delegated to Engine B (next section).
- **Instances and deriving** still use a legacy context-free path, `check_impl/2` (1238-1395), called with `ctx.env` only.
- **Typed AST has no locations.** `check_s` strips `%Loc{}` and calls `with_loc_ast(ast, loc)`, which is a no-op placeholder (280). Errors get a location from the nearest located node while checking, but the typed AST handed to backends carries none.

### 1.4 Engine B: `Vaisto.TypeSystem.Infer`

- Algorithm W with its own `TypeSystem.Context` and **its own primitives table** (`infer.ex:26-43`), which duplicates `TypeEnv.primitives` rather than reading it.
- Invoked only from `check_impl_s({:fn, params, body})` (1033) as `Infer.infer({:fn, params, body}, ctx.env)`. It receives the env but **not** the `TcCtx` substitution or counter, and its own substitution is not merged back.
- On failure the message is classified by `infer_should_fallback?/1` (1203-1207). Messages starting with "unknown expression", "unknown function", "undefined variable" or "cannot call non-function" trigger `fallback_lambda/3` (1210-1231), which re-checks the body with **every parameter typed `:any`** and returns `{:fn, [:any, ...], ret}`. Everything else is reported as a genuine error.
- Qualified calls inside lambda bodies: a known `{:fn, _, ret}` returns `ret` **without unifying the arguments**; an unknown key returns `:any` (`infer.ex:306-336`).

### 1.5 Unification

`Unify.unify/4` is symmetric structural equality with row polymorphism. Its only non-equality relations are:

- `:any` unifies with anything **without binding** (127-128). A tvar meeting `:any` is bound to `:any` earlier in the `cond` (31-42), so `:any` propagates through inference.
- `:num` accepts `:int` and `:float` (131-132).
- Singleton atoms unify with `:atom` and with each other (136-139).
- PIDs compare only process names (142-149).

There is no subtyping. Refinement subtyping (§4.3) must be a separate relation layered on HM results, never a change to `Unify`.

### 1.6 Typeclasses

Typeclasses use dictionary passing.

- **Declaration** (`collect_defclass_signature`, 3599-3636). Class parameters become tvars `0..n`. The class is stored in `:__classes__` as `{:class, name, tvar_ids, methods, defaults}`. **Each method is also registered in the env as a constrained scheme** `{:forall, ids, {:constrained, [{Class, tvar}], fn}}`, so a method name looks like an ordinary polymorphic function.
- **Instances** (`collect_instance_signature`, 3638-3658). The instance type is substituted into the method signatures, which are stored as `%{method => fn}` under `{Class, type}` in `:__instances__`. A constrained instance (`where`) stores `{:constrained, params, constraints, methods}` (3660-3681). Instance bodies are checked through the legacy, context-free `check_impl/2` (1238-1395), and method return types are never checked (§15.5).
- **Calls** (`check_class_method_call_s`, 2852-2869). The scheme is instantiated, the arguments unified, then `resolve_class_constraints` (1584-1646) picks the instance from the **first argument's** type (`resolve_constraint_type`, 1971-1974), normalized by `normalize_instance_type`. The result depends on what it finds:
  - a type variable: a plain `{:call, ...}`, with the constraint dropped;
  - `:builtin`: `{:class_call, class, method, key, args, ret}`, compiled inline;
  - a user instance: the same 6-tuple, dispatched through a dictionary;
  - a constrained instance: a 7-tuple carrying the resolved sub-dictionaries;
  - inside a constrained instance body: `{:constraint_call, idx, ...}`;
  - nothing: `no_instance_for_type`.
  With several constraints, only the last resolved one is kept (1635-1638).
- **Built-in `Num` and `Ord`** are verified after polymorphic calls (`verify_builtin_constraints`, 3248-3277). No other class is verified at that point.
- **Deriving** (3724-3812): `Eq` is synthesized as `(== x y)`, which is BEAM structural equality, for any type; `Show` only for enum-like sums.
- **Emission:** dictionary functions are named from the instance key (`core_emitter.ex:1695-1697`); `Eq`/`Show` on primitive keys are inlined.

**What this means for refinements.** Class methods get no refinements in Phase 2. A `class_call` result is opaque, with one exception: `eq`/`neq` on a built-in `Eq` instance or a **derived** `Eq` instance for a logic sort. Those are structural equality by construction, so they can be given the specification `v ⇔ x = y`. A hand-written `Eq` instance can define anything and gets no specification.

### 1.7 Row polymorphism

- **Field access** `(. r :f)` (388-444) depends on the type of `r`:
  - `{:record, _, fields}`: the field's type, or an error if the field is missing;
  - `{:row, fields, tail}`: the field's type if present; otherwise, if the tail is open, a memoized field tvar (`TcCtx.field_tvar`, keyed by `{row_id, field}`) plus a row constraint `{:row, [{f, t} | fields], {:rvar, row_id + 1}}`;
  - `{:tvar, id}`: a memoized field tvar plus the row constraint `{:row, [{f, t}], {:rvar, id}}`;
  - `:any`: a field tvar memoized on the constant base `200`, so every access to field `f` on an `:any` value in the same form shares one tvar;
  - anything else: an error.
- **The row constraint is recorded but not enforced.** It is stored in the 5-tuple typed node `{:field_access, rec, f, t, row}` and never unified with the type of `r` (D21). Row polymorphism is inferred, but a call like `(get-x 5)` is not rejected.
- **`Unify` itself implements rows fully** (`unify_rows`, `unify_row_with_record`, `unify.ex:295-422`), including fresh tails for two open rows. That code runs when row types meet each other or records inside other types. Field access never feeds it.
- **Emission:** Core uses the record's field order and `element/2` when the static type is a record, and `maps:get` otherwise (`core_emitter.ex:1180-1205`). The Elixir emitter has no field-access clause at all (D12).
- **Rows and nominal types pull in opposite directions.** A parameter without an annotation is structural: any record with the field is accepted, and because of D21 so is anything else (§16.3, probe a).

**What this means for refinements.** Field projection appears in predicates only on nominal records, as a datatype selector (§4.3, Phase 2). A field access whose record expression has a row, tvar or `:any` type is opaque. Workflow-phase typing (§16.3) needs nominal parameters, because rows would erase the distinction between phases.

### 1.8 Module interfaces (`.vsi`)

`Interface.save/4` writes `:erlang.term_to_binary(%{module, version: 2, exports, types, classes, instances})` (`interface.ex:33-51`). In practice (details in D9):

- The only caller passes an exports map built from 5-tuple `defn`/`defn_multi` nodes (`build/compiler.ex:197-208`), so `classes` and `instances` are always empty and guarded `defn`s are never exported.
- Exports are the **monotype** from the typed `defn` node, with free, unquantified tvars, not the generalized scheme.
- `Interface.build_env/2` keys exports as `:"Elixir.A:base"`; the checker looks up `:"A:base"` (`type_checker.ex:709`). The lookup always misses.
- Reading uses `:erlang.binary_to_term/1` without `[:safe]` (`interface.ex:67`). Validation checks only the key set and `version in [1, 2]`. There is no source hash, compiler version or staleness check.
- `std/` contains no `.vsi` files (`*.vsi` is gitignored); CLAUDE.md's statement that std ships interfaces is out of date.
- `Vaisto.Interface` has no direct tests.

### 1.9 How the backends consume the typed AST

Both emitters read **type terms structurally** in a handful of places. Every place a type term changes generated code:

| Type shape | Where | Effect on code |
|---|---|---|
| `{:fn, args, _}` | `core_emitter.ex:555-556` | arity when wrapping a module function as a fun |
| `{:record, name, fields}` in a call's type slot | Core 1162, 1180; Elixir 510, 1065-1127 | tagged-tuple construction; **field order = tuple layout**; runtime map↔record conversion for LLM output |
| `{:row, _, _}` | Core 1191 | field access via `maps:get` |
| `{:sum, _, variants}` in a call's type slot | Core 1213, 1242; Elixir 414 | constructor → tagged tuple instead of a function call |
| bare primitive atoms as `concrete_type` | Core 1566-1655; Elixir 547-575, 1353-1389 | inline Eq/Show vs dictionary call |
| `extract_type` of `generate` | Elixir 1009, 1015 | **`Macro.escape`d into the generated code** and passed to `Vaisto.LLM.call/4`; `llm/openai/schema.ex:31-33` raises on unknown shapes |

Several mismatches fall through **silently**: a constructor call whose type slot is not exactly `{:record, same_name, _}` or `{:sum, _, variants}` is emitted as a generic call (`undef` at runtime); field access on anything but `{:record, ...}`/`{:row, ...}` becomes `maps:get` (`badmap` at runtime). Type-system helpers have the same shape: `Core.apply_subst/2` (109) and `Core.free_vars/1` (164) end in catch-alls that return the term unchanged / an empty set.

**Consequence for this RFC:** any new constructor inside HM type terms (for example `{:refined, v, base, pred}`) would silently change code generation, hide type variables from generalization, and leak into the LLM runtime schema. This is why HM type terms never carry refinements (§9). Refinements live in Liquid Core types, which only Core Lint, the refinement checker, erasure and the evaluator see; erasure removes them before lowering (§7).

### 1.10 Task-contract forms

`defprompt` is metadata; `pipeline` and `generate` are compiled only by `Vaisto.Emitter`. CoreEmitter has no clauses for them; inside a module they fall into the `main/0` body and raise `FunctionClauseError`, which `CoreEmitter.compile/3` rescues into a generic "compilation error" (`core_emitter.ex:57-58`). The design documents describe `defcontract`, `Ctx`, and eleven operators; **none of `requires`, `ensures`, invariants, receipts, predicates, or Lean appear anywhere in the design documents.** `README.md:272` and `README.md:342-343` mention "optional constraint solving" and Z3 as a possible future backend.

### 1.11 Escape hatches (`:any` and friends)

`:any` behaves as both top and bottom. `Unify` accepts it on either side (and deeply: `(List Any)` unifies with `(List Int)`), and `join_types(:any, t) = t` (3901-3902) turns it back into a precise type. It is produced at roughly a hundred code sites across sixteen categories. The largest are `defn_multi` (every pattern variable is bound to `:any`, so a recursive `len` gets type `(List Any) -> Any`), dynamic list operations, pattern fallbacks, and missing annotations. The full inventory with counts is in §15.5.

Explicit `:any` parameters are not dynamic: `freshen_any_params` (3932-3940) turns a top-level `:any` parameter into a fresh type variable, so `[x :any]` means the same as `[x]`. Nested `:any`, as in `(List :any)`, is not freshened.

### 1.12 Verified defects that bear on refinement soundness

Each item was reproduced against `0ec7a82` (commands in Appendix A) unless marked *(read)*.

| ID | Defect | Evidence | Why it matters here |
|---|---|---|---|
| D1 | Record/sum annotations on `defn` params never resolve: `(defn idp [p :Point] :Point p)` then `(idp (Point 1 2))` → `expected Point, found Point`. | `collect_defn_signature` (3494-3499) and `check_impl_s({:defn...})` use `parse_type_expr`, which leaves `:Point` as an atom. `resolve_named_type` (1472-1485) exists but only prompts use it. The four tests that annotate params with user types (`adt_test.exs:307`, `language_features_batch4_test.exs:29,41,142`) never call the function. | Every capability/job example in the brief annotates params with user types. |
| D2 | Parameterized annotations such as `(Result :int :string)` reach the unifier as raw parser AST (`{:call, :Result, [...], %Loc{}}`). | `parse_type_expr` fallback (1464). | Same as D1. |
| D3 | `[1 "a"]` is accepted where `(List :int)` is declared. | `join_types` falls back to `:any` (3916); `{:list, :any}` unifies with `{:list, :int}`. | A refinement over `(List :int)` would reason about strings. |
| D4 | `/` is `Int -> Int -> Int` in `TypeEnv` (`type_env.ex:29`) and `Infer` (`infer.ex:30`), `Float` in the checker's own clause (`type_checker.ex:764-770`). `(defn main [] :int (let [f (fn [a b] (/ a b))] (f 7 2)))` type-checks and returns `3.5` on both backends. | Appendix A. | The two engines disagree about base types; refinements on Int would be proved about a float. |
| D5 | `and`/`or` evaluation differs by backend. Core emits a call to strict `erlang:and/2` (`core_emitter.ex:786`); Elixir emits short-circuit `and` (`emitter.ex:202`). With `y = 0`, `(and (!= y 0) (> (div x y) 1))` raises `ArithmeticError` on Core and returns `false` on Elixir. | Appendix A. | Whether the right operand may assume the left is exactly a refinement question. |
| D6 | Any call form with an atom head is a type annotation (`parser.ex:1025`). In return position, `(defn f [x] (println x) x)` parses `(println x)` as the **return type** and drops it from the body (`parser.ex:957-965`). | Appendix A. | Refinement syntax must not deepen an already ambiguous annotation grammar. |
| D7 | `(deftype opaque Password [hash :string])` is accepted as a record named `opaque`. | Appendix A. DESIGN.md:162-175 promises `opaque`. | The trusted-observation design depends on opaque types. |
| D8 | An unknown qualified call is typed `:any` with no diagnostic (`type_checker.ex:711-714`); an extern call whose arguments fail to unify silently gets the declared return type (720-728). DESIGN.md:235 says "Calling without declaring = compiler error." | Appendix A. | Unchecked values flowing into refined positions. |
| D9 | Cross-module typing is inert: `.vsi` keys never match lookups; the build env merge replaces the built-in `__classes__`/`__instances__` with empty maps; exports are monotypes with free tvars (unified without instantiation, so the first importer use fixes them); guarded `defn`s are not exported; `binary_to_term` without `[:safe]`; no staleness detection. | Agent audit of `interface.ex`, `build/compiler.ex`, `build.ex`; key mismatch and class wipe reproduced. | Refinement summaries need a working interface layer to travel in. |
| D10 | `DependencyResolver` stores parsed imports (`:A`) but keys the graph by `:"Elixir.A"`, so no in-degree is ever non-zero; the "topological" order is `Map.keys` order, which for atom keys on OTP 26+ is atom-creation order. Cycles are never reported. This is the cause of the intermittent failure at `test/build/integration_test.exs:124`: whether it passes depends on which earlier test first created `:"Elixir.B"` or `:"Elixir.C"`. | Appendix A: atoms created as `Qz3, Qy2, Qx1` with dependencies `Qx1 ← Qy2 ← Qz3` sort to `[Qz3, Qy2, Qx1]`, dependents first. | Modular checking assumes dependencies are checked first. |
| D11 | Guarded `defn` (the checker's 6-tuple, `type_checker.ex:968`) is not in CoreEmitter's top-level form list (`core_emitter.ex:196,234`); on `:core` it fails with "compilation error". On `:elixir` it works. | Appendix A. | Guards are the natural runtime counterpart of static preconditions. |
| D12 | Record field access raises `FunctionClauseError` on the `:elixir` backend (no `{:field_access, ...}` clause in `emitter.ex`); it works on `:core`. | Appendix A. | Field projection is the first thing capability predicates use. |
| D13 | Typed AST carries no source locations (`type_checker.ex:280`). | *(read)* | Refinement diagnostics must point at call sites inside bodies. |
| D14 | `apply_subst_to_ast` has no clause for the guarded `defn` 6-tuple (it has only the 5-tuple at 1902); guarded definitions pass through unsubstituted. | *(read, agent)* | Refinement pass would see unresolved tvars in guarded functions. |

The `:any` audit (§15.5) found further holes that let ill-typed programs through **without any `:any` in the source**. The ones below were executed against `0ec7a82`:

| ID | Defect | Reproduction | Why it matters here |
|---|---|---|---|
| D15 | Environments leak out of `let` (and `try`, `receive`, fallback lambdas) while the emitters scope lexically. | `(defn f [x :string] :int (do (let [x 1] x) (+ x 1)))` is accepted as `String -> Int`; `(f "hi")` raises `ArithmeticError`. | HM's type for a variable can disagree with the binding the code actually uses. |
| D16 | The declared return type is compared with a stateless unify (`types_unifiable?`, 3193), before the body's substitution is applied. | `(defn f [x] :int (do (++ x "a") x))` is accepted as `String -> Int`. | A refined caller would reason about an `Int` that is a string. |
| D17 | A numeric op on a tvar returns the other operand's type without unifying (`check_numeric_op`, 3098-3099). | `(defn f [x] (+ x "s"))` is accepted; `(defn g [x] (+ x 1))` makes `(g 1.5)` an `Int`. | Int facts about floats. |
| D18 | Concrete field types of sum constructors are replaced by fresh tvars (881-895, 3539-3566). | With `(deftype R (Ok :int) (Err :string))`, `(Ok "s")` is accepted. | Selector facts about constructor fields would be wrong (Phase 2). |
| D19 | Higher-order builtins never relate the function's parameter to the list element (2878-3034). | `(map (fn [x] (++ x "a")) [1 2])` is accepted as `(List String)`. | List element sorts would be wrong. |
| D20 | Lambda parameters cannot be annotated; the annotation becomes a second parameter. | `(fn [x :int] x)` has type `{:fn, [t0, t1], t0}`. | Refined lambdas need annotated parameters. |
| D21 | Row constraints from field access are recorded but never unified (408-434). | `(defn get-x [r] (. r :x))` then `(get-x 5)` is accepted. | Field projection on non-records. |
| D22 | Nominal types are not enforced at function boundaries. An unannotated parameter is a type variable: matching it against constructor patterns of type `A` accepts a value of type `B`, and exhaustiveness is skipped because the scrutinee type is unknown. With a known scrutinee type both are checked. | With `(deftype A (X :int))` and `(deftype B (Y :int))`: `(defn f [v] :int (match v [(X n) n]))` then `(f (Y 1))` is accepted, while `(match (Y 1) [(X n) n])` is rejected as non-exhaustive. | Workflow phases cannot be enforced (§16.3). |
| D23 | `(deftype Job p [id :int])` is silently parsed as a legacy record with fields `p` and a bracket; record types cannot take type parameters. | The parser returns `{:deftype, :Job, {:product, [{:p, :any}, {{:bracket, ...}, :any}]}}`. | Phantom phase parameters are unavailable (§16.3); another silent misparse like D7. |

Further findings from the audit, read from source but not executed, are listed in §15.5.

None of these require refinements to fix, and most are worth fixing on their own. They show that "HM's base types are sound" cannot simply be assumed. The design therefore does two things. §21 turns the defects into Phase 0 work items with acceptance tests. And nothing downstream trusts HM: its output is elaborated into Liquid Core and re-checked by Core Lint, a small checker that does no inference (§9, M1). A program that HM accepts and Core Lint rejects is an elaborator bug, found automatically.

### 1.13 What the design documents promise versus what exists

- `opaque` types: promised (DESIGN.md:162-175), not implemented, silently misparsed (D7).
- Explicit interop: "Calling without declaring = compiler error" (DESIGN.md:235); in fact unknown qualified calls are `:any` (D8).
- `defcontract`, `:satisfies`, `Ctx`, operators other than `generate`: design only (SPEC:5, README.md:259-272). What `:satisfies` would check is never specified.
- DESIGN.md:313-314 lists "A research project" under "What Vaisto Is Not", qualifying it with "using proven techniques". Refinement types are an established technique (Liquid Types, PLDI 2008; Liquid Haskell), but they are a larger step than anything in Vaisto today. This RFC keeps the core to four primitives and puts everything else in libraries, so that the "not a research project" line can stay true (§4.8).
- The brief refers to a "mathematician's guide" to formal specification. It is not in this repository or elsewhere under the author's project directories; this RFC relies only on the brief's paraphrase of it.

---

## 2. Principles

The RFC applies these as hard constraints; each mechanism in §19 is checked against them.

1. **Vaisto has a semantics; BEAM implements it.** Liquid Core and its reference evaluator define what a program means. When BEAM disagrees with the evaluator, the backend is wrong.
2. **Four primitives.** Values, refinements, effects, evidence origin. Anything else must justify why it cannot be a library over them (§4.8).
3. **The worker does not define success and does not supply its own evidence.** Only the refinement checker (static) and privileged observers (runtime) establish facts. A claim is not a fact.
4. **Erasure preserves meaning.** For every verified pure term, evaluating it and evaluating its erasure give the same value (§7.2).
5. **No silent dynamic typing.** `Dyn` converts to nothing implicitly; `decode` is the only way in (§4.6).
6. **No general macros; no `assume`.** The only trusted axioms live in privileged observer modules, are declared in the build configuration, and are recorded in provenance (§4.5).
7. **`unknown` is never `proved`.** Solver timeouts, crashes and `unknown` answers are errors.
8. **No solver syntax in Vaisto.** Users see Vaisto predicates in errors; SMT-LIB is an internal lowering.
9. **One canonical tree format** for every semantic artifact, with digests over canonical bytes that exclude metadata such as source spans (§5.1).
10. **Small trusted core.** Trusted components are listed in §18 and §19, each with a reason. The elaborator is deliberately *not* trusted.
11. **Diagnostics are part of the design.** Every mechanism names its user-facing message.
12. **Language rule.** The implementation is Elixir. The solver is an external executable, not a library binding. Nothing in Python, Go or Node.

---

## 3. Architecture: one semantics, several consumers

### 3.1 The compiler as a chain of typed passes

Each pass has a type, and each boundary carries an invariant:

```text
elaborate     : Surface         -> Result Error Core          HM inference; untrusted
lint          : Core            -> Result Error Core          Core Lint; trusted; checks, never infers
verify        : Core            -> Result Error VerifiedCore  refinement checker + solver
erase         : VerifiedCore    -> Core⁻                      drops refinements; total, syntactic
lower_effects : Core⁻           -> LoweredCore                effect requests become direct handler calls
emit          : LoweredCore     -> AbstractErlang             the only backend
compile       : AbstractErlang  -> BEAM                       OTP compile:forms/2
```

| Boundary | Invariant | How it is established |
|---|---|---|
| `lint` accepts | the Core term is well-typed, with no inference involved | Core Lint, a small checker (§9.2) |
| `verify` accepts | every refinement obligation is valid | refinement checker plus Z3 `unsat` (§11) |
| `erase` | `eval(e) = eval(erase(e))` for every verified pure term `e` | by construction; later a Lean theorem (§17.4) |
| `lower_effects` | the sequence of effect requests, and how their results are used, is unchanged | differential test now; theorem later |
| `emit` + `compile` | BEAM agrees with the reference evaluator | differential harness (§6.3); never assumed |

The **reference evaluator** (§6) runs on Core, before or after erasure; the erasure invariant says the answer is the same. The LSP and the REPL stop after `lint` or `verify`; hover types come from Core types, not from HM's internal terms.

### 3.2 One pipeline instead of five compile paths

§1.1 lists five independent paths from source to BEAM: `Compilation.compile/3`, `Build.Compiler`, the REPL, `Vaisto.compile_string/2`, and LSP hover. All of them are replaced by calls into a single pipeline module. A path that skips `verify` for a module containing refinements cannot reach `emit`. `erase` refuses Core that still carries refinements unless it is marked verified, so a skipped pass fails loudly.

### 3.3 Staging with the existing compiler

The front end stays. Phase 0 adds Liquid Core, the evaluator, Core Lint and an **adapter** from today's typed AST to Core for the pure fragment. The harness can then run every existing test program through the evaluator and both current backends without changing the parser or HM (§21). Phase 1 adds the abstract-format lowering beside the old emitters and retires each emitter only when the harness reports parity on the whole corpus (§7.3).

---

## 4. Liquid Core (assignment item 2)

Liquid Core is the language. The surface language elaborates into it; the evaluator runs it; the refinement checker reasons about it; the backend lowers it. It has four primitives: values, refinements, effects, evidence origin.

### 4.1 Values

Core is an explicitly typed, call-by-value ML: every binder carries its type, and polymorphism is explicit (definitions carry `forall`, uses carry their instantiation). Written as canonical S-expressions (§5.1):

```text
term ::= x | literal
       | (fn ((x τ) ...) term)             | (app term term ...)
       | (let ((x τ term)) term)           | (inst f τ ...)
       | (ctor T C term ...)               | (record R (f term) ...)   | (field term R f)
       | (match term (pattern term) ...)   | (prim op term ...)
       | (perform effect term ...)         ; §4.4
       | (decode τ term)                   ; §4.6

type ::= Int | Bool | Atom | Float | String | Dyn
       | (List τ) | (Tuple τ ...) | (T τ ...)            ; nominal records and sums
       | (-> (τ ...) ε τ)                                ; ε is an effect set, §4.4
       | (forall (a ...) τ)
       | (refine x τ p)                                  ; §4.3; erased before lowering
```

- **Typeclasses are elaborated away.** A class becomes a record of functions and an instance becomes a value of that record; a constrained function takes the dictionary as an argument. Core, the evaluator and the logic never see classes. This replaces today's two dispatch mechanisms (`class_call` in both emitters, §1.6) with ordinary Core.
- **Evaluation order is defined by Core, not by Erlang.** Core does not inherit whatever order the Erlang compiler uses for call arguments. Core evaluates left to right, and the elaborator names every effectful sub-expression with a `let` (A-normal form for effects), so the order of effects never depends on the backend.
- **Crashes are part of the semantics.** Pure evaluation returns a value or a crash with a reason: a failed match, a division by zero, `head` of an empty list. Crashes are deterministic, and the harness compares crash reasons as well as values. Refinements exist to rule crashes out statically; BEAM's "let it crash" is a property of the effect runtime, not of pure evaluation.
- **`Float` and `String` are values but not logic sorts** (§4.3).

### 4.2 Primitive semantics (normative)

Every primitive has a Vaisto definition. **The reference evaluator implements it; the lowering must reproduce it on BEAM; a differential test per primitive checks that it does.** Where Erlang behaves differently, the lowering must compensate or the backend is wrong. This reverses draft 1, which defined the table as "Erlang semantics".

| Primitive | Crashes when | Result `v` |
|---|---|---|
| `+ - *` on `Int` | never | `x ± y`, `x * y`, exact (unbounded) |
| unary `-` | never | `-x` |
| `div x y` | `y = 0` | **truncation toward zero**: `ediv(x, y)` |
| `rem x y` | `y = 0` | `x - y * ediv(x, y)` (sign of the dividend) |
| `/` on numbers | `y = 0` | `Float` quotient; not in the logic |
| `< > <= >=` on `Int` | never | `v ⇔ x < y` etc. |
| `== !=` on two values of the same type | never | structural equality |
| `not` | never | `v ⇔ ¬x` |
| `and`, `or` | never | **short-circuit**: `b` is evaluated only when `a` does not decide the result |
| `length xs` | never | `v = len(xs)` |
| `empty? xs` | never | `v ⇔ len(xs) = 0` |
| `head xs` | `len(xs) = 0` | first element |
| `tail xs` | `len(xs) = 0` | `len(v) = len(xs) - 1` |
| `cons x xs`, `[h \| t]` | never | `len(v) = len(xs) + 1` |
| list literal of `n` elements | never | `len(v) = n` |

Truncating `div` happens to match Erlang, so the lowering is direct. Short-circuit `and`/`or` does **not** match what the Core Erlang backend emits today (a call to strict `erlang:and/2`, D5); under this definition that backend is wrong and the lowering must emit `andalso`/`orelse`. SMT-LIB's `div` and `mod` are Euclidean and differ for negative operands; Appendix A shows Z3 returning `(div -7 2) = -4` against Vaisto's `-3`, and the `ite`-based encoding that matches Vaisto on all sign combinations.

**Admission record.** The brief requires six things for every primitive the logic admits: syntax, IR, precise semantics, a lowering to the solver, tests, and a diagnostic. The table above gives syntax and semantics; this one gives the rest.

| Primitive | IR (§5.2) | SMT-LIB lowering | Diagnostic when its requirement fails | Test |
|---|---|---|---|---|
| `+ - *` | `(arith add\|sub\|mul a b)` | `(+ a b)`, `(- a b)`, `(* a b)` | — | evaluator vs BEAM vs SMT on random integers, including values above 2^64 |
| unary `-` | `(arith neg a)` | `(- a)` | — | same |
| `div` | `(arith ediv a b)` | `ediv` (Appendix A) | "`div` requires a non-zero divisor" | C10; every sign combination; zero divisors crash with the same reason on the evaluator and on BEAM |
| `rem` | `(arith erem a b)` | `erem` | "`rem` requires a non-zero divisor" | same |
| `< <= > >=` | `(lt a b)`, `(le a b)`; `>` and `>=` swap operands | `(< a b)`, `(<= a b)` | — | boundary values |
| `== !=` | `(eq a b)`, `(not (eq a b))` | `(= a b)` | — | one test per logic sort |
| `not and or` | `(not p)`, `(and p q)`, `(or p q)` | `(not p)`, `(and p q)`, `(or p q)` | — | C9; short-circuit parity test on BEAM |
| `length` | `(measure len xs)` | `(len xs)`, `len` uninterpreted `List -> Int` | — | C6 |
| `empty?` | `(eq (measure len xs) (int 0))` | `(= (len xs) 0)` | — | guarded `head` test |
| `head` | no result term | — | "`head` requires a non-empty list" | C6 |
| `tail` | fresh `t` with `len(t) = len(xs) - 1` | `(= (len t) (- (len xs) 1))` | "`tail` requires a non-empty list" | C6 |
| `cons`, literals | fresh `v` with its length fact | `(= (len v) (+ (len xs) 1))`; `(= (len v) n)` | — | literal-length test |
| `len ≥ 0` | instantiated once per list-sorted term | `(>= (len t) 0)` | — | a VC provable only with the axiom |

### 4.3 Refinements

**Sorts.** The logic reasons about `Int` (exact), `Bool`, `List` (an uninterpreted sort with the built-in measure `len`), and, from the second step of Phase 2, nominal records, sums and enums (SMT datatypes with selectors and testers) and atoms (distinct constants). `Float`, `String`, `Dyn`, functions, pids and maps are **not** logic sorts: values of those types can be passed around in refined code, but no fact about them exists and no obligation mentioning them can be discharged. Finite authority domains are best modelled as nullary-constructor sums (`(deftype Scope (RepoRead) (RepoWrite))`), because HM unifies distinct singleton atoms (`unify.ex:136-139`, §1.5).

**Refined types.** `(refine x τ p)` is the subset of `τ` whose values satisfy `p`. A refined function type names its parameters: `x1:{v:B1 | p1} → … → {v:B | q}`, where `pi` may mention earlier parameters and `q` may mention all of them. **The first step of Phase 2 allows refinements only at the top level of single-clause function parameters and results**, with or without a `:when` guard. Not yet: refinements nested inside type constructors, on record fields (data invariants), on multi-clause functions (their pattern variables are `:any` today, §15.5), on class methods, on lambdas, or on external results (§15.3).

**Judgments.**

- `Γ` maps names to refined types; `Φ` is the list of path facts, each tagged with its origin (§4.5). `⟦Γ;Φ⟧` is their conjunction.
- **Well-formedness:** `p` has sort `Bool`, mentions only names in scope, and uses only primitives, measures, selectors, testers and declared predicate aliases. **Predicates are pure and total**: ordinary functions, and anything with a non-empty effect set, are not allowed.
- **Subtyping:** `Γ;Φ ⊢ {v:B | p} <: {v:B | q}` iff `⟦Γ;Φ⟧ ∧ p ⇒ q` is valid. Base types are already equal after Core Lint; refinement subtyping never changes a base type.
- **Synthesis** gives the strongest known refinement: selfification for variables of logic sorts (`x ⇒ {v | v = x}`), literals (`n ⇒ {v | v = n}`), primitives by §4.2, calls to refined functions by their result refinement with arguments substituted, and `true` for everything else.
- **Checking** is pushed into `if`, `match`, `let` and sequencing, so every obligation arises at a leaf with that leaf's path facts. No joins or existential types are needed.

**Rules.** Before generating obligations, every non-trivial argument or scrutinee is named with a fresh variable (A-normal form); fresh names are rendered in diagnostics as the source expression (§13).

| Construct | Obligation generated | Facts added |
|---|---|---|
| **Call** `(f a1 … an)`, `f` refined | for each `i`: `ai ⇐ {v:Bi \| pi[a1..a(i-1)/x1..x(i-1)]}` | result `r:{v:B \| q[a/x]}` |
| **Primitive** (`div`, `head`, …) | its crash condition is excluded (§4.2) | its result fact (§4.2) |
| **`if c e1 e2`** | none for `c` | with `c ⇒ {v:Bool \| φ(v)}`: `φ(true)` in `e1`, `φ(false)` in `e2` (§10.1) |
| **`and` / `or`** | as for `if` on the second operand | defined by short-circuit semantics (§4.2, §10.2) |
| **`let [x e] body`** | from `e` | `x:{v:B\|p}` where `e ⇒ {v:B\|p}` |
| **`match s …`** | from clause bodies | constructor and binding facts for the clause, and "no earlier clause matched" (§10.3) |
| **definition of `f`** | its body is checked against its result refinement | parameter refinements are *assumed* in the body; a `:when` guard adds its facts |
| **recursive call** | as for any call | `f`'s declared signature is assumed (partial correctness) |
| **lambda** | calls inside the body are checked normally | parameters are `{v:B \| true}`; captured names keep their facts, which is sound because values are immutable |
| **refined `f` passed as a value** | its parameter refinements must be valid for all inputs, since the receiver may call it with anything | none |
| **`try` / `catch`** | from sub-expressions | no facts flow from `try` into handlers |
| **`perform`** (§4.4) | the effect's own requirements, if any | its result is unrefined unless the effect is an observation (§4.5) |
| **`decode τ x`** (§4.6) | none | on success, the result has type `τ`, including `τ`'s executable refinement |
| **`Dyn` value** | cannot discharge any obligation except `true` | none |

**Guarantee.**

> **Refinement soundness.** Suppose a set of modules passes Core Lint and the refinement checker, with every obligation answered `valid`, and:
>
> - **A1** Core Lint's typing rules hold for the program (the elaborator is *not* assumed correct; Core Lint checks its output).
> - **A2** The backend agrees with the reference evaluator on this program (tested by the harness, §6.3; not assumed by definition).
> - **A3** Z3 is correct when it answers `unsat`.
> - **A4** Callers outside Vaisto reach refined functions only through decoding entry points (Q3); otherwise the guarantee is not claimed for them.
> - **A5** Imported interfaces are verified and match the digests the module was checked against (§8).
>
> Then, in the semantics of Liquid Core: whenever checked code calls a refined function, its arguments satisfy the parameter refinements; and whenever a refined function returns, its result satisfies the result refinement.

This is partial correctness: nothing is claimed about termination, or about crashes other than those §4.2 lets refinements exclude. On BEAM, processes are meant to loop forever and are allowed to crash.

**Provisional syntax.** The brief leaves syntax open; this document uses one candidate:

```scheme
(defn safe-div [x :int y {d :int | (!= d 0)}] :int (div x y))
(defn clamp0 [x :int] {r :int | (>= r 0)} (if (< x 0) 0 x))
```

`{d :int | (!= d 0)}` already parses, as `{:tuple_pattern, [:d, {:atom, :int}, :|, {:call, :!=, ...}]}` (`parser.ex:401-403`, verified). Tuples in type position are written `(Tuple ...)`, so braces there are free.

**Hazard:** the same text is accepted *without error today*, as a different function. `safe-div` above parses as a three-parameter function, with `y` untyped and the brace as a tuple-pattern third parameter. The first parser change must reject braces in parameter position until refinements exist. D6 (a call in the return-type slot is read as a type) must be fixed first as well.

### 4.4 Effects

**Semantics.** A computation is a tree of requests:

```text
Computation a ::= Pure a
                | Perform e (Result(e) -> Computation a)
```

The pure core says "I require this effect"; a **handler** says "here is what reality produced"; evaluation continues. A run is therefore not `program -> result` but:

```text
run(P, I, h) = (R, T)      h is a handler; T is the trace of (effect, result) pairs
```

**The effect algebra is closed and small.** Each constructor is new theory, so there are few, general ones:

| Group | Effects | Result |
|---|---|---|
| Processes | `spawn`, `send`, `receive selector timeout`, `self`, `monitor` | pid; `unit`; the consumed message, or `timeout`; pid; reference |
| Time | `now` | integer time |
| Randomness | `random` | integer |
| Identity | `unique` | reference |
| Foreign | `external module function args` | `Dyn` (§4.6) |
| Models | `model-call model prompt schema` | `Dyn` |
| Observation | `observe observer request` | a sealed witness (§4.5); privileged observers only |

Files, network, printing and the rest reach the world through `external`, and libraries give them typed, capability-checked wrappers. Adding a constructor to this table is a language change; adding a wrapper is not.

**Replay determinism, one process.** For a program `P`, input `I` and live handler `h`: if `run(P, I, h) = (R, T)`, then `run(P, I, replay(T)) = (R, T)`. Replay is a *semantic* property, not an observability feature: the evaluator is itself a handler-parameterized interpreter.

**Selective receive.** `receive` takes a selector, the clause patterns, which is a pure predicate. Its result is the message the handler consumed, or `timeout`. The mailbox scan is the handler's business. Local replay therefore needs only the consumed messages and timeout outcomes. A replay handler checks that each recorded message matches the selector; a trace where it does not is invalid, not "replayed differently".

**Process systems.** Replaying one process with its own trace is exact. Reproducing a whole system needs more: a **causal history** `H`, a partial order of events `(process, sequence number, effect, result, causal predecessors)`, where a `receive` depends on the `send` that produced its message. The target theorem is that the program plus a valid `H` determines every process's trace, and that any interleaving consistent with `H` produces the same per-process traces. This is Phase 6, and it is the hard part: links, exit signals, monitors and timeouts all become events in `H`.

**Types.** Function types carry an effect set `ε` drawn from the groups above; `∅` is pure. Effect sets are inferred, not annotated. Refinement predicates and measures must be pure. Higher-order functions need effect polymorphism (`map` has whatever effects its argument has); the design of that is Q19. The effect *semantics* is defined in Phase 0 so the evaluator can run effectful programs against a scripted handler; the effect *type system* arrives in Phase 4.

**Lowering.** `perform` compiles to direct BEAM operations: `send` is `erlang:send/2`, `receive` is a native `receive`. There is no free-monad interpreter at runtime. Recording is a lowering option that appends each request and its result to a trace. The invariant is that lowering preserves the sequence of effect requests and the way their results are used (§3.1).

### 4.5 Evidence origin

Every fact the refinement checker may use carries an origin:

| Origin | Examples | Established by | Enters the logic |
|---|---|---|---|
| `Static` | branch conditions, primitive results, constructor facts, successful `decode`, verified refined signatures | the checker, from the program's own code | Phase 2 |
| `Imported` | the result refinement of a verified function in another module | that module's own verification, identified by interface digest | Phase 2, after interfaces carry refinements (§8) |
| `Observed` | "these tests ran and passed"; "this file has hash H" | a privileged observer, at runtime, through `observe` | Phase 5 |

**Derived facts are not a fourth origin.** A fact concluded from others carries its premises, so its trust is the combination of its premises' trust: a conclusion that used an observation says which observation. **Stochastic information is never a fact.** Model output arrives as `Dyn`, confidence is a `Float`, and neither is a logic sort, so `(>= conf 0.9)` cannot be written as a refinement at all.

**Claim ≠ fact.** No term constructs an `Observed` fact. An ordinary value `tests-passed = true` has no status. The only routes into the logic are the checker's own reasoning, verified interfaces, and `observe`.

**Observers.** An observer is a module listed as privileged in the build configuration (Q14). It defines **sealed witness types**: opaque types (§4.7) that also have no `decode` (so they cannot come from `Dyn`), cannot be returned by `external`, and cannot be exported as constructors. The observer is the one place a trusted axiom lives:

```scheme
;; privileged observer module only
(defn run-tests [suite :TestSuite] :TestReceipt ...)                   ; performs (observe ...)
(defn passed? [r :TestReceipt] {b :bool | (iff b (tests-passed r))} ...) ; result refinement is TRUSTED, not verified
```

`tests-passed` is an uninterpreted predicate whose only source of truth is the observer. `passed?`'s result refinement is recorded as `Observed` in the interface and named by every fact derived from it.

**Runtime forgery.** Compile-time opacity does not stop Erlang code from building the same tuple. Within one node, the observer process owns a `:protected` ETS table of the receipts it issued; any process can read it, only the owner can write it, and admission looks receipts up instead of trusting the term. `make_ref/0` alone is not enough, because `erlang:list_to_ref/1` exists. Across nodes, receipts need signatures.

**What Phase 2 must already do:** every hypothesis in a verification condition carries its origin (`Static` or `Imported`), and nothing else can produce a hypothesis. `Observed` is a reserved origin that Phase 2 code rejects.

### 4.6 `Dyn` instead of `:any`

`:any` today is both top and bottom: it unifies with everything, and `join_types` turns it back into a precise type (§1.11). A refinement soundness claim cannot survive that. Liquid Core has `Dyn` instead:

- `Dyn` is the type of values whose shape is not known statically. **It converts to nothing implicitly, in either direction.**
- `(decode τ x)` is the only way out. It is generated from `τ`'s structure (primitives, lists, tuples, records, sums) and returns `(Result DecodeError τ)`. If `τ` is refined and its predicate is executable, `decode` checks the predicate too; a successful decode yields a `Static` fact, like a branch condition. The surface form might be `(as User x)` (Q1).
- There is no `decode` into functions, pids or sealed witness types.
- Values arrive as `Dyn` from the raw `external` effect, `model-call`, and messages received from processes that are not typed Vaisto processes. A declared `extern` decodes its result to its declared type (§15.3).
- Untyped parameters are **not** `Dyn`: they are already type variables (`freshen_any_params`, §1.11). Migration is per source category (§15.5): each `:any` producer becomes a type error, a type variable, or an explicit `Dyn`.

### 4.7 Opaque types and module boundaries

`(deftype opaque T ...)` (promised in DESIGN.md:162-175, misparsed today, D7) means: constructors, field access and pattern matching on `T` are available only inside the defining module. The interface exports `T`'s name without its definition, and Core Lint rejects a module that constructs or destructures an abstract type it imports.

This is abstract-data-type hygiene, not theorem proving, and three library designs depend on it:

- "only admission constructs `Accepted`" (§16.3);
- sealed witness types (§4.5);
- smart constructors whose refined parameters are the only way to build a value (transitions, delegations).

Opacity is static. BEAM tuples are transparent, so where forgery at runtime matters, the library adds a runtime check (§4.5).

### 4.8 What is not in the core

| Feature | Where it lives | New theory in Liquid Core? |
|---|---|---|
| typeclasses | elaborated to dictionaries (§4.1) | no |
| processes, actors, supervision | library over the process effects | no |
| authority and capabilities | library: an ordered record type with laws (§16.2) | no |
| workflow phases | named types (§16.3) | no |
| transitions and their composition | library: refined smart constructors, laws (§16.3) | no |
| accepted state | opaque type (§4.7) | no |
| credentials, leases, delegations | library | no |
| budgets | library: resource record with `≤` (§16.6) | no |
| provenance | library over evidence origin | mostly no |
| contracts, pipelines, operators | library over `model-call` and `external` (§17) | no |
| a new kind of external effect | effect algebra | **yes** |
| a new logical operation in refinements | refinement logic | **yes** |
| use-at-most-once resources (affine types) | would need a usage discipline | **yes**, deferred (§17.5) |
| liquid inference of refinements | checker | **yes**, deferred (§17.5) |

---

## 5. Representations: one canonical tree format (assignment item 3)

### 5.1 The format

Every semantic artifact is the same kind of tree: Core terms and types, interfaces, contracts, predicates, verification conditions, effect traces, causal histories and admission records.

- **Leaves** are symbols, integers or byte strings. **Nodes** are lists.
- **Canonical bytes** use Rivest's canonical S-expression form: length-prefixed leaves, no whitespace, with a display hint distinguishing integers and strings from symbols (`[i]2:42`, `[s]5:hello`, `3:add`).
- **Maps** are encoded as lists of `(key value)` pairs, sorted by the canonical bytes of the key.
- **Metadata** (source spans, documentation) lives in a `(meta ...)` child that is **excluded from digests**, so reformatting a file never changes a contract's identity.
- **Digest** is SHA-256 of the canonical bytes, computed with OTP's `:crypto` (no Hex dependency).
- **Two renderings of one tree**: canonical bytes for hashing and storage, and an indented readable form for people and agents. Both decode to the same tree.

Required properties, each a test: `decode(encode(t)) = t`; `encode` is injective; and golden digests stay fixed across OTP releases. `term_to_binary/1` offers none of these guarantees across releases, and `binary_to_term/1` on untrusted input is unsafe (§1.8), so neither is used for artifacts.

This is where the S-expression surface pays off. The value is not homoiconicity for macros, which Vaisto does not have. It is that code, contracts and solver questions are already trees, so freezing, hashing, diffing, giving them to an agent, and translating them into SMT-LIB (itself S-expressions) are all tree operations.

### 5.2 Predicate IR

Predicates are written as Vaisto expressions, checked by Core Lint as pure `Bool` terms, then translated into a small closed IR. The IR is the only thing the SMT lowering and the diagnostics renderer consume.

```text
sort ::= Int | Bool | List | (adt Name) | Atom
term ::= (var name sort origin)            ; origin: param | let | anf(source) | binder
       | (int n) | (bool b) | (atom a)
       | (arith add|sub|mul|neg|ediv|erem term ...)
       | (ite pred term term)
       | (select Adt Ctor index term) | (ctor Adt Ctor term ...)
       | (measure name term ...)            ; len, then user measures (§12)
pred ::= true | false
       | (eq term term) | (lt term term) | (le term term)
       | (not pred) | (and pred ...) | (or pred ...) | (implies pred pred) | (iff pred pred)
       | (is Adt Ctor term)
       | (bool-term term)
       | (alias name term ...)              ; a predicate alias, inlined before lowering (§16.2)
```

Every node carries its sort, so the lowering is total and the renderer can print it back in Vaisto syntax.

### 5.3 Refined types and signatures

In Core, a refinement is a type: `(refine x τ p)`. A function signature records, per definition:

```text
(sig name
  (params (x τ-refined) ...)
  (result (refine v τ q))
  (effects ε)
  (origin declared|imported|primitive|measure-constructor)
  (status verified|assumed)            ; assumed: primitives, and Observed axioms (§4.5)
  (meta (span ...)))
```

### 5.4 Refinement environment

The refinement checker keeps its own environment, separate from anything in HM:

```text
(env
  (binds (x (refine v τ p)) ...)                       ; Γ
  (facts (fact pred (origin static|imported) (meta (span ...))) ...)   ; Φ, in path order
  (sigs ...) (measures ...) (aliases ...) (adts ...))
```

Names are alpha-renamed to be unique per function, so a fact never refers to a shadowed binding.

### 5.5 Verification conditions

```text
(vc
  (id digest)                                ; digest of the canonical SMT-LIB lowering; cache key
  (kind precondition|postcondition|primitive-safety|vacuity|reachability)
  (vars (x sort origin) ...)                 ; universally quantified
  (hyps (fact pred origin span) ...)         ; ⟦Γ;Φ⟧ at the program point
  (goal pred)                                ; one conjunct (§13.2)
  (subject (call callee param) | (result fn))
  (meta (span ...)))
```

A VC is a closed, quantifier-free formula `∀ vars. ∧hyps ⇒ goal`, decided by checking `∧hyps ∧ ¬goal` for satisfiability.

### 5.6 Effect traces and causal histories

```text
(trace (process p1) (core-version ...) (program digest)
  (event 1 (perform (send p2 (msg ...))) (result unit))
  (event 2 (perform (receive (selector ...) (timeout 5000))) (result (message ...)))
  (event 3 (perform (external file read (args ...))) (result (dyn ...))))

(history
  (event (process p1) (seq 1) (perform ...) (result ...) (after))
  (event (process p2) (seq 4) (perform (receive ...)) (result ...) (after (p1 1))))
```

A trace is valid for a program if replaying it never hands a `receive` a message its selector rejects and never answers an effect with a result of the wrong type. A history is valid if its `after` relation is acyclic and every consumed message has exactly one producing `send`.

---

## 6. The reference evaluator: the executable specification

### 6.1 What it is

A small, deterministic, call-by-value interpreter for Liquid Core, written in Elixir, parameterized by an effect handler. It is **not** a debugging interpreter. It is the definition of what a Vaisto program means. It favours clarity over speed: no optimizations, one clause per Core construct, primitives exactly as specified in §4.2.

Who checks the evaluator? Until Phase 8, the written Core semantics in this RFC and the conformance suite. From Phase 8, the Lean model (§17.4). Keeping the evaluator small keeps that check feasible.

### 6.2 Handlers

| Handler | Used for |
|---|---|
| pure | programs with an empty effect set; any `perform` is an error |
| scripted | tests: answers each request from a fixed table |
| replay | answers from a recorded trace; rejects a trace that does not fit (§5.6) |
| live, on BEAM | production; the lowered program performs effects directly, optionally recording a trace |

### 6.3 The differential harness

For every program in the corpus:

1. **Pure programs:** `evaluator(P) = BEAM(P)`, comparing values and crash reasons.
2. **Effectful programs:** run on BEAM with recording, replay the trace on the evaluator, and require the same result.
3. **Elaboration:** a program HM accepts but Core Lint rejects is an elaborator bug.

The corpus is seeded with the source of every existing test, the reproductions of D1–D23, the conformance programs (§14), and generated programs: random well-typed Core terms produced from Core's typing rules. Each disagreement is classified as a backend bug, an elaborator bug, or an evaluator bug, and the written spec decides which.

### 6.4 What it would have found

| Defect | Found by |
|---|---|
| D3, D4, D15–D19, D21, D22 | Core Lint rejects HM's output (for example, D15's leaked `let` binding makes `(+ x 1)` add an `Int` to a `String`) |
| D5 | evaluator returns `false`; the Core Erlang backend crashes with `badarith` |
| D11, D12 | evaluator returns a value; one backend fails to compile |
| D9, D10 | not by the harness: fixed by construction, because interfaces are canonical trees generated from Core and ordered by explicit imports (§8) |
| D1, D2, D20 | not by the harness (HM rejects or misreads a valid program, so there is no Core to compare): found by conformance tests |
| D6, D7, D23 | not by the harness (the parser silently changes the meaning): found by parser round-trip tests, parse then print then parse |

The harness finds most of the defect *classes* automatically. It is not a replacement for parser and conformance tests.

---

## 7. Lowering, erasure, and the single backend (assignment item 4)

### 7.1 Why the Erlang abstract format

OTP 29's `compile` module documentation has a section, "Recommendations for Language Implementors" (verified locally, Appendix A). It lists four inputs to the Erlang compiler:

- **Erlang source:** "the most straightforward and portable way", at the cost of source-line mapping.
- **The abstract format:** supports "mapping every Erlang source line back to its corresponding line in the original source file".
- **Core Erlang:** "Primops can be added, deleted, or changed in any major release without notice", and "by generating Core Erlang directly, it is possible to construct code that the Core-to-BEAM backend has never encountered before, and there are no guarantees that the final BEAM code will be safe."
- **BEAM assembly:** "Strongly discouraged."

It closes: "Our recommendation is to use either the abstract format or Erlang source."

Vaisto's `CoreEmitter`, the default backend for the CLI and `Build`, generates Core Erlang. The Elixir backend already reaches BEAM through Elixir's own compiler, which translates to Erlang and hands the result to OTP. **One lowering, from Liquid Core to the abstract format**, puts Vaisto on the interface OTP recommends and keeps source-line mapping: Core spans become abstract-format annotations, so stack traces point at Vaisto source.

### 7.2 Erasure

`erase : VerifiedCore -> Core⁻` removes `(refine x τ p)` (keeping `τ`), measure and predicate-alias definitions, and law statements. It is total and syntactic, and it never depends on solver answers, so a solver bug cannot change generated code.

**Invariant:** for every verified pure term `e`, `eval(e) = eval(erase(e))`. Refinements have no runtime meaning. The only exceptions are explicit runtime checks that survive erasure by design, such as `decode` against a refined type and the optional decoding entry points of Q3.

**Tests:**

1. The corpus property: the evaluator gives the same answer before and after erasure.
2. C11: for the accepted conformance programs, the lowered abstract format is identical to that of the same program with refinements stripped.
3. `erase` raises on unverified refined Core (§3.2).

### 7.3 Retiring the two emitters

The new lowering is built beside `CoreEmitter` and `Emitter`. Each old emitter is retired when the harness reports parity with the evaluator on the whole corpus. Three things need care:

- **Task-contract forms** (`pipeline`, `generate`), today compiled only by the Elixir emitter (§1.10), become library code over the `model-call` effect (§17.1). They are no longer emitter special cases.
- **Processes.** Today the Elixir backend emits a GenServer and the Core backend a raw receive loop. Under the new design processes are a library over the process effects, lowered to plain Erlang processes; OTP behaviours are library wrappers. This is an observable change for code that depends on GenServer calls (Q22).
- **Type terms in generated code.** The Elixir emitter `Macro.escape`s `extract_type` into the LLM call (§1.9). Under the new design the schema is an ordinary value, the canonical tree of the type, passed to `model-call`.

---

## 8. Modules, interfaces, and artifact admission (assignment item 5)

### 8.1 Canonical interfaces

A module's interface is a canonical tree generated from its verified Core:

```text
(interface
  (module Vaisto.Admission) (core-version 1) (ir-version 1)
  (exports
    (fn admit (forall () (-> (Candidate TestReceipt) (Proc) (refine a Accepted ...)))
        (requires ...) (ensures ...)))
  (types (opaque Accepted) (record Candidate ...))
  (laws ...)
  (imports (Vaisto.Tests digest) ...)
  (status verified)
  (solver z3 "5.1.0"))
```

The **interface digest** is the digest of its canonical bytes. Compilation depends on exact interface digests, not on file names or timestamps.

This fixes D9 and D10 by construction:

- Exports are Core signatures with explicit quantifiers, not monotypes with free tvars.
- Classes are elaborated into dictionaries.
- Guarded definitions are ordinary definitions.
- There are no key-prefix conventions to get wrong.
- Build order comes from the `imports` of each interface.

### 8.2 Importer semantics

Checking is modular. At a call to an imported refined function, the importer proves the preconditions and assumes the postcondition, tagged `Imported` with the exporter's interface digest. It never re-verifies the callee's body.

### 8.3 Trust and failure modes

- **A forged interface** can make any postcondition assumed. Interfaces are produced by the build, from source; in admission-checked deployments they are covered by the admission record (§8.4).
- **An unchecked exporter** (built without refinement checking) has `(status unchecked)`. Importing a refined signature from it is an error by default.
- **A stale interface:** an importer records the digests of the interfaces it was checked against, and a changed digest forces re-checking.
- **IR versions:** an importer rejects an interface with an unknown `ir-version` instead of guessing.

### 8.4 Artifact admission (the `VSTY` chunk)

OTP lets a compiler attach arbitrary chunks to a `.beam` file (`compile:file/2` option `{extra_chunks, ...}`). Vaisto can attach a `VSTY` chunk. By itself a chunk proves only that someone attached some bytes, so it is called an **admission record**, not a certificate:

```text
(admission
  (module-digest ...) (interface-digest ...) (contract-digests ...)
  (core-version 1) (compiler (vaisto "...")) (solver (z3 "5.1.0"))
  (obligations-digest ...)
  (imports ((module digest) ...))
  (signer key-id) (signature ...))
```

The signature (Ed25519 through OTP's `:crypto`) is made by the build's key over the canonical bytes. A Vaisto loader admits a module only after checking the signature, the module and interface digests, the imported digests, and the allowed core version. Only then does it call `code:load_binary/3`.

**Limits, verified locally:**

- `code:load_binary/3` loads a module without looking at the chunk.
- `beam_lib:strip/1` removes it.
- Mix releases strip BEAM files by default (`strip_beams: true`), so a release must keep the chunk explicitly (`strip_beams: [keep: ["VSTY"]]`).

Admission is therefore **typed artifact admission for code loaded through the Vaisto loader**. It is not a BEAM security boundary. Raw `code:load_binary`, a remote shell, or any node connected over Erlang distribution can bypass it.

### 8.5 Distributed interface identity

A service exports an interface digest. When two nodes connect, each states the digests of the interfaces it serves and expects, and a mismatch (`expected abc123, received f09de2`) is refused before any message is exchanged. This prevents **version skew**, not attack: Erlang distribution gives every connected node full trust, so a hostile node is out of scope for this mechanism.

---

## 9. Elaboration and the two inference engines (assignment item 6)

### 9.1 HM becomes an untrusted elaborator

HM stays as the inference engine, so programs keep their annotation-free style. Its job changes: it produces explicitly typed Core, and nothing downstream trusts it. Two consequences:

- **HM's internal type terms never carry refinements.** §1.9 shows why: the emitters, `Unify`, `Core.apply_subst` and `Core.free_vars` all pattern-match type terms structurally, several with catch-all clauses that would silently mishandle a wrapper. Refinements attach during elaboration and live only in Core types.
- **The two engines must merge.** Draft 1 left Engine B (`TypeSystem.Infer`) alone. That is no longer possible. `fallback_lambda/3` types lambda parameters as `:any`, and in Core that would be `Dyn`, which makes the lambda unusable at any concrete type and fails Core Lint. Lambdas must be typed by the same elaborator as everything else. Merging also removes Engine B's lossy boundary (§15.5) and its separate primitives table, the cause of D4. This is Phase 1 work.

### 9.2 Core Lint

Core Lint checks elaborated Core and **infers nothing**: every binder is annotated and every instantiation explicit. It checks:

- types (with `Dyn` converting to nothing);
- effect sets (from Phase 4);
- opacity (§4.7);
- refinement well-formedness: predicates are pure, `Bool`-typed, and use only admitted symbols.

It is the trusted replacement for draft 1's "re-derive sorts inside the refined fragment" containment rule, and a stronger one: it covers the whole program, not only the terms in obligations. The model is GHC's Core Lint, which checks the output of an elaborator far larger than itself.

### 9.3 The Phase 0 adapter

Before HM is changed, a temporary adapter translates today's typed AST into Core for the pure fragment. Where the typed AST lacks information (a fallback lambda's parameter types, `:any` in the types), the adapter emits `Dyn`, and Core Lint then reports the use sites. That is exactly the list of places Phase 1 has to fix. The adapter is deleted when HM elaborates to Core directly.

### 9.4 Locations

Every Core node carries its source span in `(meta ...)`. Diagnostics use the span; digests ignore it. This resolves draft 1's location problem (the typed AST has no locations, D13) without wrapping typed AST nodes.

---

## 10. Branch refinement rules (assignment item 9)

### 10.1 `if`

Let `c ⇒ {v:Bool | φ(v)}` be the synthesized refinement of the (ANF-named) condition. Then:

```text
Γ; Φ, φ(true)  ⊢ e1 ⇐ τ          Γ; Φ, φ(false) ⊢ e2 ⇐ τ
───────────────────────────────────────────────────────── (if)
Γ; Φ ⊢ (if c e1 e2) ⇐ τ
```

This covers primitive comparisons (`(< x y)` has `φ(v) = v ⇔ x < y`) and **any function whose result refinement relates its Bool result to a predicate**. The second case is how executable checks become static facts:

```scheme
(defn le? [a :Authority b :Authority] {r :bool | (iff r (le a b))} ...)
(if (le? (. job :required) (. cap :scope)) (run cap job) (deny job))
```

Inside the `then` branch, `(le ...)` is a fact; `run`'s precondition is discharged (§16.4). This is the Bool/Prop distinction from the brief: `le?` is executable data, `le` is a proposition, and the connection between them is a checked postcondition, not an identification.

A condition with no useful refinement (an opaque call, an `:any` value) adds no facts. That is always sound.

### 10.2 `and`, `or`, `not`

Liquid Core defines `and` and `or` as short-circuiting (§4.2), so `(and a b)` checks `b` under `φa(true)` and `(or a b)` checks `b` under `φa(false)`. The idiom `(and (!= y 0) (> (div x y) 1))` is therefore accepted. This is a decision about the language, not an observation about a backend: today the Core Erlang backend evaluates both operands (D5), which makes that backend wrong, and the differential harness (§6) reports it.

### 10.3 `match` and multi-clause functions

For scrutinee `s` and clauses `pat1 … patn` (first-match semantics):

- Clause `i` gets `M(pat_i, s)`: the constructor test `is_Ctor(s)`, equations for literal patterns, bindings as selectors (`v = select(Ok, 0, s)`), and for list patterns `[] → len(s) = 0`, `[h | t] → len(s) ≥ 1 ∧ len(t) = len(s) − 1`.
- Clause `i` also gets `¬M(pat_j, s)` for every earlier clause `j` whose pattern is expressible and has no guard; for a guarded earlier clause the fact is `¬(M(pat_j, s) ∧ guard_j)`.
- Patterns that are not expressible (string patterns, patterns on sorts outside the logic) add no facts, including in later clauses' negations. This is sound because it drops hypotheses, never goals.
- `defn_multi` clauses will use the same rule with the parameter as the scrutinee, once `defn_multi` is admitted (§4.3).
- **Exhaustiveness stays with HM** (`check_exhaustiveness`). Refinements do not relax it in Phase 2; a clause that is unreachable under refinements gets a warning (§13.4), not removal.

### 10.4 `:when` guards

A guard on a single-clause `defn` adds its facts to the body's `Φ`: the body only runs when the guard is true, and otherwise BEAM raises `function_clause`. Callers are **not** required to prove the guard: a guard is a runtime check, a refined parameter is a static requirement, and the two stay distinct. Two lints make the relationship visible: a guard that is provable at every call site ("this guard can never fail"), and a guard refuted at some call site ("this call always fails the guard"). Whether a guard should also imply a static precondition is Q9.

---

## 11. Solver abstraction (assignment item 7) and the trust boundary for results (item 8)

### 11.1 Interface

```elixir
defmodule Vaisto.Refine.Solver do
  @type result :: :valid | {:invalid, counterexample :: map()} | {:unknown, reason :: term()}
  @callback check([VC.t()], keyword()) :: [{vc_id :: binary(), result()}]
  @callback identity() :: {name :: String.t(), version :: String.t()}
end
```

Implementations:

| Module | Purpose |
|---|---|
| `Solver.Z3` | Production. One `Port` per compilation unit running `z3 -in -smt2`. Each VC is sent inside `(push)`/`(pop)`. Per-query timeout (`(set-option :timeout N)`), fixed seeds (`:random-seed 0`, `smt.random_seed 0`, `sat.random_seed 0`), and `(get-value ...)` on `sat` to obtain a counterexample. |
| `Solver.Unknown` | A test double that answers `{:unknown, :test}` to everything. It exists to prove that `unknown` never passes (C13 in §14). |
| `Solver.Script` | A test double with scripted answers per VC id, for deterministic tests of diagnostics without Z3. |

The SMT-LIB lowering (`Refine.SMTLIB`) is a pure function from `%VC{}` to text. It uses deterministic naming (`x_1`, sorted declarations), so the same program yields byte-identical queries, which makes answers cacheable by `(query_digest, solver identity)`.

**Language and dependency rules.** This is Elixir code that talks to an external executable over a port, like invoking `erlc`. It adds no Hex dependency and no NIF, and no Python/Go/Node. It does add an **external tool requirement** for programs that use refinements, which `AGENTS.md` ("Do not add dependencies") does not anticipate. That needs an explicit owner decision (Q2). Programs without refinements never start the solver.

### 11.2 Trust boundary

| Solver answer | Meaning | Action |
|---|---|---|
| `unsat` for `hyps ∧ ¬goal` | obligation valid | accept |
| `sat` | obligation may fail | error, with an optional "for example" built from the model, labelled as an example |
| `unknown`, timeout, solver crash, unparsable output, solver missing | nothing is known | **error**: "could not verify …" |

Rules:

- **Soundness direction.** Every approximation in the lowering may **drop a hypothesis** (lose precision, stay sound) but must **never weaken a goal**. Unsupported constructs in hypotheses are dropped; unsupported constructs in goals make the VC unprovable. A code review checklist item and a unit test per lowering rule enforce this.
- **Trusted components for `valid`:** the VC generator, the primitive specification table, the SMT-LIB lowering, and Z3's `unsat` answers. §19 lists each one.
- **Pinned solver.** The solver identity is recorded in the module interface (§8.1). A different version is allowed but visible.
- **Counterexamples are advisory.** With uninterpreted measures a model can be spurious, so messages say "for example" and tests never assert on model values. They assert on the deterministic parts: the failing conjunct, the known facts, the location.
- **No solver, no pass.** If `z3` is missing and the module contains refinements: "refinement checking needs the `z3` solver, which was not found on PATH". Never skip.
- **Optional second opinion.** A paranoid mode that requires a second solver (cvc5) to agree on `unsat` is possible later (Q13), and is not needed for Phase 2.
- **Caching.** A cache of `valid` answers keyed by query digest and solver identity is an optimization only. Whether CI may run without a solver, using a cache, is Q7. A cache is a trusted component.

### 11.3 Vacuity and reachability checks

A proof under an impossible hypothesis proves nothing, so the pass also asks satisfiability questions:

- **Vacuous requirements (error).** For each refined `defn`, check that `p1 ∧ … ∧ pn` is satisfiable. If not: "the requirements of `f` can never be met". A contract whose precondition is `false` must look suspicious, not verified.
- **Unreachable branches (warning).** For each `if` branch and `match` clause, check that `⟦Γ;Φ⟧` is satisfiable. If not: "this branch can never run, given the requirements on `x`".
- **Contradictory facts at a call (error).** If the hypotheses at an obligation are unsatisfiable while the function's requirements are satisfiable, the obligation is only vacuously valid: that call site is dead code. It is reported as a warning.

---

## 12. Measures (assignment item 10)

Measures arrive at the end of Phase 2. Before that, the only measure is the built-in `len` over lists, with the facts of §4.2.

**Proposed form** (provisional syntax):

```scheme
(defmeasure phase [j :Job] :Phase
  [(Job _ p _) p])

(defmeasure depth [t :Tree] :int
  [(Leaf)       0]
  [(Node l _ r) (+ 1 (max2 (depth l) (depth r)))])   ; max2 must itself be a measure or primitive
```

**Discipline:**

1. **One argument**, whose type is a nominal sum or record (or `List`, for built-ins only).
2. **Result sort** in the logic: `Int`, `Bool`, `Enum`, `Rec`, `Sum`.
3. **One clause per constructor**, exhaustive, non-overlapping, no guards, flat patterns (constructor with variable or `_` fields).
4. **Bodies are in the logic fragment:** literals, pattern variables, arithmetic, boolean operators, constructor applications, selectors, previously defined measures, and **self-recursion only on fields of the matched constructor** that have the same type.
5. **No** ordinary functions, externs, `:any`, strings, floats.

Rule 4 gives termination by construction: recursion is on immediate subterms, and BEAM terms are finite and acyclic.

**Translation (quantifier-free, after Liquid Haskell):**

- Each constructor's type is strengthened: `Node : l → x → r → {v:Tree | depth(v) = 1 + max(depth(l), depth(r))}`.
- In a `match` clause for constructor `C`, the fact `m(s) = body_C[fields]` is added (one-level unfolding).
- The measure is declared to the solver as an uninterpreted function; no recursive definition is sent, so queries stay decidable.

**Trust.** Measures are checked by the compiler (the rules above), not trusted. The only trusted parts are the translation rules, which are small and have one test each.

---

## 13. Error messages (assignment item 11)

### 13.1 Principles

- Existing Vaisto error style: one-line message, source line, caret, `note:` and `hint:` lines. DESIGN.md forbids type variables and "unification failed" in messages. The same applies here: never "VC", "unsat", "SMT", "solver said", or ANF temporaries.
- **Show the requirement in Vaisto syntax, with the caller's names substituted.**
- **Show what is known**: the facts from the path, rendered as source conditions with the line they come from.
- **Name the failing conjunct** (§13.2).
- **Hint only when an obvious fix exists.**

### 13.2 Conjunct splitting

A required predicate `p1 ∧ p2 ∧ …` is checked as one VC per conjunct. The message names exactly the conjunct that failed, without relying on solver models, so the diagnostic is deterministic across solver versions.

### 13.3 Examples

Precondition not established:

```text
error: requirement not met
  at line 7
    (safe-div total n)
                    ^ `safe-div` requires (!= n 0) for its argument `y`
  note: nothing is known about `n` here
  hint: check it first, for example (if (!= n 0) (safe-div total n) 0)
```

Postcondition not established, with path facts:

```text
error: result does not satisfy the declared type
  at line 3
    (defn clamp0 [x :int] {r :int | (>= r 0)} (if (< x 0) x 0))
                                                           ^ this result must satisfy (>= r 0)
  note: in this branch, (< x 0) holds (line 3)
```

A conjunct of a compound requirement:

```text
error: requirement not met
  at line 12
    (at xs (length xs))
           ^ `at` requires (< k (len xs)) for its argument `i`
  note: the other requirement, (>= k 0), holds
  note: here k is (length xs)
```

Undecoded dynamic value:

```text
error: this value has type Dyn
  at line 4
    (safe-div 10 (lookup-config "workers"))
                 ^ `safe-div` needs an Int here; nothing is known about a Dyn value
  note: `lookup-config` returns data from outside the type system
  hint: decode it first, for example (match (as Int (lookup-config "workers")) ...)
```

Unknown function (an error on its own, before any refinement is involved; §15.2):

```text
error: `Nope/thing` is not declared anywhere
  at line 4
    (safe-div 10 (Nope/thing))
                  ^ declare it with (extern ...) or import the module that defines it
```

Vacuous requirements:

```text
error: the requirements of `never` can never be met
  at line 1
    (defn never [x {v :int | (and (> v 0) (< v 0))}] :int x)
                   ^ no integer satisfies (> v 0) and (< v 0) together
```

Unknown answer:

```text
error: could not verify this requirement in 2s
  at line 9
    (run cap job)
             ^ `run` requires (le (. j :required) (. cap :scope))
  hint: split the condition, or check it at runtime with (le? ...)
```

The domain-specific rendering in the brief ("required: production.write / available: repo.read") comes from optional per-alias explanations (§16.5), not from special cases in the checker.

### 13.4 Warnings

- unreachable branch (§11.3);
- guard can never fail / always fails (§10.4);
- refinement on a sort outside the logic ("refinements on `Float` are not supported") is an error, not a warning.

---

## 14. Conformance suite (assignment item 12)

Ten programs, each with its expected outcome. Programs marked **P0** depend on a Phase 0 fix or on the single backend (§7). The expected diagnostic text is normative only for the parts listed (message line, caret target, failing conjunct); notes and hints may change wording.

| # | Program | Expected |
|---|---|---|
| C1 | `safe-div` called under a branch fact | **compiles** |
| C2 | `safe-div` called with no fact about the divisor | **error:** requirement not met, conjunct `(!= n 0)` |
| C3 | `clamp0` correct | **compiles** |
| C4 | `clamp0` returns `x` in the negative branch | **error:** result does not satisfy `(>= r 0)` |
| C5 | `max2` correct; `max2-bad` swapped | first **compiles**, second **error** on conjunct `(>= v x)` |
| C6 | recursive `at` over lists; caller guards with `length`; off-by-one caller | `at` and guarded caller **compile**; `(at xs (length xs))` **error** on `(< k (len xs))` |
| C7 | contradictory precondition | **error:** requirements can never be met |
| C8 | an undecoded `Dyn` value feeding a refined parameter | **error:** this value has type Dyn |
| C9 **P0** | `(and (!= y 0) (> (safe-div x y) 1))` | **compiles** (short-circuit on both backends) |
| C10 | truncating division | `half-up` **compiles**; `half-floor` **error** (would compile under SMT `div`) |

Two infrastructure properties accompany the suite:

- **C11 (erasure):** for C1, C3, C5, C6, C9, C10 (accepted parts), emitted Core Erlang and Elixir AST equal those of the refinement-stripped program.
- **C12 (modularity, P0):** C1's `safe-div` in module `A`, `avg` in module `B`: `B` compiles against `A`'s interface without re-checking `A`'s body; changing `safe-div`'s requirement to `(> d 0)` makes `B` fail on rebuild.
- **C13 (unknown ≠ proved):** with `Solver.Unknown`, C1 fails with "could not verify".

The programs (provisional syntax, §4.3):

```scheme
;; C1 / C2
(defn safe-div [x :int y {d :int | (!= d 0)}] :int (div x y))
(defn avg     [total :int n :int] :int (if (> n 0) (safe-div total n) 0))   ; C1 compiles
(defn avg-bad [total :int n :int] :int (safe-div total n))                   ; C2 error

;; C3 / C4
(defn clamp0     [x :int] {r :int | (>= r 0)} (if (< x 0) 0 x))             ; C3 compiles
(defn clamp0-bad [x :int] {r :int | (>= r 0)} (if (< x 0) x 0))             ; C4 error

;; C5
(defn max2     [x :int y :int] {v :int | (and (>= v x) (>= v y))} (if (> x y) x y))   ; compiles
(defn max2-bad [x :int y :int] {v :int | (and (>= v x) (>= v y))} (if (> x y) y x))   ; error

;; C6: exercises recursion, head/tail specs, len, and branch facts
(defn at [xs (List :int) i {k :int | (and (>= k 0) (< k (len xs)))}] :int
  (if (== i 0) (head xs) (at (tail xs) (- i 1))))
(defn first-or-zero [xs (List :int)] :int
  (if (> (length xs) 0) (at xs 0) 0))                                        ; compiles
(defn last-bad [xs (List :int)] :int (at xs (length xs)))                    ; error

;; C7
(defn never [x {v :int | (and (> v 0) (< v 0))}] :int x)                    ; error: never satisfiable

;; C8: lookup-config is a raw (external ...) wrapper returning Dyn
(defn uses-dyn [] :int (safe-div 10 (lookup-config "workers")))            ; error: Dyn used as Int

;; C9 (after D5)
(defn ratio-over-one? [x :int y :int] :bool (and (!= y 0) (> (safe-div x y) 1)))  ; compiles

;; C10: Erlang div truncates toward zero; SMT-LIB div is Euclidean
(defn half-up    [x {v :int | (< v 0)}] {r :int | (>= (* 2 r) x)} (div x 2))  ; compiles: -7 div 2 = -3, -6 >= -7
(defn half-floor [x {v :int | (< v 0)}] {r :int | (<= (* 2 r) x)} (div x 2))  ; error: x = -7 gives -6 > -7
```

In C6, `at`'s body is verified using: `i ≠ 0 ∧ 0 ≤ i < len(xs)` ⇒ `len(xs) > 0` (for `tail`), and `0 ≤ i − 1 < len(tail xs) = len(xs) − 1` (for the recursive call); in the `i = 0` branch, `0 < len(xs)` (for `head`).

**Test harness.** Each program is a test file under `test/refine/conformance/` with an expectations header, run on both backends for accepted programs. Diagnostic assertions match the normative parts only. The suite runs with `Solver.Z3` when available and fails (it does not skip) when it is not, unless the test is tagged as solver-free (C13).

### 14.1 Core and backend conformance

The refinement programs above test the checker. These properties test the architecture:

| # | Property | Expected |
|---|---|---|
| K1 | **Differential, pure.** For every accepted program in the corpus, `evaluator(P) = BEAM(P)`, values and crash reasons. | equal; any difference is classified per §6.3 |
| K2 | **Replay.** An effectful program (`send`, `receive`, `external`) run on BEAM with recording and replayed on the evaluator gives the same result. A trace edited so that a `receive` gets a message its selector rejects is refused. | same result; tampered trace rejected |
| K3 | **`Dyn`.** A raw `(external ...)` result (type `Dyn`) used as `Int` is rejected; matching on `(decode :int ...)` with `Ok`/`Err` clauses is accepted. An `extern` declared with result `:int` returns an `Int`: when the foreign function returns something else, the call crashes with a decode error instead of producing a mistyped value (§15.3). | reject / accept / decode crash |
| K4 | **Erasure.** For C1, C3, C5, C6 and C10, the evaluator gives the same result before and after erasure, and the lowered abstract format is identical to that of the stripped program (C11). | equal |
| K5 | **Core Lint catches the elaborator.** The D15 program is accepted by HM and rejected by Core Lint, with a message that names it as a compiler bug. | Lint error |
| K6 | **Canonical codec.** Round trip; injectivity (property-based); golden digests of a fixed interface are identical under two OTP releases. | pass |
| K7 | **Admission.** The Vaisto loader refuses a module whose `VSTY` chunk has one byte flipped, refuses a stripped module, and loads a correctly signed one. | refuse / refuse / load |
| K8 | **Opacity.** Constructing or matching on `Accepted` outside `Vaisto.Admission` is rejected. | Lint error |
| K9 | **Evidence.** Using `(tests-passed r)` as a fact without going through `passed?` cannot be proved. A non-privileged module that declares `passed?`'s trusted result refinement is rejected. | not provable / rejected |

---

## 15. Migration risks: `:any`, externs, unknown calls, lambda fallback (assignment item 13)

The new architecture turns every one of these from "a hole the refinement checker must work around" into "a thing that does not exist in Core".

### 15.1 `:any`

`:any` has no Core counterpart. Each source in the inventory (§15.5) becomes one of three things:

- **A type variable.** Untyped parameters already are one (`freshen_any_params`).
- **A type error.** A heterogeneous list, an unknown call, a failed join.
- **An explicit `Dyn`**, decoded before use. Interop results, model output, untyped messages.

The Phase 0 adapter (§9.3) emits `Dyn` wherever today's typed AST has `:any`, and Core Lint reports every use site. That report is the Phase 1 work list, generated rather than hand-maintained.

**Risk:** existing programs lean on `:any` (untyped `defn_multi`, empty lists, interop). Programs that only pass such values through are unaffected; programs that use them at concrete types will need a `decode` or a real type. That pressure is intended.

### 15.2 Unknown qualified calls

A call to a function that no interface or extern declares cannot be elaborated into Core, so it is an error. This finally matches DESIGN.md:235 ("Calling without declaring = compiler error"). In Phase 0 the adapter emits `Dyn` for such calls and Core Lint reports them; the transition from warning to error happens when HM elaborates to Core directly (Phase 1).

### 15.3 Externs

An `extern` declaration becomes a typed wrapper over the `external` effect:

- **Argument types** are checked statically on the Vaisto side, as today but without the silent fallback (D8).
- **The declared result type is a decode target.** The raw `external` effect returns `Dyn`; the wrapper decodes it to the declared type on every call and crashes with a decode error when the foreign function returns something else. A wrong declaration becomes a crash with a clear reason, never a mistyped value. The result is ordinary typed data, not `Dyn`, so declared externs stay convenient.
- **Argument refinements** are allowed. They only add obligations for callers, which is always sound.
- **Result refinements** are allowed **only as decode targets** (runtime-checked when executable). A result refinement that cannot be checked at runtime is rejected: it would be an unchecked assumption about foreign code, which is exactly an `assume`.
- **Externs cannot return sealed witness types**, since no `decode` exists for them (§4.5).

### 15.4 Lambda fallback

It disappears. Lambdas are typed by the same elaborator as everything else (§9.1), so `fallback_lambda/3` and the prefix-based `infer_should_fallback?/1` classification go away. With them goes the risk that a genuine type error, misclassified as an inference limitation, silently turns into `:any`.

### 15.5 `:any` source inventory

From the escape-hatch audit of `type_checker.ex`, `infer.ex`, `unify.ex`, `parser.ex` and `tc_ctx.ex`. Counts are distinct code sites. In the target design every row becomes a type variable, a type error, or an explicit `Dyn` (§15.1). The last column says how a value from that source is handled **in the interim**, before Phase 1 removes `:any`.

| Category | Sites | Examples | Interim treatment (before `Dyn`) |
|---|---|---|---|
| Missing annotations | 11 | untyped params (`parser.ex:1056,1059`); no return type (944-970); first-pass legacy signatures (3415, 3419) | Refined functions must annotate every refined position; unrefined positions are `true`. |
| `defn_multi` | 21 | pass-1 signature `{:fn, [:any], :any}` (3518); all pattern variables `:any` (2482-2517) | Excluded from Phase 2 (§4.3). |
| Join fallback | 4 | `join_types(_, _) -> :any` (3916); empty list (3892) | Opaque; P0-3 rejects the heterogeneous-list case. |
| Unknown qualified calls | 2 | 713; `infer.ex:326` | Opaque; becomes an error in Phase 1 (§15.2). |
| Externs (trust sites) | 3 | 725, 728; `infer.ex:313` (arguments never unified inside lambdas) | Parameter refinements allowed, result refinements forbidden (§15.3). |
| Lambda fallback | 4 | 1213-1222 | Opaque parameters (§15.4). |
| Tuples and interop shapes | 7 | `cons` as a value (201); tuple patterns (2161-2171; `infer.ex:916-917`) | Opaque (tuples are outside the logic in Phase 2). |
| Dynamic list operations | 19 | `[]` (308, 342); `head`/`tail` on a tvar (461-474); cons-pattern bindings `:any` even for `(List Int)` (2176) | `len` facts still apply; element sorts are outside the logic in Phase 2. |
| Pattern fallbacks | 13 | unresolvable constructor or type name (2111-2148, 2365-2406) | No facts from that pattern. |
| Messages and processes | 3 | `receive` patterns typed against `:any` (2774, 2778) | Unrefined in Phase 2. |
| `try`/`catch` | 2 | 2793, 2799 | Unrefined. |
| Error recovery | 1 | failing `defval` (3511), transient | n/a |
| Type classes | 3 | constraint placeholder `{class, :any}` (1691, 1716); `typed_ast_type` fallback (1843) | Class methods unrefined in Phase 2. |
| Generalization pinning | 2 | non-eligible constrained tvars pinned to `:any` (`tc_ctx.ex:132-139`, 3943-3956) | Opaque. |
| Unification binding a tvar to `:any` | 2 | 3304; `unify.ex:30-42` | Opaque. |
| Unreachable defaults | 2 | 890, 3553 | n/a |

Values that behave like `:any` without being `:any`:

- An unbound identifier becomes a singleton atom `{:atom, name}` (198-203; `infer.ex:103-113`), so a typo can type-check as an atom literal.
- Field access on `:any` or a tvar returns a fresh, unconstrained tvar (423-434), shared by every access to the same field name in a form.

Further holes the audit found by reading, not yet executed, that Phase 0 should confirm and triage:

- **Engine B loses information at the boundary:** outer constraints bound inside a lambda are discarded (the enclosing `defn` can be over-generalized); typeclass constraints on calls inside lambdas are never checked; `match` inside lambdas has no exhaustiveness check; Infer's tvar counter starts at 0 and can collide with ADT tvars.
- **Typeclasses:** instance method return types are never checked; `(defn f [x] (show x))` gets no `Show a` constraint; only `Num`/`Ord` are verified, and only on the polymorphic call path.
- **Processes:** handler return types are not unified with the state type; `!!` never checks the message; `spawn` ignores the initial state type.
- **Patterns:** literal patterns are not checked against the scrutinee; arity mismatches are truncated by `Enum.zip`; constructor destructuring in `let` has no refutability check.
- **Crashes:** `check_impl_s` has no catch-all (`(def f [a] ...)` raises `FunctionClauseError`); `unify_call_s` has no clause for a non-function callee type.

---

## 16. The standard library over the core, and the capability example (assignment item 14)

### 16.1 The criterion

Everything in this section is **library**: ordinary types, refined functions, predicate aliases and laws, built from the four primitives. None of it adds theory to Liquid Core (§4.8). That is the test the whole agent vocabulary has to pass. It also means Vaisto can be used for things other than agent work without dragging `Job` and `Capability` into the language definition.

Two library-level forms are needed. Both belong to the refinement primitive, and neither adds runtime meaning:

- **`defpred`** names a pure predicate. It is non-recursive and inlined before lowering to the solver. It may carry an `:explain` string for diagnostics (§16.5).
- **`deflaw`** states a closed refinement obligation with no runtime content. It is checked once, recorded in the interface, and later exported to Lean as a theorem statement (§17.4).

### 16.2 Authority as an ordered type

For a small, closed set of scopes, authority is a record of booleans, and the order is field-wise implication. This is quantifier-free and decidable, and needs no set theory:

```scheme
(deftype Authority [repo-read :bool repo-write :bool prod-write :bool])

(defpred le [a :Authority b :Authority]
  :explain "requires authority not available here"
  (and (=> (. a :repo-read)  (. b :repo-read))
       (=> (. a :repo-write) (. b :repo-write))
       (=> (. a :prod-write) (. b :prod-write))))

(defn meet [a :Authority b :Authority] {m :Authority | (and (le m a) (le m b))}
  (Authority (and (. a :repo-read) (. b :repo-read))
             (and (. a :repo-write) (. b :repo-write))
             (and (. a :prod-write) (. b :prod-write))))

(deflaw le-reflexive  [a :Authority] (le a a))
(deflaw le-transitive [a :Authority b :Authority c :Authority]
  (=> (and (le a b) (le b c)) (le a c)))
```

`meet` (intersection) is exported. A join (union) is **not** exported by default, because union is where authority expansion happens; a library can provide it to explicitly privileged code only. A bottom element is the all-false record. A proper `(Set E)` sort over finite enums (bit-vector encoded) is a later refinement of the same design (Q6).

### 16.3 Workflow phases and transitions

**Question from the brief:** which addition makes `deploy : Job Accepted -> Deployment` reject a `Job Executed` before runtime, with the least new machinery?

**What the checker does today** (executed against `0ec7a82`, with `(deftype Executed [id :int])` and `(deftype Accepted [id :int])`):

| Probe | Program shape | Result |
|---|---|---|
| a | `(defn deploy [j] :int (. j :id))` called with `(Executed 1)` | accepted: an unannotated parameter is structural (row-typed) |
| b | `deploy` matches `[(Accepted id) id]`, called with `(Executed 1)` | accepted (D22) |
| c | `(defn deploy [j :Accepted] ...)` called with `(Accepted 1)`, the *right* phase | rejected: `expected Accepted, found Accepted` (D1) |
| d | one sum type `(deftype Job (Proposed :int) (Executed :int) (Accepted :int))`; `deploy` matches only `Accepted` on an unannotated parameter | accepted: exhaustiveness is skipped for an unknown scrutinee type (D22) |
| d2 | the same partial match on a constructed value `(Proposed 1)` | rejected: non-exhaustive |
| f | `(deftype Job p [id :int])` as a phantom-parameter record | accepted, but as a record with fields `p` and a bracket (D23) |

Nothing rejects a wrong phase at a function boundary today. Every option below needs at least P0-1 (D1) and P0-21 (D22).

| Option | Example | New machinery | Rejects a wrong phase | Expresses value relations |
|---|---|---|---|---|
| **Distinct nominal type per phase** | `(deftype Accepted [job :JobData receipt :TestReceipt])`; `(defn deploy [j :Accepted] ...)` | none beyond P0-1, P0-21 | yes, at the call site | no |
| Phantom type parameter | `(Job Accepted)` | parameterized records (D23), parameterized annotations (D2), phase-kinded types | yes | no |
| Phase as an enum field plus refinement | `{j :Job \| (= (. j :phase) Accepted)}` | Phase 2 records | yes | no |
| One sum type, phase as constructor | `(deftype Job (Proposed d) (Accepted d r))` | a constructor-test refinement, or nothing (runtime match) | only with a refinement | no |
| **Refinement on an owner field** | `{c :Credential \| (= (. c :job) (. j :id))}` | Phase 2 records | n/a | **yes** |

**Recommendation.** These are three different mechanisms, and none of them is new core theory:

- **Phases are named types.** A finite set of phases known at compile time is best expressed as distinct nominal types. Phantom parameters give the same power for more machinery.
- **Relations between values are refinements.** Ownership, "same job", "produced by this job" and "budget did not grow" depend on runtime values, which no phase type can express.
- **"Only admission produces `Accepted`" is constructor privacy.** `Accepted` is opaque to the admission module (§4.7).

**Transitions** are values of a library type, built only by a refined smart constructor:

```scheme
(deftype opaque Transition [before :State operation :Operation after :State evidence :Evidence])

(defn transition [b :State op :Operation a :State e {x :Evidence | (valid-step b op a x)}]
  :Transition
  (Transition b op a e))

(defn compose [p :Transition q {t :Transition | (= (. t :before) (. p :after))}]
  {r :Transition | (and (= (. r :before) (. p :before)) (= (. r :after) (. q :after)))}
  ...)
```

This is the brief's `ValidTransition(before, job, result, after)`: a transition can only exist if its evidence justified it, and composition requires matching endpoints. A workflow `S0 → S1 → S2` is a chain of such values. It is the typed path through operational state from the brief, built from ordinary types, refinements and opacity.

**Laws.** Identity is a `deflaw` the solver can check. Associativity of `compose`, stated as equality of composed transitions, needs induction over the operation sequence and is outside the decidable fragment. It is a Phase 8 Lean theorem, not something the refinement checker claims.

### 16.4 The capability example

```scheme
(deftype Capability [actor :atom scope :Authority])
(deftype Job [id :int required :Authority])

(defn run [cap :Capability job {j :Job | (le (. j :required) (. cap :scope))}] :Result
  ...)

;; executable check tied to the proposition; its body is verified, not trusted
(defn le? [a :Authority b :Authority] {r :bool | (iff r (le a b))}
  (and (or (not (. a :repo-read))  (. b :repo-read))
       (or (not (. a :repo-write)) (. b :repo-write))
       (or (not (. a :prod-write)) (. b :prod-write))))

;; delegation cannot invent authority
(defn delegate [parent :Capability child {c :Capability | (le (. c :scope) (. parent :scope))}]
  :Capability
  child)
```

Demonstrations:

1. `(run read-cap read-job)`, both built from literals, **compiles**: constructor facts give the field values and `le` reduces to true.
2. `(run read-cap deploy-job)` is a **compile error**, reported through `:explain` and conjunct splitting:

   ```text
   error: requires authority not available here
     at line 14
       (run read-cap deploy-job)
                     ^ `run` requires (le (. j :required) (. cap :scope))
     note: required: prod-write
     note: available: repo-read
   ```

3. **Runtime data, the realistic case.** Capabilities and jobs arrive at runtime, not as literals. `(if (le? (. job :required) (. cap :scope)) (run cap job) (deny job))` compiles; `(run cap job)` outside such a check does not. *Check once at the boundary, carry the fact statically.* This is the brief's Bool/proposition distinction: `le?` is executable, `le` is a proposition, and a checked result refinement connects them.
4. `delegate` rejects a child with more scope than its parent. Chains of delegation verify through `le-transitive`.

Nothing here is trusted beyond §18's components. It needs values and refinements over records (Phase 2) and nothing from effects.

### 16.5 Explanations

`:explain` on `defpred` is display text only; it cannot change what is checked. The "required / available" lines come from conjunct splitting (§13.2): each conjunct of the inlined alias is checked separately, and the failing `=>` conjuncts are rendered by field name.

### 16.6 Budgets

A budget is a resource record, and consumption is refined to be monotone:

```scheme
(deftype Budget [cpu :int tokens :int net :int])
(defpred within [a :Budget b :Budget]
  (and (<= (. a :cpu) (. b :cpu)) (<= (. a :tokens) (. b :tokens)) (<= (. a :net) (. b :net))))
(defn spend [b :Budget cost {c :Budget | (within c b)}]
  {r :Budget | (and (within r b) (>= (. r :tokens) 0))}
  ...)
```

Refinements guarantee monotonicity **along each path**. They do not stop the same budget value being spent twice on two branches. That is what an affine discipline would add, and it would be new core theory, so it is deferred (§17.5).

### 16.7 Staging

| Step | Needs | Unlocks |
|---|---|---|
| Authority, `le`, `meet`, laws | Phase 2 records, `defpred`, `deflaw` | §16.2 |
| Capability example | the above, plus P0-1 | §16.4, the first demo that justifies the project |
| Phases and transitions | P0-21, `opaque` (Phase 1) | §16.3 |
| Budgets | Phase 2 | §16.6 |
| Evidence-backed transitions | Phase 5 observers | `valid-step` over observed facts |

All of this is Phase 3 in the roadmap (§21). Everything except evidence-backed transitions works without the effect system.

---

## 17. Contracts, operators, intent, and theorems

### 17.1 Task-contract operators and `Ctx` (brief §13, §33)

Operators are library code over the effect algebra, not compiler special cases. Today only `generate` exists, inside `pipeline`, with unfinished payload threading (TODO at `type_checker.ex:1524-1531`).

| Operator | Built on | Refinement contract it can expose | Kind of any facts | Phase |
|---|---|---|---|---|
| `retrieve` | `external`, or an observer | none on the documents; external data is at best observed | `Observed` via an observer | 7 |
| `rerank` | pure code or `model-call` | a permutation: `len(out) = len(in)` | `Static` | 3 |
| `generate` | `model-call` | none about content: model output is a **candidate** and arrives as `Dyn` | — | never |
| `extract` | pure `decode` or `model-call` | a result refinement if the extractor is checked Vaisto code (`decode` into a refined type) | `Static` | 7 |
| `verify` | a checked predicate, or an observer | the bridge from observation to fact; its confidence update stays stochastic | `Static` or `Observed` | 5 |
| `tool` | `external` | **requirements on the request** (the capability example applies: calling a tool requires authority); the response is `Dyn` unless an observer returns a witness | `Static` (request), `Observed` (response) | 3 / 5 |
| `branch` | pure code | a payload predicate refines each arm (§10.1); a predicate over `conf` gives **no** facts, deliberately | `Static` | 2 |
| `map` | pure code | `len(out) = len(in)`; per-element refinements need nested refinements, not yet admitted | `Static` | 2 |
| `parallel` | process effects | each branch starts from the input's facts; **no fact relates one sibling's output to another's** | `Static` | 6 |
| `fold` | pure code | needs an explicit invariant, a refinement on the accumulator checked at the start and after each step | `Static` | 2 |
| `escalate` | pure code | a sum `Escalated a`; matching gives constructor facts; the reason is data, not a fact | `Static` | 7 |

`Ctx` keeps its four fields, and they now line up with the core:

- `payload`: an ordinary value; its refinements erase.
- `trace`: **the effect trace** (§5.6). The design documents' "ordered operation history for caching and replay" is exactly the semantic trace.
- `budget`: the library type of §16.6.
- `prov`: library data referencing evidence.

### 17.2 User intent and frozen contracts (brief §14, §15)

**What Vaisto can do:** make the contract a typed artifact, compiled separately from the work that claims to satisfy it, and make the separation structural. **What it cannot do:** prove that the contract means what the human intended.

- **Separate module.** A contract lives in its own module; a pipeline or job module imports its interface.
- **Structural immutability from the worker's side.** A module may not add or change refinements on a name it imports: "`legal-qa` is defined in `Contracts.Legal`; its requirements cannot be changed here". A worker that can edit only its own module cannot weaken the acceptance condition.
- **Identity by digest.** An accepted result names the contract by its interface digest: canonical bytes (§5.1, §8.1), never a file name.
- **Change control.** Changing the contract changes its digest, so results produced against the old digest are not accepted under the new one. Who may change the contract module (repository permissions, review) is outside the compiler and must be settled by whatever runs the agents.
- **Against weakened or vacuous specifications:** the vacuity check (§11.3) applies to every requirement; contracts carry concrete examples (inputs that must be admissible and inputs that must be rejected), compiled as tests; a contract diff is reviewed as a specification change.

### 17.3 Contracts (Phase 7)

`defcontract` elaborates to a Core interface plus library types. Its `requires`/`ensures` reuse the refinement calculus unchanged. `quality` stays stochastic and is never discharged by the solver. `failure` clauses are ordinary sum types handled by library supervision over the process effects.

### 17.4 Theorems, and the role of Lean (brief §21)

**Lean does not run Vaisto. Lean formalizes Liquid Core.** It never becomes a compile dependency. The theorem statements are written by the language's authors or a contract's author, never by the agent searching for a proof.

**Language theorems**, about Liquid Core:

| Theorem | Statement |
|---|---|
| Preservation | evaluation of a Lint-accepted term preserves its type |
| Progress | a Lint-accepted pure term is a value, crashes with a defined reason, or steps |
| Refinement soundness | if every VC is valid, values satisfy their refinements (§4.3) |
| Erasure | `eval(e) = eval(erase(e))` for verified pure `e` (§7.2) |
| Effect lowering | lowering preserves the sequence of effect requests and the use of their results |
| Replay determinism | same program, input and trace give the same result (§4.4); later, the same for a valid causal history |
| Evidence integrity | an `Observed` fact cannot arise except from `observe` performed by a privileged observer |

**Library theorems**, about the standard library, stated as `deflaw` and proved in Lean where outside the decidable fragment:

- authority is a partial order with meet;
- delegation chains never increase authority;
- transition composition is associative, and composing admissible transitions gives an admissible transition;
- budgets are monotone along every path.

**Where to start:** Lean work can begin right after Phase 0, on the pure fragment of Core with the evaluator as the thing being modelled. It runs in parallel with Phases 1–7.

### 17.5 Later

- **Liquid inference** of refinements needs one elaborator (§9.1) and a qualifier vocabulary; not before explicit refinements have been used in anger.
- **Affine resources** (a budget or credential used at most once) are new core theory, deferred until the library shows where refinements alone fall short (§16.6).
- **Other backends** (WASM, native) are second *implementations* validated against the evaluator by the same harness, not new semantics.

---

## 18. Trust model (brief §35, §36)

### 18.1 Trusted, not trusted, conditionally trusted

**Trusted.** A bug in one of these can make a wrong program pass.

| Component | Mechanism | Phase |
|---|---|---|
| Liquid Core specification and reference evaluator (the definition; checked by Lean from Phase 8) | M3 | 0 |
| Core Lint | M1 | 0 |
| Canonical codec and digests | M2 | 0 |
| Primitive semantics table | M5 | 0 |
| Refinement checker: VC generation, branch and pattern rules, SMT-LIB lowering | M7, M8, M9 | 2 |
| Erasure | M12 | 2 |
| Measure checker and constructor strengthening | M11 | 2 |
| `defpred` inliner and `deflaw` checking | M21 | 3 |
| Interface writer and reader | M18 | 1 |
| Effect handlers on BEAM, for "performed the request and reported its result" | M14 | 4 |
| Privileged observers and their declared axioms | M15 | 5 |
| Admission loader and the build's signing key | M19 | 5 |
| Lean model and proof-checker configuration | — | 8 |

**Not trusted.** None of these is ever a source of facts.

- **The HM elaborator.** Core Lint checks its output.
- **The backend** (lowering, OTP compiler, BEAM). The harness tests it against the evaluator.
- **Model output.** LLM output, explanations and self-reported confidence.
- **Worker claims.** Prompt text, pipeline implementations that claim success, agent-generated tests on their own, and agent-generated evidence claims.
- **Unchecked values.** `Dyn` values until decoded, and raw `external` results.
- **Outside callers.** Erlang and Elixir callers of refined functions (A4, Q3).
- **Guards.** `:when` guards are runtime checks, so callers get no facts from them.
- **Solver counterexamples.** They are advisory.
- **Proof search.** Tactics are untrusted; only the Lean kernel's verdict counts.

**Conditionally trusted.**

| Component | Trusted for | Not trusted for | Condition |
|---|---|---|---|
| Z3 | `unsat` answers | `sat` models (advisory); `unknown` is an error | pinned version recorded in interfaces; optional second solver (Q13) |
| An interface from an earlier build | imported postconditions | anything, if `(status unchecked)` or a digest does not match | built from source; covered by admission where admission is used |
| A recorded trace | replaying a run | anything, if it is invalid for the program (§5.6) | validated on replay |
| Answer cache | skipping re-solving | anything, if the query digest or solver identity differ | off in the first refinement phases (Q7) |
| Test runner, tool receipt, kernel observer, remote executor | the specific observation it issued | anything else, including the property the observation is *about* | privileged observer, runtime-unforgeable receipts (§4.5) |

### 18.2 Who creates the claim, who establishes the fact, who checks the relationship

| Claim | Created by | Established by | Checked by |
|---|---|---|---|
| The program is well-typed | HM elaborator | — | Core Lint |
| The backend implements the semantics | compiler authors | the lowering | differential harness |
| Requirement on a parameter of `f` | the author of `f` | the caller's code | refinement checker, at each call |
| Guarantee on the result of `f` | the author of `f` | the body of `f` | refinement checker, at each return point |
| Imported guarantee | the exporter's author | the exporter's body, in its own build | importer, through interface status and digest |
| Primitive safety (`div`, `head`) | the primitive table | the caller's code | refinement checker |
| A contract can be satisfied at all | the contract author | — | vacuity check |
| Branch fact | the program's own test | runtime evaluation of the condition | refinement checker, only inside that branch |
| A trace is faithful | the recording handler | the live run | replay validity (§5.6) |
| Observation ("tests passed") | the observer module | the observer's execution against reality | admission, by lookup in the observer's table |
| Accepted state | the admission function | static obligations plus observations | refinement checker, plus runtime admission |
| Job acceptance condition | the contract author (human) | — | fixed by module separation and digest (§17.2) |
| Loaded code is the verified code | the build | the signature over the admission record | the Vaisto loader |
| Quality, confidence | a model or evaluator | — | never checked statically |

The author of `f` appears twice, stating `f`'s guarantee and writing the body that establishes it. That is acceptable because a third party, the refinement checker, checks the relationship. The brief's rule applies to the contract row: an agent may write `f` and `f`'s refinements, but never the contract it is judged against. The solver is the one component that both establishes and checks (M9); the optional second solver (Q13) is the mitigation.

---

## 19. Mechanism ledger

For every mechanism: soundness assumption, trusted component, runtime representation, erasure, failure mode, user-facing diagnostic, test strategy, and the brief's §36 question (who creates, establishes, checks).

### M1. Liquid Core and Core Lint

| | |
|---|---|
| Soundness assumption | Core Lint's rules are the typing rules of Liquid Core. |
| Trusted component | Core Lint. |
| Runtime representation | None; Core is compile-time. |
| Erasure | Lint runs before and after erasure. |
| Failure mode | Lint accepts an ill-typed term: everything downstream reasons about a wrong program. |
| Diagnostic | For elaborator bugs: "internal error: the type checker produced an ill-typed program at line N (this is a compiler bug)". |
| Test strategy | One accept and one reject test per typing rule; K5; the harness's elaboration check (§6.3). |
| Creates / establishes / checks | HM creates; nothing establishes; Lint checks. |

### M2. Canonical tree codec and digests

| | |
|---|---|
| Soundness assumption | Encoding is injective and stable across OTP releases. |
| Trusted component | Codec. |
| Runtime representation | Interface files, admission records, traces. |
| Erasure | n/a |
| Failure mode | Two different trees with one encoding: two contracts share a digest. |
| Diagnostic | "interface for `A` does not decode: …". |
| Test strategy | K6: round trip, property-based injectivity, golden digests under two OTP releases. |
| Creates / establishes / checks | The build creates; the codec establishes; importers and the loader check. |

### M3. Reference evaluator

| | |
|---|---|
| Soundness assumption | The evaluator implements the written Core semantics. |
| Trusted component | The evaluator: it is the definition. |
| Runtime representation | None in production. |
| Erasure | Runs on Core before or after erasure; results must agree. |
| Failure mode | An evaluator bug makes a correct backend look wrong, or two bugs agree. |
| Diagnostic | Harness reports: "evaluator and BEAM disagree on …". |
| Test strategy | Conformance suite; the evaluator kept small enough to review line by line; Lean model from Phase 8. |
| Creates / establishes / checks | The spec creates; the evaluator establishes; review now, Lean later, checks. |

### M4. Elaboration (HM to Core)

| | |
|---|---|
| Soundness assumption | None: the elaborator is untrusted. |
| Trusted component | None. |
| Runtime representation | None. |
| Erasure | n/a |
| Failure mode | Produces ill-typed Core (caught by Lint), or rejects valid programs (caught by conformance tests, e.g. D1). |
| Diagnostic | Ordinary type errors, unchanged in style. |
| Test strategy | Existing type-checker tests; the harness's elaboration check. |
| Creates / establishes / checks | HM creates; Lint checks. |

### M5. Primitive semantics (normative)

| | |
|---|---|
| Soundness assumption | The SMT encoding matches the evaluator's definition. The lowering matching it is tested, not assumed. |
| Trusted component | The table. |
| Runtime representation | The lowering of each primitive. |
| Erasure | n/a |
| Failure mode | A wrong encoding (for example Euclidean `div`) proves false things. |
| Diagnostic | Crash conditions render as "`div` requires a non-zero divisor". |
| Test strategy | Per primitive: evaluator vs BEAM vs SMT on random inputs including negatives, zero and bignums; C10. |
| Creates / establishes / checks | The spec creates; the evaluator establishes; differential tests check. |

### M6. Refined types in Core and the predicate IR

| | |
|---|---|
| Soundness assumption | IR semantics equals Core semantics for every admitted construct. |
| Trusted component | Translation from predicate terms to IR. |
| Runtime representation | None; also stored in interfaces. |
| Erasure | Removed by `erase`. |
| Failure mode | A construct outside the fragment reaches the IR: rejected, never approximated in a goal. |
| Diagnostic | "`foo` cannot be used in a refinement: only comparisons, arithmetic, `len`, fields and declared predicates are allowed". |
| Test strategy | One test per IR node: Vaisto spelling, IR, SMT, rendering back. |
| Creates / establishes / checks | The author creates the predicate; Lint and the translator check it. |

### M7. VC generation (ANF, facts, obligations, conjunct splitting)

| | |
|---|---|
| Soundness assumption | Facts are true on the path where they are added; every obligation in §4.3 is generated. |
| Trusted component | The VC generator. |
| Runtime representation | None. |
| Erasure | n/a |
| Failure mode | A missed obligation (unsound) or a missing fact (spurious error). |
| Diagnostic | §13.3. |
| Test strategy | Golden VCs in canonical form for C1–C10; mutation tests that delete a rule and expect a conformance failure. |
| Creates / establishes / checks | The compiler creates obligations; program facts establish them; the solver checks. |

### M8. Branch and pattern facts

| | |
|---|---|
| Soundness assumption | Core's evaluation order (§4.1, §4.2), not any backend's. |
| Trusted component | The VC generator's branch rules. |
| Runtime representation | None. |
| Erasure | n/a |
| Failure mode | A fact assumed on a path where it does not hold. |
| Diagnostic | Notes list the facts used ("in this branch, (< x 0) holds"). |
| Test strategy | C1, C3–C6, C9; K1 checks the backend evaluates branches as Core does. |
| Creates / establishes / checks | The program's condition creates; runtime evaluation establishes; the harness checks the backend. |

### M9. Solver behaviour and Z3 port

| | |
|---|---|
| Soundness assumption | Z3 `unsat` is correct; the lowering never weakens a goal. |
| Trusted component | SMT-LIB lowering, port, Z3 binary. |
| Runtime representation | None (compile time). |
| Erasure | n/a |
| Failure mode | `unknown`, timeout, crash, or missing binary: all errors. A misparsed answer is treated as `unknown`. |
| Diagnostic | "could not verify …"; "refinement checking needs `z3` …". |
| Test strategy | `Solver.Unknown` (C13); `Solver.Script` for diagnostics; Z3 integration tests; a parser test per answer shape. |
| Creates / establishes / checks | The VC generator asks; Z3 answers; nothing further (Q13). |

### M10. Vacuity and reachability checks

| | |
|---|---|
| Soundness assumption | Solver answers to satisfiability questions are correct. |
| Trusted component | Same as M9. |
| Runtime representation | None. |
| Erasure | n/a |
| Failure mode | `unknown` on a vacuity question: reported as an error, because a vacuous contract must not pass quietly. |
| Diagnostic | "the requirements of `f` can never be met"; unreachable-branch warning. |
| Test strategy | C7; a reachability warning test. |
| Creates / establishes / checks | The author's requirements create; the solver checks. |

### M11. Measures

| | |
|---|---|
| Soundness assumption | Measures satisfy the discipline of §12, which is checked. |
| Trusted component | Constructor strengthening and unfolding rules. |
| Runtime representation | None by default (Q5). |
| Erasure | Removed. |
| Failure mode | A non-structural measure admitted: non-termination in the logic. Prevented by the checker. |
| Diagnostic | "`depth` must recurse only on fields of the matched constructor". |
| Test strategy | One accept and one reject test per discipline rule. |
| Creates / establishes / checks | The measure's author creates; its structural definition establishes; the discipline checker checks. |

### M12. Erasure

| | |
|---|---|
| Soundness assumption | Refinements have no runtime meaning apart from explicit runtime checks. |
| Trusted component | `erase`. |
| Runtime representation | n/a (it removes things). |
| Erasure | It is erasure. |
| Failure mode | Something with runtime meaning removed, or a refinement left in: caught by the corpus property. |
| Diagnostic | None (internal); `erase` raises on unverified refined Core. |
| Test strategy | K4; C11. |
| Creates / establishes / checks | The compiler creates and establishes; the corpus property checks. |

### M13. Single lowering to the Erlang abstract format

| | |
|---|---|
| Soundness assumption | None: the backend is untrusted and tested. |
| Trusted component | None (OTP's compiler is outside Vaisto's trusted base; the harness covers it). |
| Runtime representation | BEAM modules. |
| Erasure | Receives erased Core only. |
| Failure mode | A disagreement with the evaluator: a backend bug, reported by the harness. |
| Diagnostic | Harness report; in production, ordinary crash reports with Vaisto source lines (§7.1). |
| Test strategy | K1, K2, K4; parity on the whole corpus before each old emitter is retired. |
| Creates / establishes / checks | Compiler authors create; the lowering establishes; the harness checks. |

### M14. Effect algebra, handlers and traces

| | |
|---|---|
| Soundness assumption | The live handler performs the requested effect and reports its result faithfully. |
| Trusted component | Handlers on BEAM. |
| Runtime representation | Direct BEAM operations; optional trace recording. |
| Erasure | Effects are not erased; they are the program's meaning. |
| Failure mode | A handler that misreports a result makes replay reproduce a run that never happened. |
| Diagnostic | "trace does not fit this program at event N" for invalid traces. |
| Test strategy | K2; replay of recorded runs for every effectful conformance program. |
| Creates / establishes / checks | The program requests; reality, through the handler, establishes; replay validity checks. |

### M15. Evidence origin and observers

| | |
|---|---|
| Soundness assumption | Only privileged observers can produce `Observed` facts; their axioms are faithful to reality. |
| Trusted component | Observer modules and their declared axioms; the build configuration listing them. |
| Runtime representation | Sealed witness values; the observer's `:protected` ETS table. |
| Erasure | Witness types are runtime values and stay; origin tags are compile-time. |
| Failure mode | A forgery route: construction outside the module, `Dyn`, externs, a forged interface, a hand-built tuple at runtime. Each is closed in §4.5. |
| Diagnostic | "`TestReceipt` can only be produced by `Vaisto.Tests`". |
| Test strategy | K9; one compile-error test per forgery route; runtime lookup test. |
| Creates / establishes / checks | The observer creates; its run against reality establishes; admission checks. |

### M16. `Dyn` and `decode`

| | |
|---|---|
| Soundness assumption | Generated decoders accept exactly the values of their type, including executable refinements. |
| Trusted component | The decoder generator. |
| Runtime representation | Generated decode functions. |
| Erasure | Not erased: `decode` is a runtime check by design. |
| Failure mode | A decoder that accepts a wrong value lets a mistyped value into typed code. |
| Diagnostic | Statically: "`x` has type Dyn; decode it first, e.g. (as Int x)". At runtime: a decode error naming the type and the offending value. |
| Test strategy | K3; property test: for random values, `decode` accepts exactly the well-typed ones. |
| Creates / establishes / checks | A foreign source creates a value; `decode` establishes its type; Lint checks that nothing skips `decode`. |

### M17. Opaque types

| | |
|---|---|
| Soundness assumption | Core Lint enforces construction and inspection privacy. |
| Trusted component | Core Lint. |
| Runtime representation | Ordinary BEAM terms; opacity is static. |
| Erasure | n/a |
| Failure mode | A missed Lint rule lets another module construct an `Accepted`. |
| Diagnostic | "`Accepted` is opaque outside `Vaisto.Admission`". |
| Test strategy | K8. |
| Creates / establishes / checks | The defining module creates; Lint checks. |

### M18. Canonical interfaces and importer semantics

| | |
|---|---|
| Soundness assumption | Imported summaries were verified against the sources that are loaded (A5). |
| Trusted component | Interface writer and reader. |
| Runtime representation | Interface files (canonical trees). |
| Erasure | Not in BEAM. |
| Failure mode | A stale or forged interface: assumed postconditions that do not hold. |
| Diagnostic | "`A/f` has requirements that were never checked"; "interface for `A` is out of date". |
| Test strategy | Round trip; C12; stale rebuild; unchecked-status rejection; unknown `ir-version` rejection. |
| Creates / establishes / checks | The exporter's build creates; its verification establishes; importers check status and digests. |

### M19. Artifact admission (`VSTY`) and the loader

| | |
|---|---|
| Soundness assumption | Code reaches the node only through the Vaisto loader; the signing key is not compromised. |
| Trusted component | Loader; signing key; build. |
| Runtime representation | A `VSTY` chunk in each `.beam`. |
| Erasure | n/a |
| Failure mode | Bypass through raw `code:load_binary`, a remote shell or distribution; stripped chunks. All documented as out of scope (§8.4). |
| Diagnostic | "module `M` refused: signature does not match" / "no admission record". |
| Test strategy | K7. |
| Creates / establishes / checks | The build creates; the signature establishes; the loader checks. |

### M20. Decoding entry points for exported functions (optional, Q3)

| | |
|---|---|
| Soundness assumption | External callers use the exported name; Vaisto callers use an internal one. |
| Trusted component | Wrapper generator. |
| Runtime representation | Each exported refined function gets a wrapper that decodes its arguments, including executable refinements, before calling the internal version. |
| Erasure | Not erased; opt-in. |
| Failure mode | A predicate that is not executable gets no check, which is documented per function. |
| Diagnostic | Runtime: a decode error naming the function and parameter. |
| Test strategy | Call from Elixir with a violating argument. |
| Creates / establishes / checks | The function's author creates; the wrapper establishes; the runtime checks. |

### M21. Library laws (`defpred`, `deflaw`)

| | |
|---|---|
| Soundness assumption | Aliases are non-recursive; laws are closed obligations. |
| Trusted component | Inliner; the same VC and solver path as M7 and M9. |
| Runtime representation | None. |
| Erasure | Removed. |
| Failure mode | A recursive alias (rejected); a law outside the decidable fragment (reported as "needs a proof", deferred to Lean). |
| Diagnostic | `:explain` text; "`le-transitive` could not be checked automatically; it needs a proof". |
| Test strategy | §16.2 laws as conformance programs; recursion rejection. |
| Creates / establishes / checks | The library author creates; the solver or Lean establishes; the checker records the result. |

---

## 20. Open questions

| # | Question | Status | Recommendation |
|---|---|---|---|
| Q1 | Surface syntax for refinements and `decode` | **open, owner decision** | `{v :int \| p}` in type position; `(requires ...)`/`(ensures ...)` sugar; head-tagged forms, never positional guessing |
| Q2 | Z3 as an external tool requirement (AGENTS.md: "Do not add dependencies") | **open, owner decision** | require `z3` on PATH for refined code; no internal solver |
| Q3 | Decoding entry points for exported functions (M20) | open | opt-in first; default-on once measured |
| Q4 | Locations in the typed AST | **resolved**: Core spans (§9.4) | — |
| Q5 | Should measures also compile to executable functions? | open | logic-only first |
| Q6 | Sets over finite enums | open | record-of-bools first (§16.2), `(Set E)` later |
| Q7 | May CI run without a solver, using cached answers? | open | no, in the first refinement phases |
| Q8 | Refinements on record fields (data invariants) | open | after Phase 2's first step |
| Q9 | Should `:when` guards imply static preconditions? | open | no; add the two lints of §10.4 |
| Q10 | Unreachable branch: warning or error? | open | warning |
| Q11 | Canonical encoding for digests | **resolved**: canonical S-expressions (§5.1) | residual: exact leaf hints |
| Q12 | Hot code loading of refined modules | **partly resolved**: the admission loader (§8.4); raw loading is out of scope | — |
| Q13 | Second solver cross-check | open | not in the first refinement phases |
| Q14 | Where privileged observers are declared | open | build configuration |
| Q15 | Strict or short-circuit `and`/`or` | **resolved**: Core defines short-circuit (§4.2) | — |
| Q16 | Row polymorphism versus nominal phases | open | P0-21 plus a lint for unannotated parameters matched against nominal constructors |
| Q17 | How witness types resist `:any` | **resolved**: `Dyn` converts to nothing; sealed types have no `decode` (§4.5, §4.6) | — |
| Q18 | New modules and a new compiler back half versus AGENTS.md | **open, owner decision**; larger than in draft 1 | accept: a separate core is the point of the design |
| Q19 | Effect polymorphism for higher-order functions | open | effect variables on function types, inferred |
| Q20 | Polymorphism in Core: explicit type abstraction and application, or let-schemes | open | explicit, as in GHC Core, so Lint never infers |
| Q21 | When to retire each old emitter | open | when the harness reports parity on the whole corpus |
| Q22 | Processes: plain Erlang processes, or GenServer by default | open | plain processes in Core; OTP behaviours as library |
| Q23 | Derived facts: a fourth origin, or premise sets | **resolved**: premise sets (§4.5) | — |
| Q24 | Is `observe` a separate effect or a privileged use of `external`? | open | separate, so privilege is visible in the effect set |
| Q25 | Evaluator implementation: Elixir now; later extracted from the Lean model? | open | Elixir now; revisit after Phase 8 |

---

## 21. Roadmap

### 21.1 Phases

| Phase | Name | Delivers | Depends on |
|---|---|---|---|
| **0** | Foundation | written Core spec (§4 as a standalone document); canonical codec; reference evaluator with pure and scripted handlers; Core Lint; the typed-AST-to-Core adapter; differential harness; parser round-trip tests; the defect fixes marked "keep" below | — |
| **1** | Consolidation | `Dyn` replaces `:any`; engines merged and HM elaborating directly to Core; `opaque`; lowering to the abstract format; old emitters retired at parity; canonical interfaces and digests | 0 |
| **2** | Refinements | step 1: `Int`/`Bool`/`List`, primitive specs, branch facts, Z3, diagnostics, C1–C13; step 2: records, sums, enums in the logic; step 3: measures | 1 |
| **3** | Library | `defpred`, `deflaw`; authority, phases, transitions, budgets; **the capability demo** | 2 |
| **4** | Effects | effect types and inference; the handler interface on BEAM; trace recording; single-process replay (K2) | 1 |
| **5** | Evidence and admission | observers, sealed witnesses, `Observed` facts; `VSTY` admission and the loader | 3, 4 |
| **6** | Process systems | causal histories; multi-process replay; distributed interface identity | 4 |
| **7** | Contracts and pipelines | `defcontract`, operators and `Ctx` as library over `model-call` and `external` | 3, 4, 5 |
| **8** | Lean | formal model of Liquid Core and the language theorems; library laws outside the decidable fragment | 0; runs in parallel |

- **Shortest path to the first refinement demo** (`safe-div`, `clamp`, bounded index): Phases 0 and 1, then step 1 of Phase 2.
- **Shortest path to the capability demo:** add Phase 2 step 2 and the first part of Phase 3. No effects are needed.

### 21.2 Defect fixes from draft 1

Draft 1's Phase 0 items are still the acceptance tests for today's defects. The last column says what the new architecture does with each.

| # | Fixes | Change | Acceptance test | Under draft 2 |
|---|---|---|---|---|
| P0-1 | D1, D2 | Resolve named and parameterized type annotations in `collect_defn_signature` and `check_impl_s({:defn, ...})`, reusing `resolve_named_type`. | `(defn idp [p :Point] :Point p)` called with `(Point 1 2)` type-checks; calling it with `(Other 1)` fails with a type mismatch naming both types. Same for `(Result :int :string)`. | **keep**, Phase 0: HM stays as the front end |
| P0-2 | D4 | Engine B reads primitives from `TypeEnv`; `TypeEnv`'s `/` becomes `Float`. | The D4 program fails to type-check (declared `:int`, body `Float`). | one-line fix now; superseded by merging the engines (Phase 1) |
| P0-3 | D3 | A list literal whose element types do not join is an error when the literal meets a declared element type (or always). | `(defn main [] (List :int) [1 "a"])` is rejected. | **keep**, Phase 0 (Core Lint would also catch it) |
| P0-4 | D5 | Both backends implement the same `and`/`or` evaluation (Q15). | Parity test: `(and (!= y 0) (> (div x y) 1))` with `y = 0` returns `false` on both. | old Core backend only, while it lives; the new lowering is short-circuit by definition (§4.2) |
| P0-5 | D6 | Only capitalized call heads (`List`, `Tuple`, user types) are type annotations in return position. | `(defn f [x] (println x) x)` keeps `(println x)` in the body. | **keep**, Phase 0 (parser) |
| P0-6 | D7 | Until `opaque` is implemented, `(deftype opaque ...)` is a parse error ("opaque types are not implemented yet"). | The D7 program is rejected with that message. | **keep** as a stopgap until `opaque` lands (Phase 1) |
| P0-7 | D8 | Warning for unknown qualified calls; error in strict mode; extern argument mismatches warn. | A test per case. | interim warning now; superseded in Phase 1: unknown calls cannot elaborate, externs become `external` plus `decode` (§15) |
| P0-8 | D9 | `.vsi` key prefix consistent; export generalized schemes; include guarded `defn`s; do not overwrite built-in class registries on merge; `binary_to_term(..., [:safe])`; module-name check. | Cross-module call `(A/base)` is typed from `A.vsi`; a wrong argument type is an error; `(show 1)` still dispatches to the Show instance after an import. | superseded by canonical interfaces (Phase 1, §8.1); fix the key prefix now only if cross-module work cannot wait |
| P0-9 | D10 | Dependency resolver matches imports to graph keys; cycles are reported. | Integration test passes deterministically under 50 random seeds; a two-module cycle returns `{:error, :circular_dependency}`. | **keep, do first**: cheap, and it is the flaky test |
| P0-10 | D11, D12 | CoreEmitter compiles guarded `defn`; Elixir emitter compiles field access. | Both programs run on both backends; added to the parity suite. | superseded by the single backend (§7.3); fix only if the old emitters must live long |
| P0-11 | D14 | `apply_subst_to_ast` handles guarded `defn`. | A polymorphic guarded function has no free tvars in its typed AST. | **keep**, Phase 0 (cheap) |
| P0-12 | D13 | Located typed AST under option A, behind a flag, with the corpus property test. | The property holds across the test corpus. | superseded by Core spans (§9.4) |
| P0-13 | §15.4 | One test per `infer_should_fallback?` prefix showing a genuine type error still surfaces. | Tests pass. | superseded by merging the engines (§9.1) |
| P0-14 | D15 | Scope `let`, `try`, `receive` and fallback-lambda bindings lexically in the checker, as the emitters already do. | The D15 program is rejected (`+` on a String). | **keep**, Phase 0 |
| P0-15 | D16 | Check the declared return type with the threaded substitution (`unify_types_s`), not `types_unifiable?`. | The D16 program is rejected. | **keep**, Phase 0 |
| P0-16 | D17 | `check_numeric_op` unifies a tvar operand with the other operand's type. | `(defn f [x] (+ x "s"))` is rejected; `(g 1.5)` is `Float` or rejected. | **keep**, Phase 0 |
| P0-17 | D18 | Sum constructors keep their declared concrete field types; only real type parameters become tvars. | `(Ok "s")` is rejected for `(deftype R (Ok :int) (Err :string))`. | **keep**, before Phase 2 records |
| P0-18 | D19 | Higher-order builtins unify the function's parameter with the list element type. | `(map (fn [x] (++ x "a")) [1 2])` is rejected. | **keep**, Phase 0 |
| P0-19 | D20 | Lambda parameters accept annotations like `defn` parameters. | `(fn [x :int] x)` has type `Int -> Int`. | **keep**, Phase 1 (with the engine merge) |
| P0-20 | D21 | Field access on a tvar unifies it with the recorded row. | `(get-x 5)` is rejected. | **keep**, Phase 0 |
| P0-21 | D22 | A `match` against constructor patterns unifies the scrutinee with the patterns' type, which also turns on exhaustiveness for type-variable scrutinees. | `(f (Y 1))` from D22 is rejected; a partial match on an unannotated parameter is reported as non-exhaustive. | **keep**, before Phase 3 workflow phases |
| P0-22 | D23 | `deftype` rejects a type-parameter list on records, with a clear message, until parameterized records exist. | `(deftype Job p [id :int])` is a parse error. | **keep**, Phase 0 (parser) |

---

## Appendix A. Reproduction

All commands were run in a checkout of `0ec7a82` (`lib/` and `std/` identical to `origin/main`) with `mix run -e`.

```elixir
# D1: user-type param annotations
Vaisto.TypeChecker.check(Vaisto.Parser.parse("""
(deftype Point [x :int y :int])
(defn idp [p :Point] :Point p)
(defn main [] :Point (idp (Point 1 2)))
"""))
# => {:error, [%Error{message: "type mismatch", ...}]}   rendered: expected `Point`, found `Point`

# D3: heterogeneous list accepted as (List :int)
check("(defn main [] (List :int) [1 \"a\"])")          # => {:ok, ...}

# D4: Int-declared function returns a float
Vaisto.Runner.run("(defn main [] :int (let [f (fn [a b] (/ a b))] (f 7 2)))", backend: :core)
# => {:ok, 3.5}   (same on :elixir)

# D5: and-evaluation differs by backend
src = "(defn chk [x :int y :int] :bool (and (!= y 0) (> (div x y) 1)))\n(defn main [] :bool (chk 10 0))"
Vaisto.Runner.run(src, backend: :core)    # raises ArithmeticError
Vaisto.Runner.run(src, backend: :elixir)  # => {:ok, false}

# D6: a call in return position is swallowed as a type
Vaisto.Parser.parse("(defn f [x] (println x) x)")
# => {:defn, :f, [x: :any], :x, {:call, :println, [:x], %Loc{}}, %Loc{}}

# D7: opaque misparsed
check("(deftype opaque Password [hash :string]) (defn main [] :int 1)")
# => {:ok, [{:record, :opaque, [{:Password, :any}, ...]}, ...]}

# D8: unknown qualified call and extern mismatch accepted
check("(defn main [] :int (Nope/thing 1 2))")                                  # => {:ok, ...}
check("(extern erlang:abs [:int] :int) (defn main [] :int (erlang:abs \"x\"))")  # => {:ok, ...}

# D10: dependency order follows atom creation order
for n <- ["Elixir.Qz3", "Elixir.Qy2", "Elixir.Qx1"], do: String.to_atom(n)
# graph: Qx1 <- Qy2 <- Qz3 with parser-shaped imports
Vaisto.Build.DependencyResolver.topological_sort(graph)  # => order [Qz3, Qy2, Qx1]

# D11 / D12: backend gaps
Vaisto.Runner.run("(defn pos [x :int :when (> x 0)] :int x)\n(defn main [] :int (pos 5))", backend: :core)
# => {:error, %Error{message: "compilation error"}}   (:elixir => {:ok, 5})
Vaisto.Runner.run("(deftype Point [x :int y :int])\n(defn main [] :int (. (Point 1 2) :x))", backend: :elixir)
# raises FunctionClauseError   (:core => {:ok, 1})
```

D15–D23 (HM holes without `:any` in the source):

```elixir
check("(defn f [x :string] :int (do (let [x 1] x) (+ x 1)))")        # D15 => {:ok, {:fn, [:string], :int}}
Vaisto.Runner.run(~s|(defn f [x :string] :int (do (let [x 1] x) (+ x 1)))\n(defn main [] :int (f "hi"))|, backend: :core)
                                                                       #      raises ArithmeticError
check(~s|(defn f [x] :int (do (++ x "a") x))|)                        # D16 => {:ok, {:fn, [:string], :int}}
check(~s|(defn f [x] (+ x "s"))|)                                     # D17 => {:ok, {:forall, ...}}
check(~s|(deftype R (Ok :int) (Err :string)) (defn main [] :any (Ok "s"))|)   # D18 => {:ok, ...}
check(~s|(defn main [] :any (map (fn [x] (++ x "a")) [1 2]))|)        # D19 => {:ok, {:fn, [], {:list, :string}}}
check("(fn [x :int] x)")                                              # D20 => {:ok, {:fn, [tvar: 0, tvar: 1], ...}}
check("(defn get-x [r] (. r :x)) (defn main [] :any (get-x 5))")      # D21 => {:ok, ...}

# D22: nominal types not enforced through a parameter; enforced on a known scrutinee
check("(deftype A (X :int)) (deftype B (Y :int))\n(defn f [v] :int (match v [(X n) n]))\n(defn main [] :int (f (Y 1)))")
                                                                       # => {:ok, ...}
check("(deftype A (X :int)) (deftype B (Y :int))\n(defn f [] :int (match (Y 1) [(X n) n]))")
                                                                       # => {:error, ["non-exhaustive pattern match"]}
# D23: a record "type parameter" is misparsed
Vaisto.Parser.parse("(deftype Job p [id :int])")
# => {:deftype, :Job, {:product, [{:p, :any}, {{:bracket, ...}, :any}]}, %Loc{}}

# §16.3 phase probes (a, b, c, d, d2, f) use the same helpers with
# (deftype Executed [id :int]) (deftype Accepted [id :int]); results are in the §16.3 table.
```

Division semantics (Z3 5.1.0 and OTP 29):

```text
z3:     (div -7 2) = -4    (mod -7 2) = 1          ; SMT-LIB, Euclidean
erlang: -7 div 2   = -3    -7 rem 2   = -1         ; truncating
        7 div -2   = -3    7 rem -2   = 1
z3 with the ite-based ediv/erem encoding: -3, -1, -3, 1   ; matches Vaisto (§4.2), which Erlang also implements
```

OTP guidance for language implementors (§7.1), read from OTP 29's own documentation:

```erlang
{ok, {docs_v1, _, _, _, #{<<"en">> := MD}, _, _}} = code:get_doc(compile),
[_, After] = string:split(MD, <<"## Recommendations for Language Implementors">>).
%% "Core Erlang: ... Primops can be added, deleted, or changed in any major release
%%  without notice. Note that by generating Core Erlang directly, it is possible to
%%  construct code that the Core-to-BEAM backend has never encountered before, and
%%  there are no guarantees that the final BEAM code will be safe."
%% "Our recommendation is to use either the abstract format or Erlang source."
```

Custom `.beam` chunks (§8.4):

```erlang
{ok, certdemo, Bin} = compile:file("certdemo.erl",
    [binary, {extra_chunks, [{<<"VSTY">>, <<"(cert (contract-digest \"abc123\"))">>}]}]),
beam_lib:chunks(Bin, ["VSTY"]).                    % => the chunk round-trips
code:load_binary(certdemo, "certdemo.beam", Bin).  % => loads; the chunk is never consulted
{ok, {certdemo, S}} = beam_lib:strip(Bin),
beam_lib:chunks(S, ["VSTY"], [allow_missing_chunks]).  % => [{"VSTY", missing_chunk}]
```

`mix help release` (Elixir 1.20.4): "`:strip_beams` - controls if BEAM files should have their debug information, documentation chunks, and other non-essential metadata removed. Defaults to `true` ... Also accepts `[keep: ["Docs", "Dbgi"]]`".

---

## Appendix B. Changes from draft 1

| Area | Draft 1 | Draft 2 | Why |
|---|---|---|---|
| Semantic authority | BEAM: the primitive table was "Erlang semantics", and assumption A2 said the table matches BEAM | Liquid Core and the reference evaluator; the backend must agree, and the harness checks | a refinement proof must be about a semantics; the backends disagree today |
| Structure | refinements layered on today's compiler | four-primitive core; everything else library | keeps the trusted core small enough to formalize |
| Where refinements live | a side table (`refine_sig` forms) beside the HM typed AST | Core types; HM never sees them; `erase` is Core to Core | the only consumers of Core types are Lint, the checker, erasure and the evaluator |
| Containment of HM holes | re-derive sorts inside the refined fragment (old M15) | Core Lint over the whole program | stronger and simpler; the GHC model |
| Backends | two emitters, patched to parity (old P0-4, P0-10) | one lowering to the Erlang abstract format | OTP's own guidance; removes the divergence class |
| Locations | `{:at, loc, node}` wrappers in the typed AST | spans in Core `(meta ...)` | no typed-AST shape changes |
| Effects | not in Phase 1; purity typing and record/replay were separate steps | an algebra of requests in the core semantics; replay determinism is semantic; the effect type system in Phase 4 | one mechanism instead of two |
| `:any` | opaque to the logic, but still everywhere | removed; `Dyn` with checked `decode` | a silent top-and-bottom type makes soundness claims false |
| Externs | result refinements forbidden | result types and refinements become runtime decode targets | a declaration is checked instead of trusted |
| Inference engines | left separate | must merge (Phase 1) | Core cannot type fallback lambdas |
| Interfaces | `.vsi` version 3 via `term_to_binary` | canonical S-expression interfaces with digests | stable, safe, diffable, hashable |
| Evidence | five kinds of truth | three origins; derived facts carry premises; stochastic information is never a fact | smaller; same guarantees |
| Authority, phases, transitions, budgets | stages of refinement work | a standard library with laws (§16) | "does it need new theory?" is no |
| `.beam` metadata | not discussed | `VSTY` admission record, with verified limits | extends interface provenance to load time |
| Roadmap | Phase 0 = a list of 22 defect fixes | Phase 0 builds the oracle; defect fixes become its seed corpus and regression tests | finds the next defects automatically |
| Numbering | — | sections reorganized; D1–D23 unchanged; Q-numbers kept stable with a status column; mechanisms renumbered M1–M21 | — |
