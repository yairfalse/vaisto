# RFC: Liquid Vaisto

- **Status:** Draft 4 for discussion, amended: the type system is now `liquid-types.md` (§0.5, §9.1). Phase 0 is implemented: `liquid-core.md`, `lib/vaisto/liquid/`.
- **Date:** 2026-10-01
- **Supersedes:** draft 3 (merged in PR #6), after a second adversarial review. Appendix C lists the findings of both reviews and their fixes. Draft 3 superseded draft 2 (2026-09-30). Draft 2 superseded draft 1, written earlier the same day. Draft 1 layered refinements on top of today's compiler and treated BEAM behaviour as the definition of the primitives. Review of that draft produced the central idea of this one: **Vaisto has a semantics; BEAM implements it.** Appendix B lists every change.
- **Baseline:** `origin/main` at `0ec7a82`. Test baseline: 1268 passing, 1 excluded, 1 intermittent failure (D10).
- **Scope:** the fourteen items of the brief's "Immediate assignment", and the parts of the brief that constrain them.
- **Notation:** `{v:B | p}` is a refined type: base type `B`, predicate `p` over the value `v`. Snippets that use refinements use **provisional syntax** (§4.4); the surface syntax is an open question (Q1).

---

## 0. Summary and verdict

**Question.** Can Liquid Vaisto be made sound enough, small enough, and native enough to the existing compiler to become Vaisto's next type-system layer?

**Answer, part by part:**

- **Sound enough: yes**, under seven named assumptions (§4.4), none of them hidden.
- **Small enough: yes.** The core is four primitives over one type algebra: unit, labelled products and sums, functions with effect rows, and named types as branded rows. Authority, transitions and budgets are library theories with laws (§16).
- **Native enough: to the language, not to the compiler.** The surface syntax, HM inference and the BEAM runtime stay. The compiler's middle and back end do not: Core, Core Lint, the reference evaluator and a single new backend are new, and today's checker becomes an untrusted front end.

The reason is that today the meaning of a Vaisto program is whatever one of its two backends happens to produce, and the two disagree (D5, D11, D12, D15, D24). A refinement proof has to be a proof *about something*. This draft therefore puts a small mathematical core at the centre, **Liquid Core**, and demotes BEAM from "the definition" to "the production implementation".

### 0.1 The core: four primitives

| Primitive | What it is | Why it has to be in the core |
|---|---|---|
| **Values** | a deterministic ML-style language: literals, functions, application, `let`, constructors, records, `match`, primitive operations | everything else is about values; primitive operations get *Vaisto* semantics |
| **Refinements** | subsets of and relations between values, `{v:B \| p}`, checked statically and erased | only the core can say which values are admissible and what a function guarantees |
| **Effects** | computations are the free algebra over a small signature of operations; handlers are algebras | purity typing and replay determinism both follow from it; the world enters only here |
| **Evidence origin** | every fact the logic may use is `Static`, `Imported` or `Observed` | "a claim is not a fact" must be enforced by the language, not by convention |

Everything else is **library**: authority, capabilities, budgets, workflow phases, transitions, jobs, credentials, leases, contracts, receipts, accepted states, pipelines. The test for any proposed feature is: **does it need new theory in Liquid Core?** The expected answer is no. Beside the four primitives sits a module system (names, opacity, sealing, interfaces), and the library needs one small piece of new refinement theory: invariants and abstract measures of opaque types (§4.1, §4.8). A new external effect extends the effect algebra; a new logical operation extends the refinement logic; almost everything else is a type, a refined function and a law in the standard library (§4.8, §16).

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
            │ differential conformance: BEAM(P, I) = (R, T), then evaluator(P, I, replay T) = R
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

1. **The core and its reference evaluator come before refinements.** They are the oracle. The 24 defects in §1.12 (22 reproduced by execution, D13 and D14 confirmed by reading) stop being a hand-maintained bug list and become the seed corpus of a differential harness. The harness finds most defect *classes* automatically: backend divergences and elaborator bugs. It does not find parser misreadings or interface bugs, which have their own tests (§6.3, §6.4).
2. **`:any` becomes `Dyn`**, a real dynamic type with no implicit conversion in either direction. Values cross into the typed world only through checked `decode` (§4.6). A silent top-and-bottom type makes any refinement soundness claim false.
3. **Construction privacy (`opaque`) exists.** Workflow phases are named types, relations are refinements, and "only admission can construct `Accepted`" is constructor privacy. These are three different mechanisms, and the third is not refinement typing (§4.1, §16.3).
4. **One backend.** A single lowering to the abstract format replaces the two emitters, once the harness shows it agrees with the evaluator (§7).

### 0.5 Central decisions

| Decision | Choice | Evidence or reason |
|---|---|---|
| Semantic authority | Liquid Core, executed by the reference evaluator; a backend is correct if and only if it agrees | the backends disagree today (D5, D11, D12, D15, D24) |
| Type inference | **HM retires.** A bidirectional elaborator reads each type former's universal property: introductions are checked, eliminations synthesize. Unification stays, but only locally, to instantiate at an elimination site. Top-level definitions carry signatures, and **Core Lint** re-checks the output with the same rule table. See `liquid-types.md` | HM accepts ill-typed programs (D15–D19, D21, D22); over the test suite, 21 of the 25 programs Lint rejects are HM generalizing what it never constrained |
| Refinements | live in Core types; HM never sees them; erased Core-to-Core before lowering | emitters and `Unify` pattern-match type terms structurally (§1.9) |
| Effects | algebraic requests; handlers are the BEAM runtime or a trace replayer | replay determinism becomes a property of the semantics (§4.5) |
| Type algebra | unit, labelled products (rows) and sums, functions with effect rows, named types as branded rows, `Pid`, `Map`, `Dyn` | covers the whole surface language; rows and nominal phases coexist (§4.1) |
| `:any` | replaced by `Dyn`, with an embedding–projection pair per decodable type | §4.6, §15 |
| Typed messages | a typed `receive` is the projection from `Dyn` | no assumption about who sent a message (§4.6) |
| Library | algebraic theories: signature, laws with recorded status, hidden models; reflection for constants | authority scales to named resources (§16) |
| Roadmap | refinements start right after Phase 0, through a checked commuting-square bridge | the first demo does not wait for the new backend (§21.1) |
| Backend | one lowering to the Erlang abstract format | §7.1 |
| Artifacts | one canonical S-expression format; digests over canonical bytes, excluding spans | §5.1 |
| Solver | Z3 as an external process over SMT-LIB; `unknown` is never `proved` | §11 |
| `.beam` metadata | a `VSTY` chunk is a *typed artifact admission record*, not a certificate; only the Vaisto loader enforces it | verified: `code:load_binary` ignores it and `beam_lib:strip` removes it (§8.4) |

The long-term target is the statement from the design review: **any execution admitted by the Liquid Vaisto semantics satisfies the properties encoded in its verified core, assuming the explicitly enumerated trusted observation and backend boundaries.** §17.4 lists the component theorems.

### 0.6 Tensions with the brief, stated plainly

- **"Not a rewrite of Vaisto."** The surface language stays, and programs keep their meaning except where today's meaning is a bug. HM does not stay: top-level definitions gain signatures, inserted mechanically from HM's own suggestions and then checked (`liquid-types.md` §13). The compiler's back half *is* replaced: two emitters become one lowering. This is staged: the new lowering runs beside the old emitters, and each old emitter is retired only when the harness shows parity (§7.3).
- **"Full effect system" is a first-implementation non-goal.** The *semantics* of effects is in the core from the start, because determinism needs it. The *effect type system* starts minimal and arrives in Phase 4, after refinements (Phase 2).
- **"Proof-carrying BEAM bytecode" is a non-goal.** `VSTY` carries hashes and a signature, not proofs, and nothing is proved at load time (§8.4).
- **The first refinement target** (`Nat`, `safe-div`, `clamp`, bounded index) still comes first among refinement work, right after Phase 0. It never reasons directly about today's HM output, which would verify programs that crash (D15, D16). It reasons about Core that Core Lint has checked, while the programs still run on today's emitters through a bridge whose correctness is checked per program (§21.1).

### 0.7 Where each item is answered

| Assignment item | Topic | Section |
|---|---|---|
| 1 | Map of the existing type-checking architecture | §1 |
| 2 | Minimum refinement calculus | §4.4 (inside Liquid Core, §4) |
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
| §5, §18 | small logic, admission rule per primitive, predicate IR, SMT as a tool | §4.3, §5.2, §11 |
| §7, §20 | kinds of truth; where each fact came from | §4.7 |
| §8, §31 | trusted observations; the worker cannot mint evidence | §4.7 |
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
- There are **five independent paths from source to BEAM**, plus several type-check-only entry points, and the new pipeline (§3) must replace all of them, or some path will silently bypass it:
  1. `Compilation.compile/3` (CLI, `Runner`).
  2. `Build.Compiler.compile/3`, which calls `Compilation.typecheck/2` and `Compilation.emit/4` separately (`build/compiler.ex:149,164`) and writes `.vsi` from the typed AST after `emit` (`build/compiler.ex:74-76`).
  3. The REPL: `Compilation.emit(typed_ast, name, :core)` (`repl.ex:274-275`).
  4. `Vaisto.compile_string/2`: `TypeChecker.check!` then `Backend.Core.compile` directly (`vaisto.ex:24-31`).
  5. `Vaisto.Backend.compile/4` (`backend.ex:79-87`), unused by `Compilation` but public.

  Type-check-only entry points: LSP hover (`lsp/hover.ex:274`), LSP diagnostics (`lsp/handler.ex:210`), inlay hints (`lsp/inlay_hints.ex:41`), `Vaisto.check/1` (`vaisto.ex:36-40`) and `TypeChecker.infer/2` (`type_checker.ex:160-161`).
- Default backends differ by path: CLI and `Build.Compiler` default to `:core`; `Runner` defaults to `:elixir` (`runner.ex:64,170`). The test suite therefore exercises mostly the Elixir backend for end-to-end behaviour.

### 1.2 Type representations

Declared in `@type vaisto_type` (`type_checker.ex:19-37`) and used more widely than declared:

| Term | Meaning | Notes relevant to refinements |
|---|---|---|
| `:int :float :string :bool :atom :unit :any` | primitives | `:int` is an unbounded BEAM integer. |
| `:num` | int-or-float | Produced by numeric fallback (`unify_types_s`, 3202-3216). No single logical semantics. |
| `{:atom, a}` | singleton atom | Two *different* singleton atoms unify (`unify.ex:136-139`), so `(if c :read :write)` type-checks with the then-branch's singleton `{:atom, :read}` as its type. HM gives no discrimination between atom literals. |
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
- **Guards** (`(defn f [x :int :when (> x 0)] ...)`) exist: the parser splits at `:when` (`parser.ex:991`) and the checker requires a `Bool` guard (956-957). Guards are runtime-only today: `(pos -5)` type-checks, then raises `function_clause` on `:elixir`; on `:core` guarded definitions do not compile at all (D11).
- **Calls** go through one choke point, `unify_call_poly_s` (3289-3340), which unifies each argument against the parameter type. `:any` on either side is accepted (3302-3306), and a type-variable parameter that meets `:any` is bound to `:any` (3304). In the new design HM only elaborates; precondition obligations attach to calls in Liquid Core (§4.4), not here.
- **`if`** (660-668): the condition must be `Bool`; branches are checked in the same env; no path information is recorded.
- **`match`** (671-676, 2704-2753): each clause extends the env with pattern bindings and restores it afterwards. A clause knows nothing about the earlier clauses having failed.
- **`let`** (695-705, 2636-2701): bindings are generalized for let-polymorphism; no equation `x = e` is recorded.
- **Lambdas** (1033-1054) are delegated to Engine B (next section).
- **Instances and deriving** still use a legacy context-free path, `check_impl/2` (1238-1395), called with `ctx.env` only.
- **Typed AST has no locations.** `check_s` strips `%Loc{}` and calls `with_loc_ast(ast, loc)`, which is a no-op placeholder (280). Errors get a location from the nearest located node while checking, but the typed AST handed to backends carries none.

### 1.4 Engine B: `Vaisto.TypeSystem.Infer`

- Algorithm W with its own `TypeSystem.Context` and **its own primitives table** (`infer.ex:26-43`), which duplicates `TypeEnv.primitives` rather than reading it.
- Invoked from `check_impl_s({:fn, params, body})` (1033) as `Infer.infer({:fn, params, body}, ctx.env)`, and directly by `TypeChecker.infer/2` (160-161), which tests use. It receives the env but **not** the `TcCtx` substitution or counter, and its own substitution is not merged back.
- On failure the message is classified by `infer_should_fallback?/1` (1203-1207). Messages starting with "unknown expression", "unknown function", "undefined variable" or "cannot call non-function" trigger `fallback_lambda/3` (1210-1231), which re-checks the body with **every parameter typed `:any`** and returns `{:fn, [:any, ...], ret}`. Everything else is reported as a genuine error.
- Qualified calls inside lambda bodies: a known `{:fn, _, ret}` returns `ret` **without unifying the arguments**; an unknown key returns `:any` (`infer.ex:306-336`).

### 1.5 Unification

`Unify.unify/4` is symmetric structural equality with row polymorphism. Its only non-equality relations are:

- `:any` unifies with anything **without binding** (127-128). A tvar meeting `:any` is bound to `:any` earlier in the `cond` (31-42), so `:any` propagates through inference.
- `:num` accepts `:int` and `:float` (131-132).
- Singleton atoms unify with `:atom` and with each other (136-139).
- PIDs compare only process names (142-149).

There is no subtyping. Refinement subtyping (§4.4) must be a separate relation layered on HM results, never a change to `Unify`.

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

**What this means for refinements.** Field projection appears in predicates only on nominal records, as a datatype selector (§4.4, Phase 2). A field access on a row-typed value is an uninterpreted function of the value and the label (§4.4); on a tvar or `:any` value it is opaque. Workflow-phase typing (§16.3) needs nominal parameters, because rows would erase the distinction between phases.

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

Several mismatches fall through **silently**: a constructor call whose type slot is not exactly `{:record, same_name, _}` or `{:sum, _, variants}` falls through to the generic call clause, which for records calls a function that does not exist (`undef` at runtime; sum constructors on Core Erlang are also exported as functions, so that case may survive); field access on anything but `{:record, ...}`/`{:row, ...}` becomes `maps:get` (`badmap` at runtime). Type-system helpers have the same shape: `Core.apply_subst/2` (109) and `Core.free_vars/1` (164) end in catch-alls that return the term unchanged / an empty set.

**Consequence for this RFC:** any new constructor inside HM type terms (for example `{:refined, v, base, pred}`) would silently change code generation, hide type variables from generalization, and leak into the LLM runtime schema. This is why HM type terms never carry refinements (§9). Refinements live in Liquid Core types, which only Core Lint, the refinement checker, erasure and the evaluator see; erasure removes them before lowering (§7).

### 1.10 Task-contract forms

`defprompt` is metadata; `pipeline` and `generate` are compiled only by `Vaisto.Emitter`. CoreEmitter has no clauses for them; inside a module they fall into the `main/0` body and raise `FunctionClauseError`, which `CoreEmitter.compile/3` rescues into a generic "compilation error" (`core_emitter.ex:57-58`). The design documents describe `defcontract`, `Ctx`, and eleven operators; **none of `requires`, `ensures`, invariants, receipts, predicates, or Lean appear anywhere in the design documents.** `README.md:272` and `README.md:342-343` mention "optional constraint solving" and Z3 as a possible future backend.

### 1.11 Escape hatches (`:any` and friends)

`:any` behaves as both top and bottom. `Unify` accepts it on either side (and deeply: `(List Any)` unifies with `(List Int)`), and `join_types(:any, t) = t` (3901-3902) turns it back into a precise type. It is produced at roughly a hundred code sites across sixteen categories. The largest are `defn_multi` (every pattern variable is bound to `:any`, so a recursive `len` gets type `Any -> Any`), dynamic list operations, pattern fallbacks, and missing annotations. The full inventory with counts is in §15.5.

Explicit `:any` parameters are not dynamic: `freshen_any_params` (3932-3940) turns a top-level `:any` parameter into a fresh type variable, so `[x :any]` means the same as `[x]`. Nested `:any`, as in `(List :any)`, is not freshened.

### 1.12 Verified defects that bear on refinement soundness

Each item was reproduced by execution against `0ec7a82` unless marked *(read)*, and reproduced again by an independent fact-check. Appendix A gives commands for most items.

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
| D9 | Cross-module typing is inert: `.vsi` keys never match lookups; the build env merge replaces the built-in `__classes__`/`__instances__` with empty maps; exports are monotypes with free tvars (unified without instantiation, so the first importer use fixes them); guarded `defn`s are not exported; `binary_to_term` without `[:safe]`; no staleness detection. After an import, polymorphic `+` and `<` fail with "no instance of `Num`/`Ord` for type `Int`" and `(show 1)` loses its class dispatch. An exported `{:forall, ...}` scheme would hit `extern_not_a_function` (731-732). | Audit of `interface.ex`, `build/compiler.ex`, `build.ex`; key mismatch and class wipe reproduced. | Refinement summaries need a working interface layer to travel in. |
| D10 | `DependencyResolver` stores parsed imports (`:A`) but keys the graph by `:"Elixir.A"`, so no in-degree is ever non-zero; the "topological" order is `Map.keys` order, which for atom keys on OTP 26+ is atom-creation order. Cycles in a graph built from files are never reported. A hand-built graph with `Elixir.`-prefixed imports is reported, which is why the existing cycle test passes (`integration_test.exs:127-142`). This is the cause of the intermittent failure at `test/build/integration_test.exs:124`: whether it passes depends on which earlier test first created `:"Elixir.B"` or `:"Elixir.C"`. | Appendix A: atoms created as `Qz3, Qy2, Qx1` with dependencies `Qx1 ← Qy2 ← Qz3` sort to `[Qz3, Qy2, Qx1]`, dependents first. | Modular checking assumes dependencies are checked first. |
| D11 | Guarded `defn` (the checker's 6-tuple, `type_checker.ex:968`) is not in CoreEmitter's top-level form list (`core_emitter.ex:196,234`); on `:core` it fails with "compilation error". On `:elixir` it works. | Appendix A. | Guards are the natural runtime counterpart of static preconditions. |
| D12 | Record field access raises `FunctionClauseError` on the `:elixir` backend (no `{:field_access, ...}` clause in `emitter.ex`); it works on `:core`. | Appendix A. | Field projection is the first thing capability predicates use. |
| D13 | Typed AST carries no source locations (`type_checker.ex:280`). | *(read)* | Refinement diagnostics must point at call sites inside bodies. |
| D14 | `apply_subst_to_ast` has no clause for the guarded `defn` 6-tuple (it has only the 5-tuple at 1902); guarded definitions pass through unsubstituted. | *(read, agent)* | Refinement pass would see unresolved tvars in guarded functions. |

The `:any` audit (§15.5) found further holes that let ill-typed programs through **without any `:any` in the source**. The ones below were executed against `0ec7a82`:

| ID | Defect | Reproduction | Why it matters here |
|---|---|---|---|
| D15 | Environments leak out of `let` (and `try`, `receive`) in the checker. The Core Erlang emitter scopes `let` lexically, but the Elixir emitter does not: it emits `x = v; body` as a plain block (`emitter.ex:147-158`), so the backends disagree too. | `(defn f [x :string] :int (do (let [x 1] x) (+ x 1)))` is accepted as `String -> Int`; `(f "hi")` raises `ArithmeticError` on `:core` and returns `2` on `:elixir`. `(defn f [x :int] :int (do (let [x 100] x) x))` gives `(f 1)` = `1` on `:core` and `100` on `:elixir`. | HM's type for a variable can disagree with the binding the code actually uses. |
| D16 | The declared return type is compared with a stateless unify (`types_unifiable?`, 3193), before the body's substitution is applied. | `(defn f [x] :int (do (++ x "a") x))` is accepted as `String -> Int`. | A refined caller would reason about an `Int` that is a string. |
| D17 | A numeric op on a tvar returns the other operand's type without unifying (`check_numeric_op`, 3098-3099). | `(defn f [x] (+ x "s"))` is accepted; `(defn g [x] (+ x 1))` makes `(g 1.5)` an `Int`. | Int facts about floats. |
| D18 | Concrete field types of sum constructors are replaced by fresh tvars (881-895, 3539-3566). | With `(deftype R (Ok :int) (Err :string))`, `(Ok "s")` is accepted. | Selector facts about constructor fields would be wrong (Phase 2). |
| D19 | Higher-order builtins never relate the function's parameter to the list element (2878-3034). | `(map (fn [x] (++ x "a")) [1 2])` is accepted as `(List String)`. | List element sorts would be wrong. |
| D20 | Lambda parameters cannot be annotated; the annotation becomes a second parameter. | `(fn [x :int] x)` has type `{:fn, [t0, t1], t0}`. | Refined lambdas need annotated parameters. |
| D21 | Row constraints from field access are recorded but never unified (408-434). | `(defn get-x [r] (. r :x))` then `(get-x 5)` is accepted. | Field projection on non-records. |
| D22 | Nominal types are not enforced at function boundaries. An unannotated parameter is a type variable: matching it against constructor patterns of type `A` accepts a value of type `B`, and exhaustiveness is skipped because the scrutinee type is unknown. With a known scrutinee type both are checked. | With `(deftype A (X :int))` and `(deftype B (Y :int))`: `(defn f [v] :int (match v [(X n) n]))` then `(f (Y 1))` is accepted, while `(match (Y 1) [(X n) n])` is rejected as non-exhaustive. | Workflow phases cannot be enforced (§16.3). |
| D24 | Row-polymorphic field access crashes at runtime on both backends: row-typed access compiles to `maps:get` (`core_emitter.ex:1191`) but records are tagged tuples, and the Elixir emitter has no field-access clause. | `(defn get-x [r] (. r :x))` applied to `(Point 1 2)` type-checks, then raises `BadMapError` on `:core` and `FunctionClauseError` on `:elixir`. | A documented feature (row polymorphism) does not work; fixed by row evidence (§4.1). |
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
2. **Four primitives, plus a module system.** Values, refinements, effects, evidence origin; and names, opacity, sealing and interfaces. Anything else must justify why it cannot be a library over them (§4.8).
3. **The worker does not define success and does not supply its own evidence.** Only the refinement checker (static) and privileged observers (runtime) establish facts. A claim is not a fact.
4. **Erasure preserves meaning.** For every verified pure term, evaluating it and evaluating its erasure give the same value (§7.2).
5. **No silent dynamic typing.** `Dyn` converts to nothing implicitly; `decode` is the only way in (§4.6).
6. **No general macros; no `assume`.** The trusted axioms are enumerable: the primitive table (§4.3), observer axioms (§4.7), and library laws whose status is `tested` or `assumed` (§16.1). Each is declared and recorded; ordinary code cannot add one.
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
materialize   : VerifiedCore    -> VerifiedCore               runtime checks made explicit code (§7.2)
erase         : VerifiedCore    -> Core⁻                      drops refinements; total, syntactic
lower_effects : Core⁻           -> LoweredCore                effect requests become direct handler calls
emit          : LoweredCore     -> AbstractErlang             the only backend
compile       : AbstractErlang  -> BEAM                       OTP compile:forms/2
```

| Boundary | Invariant | How it is established |
|---|---|---|
| `lint` accepts | the Core term is well-typed, with no inference involved | Core Lint, a small checker (§9.2) |
| `verify` accepts | every refinement obligation is valid | refinement checker plus Z3 `unsat` (§11) |
| `materialize`, then `erase` | `eval(materialize(e)) = eval(erase(materialize(e)))` for every verified pure term `e` | by construction; later a Lean theorem (§17.4) |
| `lower_effects` | the sequence of effect requests, and how their results are used, is unchanged | differential test now; theorem later |
| `emit` + `compile` | BEAM agrees with the reference evaluator | differential harness (§6.3); never assumed |

The **reference evaluator** (§6) runs on Core, before or after erasure; the erasure invariant (after materialization) says the answer is the same. The LSP and the REPL stop after `lint` or `verify`; hover types come from Core types, not from HM's internal terms.

### 3.2 One pipeline instead of five compile paths

§1.1 lists five independent paths from source to BEAM: `Compilation.compile/3`, `Build.Compiler`, the REPL, `Vaisto.compile_string/2`, and LSP hover. All of them are replaced by calls into a single pipeline module. A path that skips `verify` for a module with obligations (§4.4) cannot reach `emit`. `erase` refuses Core that still carries refinements unless it is marked verified, so a skipped pass fails loudly.

### 3.3 Staging with the existing compiler

The front end stays. Phase 0 adds Liquid Core, the evaluator, Core Lint and an **adapter** from today's typed AST to Core for the pure fragment. The harness can then run every existing test program through the evaluator and both current backends without changing the parser or HM (§21). Phase 1b adds the abstract-format lowering beside the old emitters and retires each emitter only when the harness reports parity on the whole corpus (§7.3). Until then, refined programs run on the old emitters through the bridge of §21.1.

---

## 4. Liquid Core (assignment item 2)

Liquid Core is the language. The surface language elaborates into it; the evaluator runs it; the refinement checker reasons about it; the backend lowers it. It has four primitives: values, refinements, effects and evidence origin. Beside them sits a **module system**: names, opacity and interfaces. Every language needs one, and the four primitives assume it. Stated plainly, that is a fifth mechanism (§4.8). Core's types form one small algebra (§4.1), and every construct of today's surface language, including rows, typed processes, maps and `try`, has a place in it.

### 4.1 The type algebra

Core has three kinds: `Type`, `Row` and `Eff`. Effect rows reuse the row algebra, so one mechanism serves both records and effects.

```text
τ ::= Int | Float | String | Atom | Ref | Dyn          base sorts
    | Unit                                             the empty product (1)
    | {l₁: τ₁, …, lₙ: τₙ | ρ}                          labelled product: a record row
    | <C₁: τ₁ | … | Cₙ: τₙ>                             labelled sum (closed)
    | (Tuple τ₁ … τₙ)                                  positional product: labels 0 … n-1, closed
    | (-> ((x₁ τ₁) … (xₙ τₙ)) ε τ)                     dependent function with effect row ε:
                                                       later τᵢ and the result may mention earlier xⱼ
    | (T τ …)                                          named type (§ below); may be recursive
    | (Pid M)                                          a process that accepts protocol M
    | (Map τ τ)                                        homogeneous finite map
    | (forall ((a Type) (ρ Row) (e Eff) …) τ)
    | {v: τ | p}                                       refinement: a subset of τ (§4.4)

ρ ::= ∅ | ρ-variable | (l: τ, ρ)                       rows: finite maps from labels to types
ε ::= ∅ | ε-variable | (E, ε)                          effect rows over the effect labels of §4.5
```

The short form `(-> (τ₁ … τₙ) ε τ)` abbreviates a dependent arrow whose parameter names are unused; most signatures in this document use it.

**Equality is syntactic up to label order.** Two rows are equal when they have the same labels with equal types; order does not matter and duplicate labels are not allowed. There are no implicit isomorphisms: `(Tuple Int Int)` is not the same type as `{0: Int, 1: Int}` written by hand. Core Lint compares types by normalizing rows (sorting labels) after substituting explicit instantiations, so it never has to infer.

**Familiar types are instances**, not primitives:

- `Bool` is `<true: Unit | false: Unit>`.
- `(List a)` is the named recursive sum `<Nil: Unit | Cons: (Tuple a (List a))>`.
- `Result` and `Option` are library sums.
- `:num` disappears (§4.2).

**Named types are branded rows.** `(deftype Point [x :int y :int])` defines

```text
Point ≅ {#Point: Unit, x: Int, y: Int}
```

The **brand** `#Point` is a label that only `Point`'s constructor produces. Three things follow:

1. **Row polymorphism still works.** `get-x : (forall ((a Type) (ρ Row)) (-> ({x: a | ρ}) ∅ a))` accepts a `Point`, because the brand and `y` are absorbed by `ρ`.
2. **Nominal distinctions are enforced.** A function that requires `Accepted`, the closed row `{#Accepted, id: Int}`, rejects `(Executed 1)` even though the fields are identical: the rows differ in their brand label. This is how workflow phases (§16.3) and row polymorphism coexist, which resolves Q16.
3. **The algebra mirrors the representation.** On BEAM a record is a tagged tuple, and the brand *is* the tag.

**Brands are module-qualified.** The brand is `#Geometry.Point`, not `#Point`, and so is the runtime tag. Two modules that each define `Point` therefore get distinct types *and* distinct runtime values, and `decode` can tell them apart. Today's backends tag with the bare name, so this is a representation change visible to Erlang code that inspects Vaisto records (Q32).

A named sum `(deftype Job (Proposed d) (Accepted d r))` is the closed labelled sum `<Proposed: … | Accepted: …>` under the name `Job`; its constructors are its labels.

**Opaque types** (`(deftype opaque T ...)`, promised in DESIGN.md:162-175, misparsed today, D7) export `T` as an abstract type: no row, no brand, no constructors, no patterns. Outside the defining module, `T` values can only be passed around and handed to the module's functions. Core Lint enforces this. Three library designs depend on it:

- only admission constructs `Accepted` (§16.3);
- sealed witness types (§4.7);
- smart constructors whose refined parameters are the only way to build a value.

Opacity is static; BEAM tuples are transparent. A **sealed** type is an opaque type whose values also carry an authenticator that the defining module checks whenever a value arrives from outside (§4.6). Sealed types are how the library represents things that must not be minted: observation witnesses, authority and accepted states.

**Invariants and abstract measures of opaque types.** An opaque type may declare a **type invariant**, a predicate over its hidden representation, plus **abstract measures**: functions of the type exported to the logic as uninterpreted symbols, such as `(before t)` and `(after t)` for a transition.
- Inside the module, every constructor and every function returning the type must establish the invariant (checked, or carrying a law status, §16.1).
- Outside, clients may assume the invariant of every value of the type, stated in terms of the abstract measures, and can write refinements over those measures without seeing the representation. A value that arrives in a message has been decoded against the invariant (§4.6), so the assumption also holds for values that came from outside.

This is the one piece of new refinement theory that the library of §16 needs, and §4.8 says so.

**Row evidence.** A row-polymorphic function does not know where field `x` lives in the record it receives. At each instantiation the elaborator passes **row evidence**, a hidden argument that says how to reach each label the function uses, in exactly the way class constraints receive dictionaries (the evidence-passing compilation of rows, Gaster and Jones 1996). The evidence is an *accessor*, so it works for both representations:

- branded records: `element(N, R)`;
- anonymous rows, today's atom-keyed map literals, which stay Erlang maps for interoperability: `maps:get(x, R)`.

When the representation is known statically, the accessor is inlined. Today's backends instead guess the representation from the static type and emit `maps:get` for rows. That is **D24**: `(defn get-x [r] (. r :x))` applied to `(Point 1 2)` type-checks and then crashes on both backends (`BadMapError` on Core Erlang, `FunctionClauseError` on Elixir).

**Maps.** `(Map k v)` is the homogeneous finite map, the type of dynamic Erlang maps. An atom-keyed literal with fixed keys, `#{:port 8080 :host "x"}`, is an anonymous closed row, and DESIGN.md's untyped map is `(Map Atom Dyn)`.

**Processes.** `(Pid M)` is the type of a process that accepts protocol `M`, a sum of message constructors. `Pid` is contravariant in `M`: a process that accepts more messages can stand in for one that accepts fewer. Core treats it invariantly at first; an explicit restriction coercion can come later (Q29). How messages are typed at the receiving end is §4.6.

### 4.2 Terms and evaluation

```text
e ::= x | literal
    | (fn ((x τ) …) e) | (app e e …) | (let ((x τ e)) e) | (letrec ((f τ e) …) e) | (inst e τ …)
    | (tuple e …) | (record T? (l e) …) | (select e l)
    | (inj T C e) | (match e (p g? e) …) | (if e e e)
    | (prim op e …)
    | (perform op e …) | (handle-crash e ((x Reason) e))     ; §4.5
    | (up τ e) | (decode τ e)                                ; §4.6
p ::= _ | x | literal | (tuple p …) | (record T? (l p) …) | (inj T C p) | (as x p)
g ::= a guard: a guard-safe expression (below)
```

- **Control forms.** `if` and `match` are control forms: only the chosen branch is evaluated. `(and a b)` is `(if a b false)` and `(or a b)` is `(if a true b)`, so short-circuit evaluation is a consequence of the grammar, not a property of a primitive. `do` is `let` with an unused binder, and a list literal is nested `Cons`.
- **Access and recursion.** `(. r :x)` is `select` through row evidence (§4.1). Top-level definitions and local recursion are `letrec`.
- **`up`** injects a value into `Dyn` (§4.6). It is needed, for example, to pass arguments to `external`.

- **Typeclasses, and `:num`, are elaborated away.** A class is a record of functions and an instance is a value of that record; a constrained function takes the dictionary as an argument. Row evidence and dictionaries are one mechanism: hidden arguments inserted at `inst`. `:num` becomes the `Num` class with instances for `Int` and `Float`. **A class may declare laws, and only a lawful instance's operations enter the logic.** `Num Int` satisfies the commutative-ring laws. `Num Float` satisfies none (IEEE arithmetic is not associative) and never appears in a refinement. The same rule covers `Eq`: derived `Eq` is structural equality and lawful by construction; a hand-written `Eq` instance is not usable in the logic.
- **Evaluation order is defined by Core.** Core evaluates left to right, call-by-value. The elaborator names every sub-expression that may perform an effect, `crash` included, with a `let` (A-normal form). It does so **within that sub-expression's own evaluation context**: a sub-expression of an `if` branch is named inside that branch, never hoisted above the `if`. The order of effects and crashes therefore never depends on the order the Erlang compiler uses for call arguments, and short-circuiting is never undone by hoisting.
- **Crashes are an effect** (§4.5). A failed match, a division by zero or `head` of an empty list *performs* `crash`. `try` elaborates to `handle-crash`, and `after` to sequencing on both paths. A function that performs no effect at all, not even `crash`, is total. Refinements are how `crash` is proved unreachable (§4.4).
- **Crash reasons are normalized** to a small closed set: `badarith`; `badarg` (what `hd`, `tl` and `element` raise); `no_match` for any failed match, whichever BEAM construct reports it (`case_clause`, `function_clause` or `badmatch`); `bad_decode`; `user`; and `raised` for foreign exceptions. Merging the match failures is what lets the evaluator and the backend agree on reasons whichever construct the lowering chose. Resource exhaustion (`system_limit`, out-of-memory) is outside the semantics.
- **Guards** (`:when` on definitions, and guards in `match` and `receive` clauses) are restricted to guard-safe expressions: comparisons, arithmetic, type tests and selectors, as BEAM guards are. They may not call user functions. **A crash inside a guard makes the guard false**, which is Erlang's rule, so the lowering can always emit a real BEAM guard. In the logic a guard `g` therefore means **`def(g) ∧ g`**, where `def(g)` collects what `g` needs in order not to crash: non-zero divisors, the testers under its selectors, and its type tests. That is the meaning used both when a guard adds a fact and when a failed guard is negated (§10.3).
- **`Float` and `String` are values but not logic sorts** (§4.4).

### 4.3 Primitive semantics (normative)

Every primitive has a Vaisto definition. **The reference evaluator implements it; the lowering must reproduce it on BEAM; a differential test per primitive checks that it does.** Where Erlang behaves differently, the lowering must compensate or the backend is wrong. This reverses draft 1, which defined the table as "Erlang semantics".

| Primitive | Crashes when | Result `v` |
|---|---|---|
| `+ - *` on `Int` | never (resource exhaustion is outside the semantics, §4.2) | `x ± y`, `x * y`, exact (unbounded) |
| unary `-` | never | `-x` |
| `div x y` | `y = 0` | **truncation toward zero**: `tdiv(x, y)` |
| `rem x y` | `y = 0` | `x - y * tdiv(x, y)` (sign of the dividend) |
| `/` on numbers | `y = 0` | `Float` quotient; not in the logic |
| `+. -. *.` and comparisons on `Float` | never | IEEE-754 binary64; no laws claimed; not in the logic |
| `++` on `String` | never | concatenation (a free monoid); not in the logic until strings are admitted |
| `< > <= >=` on `Int` | never | `v ⇔ x < y` etc. |
| `== !=` on two values of the same type | never | exact term equality (`=:=`); on `Float`, `0.0` and `-0.0` are different (verified on OTP 29, where `0.0 == -0.0` but not `0.0 =:= -0.0`) |
| `not` | never | `v ⇔ ¬x` |
| `and`, `or` | never | **short-circuit**: `b` is evaluated only when `a` does not decide the result |
| `length xs` | never | `v = len(xs)` |
| `empty? xs` | never | `v ⇔ len(xs) = 0` |
| `head xs` | `len(xs) = 0` | first element |
| `tail xs` | `len(xs) = 0` | `len(v) = len(xs) - 1` |
| `cons x xs`, `[h \| t]` | never | `len(v) = len(xs) + 1` |
| list literal of `n` elements | never | `len(v) = n` |

**Independence of the oracle.** The evaluator implements each primitive **from its definition in this table**, not by calling the Erlang operator it is being compared against, wherever the two could plausibly differ. `tdiv`/`trem` are computed from their defining equations, `and`/`or` by `match` on the first operand, and list operations by recursion on `Cons`. Where divergence is implausible (bignum `+`, `*` and comparison), the evaluator uses the host operator, and independence comes from the other two corners of a triangle: the SMT encoding, and law tests (the commutative-ring and total-order laws on random integers). Every primitive is therefore checked by at least two parties that do not share an implementation. Using the host operator everywhere would make the differential test compare BEAM with itself.

Truncating `div` happens to match Erlang, so the lowering is direct. Short-circuit `and`/`or` does **not** match what the Core Erlang backend emits today (a call to strict `erlang:and/2`, D5); under this definition that backend is wrong and the lowering must emit `andalso`/`orelse`. SMT-LIB's `div` and `mod` are Euclidean and differ for negative operands; Appendix A shows Z3 returning `(div -7 2) = -4` against Vaisto's `-3`, and the `ite`-based encoding that matches Vaisto on all sign combinations.

**Admission record.** The brief requires six things for every primitive the logic admits: syntax, IR, precise semantics, a lowering to the solver, tests, and a diagnostic. The table above gives syntax and semantics; this one gives the rest.

| Primitive | IR (§5.2) | SMT-LIB lowering | Diagnostic when its requirement fails | Test |
|---|---|---|---|---|
| `+ - *` | `(arith add\|sub\|mul a b)` | `(+ a b)`, `(- a b)`, `(* a b)` | — | evaluator vs BEAM vs SMT on random integers, including values above 2^64 |
| unary `-` | `(arith neg a)` | `(- a)` | — | same |
| `div` | `(arith tdiv a b)` | `tdiv` (Appendix A) | "`div` requires a non-zero divisor" | C10; every sign combination; zero divisors crash with the same reason on the evaluator and on BEAM |
| `rem` | `(arith trem a b)` | `trem` | "`rem` requires a non-zero divisor" | same |
| `< <= > >=` | `(lt a b)`, `(le a b)`; `>` and `>=` swap operands | `(< a b)`, `(<= a b)` | — | boundary values |
| `== !=` | `(eq a b)`, `(not (eq a b))` | `(= a b)` | — | one test per logic sort |
| `not and or` | `(not p)`, `(and p q)`, `(or p q)` | `(not p)`, `(and p q)`, `(or p q)` | — | C9; short-circuit parity test on BEAM |
| `length` | `(measure len xs)` | `(len xs)`, `len` uninterpreted `List -> Int` | — | C6 |
| `empty?` | `(eq (measure len xs) (int 0))` | `(= (len xs) 0)` | — | guarded `head` test |
| `head` | no result term | — | "`head` requires a non-empty list" | C6 |
| `tail` | fresh `t` with `len(t) = len(xs) - 1` | `(= (len t) (- (len xs) 1))` | "`tail` requires a non-empty list" | C6 |
| `cons`, literals | fresh `v` with its length fact | `(= (len v) (+ (len xs) 1))`; `(= (len v) n)` | — | literal-length test |
| `len ≥ 0` | instantiated once per list-sorted term | `(>= (len t) 0)` | — | a VC provable only with the axiom |

### 4.4 Refinements

**Sorts.** The logic reasons about `Int` (exact), `Bool`, `List` (an uninterpreted sort with the built-in measure `len`), and, from the second step of Phase 2, nominal records, sums and enums (SMT datatypes with selectors and testers) and atoms (distinct constants). Branded records are datatypes whose single constructor carries the brand. A field of a *row-typed* value is an uninterpreted function of the value and the label (`(field x r)`), consistent everywhere it occurs, so facts about `(. r :x)` survive without knowing `r`'s full row. When a branded record reaches a refined row-polymorphic function, the instantiation adds `field_l(r) = sel_T_l(r)` for each label used, so the caller's selector facts and the callee's field facts talk about the same value. `Float`, `String`, `Dyn`, functions, pids and maps are **not** logic sorts: values of those types can be passed around in refined code, but no fact about them exists and no obligation mentioning them can be discharged. Finite authority domains are best modelled as nullary-constructor sums (`(deftype Scope (RepoRead) (RepoWrite))`), because HM unifies distinct singleton atoms (`unify.ex:136-139`, §1.5).

**Refined types.** `(refine x τ p)` is the subset of `τ` whose values satisfy `p`. A refined function type names its parameters: `x1:{v:B1 | p1} → … → {v:B | q}`, where `pi` may mention earlier parameters and `q` may mention all of them. **The first step of Phase 2 allows refinements only at the top level of single-clause function parameters and results**, with or without a `:when` guard. Not yet: refinements nested inside type constructors, on record fields (data invariants), on multi-clause functions (their pattern variables are `:any` today, §15.5), on class methods, on lambdas, or on external results (§15.3).

**Which code is checked.** The checker generates obligations in two places:

1. inside every function that declares a refinement: its result, its calls, and the primitive safety of `div`, `head` and the like;
2. at every call, **anywhere**, to a function whose parameters are refined, including imported functions and calls from modules that contain no refinement syntax.

Unrefined code keeps today's crash semantics, and its effect row includes `Crash`. So the solver starts when obligations exist, not when refinement syntax appears. Existing programs keep their meaning, and an unrefined caller of an imported `safe-div` is still checked.

**Type variables.** A value whose type is a type variable belongs to an uninterpreted sort, one per variable. A predicate may use equality and uninterpreted functions on it and nothing else. Instantiation is sound because nothing was assumed about the sort.

**`decode` and refined types.** `(decode τ d)` has type `(Result DecodeError ⌊τ⌋)`, where `⌊τ⌋` is `τ` with refinements erased. In the `Ok v` branch, `τ`'s refinement is added as a fact. So no refinement is ever nested inside `Result`, and step 1's restriction to top-level refinements holds. A decode target whose refinement is not executable (a logic-only measure, Q5) is rejected.

**Judgments.**

- `Γ` maps names to refined types; `Φ` is the list of path facts, each tagged with its origin and provenance (§4.7). `⟦Γ;Φ⟧` is their conjunction.
- **Well-formedness:** `p` has sort `Bool`, mentions only names in scope, and uses only primitives, measures, selectors, testers and declared predicate aliases. **Predicates are pure and total by construction**: ordinary functions and anything with a non-empty effect row are not allowed, and every partial operator must be guarded where it occurs. A divisor must be proved non-zero by the surrounding conjunction or implication, and a sum selector must sit under its tester, as in `(=> (is Ok r) (> (Ok.0 r) 0))`. Otherwise the SMT value of the predicate, which is total, would differ from its runtime behaviour in `decode`, guards and entry points.
- **Faithful translation:** HM *types* a predicate, but the small, trusted predicate translator (M6) produces its Core form. A print-back test (render the Core predicate in surface syntax and compare it with the source) checks it. Core Lint checks that Core is well typed, not that it means the source, so this translator is part of the trusted base.
- **Subtyping:** `Γ;Φ ⊢ {v:B | p} <: {v:B | q}` iff `⟦Γ;Φ⟧ ∧ p ⇒ q` is valid. Base types are already equal after Core Lint; refinement subtyping never changes a base type.
- **Synthesis** gives the strongest known refinement:
  - variables of logic sorts: selfification, `x ⇒ {v | v = x}`;
  - literals: `n ⇒ {v | v = n}`;
  - primitives: by §4.3;
  - selection: `(select x l) ⇒ {v | v = field_l(x)}`;
  - construction: `(record T (l e) …) ⇒ {v | field_l(v) = e ∧ …}`, and `(inj T C e) ⇒ {v | is_C(v) ∧ C.0(v) = e}`;
  - calls to refined functions: their result refinement with arguments substituted;
  - **conditionals in synthesis position:** a *guarded join* `(if c e1 e2) ⇒ {v | (c ⇒ q1[v]) ∧ (¬c ⇒ q2[v])}`, where each branch's local facts are guarded by its condition in the same way. This stays quantifier-free, and `and`/`or` are covered because they *are* `if` (§4.2);
  - `true` for everything else.
- **Checking** is pushed into `if`, `match`, `let` and sequencing where possible, so most obligations arise at a leaf with that leaf's path facts. **Facts are scoped to the branch that produced them**: `Φ` is a path condition, never a flat per-function list. So nothing learned inside the second operand of an `and` is available after the `and`.

**Rules.** Before generating obligations, every non-trivial argument or scrutinee is named with a fresh variable (A-normal form); fresh names are rendered in diagnostics as the source expression (§13).

| Construct | Obligation generated | Facts added |
|---|---|---|
| **Call** `(f a1 … an)`, `f` refined | for each `i`: `ai ⇐ {v:Bi \| pi[a1..a(i-1)/x1..x(i-1)]}` | result `r:{v:B \| q[a/x]}` |
| **Primitive** (`div`, `head`, …) | its `crash` is unreachable (§4.3) | its result fact (§4.3) |
| **`if c e1 e2`** | none for `c` | with `c ⇒ {v:Bool \| φ(v)}`: `φ(true)` in `e1`, `φ(false)` in `e2` (§10.1) |
| **`and` / `or`** | as for `if` on the second operand | defined by short-circuit semantics (§4.3, §10.2) |
| **`let [x e] body`** | from `e` | `x:{v:B\|p}` where `e ⇒ {v:B\|p}` |
| **`match s …`** | from clause bodies | constructor and binding facts for the clause, and "no earlier clause matched" (§10.3) |
| **definition of `f`** | its body is checked against its result refinement | parameter refinements are *assumed* in the body; a `:when` guard adds its facts |
| **recursive call** | as for any call | `f`'s declared signature is assumed (partial correctness) |
| **lambda** | calls inside the body are checked normally | parameters are `{v:B \| true}`; captured names keep their facts, which is sound because values are immutable |
| **refined `f` in any position other than a call head** (passed, `let`-bound, stored) | its parameter refinements must be valid for all inputs, *unless* the receiving position has a dependent arrow type whose refinements imply them (§4.1) | none |
| **`select`** on a row (§4.1) | none | `(field l r)`, an uninterpreted function of `r` |
| **`handle-crash`** (`try`) | the handler is checked against the same expected type | no facts flow from the body into the handler, since the crash may have happened anywhere in it |
| **`perform`** (§4.5) | the effect's own requirements, if any | its result is unrefined unless the effect is an observation (§4.7) |
| **`decode τ x`** (§4.6) | none | on `Ok v`: `v : τ` and `τ`'s refinement, justified by the projection law (§4.6) |
| **`Dyn` value** | none: `Dyn` has no logic sort, and using it at another type is an ordinary type error from HM | none until `decode` |

**Guarantee.**

> **Refinement soundness.** Suppose a set of modules passes Core Lint and the refinement checker, with every obligation answered `valid`, and:
>
> - **A1** Core Lint's typing rules hold for the program (the elaborator is *not* assumed correct; Core Lint checks its output).
> - **A2** The backend agrees with the reference evaluator on this program. This is an **assumption**. The harness supplies evidence for it (§6.3), not a proof, and testing agreement on some inputs does not establish it for all.
> - **A3** Z3 is correct when it answers `unsat`.
> - **A4** **Whole-node assumption.** Code on the node that is not checked Vaisto code (Erlang, Elixir, OTP drivers, remote shells) reaches refined functions only through decoding entry points (M20), or not at all. This includes the internal entry points that cross-module Vaisto calls need, which BEAM necessarily exports.
> - **A5** Imported interfaces are verified and match the digests the module was checked against (§8), and the code actually loaded is the admitted code: no autoloading past the Vaisto loader and no hot replacement outside it (§8.4).
> - **A6** The trusted components of §18.1 are correct: the predicate translator, VC generator, primitive table, SMT lowering, materialization and erasure, decoders and row evidence.
> - **A7** When traces are used, the recording build performs the same effects as the production build. It is a different binary; the harness runs on both.
>
> Then, in the semantics of Liquid Core: whenever checked code calls a refined function, its arguments satisfy the parameter refinements; and whenever a refined function returns, its result satisfies the result refinement.

This is partial correctness: nothing is claimed about termination, or about crashes other than those §4.3 lets refinements exclude. On BEAM, processes are meant to loop forever and are allowed to crash.

**Totality through the effect algebra.** Because a crash is an effect (§4.5), crash-freedom is visible in types. When every `crash` site in a function (primitive preconditions, match failures) is discharged by the checker, and the function calls only total functions, its effect row excludes `Crash`. If it is also proved to **terminate** (it is non-recursive, or structurally recursive in the discipline of §12), the checker reports it as **total**. Otherwise it is only *crash-free*: partial correctness says nothing about a function that never returns, and such a function never becomes a symbol in the logic (§16.1). Refinements *remove an effect*: that is the precise connection between the refinement and effect primitives.

**Provisional syntax.** The brief leaves syntax open; this document uses one candidate:

```scheme
(defn safe-div [x :int y {d :int | (!= d 0)}] :int (div x y))
(defn clamp0 [x :int] {r :int | (>= r 0)} (if (< x 0) 0 x))
```

`{d :int | (!= d 0)}` already parses, as `{:tuple_pattern, [:d, {:atom, :int}, :|, {:call, :!=, ...}]}` (`parser.ex:401-403`, verified). Tuples in type position are written `(Tuple ...)`, so braces there are free.

**Hazard:** the same text is accepted *without error today*, as a different function. `safe-div` above parses as a three-parameter function, with `y` untyped and the brace as a tuple-pattern third parameter. The first parser change must reject braces in parameter position until refinements exist. D6 (a call in the return-type slot is read as a type) must be fixed first as well.

### 4.5 Effects as an algebraic theory

**Signature.** An effect is an operation with an argument type and a result type. The signature `Σ` is closed and small, because each operation is new theory:

| Label | Operation | Argument → result |
|---|---|---|
| `Proc M` | `send` | `(Tuple (Pid N) N) → Unit` |
|  | `receive` | `(Selector M) (Timeout) → M + Timeout` (§4.6) |
|  | `self` | `Unit → (Pid M)` |
|  | `spawn` | `(Process N) → (Pid N)` |
|  | `monitor` | `(Pid N) → Ref` |
|  | `link`, `unlink` | `(Pid N) → Unit` |
|  | `trap-exit` | `Bool → Bool` |
|  | `exit` | `(Tuple (Pid N) Reason) → Unit` (send an exit signal; **privileged**, see below) |
| `Time` | `now` | `Unit → Int` |
| `Random` | `random` | `Unit → Int` |
| `Unique` | `unique` | `Unit → Ref` |
| `External` | `external` | `(Tuple Atom Atom (List Dyn)) → Dyn` |
| `Model` | `model-call` | `ModelRequest → Dyn` |
| `Observe` | `observe` | `ObservationRequest → Witness` (privileged, §4.7) |
| `Crash` | `crash` | `Reason → 0` (no result: the continuation is never called) |

Files, network and printing reach the world through `external`, wrapped by typed library functions. Adding an operation to this table is a language change; adding a wrapper is not.

**Results can be exceptional.** Every operation may answer `Raised class reason` instead of its result, when a foreign function raises or an exit signal arrives. A process may also be **terminated** asynchronously, by a kill or a linked exit it does not trap; its trace then ends with `(terminated reason after n)`. Incoming exit signals and `DOWN` messages are *deliveries*, recorded like consumed messages. Termination is not a request the program made, so it is the one event that ends a fold from outside.

**`external` is privileged.** Its `(module, function)` must be on an **allowlist** declared in the pinned build configuration, the same file that lists observers (Q14). Ordinary modules reach the world only through the typed library wrappers over allowlisted functions. Without the allowlist, `external` is a universal escape hatch: a worker could kill an observer and recreate its ETS table (§4.7), or load code past the Vaisto loader (§8.4). **`exit` is privileged the same way**: a worker that holds an observer's or issuer's pid must not be able to kill it with an untrappable `kill`. `link` stays unprivileged, and privileged processes trap exits.

**Allowlisted functions that use the mailbox.** A `gen_server:call` consumes a reply from the *calling* process's mailbox, behind the effect algebra's back. Such functions are marked `mailbox` in the allowlist. A recording run records the messages consumed during the call, and replay treats the call as one block whose mailbox effect comes from the trace.

**Computations are the free algebra.** A computation of type `a` is a tree:

```text
Computation a ::= Pure a
                | Perform op arg (Outcome(op) -> Computation a)

Outcome(op)   ::= Ok r            ; r of op's result type
                | Raised class reason
```

This is the free `Σ`-algebra over `a`: operations are uninterpreted, and nothing identifies two trees except syntax. `crash` has result type `0`, so its arity is empty and it has no continuation: `crash r` followed by anything is `crash r`. That is not an equation of the theory but a consequence of arity 0, and the free model over `crash` alone is `A + Reason`, the algebraic definition of an exception (`liquid-types.md` §7.1).

**A handler is a Σ-algebra; running is the unique fold.** A handler interprets each operation. Running a computation with a handler is the unique homomorphism from the free algebra into the handler's algebra: a fold over the tree (the algebraic-effects view of Plotkin and Pretnar). The handlers of §6.2 are algebras:

- the pure handler, with no operations;
- the scripted handler, a fixed table;
- the replay handler, built from a trace;
- the live handler on BEAM.

`try` is a *local* handler for `crash` alone.

**Replay determinism is a corollary of freeness.** A trace `T = [(op₁, arg₁, res₁), …]` defines a partial algebra `h_T`: its `n`-th step answers the `n`-th request with `resₙ`, **provided** the request equals `(opₙ, argₙ)` and `resₙ` is an outcome of that operation (`Ok r` with `r` of its result type, or `Raised class reason`); otherwise it fails with "trace does not fit". Suppose a live run `run_h(P, I)` returns `R` and records `T`. Then the tree `P I` has one path that `h` and `h_T` both follow, and because the fold is unique, `run_{h_T}(P, I) = R`. Nothing about BEAM enters the replay argument itself: it depends only on the evaluator being a fold and the pure part being deterministic. Relating a replay on the evaluator to the BEAM run that produced the trace additionally needs A2 and A7. By default the elaboration re-raises a `Raised` outcome as `crash`, so an unhandled foreign exception is part of the `Crash` effect.

**Effect rows and polymorphism.** A function type carries an effect row `ε`, and the empty row means pure. Because effect rows are rows (§4.1), effect polymorphism is row polymorphism: `map : (forall ((a Type) (b Type) (e Eff)) (-> ((-> (a) e b) (List a)) e (List b)))`. This resolves Q19. Effect rows are inferred, never annotated. Refinement predicates and measures must have the empty row.

**Processes and concurrency.** A process is a computation over `Proc M` plus the other effects. A run of a whole system is a **partial order of events**: Lamport's happens-before. Each process's events are totally ordered, and every delivery follows its producer: the `send`, exit, monitor, timer or port event that caused it.

- **Per-process projection is invariant.** Projecting any linearization onto one process gives that process's own sequence of requests and results, because every linearization keeps each process's events in order. So one process can be replayed from its own trace, and that trace needs only the messages it consumed and the outcomes of its timeouts, not the mailbox contents.
- **A system history needs more than happens-before.** A *causal history* (§5.6) also records:
  - **per receiver, the arrival order** of messages and signals, because a receive's outcome depends on which of several concurrent matching messages arrived first;
  - **for each timeout, how many deliveries had arrived** when it fired, because a timeout depends on the *absence* of a matching message.
- **Validity is then checkable:**
  - per-pair FIFO: messages from one sender to one receiver arrive in send order;
  - first match: each consumed message is the first matching one in arrival order at that point;
  - every delivery has exactly one producer, and signals, timers and port messages count as producers.
- **The theorem is realizability.** A history recorded from a BEAM run is valid, and replaying every process from a valid history reproduces all of their traces. The replay half is close to trivial once everything is recorded; the substance is in the validity rules and in recording faithfully.
- **Recording.** A process sees what it *consumes*, not what *arrives*. Recording arrival order needs BEAM's tracing facility (`erlang:trace/3` with the `'receive'` flag), run by a privileged tracer process. The same tracer captures messages consumed inside `mailbox` externs, and gives messages from processes outside the recorded system a producer event `(external-sender pid)`.
- **Withdrawn.** Draft 2's claim that *any* interleaving consistent with happens-before gives the same traces is false for live execution: concurrent matching sends race, and timeouts depend on absence. This is Phase 6.

**OTP behaviours are foreign drivers.** A `gen_server` owns the `receive` and calls Vaisto callbacks with arbitrary terms, which inverts control. Its callbacks are entry points that decode their arguments (M20, A4). Nothing they do is recorded unless the driver is itself a Vaisto library over `Proc`. With the signal operations above, a Vaisto-native supervisor is ordinary library code; wrapping OTP's is a foreign-driver boundary.

**Lowering.** The lowered program performs effects directly: `send` is `erlang:send/2`, `receive` a native `receive` (with the guards of §4.6), and `handle-crash` a `try`. There is no free-monad interpreter at runtime. The correctness condition is that the lowered program **implements the live algebra**: on every run, it issues the same sequence of requests as the fold with the live handler, and uses their results the same way. Recording is an option that appends each request and result to a trace. The recording build is therefore a different binary from the production build, and that both perform the same effects is assumption A7 (§4.4).

### 4.6 `Dyn`, decoding, and typed messages

**`Dyn` is the universal type**, the type of arbitrary BEAM terms. It converts to nothing implicitly, in either direction; this replaces `:any`, which is both top and bottom today (§1.11).

**Every decodable type embeds into `Dyn`.** For each decodable `τ` there is an embedding–projection pair:

```text
up_τ   : τ -> Dyn                          the identity on representations
down_τ : Dyn -> (Result DecodeError τ)     generated from τ's structure

retraction:  down_τ (up_τ v) = Ok v
projection:  down_τ d = Ok v   implies   up_τ v = d
```

The first law says decoding accepts every well-typed value. The second says decoding never "fixes up" a value: whatever it accepts is exactly what was there. These two laws are the whole correctness condition of generated decoders; M16 property-tests them. (Embedding–projection pairs are the standard algebraic account of casts in gradual typing, for example New and Ahmed, ICFP 2018.)

- **Decodable:**
  - base sorts and `Unit`;
  - tuples;
  - rows (records, whose module-qualified brand is checked as the tuple tag);
  - sums;
  - **proper** lists (`is_list` alone accepts the improper `[1|2]`; the decoder walks to `[]`);
  - maps;
  - `(Pid Dyn)`;
  - refined types whose predicate is executable (the decoder checks the predicate too).
- **Not decodable:** a type any component of which is a function, or a `(Pid M)` for a specific `M` (a pid's protocol cannot be observed at runtime).
- **Opaque types decode only through their own module.** Only `T`'s defining module has a decoder for an opaque `T`; any other module that decodes a type containing a `T` calls that decoder. This keeps **abstraction**, because nobody else can see or build the representation, but it does not keep **integrity**: on BEAM, any process can build a tuple of the right shape and send it. So an opaque decoder **also checks the type invariant** (§4.1). A type whose invariant is not executable (a transition, whose steps must be *observed* valid) cannot be decoded as merely opaque; it must be sealed.
- **Sealed types add authentication.** Witnesses (§4.7), `Authority` and `Accepted` (§16) are *sealed*: opaque, and every value carries an authenticator that the defining module's decoder verifies. A forged tuple fails to decode.

  **Authenticators are deterministic.** They are MAC chains over the value's canonical content, in the style of macaroons (Birgisson et al., NDSS 2014):
  - minting a root value needs the issuer's key, so it is an issuer-only effect;
  - *attenuating* a value (adding a caveat) computes the next MAC from the previous one and the caveat, which anyone holding the value can do and nobody can undo;
  - verification needs the key, so a sealed decoder asks the issuer, and decoding a sealed type is an effect.

  Because attenuation is a deterministic function of its inputs, operations such as `meet` on authority remain pure functions that can be symbols in the logic, with congruence. The reference evaluator and the logic both define them on the canonical content; the MAC is carried alongside.

So opaque and sealed values can cross process boundaries inside messages, while only the issuer can mint a sealed value. Abstraction and authentication are separate properties, and the type system names both.

**Equality on opaque and sealed types.** Raw `==` on an opaque or sealed type is not available outside its module. The module exports a lawful `Eq` over the canonical content: for a sealed type, *ignoring* the authenticator. Otherwise clients could tell supposedly indistinguishable models apart, and a law such as `le a b ⇔ meet a b = a` would fail on differing MACs.

**Facts from decoding.** When `(decode τ d)` returns `Ok v`, the checker adds `v : τ` and `τ`'s refinement as `Static` facts. Their justification is the projection law of a trusted, generated decoder. (The relation `up v = d` itself is not a logic fact, because `Dyn` is not a logic sort.)

**Typed messages without an extra assumption.** On BEAM anyone can send anything to any pid, so a mailbox is a queue of `Dyn`. A process with protocol `M` receives through `down_M`, in two stages:

1. **Selection** is a selective receive on the message **tag**, plus whatever `down_M` can check as BEAM guards (scalars, tuple shapes). The receive ends with a tag-level catch-all only for tags outside `M`, so it never eats a valid message meant for a later `receive`.
2. **Decoding** happens after consumption, for what guards cannot express: lists, opaque components (decoded by their own module), sealed authenticators (which ask the issuer). A message that fails is *stray*.

So "consumed only if it decodes" holds for everything guards can check and becomes "consumed, then rejected as stray" for the rest. A timed receive that rejects a stray re-enters with the remaining timeout. The evaluator models exactly this, and the first-match validity rule of §4.5 applies to the tag-level selection. Every process has a stray policy (log and drop, crash, or an explicit `Stray Dyn` clause), with the default an open question (Q26).

- **Sending** is statically typed: `send` takes a `(Pid N)` and an `N`, so Vaisto code cannot send an ill-typed message.
- **Receiving** never *assumes* the mailbox is well-typed. It projects.

This resolves the contradiction in draft 2 between typed messages and `Dyn`: no receiver needs to know who sent a message.

**Cost.** Decoding is proportional to the size of what is checked. Tags and scalar fields cost a guard each; deep structures (long lists inside a message) cost a traversal. Whether large payloads decode lazily at first use or eagerly is Q28.

**Externs** return `Dyn` from the raw `external` operation; a declared `extern` decodes its result to its declared type (§15.3). A wrong declaration therefore becomes a decode crash with a clear reason, never a mistyped value.

**Untyped parameters are not `Dyn`.** They are already type variables (`freshen_any_params`, §1.11). Migration is per source category (§15.5): each `:any` producer becomes a type error, a type variable, or an explicit `Dyn`.

### 4.7 Evidence origin

Every fact the refinement checker may use carries an origin:

| Origin | Examples | Established by | Enters the logic |
|---|---|---|---|
| `Static` | branch conditions, primitive results, constructor facts, successful `decode`, verified refined signatures, reflection (§16.2) | the checker, from the program's own code | Phase 2 |
| `Imported` | the result refinement of a verified function in another module | that module's verification, identified by interface digest | Phase 2, once interfaces carry refinements (§8) |
| `Observed` | "these tests ran and passed"; "this file has hash H" | a privileged observer, at runtime, through `observe` | Phase 5 |

**Provenance is a free join-semilattice.** A fact's provenance is the finite set of external sources it depends on: interface digests and observation receipts. Static reasoning contributes nothing. Combining facts takes the union of their sets. So a derived fact is not a fourth origin: its trust is exactly the join of its premises'. Two consequences:

- A conclusion that used an observation names that observation.
- A fact is *purely static* exactly when its provenance is empty.

This is the whole algebra of "where did this come from", and it is associative, commutative and idempotent by construction.

**Stochastic information is never a fact about truth.** Model output arrives as `Dyn`; once decoded, it yields facts about the value's **shape** (these fields exist, this list has three citations), never about whether it is correct or good. There is no predicate for correctness, and confidence is a `Float`, which is not a logic sort, so `(>= conf 0.9)` cannot be written as a refinement at all. What a contract chooses to require of shapes is a specification question (§17.2), not something the language can decide.

**Claim ≠ fact.** No term constructs an `Observed` fact. An ordinary value `tests-passed = true` has no status. The only routes into the logic are the checker's own reasoning, verified interfaces, and `observe`.

**Observers.** An observer is a module listed as privileged in the build configuration (Q14). It defines **sealed witness types** (§4.6): opaque, authenticated by the observer, and never returnable by `external`. The observer is the one place an observation axiom lives:

```scheme
;; privileged observer module only
(defn run-tests [suite :TestSuite] :TestReceipt ...)                   ; performs (observe ...)
(defn passed? [r :TestReceipt] {b :bool | (iff b (tests-passed r))} ...) ; result refinement is TRUSTED, not verified
```

`tests-passed` is an uninterpreted predicate whose only source of truth is the observer. `passed?`'s result refinement is recorded as `Observed` in the interface, and its receipt enters the provenance of every fact derived from it.

**The receipt is the observation.** `tests-passed` must be a **pure function of the immutable witness's contents**. If `passed?` consulted mutable state, such as a revocation table, then two calls on the same receipt could disagree. A branch that saw both answers would hold contradictory facts, which prove anything. Checking that a witness is *authentic* (issued by the live observer, not revoked) is a separate admission step with its own runtime check, not part of the axiom. For the same reason, **contradictory hypotheses that involve an `Observed` fact are errors**, not the warnings they are elsewhere (§11.3).

**Runtime forgery.** Compile-time opacity does not stop Erlang code from building the same tuple. Within one node, the observer process owns a `:protected` ETS table of the receipts it issued: any process can read it, only the owner can write it, and admission looks receipts up instead of trusting the term. Admission holds the table's **`tid`, not its name**, and checks that its owner is the observer's pid; a table re-created under the same name by another process is not the observer's. Killing the observer, or re-creating its table, needs `exit` or `external` powers that the allowlist withholds from workers (§4.5). Signatures on-node are the stronger option. `make_ref/0` alone is not enough, because `erlang:list_to_ref/1` exists. Across nodes, receipts need signatures.

**What Phase 2 must already do:** every hypothesis in a verification condition carries its origin and provenance, and nothing else can produce a hypothesis. `Observed` is a reserved origin that Phase 2 code rejects.

### 4.8 What is not in the core

The test for every feature is whether it needs new theory. The table also records which library structures have algebraic laws (§16).

| Feature | Where it lives | New theory in Liquid Core? |
|---|---|---|
| typeclasses, `:num` | dictionaries (§4.2); lawful instances only in the logic | no |
| row polymorphism | row evidence (§4.1) | no: part of the type algebra |
| nominal types, workflow phases | branded rows (§4.1, §16.3) | no |
| processes, actors, supervision | library over `Proc M`; typed receive is decoding (§4.6) | no |
| exceptions (`try`) | handler for `crash` (§4.5) | no |
| authority and capabilities | library: an abstract meet-semilattice with models (§16.2) | no |
| transitions and their composition | library: the free category on admissible steps (§16.3) | no |
| budgets | library: an ordered commutative monoid (§16.6) | no |
| accepted state, authority, witnesses | sealed types (§4.1, §4.6) | no |
| invariants and abstract measures of opaque types | refinement logic (§4.1) | **yes, small**: a type invariant established inside the module and assumed outside |
| the module system: names, opacity, sealing, interfaces | beside the four primitives | a **fifth mechanism**, stated plainly: every language has one, and the primitives assume it |
| provenance | the free join-semilattice of §4.7 | no |
| contracts, pipelines, operators | library over `model-call` and `external` (§17) | no |
| a new kind of external effect | the signature `Σ` | **yes** |
| a new logical operation in refinements | refinement logic | **yes** |
| use-at-most-once resources (affine types) | would need a usage discipline | **yes**, deferred (§17.5) |
| liquid inference of refinements | checker | **yes**, deferred (§17.5) |

---

## 5. Representations: one canonical tree format (assignment item 3)

### 5.1 The format

Every semantic artifact is the same kind of tree: Core terms and types, interfaces, contracts, predicates, verification conditions, effect traces, causal histories and admission records.

- **Leaves** are symbols, integers, floats or byte strings. **Nodes** are lists.
- **Integers** are written in decimal, with no leading zeros and no `-0`. **Floats** are written as the 16 hex digits of their IEEE-754 bits, so `0.0` and `-0.0` differ and every value has exactly one encoding.
- **Pids, references and funs** have no canonical encoding as values. In a trace they are recorded as **symbolic identities** assigned at recording time (`(pid 3)`, `(ref 17)`), consistent within the trace. A trace that carries a fun is marked **not replayable** from that point: a digest of a closure's environment cannot be inverted, and anonymous functions have no stable name. Protocols that must be replayable carry data, not closures.
- **Canonical bytes** use Rivest's canonical S-expression form: length-prefixed leaves, no whitespace, with a display hint distinguishing integers and strings from symbols. In canonical form the hint is itself length-prefixed: `[1:i]2:42`, `[1:s]5:hello`, `3:add`.
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
       | (abstract Name)                    ; an opaque or sealed type, seen through its signature
       | (tvar a)                           ; one uninterpreted sort per type variable (§4.4)
term ::= (var name sort origin)            ; origin: param | let | anf(source) | binder
       | (int n) | (bool b) | (atom a)
       | (arith add|sub|mul|neg|tdiv|trem term ...)
       | (ite pred term term)
       | (select Adt Ctor index term) | (ctor Adt Ctor term ...)
       | (measure name term ...)            ; len, then user measures (§12)
       | (field label term)                 ; a field of a row-typed value (§4.4)
       | (apply name term ...)              ; an exported function lifted into the logic (§16.1)
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
  (status verified|assumed)            ; assumed: primitives, and Observed axioms (§4.7)
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

A trace is valid for a program if, at every step, the program's request equals the recorded request, the recorded outcome is `Ok r` with `r` of the operation's result type, or `Raised class reason`, and every consumed message is the first matching delivery in arrival order. A history is valid under the rules of §4.5: per-receiver arrival order, per-pair FIFO, first match, timeout counts, and a producer for every delivery, with signals, timers and ports counting as producers.

---

## 6. The reference evaluator: the executable specification

### 6.1 What it is

A small, deterministic, call-by-value interpreter for Liquid Core, written in Elixir, parameterized by an effect handler. It is **not** a debugging interpreter. It is the definition of what a Vaisto program means. It favours clarity over speed: no optimizations, one clause per Core construct, primitives exactly as specified in §4.3.

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
4. **The bridge** (Phase 2.1): `adapter(HM(P)) = erase(elaborate_refined(P))` for every refined program (§21.1, K11).

For primitives, the oracle is a triangle: evaluator, BEAM and SMT, with at least two corners not sharing an implementation (§4.3, K12). Otherwise the differential test would compare BEAM with itself.

The corpus is seeded with the source of every existing test, the reproductions of D1–D24, the conformance programs (§14), and generated programs: random well-typed Core terms produced from Core's typing rules. Each disagreement is classified as a backend bug, an elaborator bug, or an evaluator bug, and the written spec decides which.

### 6.4 What it would have found

| Defect | Found by |
|---|---|
| D3, D4, D15–D19, D21, D22 | Core Lint rejects HM's output (for example, D15's leaked `let` binding makes `(+ x 1)` add an `Int` to a `String`) |
| D5 | evaluator returns `false`; the Core Erlang backend crashes with `badarith` |
| D24 | evaluator returns the field; both backends crash (`BadMapError`, `FunctionClauseError`) |
| D11, D12 | evaluator returns a value; one backend fails to compile |
| D9, D10 | not by the harness: fixed by construction, because interfaces are canonical trees generated from Core and ordered by explicit imports (§8) |
| D1, D2, D20 | not by the harness (HM rejects or misreads a valid program, so there is no Core to compare): found by conformance tests |
| D8 | Core Lint: an unknown call cannot be elaborated, and an extern argument mismatch fails Lint |
| D13, D14 | not behavioural bugs (missing locations; a missing substitution clause): removed by construction (Core spans, §9.4; Core Lint re-checks every type) |
| D15 (also) | the backend comparison: `2` on `:elixir`, a crash on `:core` |
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

It closes: "Our recommendation is to use either the abstract format or Erlang source code."

Vaisto's `CoreEmitter`, the default backend for the CLI and `Build`, generates Core Erlang. The Elixir backend already reaches BEAM through Elixir's own compiler, which translates to Erlang and hands the result to OTP. **One lowering, from Liquid Core to the abstract format**, puts Vaisto on the interface OTP recommends and keeps source-line mapping: Core spans become abstract-format annotations, so stack traces point at Vaisto source.

### 7.2 Erasure

Erasure runs in two steps:

1. **`materialize : VerifiedCore -> VerifiedCore`** turns every runtime check that refinements imply into ordinary Core code:
   - `decode` against a refined type becomes the base decoder followed by an explicit test of the predicate;
   - extern result decoders become ordinary calls;
   - decoding entry points (M20) become wrapper functions.
2. **`erase : VerifiedCore -> Core⁻`** then removes `(refine x τ p)` (keeping `τ`), measure, predicate-alias, invariant and law definitions. It is total and syntactic, and it never depends on solver answers, so a solver bug cannot change generated code.

**Invariant:** for every verified pure term `e`, `eval(materialize(e)) = eval(erase(materialize(e)))`. After materialization, refinements have no runtime meaning at all, because every runtime check they imply is already explicit code. So erasure is purely syntactic and has no exceptions.

**Tests:**

1. The corpus property: the evaluator gives the same answer before and after erasure.
2. C11 (§14): the backend output for each accepted conformance program equals that of the same program with refinements stripped.
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
- **An unchecked exporter** (built without refinement checking) has `(status unchecked)`. Importing a refined signature from it is an error, with no override: an override would be an `assume` (principle 6). The exporter must be rebuilt with refinement checking.
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

Two ordinary features bypass it too, without any attack:

- **Autoloading.** In interactive mode, the code server loads a module from the code path on its first call. Admission therefore requires **embedded mode** (`-mode embedded`), with a code path that contains only admitted modules.
- **Hot loading.** Replacing a module goes through the Vaisto loader, or it breaks A5.

The files admission depends on must be outside every worker's write access, and covered by the build's signature: the interfaces a build used, and the build configuration listing observers, the issuer and allowlisted externs. Otherwise a worker could forge an interface or add itself to the allowlist.

### 8.5 Distributed interface identity

A service exports an interface digest. When two nodes connect, each states the digests of the interfaces it serves and expects, and a mismatch (`expected abc123, received f09de2`) is refused before any other message is exchanged. The exchange happens as the first application-level messages on a connection, because stock Erlang distribution has no hook for it. Refusing other traffic until it completes is application discipline, not something distribution enforces. This prevents **version skew**, not attack: Erlang distribution gives every connected node full trust, so a hostile node is out of scope for this mechanism.

---

## 9. Elaboration and the two inference engines (assignment item 6)

### 9.1 HM retires; a bidirectional elaborator replaces it

*Amended.* Earlier drafts kept HM as an untrusted elaborator. Phase 0 measured what that costs. Over the 133 programs of the test suite, Core Lint rejected 25 that HM accepted. In 21 of them, HM generalized a type it never constrained (D17), or did not tie a scrutinee to its patterns (D22); the other 4 fell back to `:any`. Global inference fails silently when it cannot decide, and every feature Liquid Vaisto adds breaks its principal-types guarantee: refinement subsumption, higher rank, quotients, grades.

The front end is therefore a **bidirectional elaborator** (`liquid-types.md`):

- **It reads the algebra.** Each type former's introduction forms are checked and its elimination forms synthesize (`liquid-types.md` §8). Core Lint implements the same rule table independently, so a disagreement between the two is an elaborator bug.
- **Inference is local unification.** Unification is used only to choose type, row and effect arguments and class dictionaries at an elimination site, inside one definition (`liquid-types.md` §9).
- **Top-level definitions carry signatures**, which are their contracts. Local `let`s are not generalized, and lambdas checked against a known arrow need no annotations, so the fallback lambda (§15.4) disappears. There is no `:any`.
- **Refinements never enter HM's type terms.** This part of the old design is kept, for the reason §1.9 gives: the emitters, `Unify`, `Core.apply_subst` and `Core.free_vars` all pattern-match type terms structurally.

Until the elaborator lands in Phase 1a, today's `TypeChecker` and the Phase 0 adapter (§9.3) serve as the untrusted front end, as before. Both engines (`TypeChecker` and `TypeSystem.Infer`) retire when the elaborator reaches parity on the corpus. Their inferred types survive only as *suggested* signatures during migration (`liquid-types.md` §13).

### 9.2 Core Lint

Core Lint checks elaborated Core and **infers nothing**: every binder is annotated and every instantiation explicit. It checks:

- **types**, with `Dyn` converting to nothing; **kinds** (`Type`, `Row`, `Eff`); rows without duplicate labels;
- the **value restriction** on polymorphic `let`, so a received message can never be generalized to `forall a. a`;
- **exhaustiveness** of every `match` (HM is untrusted, so its exhaustiveness check does not count);
- **pattern typing**: literal types, constructor arity, linearity;
- **guard safety**: guards use only guard-safe operations (§4.2);
- **opacity and sealing**: no construction, selection or matching on an opaque type outside its module (§4.1); no decoder for any type with a function or specific-pid component; opaque components decoded only through their own module (§4.6);
- **privilege**: `observe`, `external` and `exit` (allowlist), issuer-only functions and trusted result refinements appear only in modules the pinned configuration names; and **a function with a trusted result refinement has an empty effect row**, so an observation axiom cannot depend on mutable state (§4.7);
- **liftability**: only functions with an empty effect row that are proved to terminate appear as symbols in the logic (§16.1);
- **refinement well-formedness**: predicates are pure, `Bool`-typed, total by construction (§4.4), and use only admitted symbols;
- **A-normal form**: effectful and crashing sub-expressions are named within their evaluation context (§4.2), which the lowering relies on before Phase 4 adds effect rows;
- **effect rows** (from Phase 4);
- **specification bindings**: the derived-`Eq` specification applies to the derived instance function, not to the class method;
- **interface conformance**: exported signatures match the digest the interface records.

It is the trusted replacement for draft 1's "re-derive sorts inside the refined fragment" containment rule, and a stronger one: it covers the whole program, not only the terms in obligations. The model is GHC's Core Lint, which checks the output of an elaborator far larger than itself.

### 9.3 The Phase 0 adapter

Before HM is changed, a temporary adapter translates today's typed AST into Core for the pure fragment. Where the typed AST lacks information (a fallback lambda's parameter types, `:any` in the types), the adapter emits `Dyn`, and Core Lint then reports the use sites. That is exactly the list of places Phase 1a has to fix. The adapter is also one side of the Phase 2.1 bridge (§21.1): refined programs are checked through the adapter's Core while they still run on the old emitters. It is deleted when HM elaborates to Core directly (Phase 1a).

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

Liquid Core defines `and` and `or` as short-circuiting (§4.3), so `(and a b)` checks `b` under `φa(true)` and `(or a b)` checks `b` under `φa(false)`. The idiom `(and (!= y 0) (> (div x y) 1))` is therefore accepted. This is a decision about the language, not an observation about a backend: today the Core Erlang backend evaluates both operands (D5), which makes that backend wrong, and the differential harness (§6) reports it.

### 10.3 `match` and multi-clause functions

For scrutinee `s` and clauses `pat1 … patn` (first-match semantics):

- Clause `i` gets `M(pat_i, s)`: the constructor test `is_Ctor(s)`, equations for literal patterns, bindings as selectors (`v = select(Ok, 0, s)`), and for list patterns `[] → len(s) = 0`, `[h | t] → len(s) ≥ 1 ∧ len(t) = len(s) − 1`.
- Clause `i` also gets, for every earlier clause `j`, the negation of that clause's **test**, never of its binding equations. The test `T(pat_j, s)` is the constructor tests and literal equalities; the bindings are defined only when the test holds. For a guarded earlier clause the fact is `¬(T(pat_j, s) ∧ def(guard_j) ∧ guard_j)`, with the guard rewritten over selectors of `s`.
- **Definedness matters.** A guard that crashes counts as false, so negating the bare guard would assume it was evaluated. With `[x :when (> (div 10 x) 1) …]`, `[x :when (< (div 10 x) 2) …]`, `[_ (safe-div 1 n)]`, the third clause without `def` would get contradictory hypotheses and discharge `n ≠ 0` vacuously. Yet at `n = 0` both guards crash and the third clause runs. With `def`, the obligation correctly fails (checked with Z3; C19).
- **Polarity.** Negation reverses what "dropping a hypothesis" means. Dropping the unsupported part of `T(pat_j, s)` *before* negating would strengthen the negated fact: for `(Ok "foo")`, dropping the string test gives `¬is_Ok(s)`, which is false for `(Ok "bar")`. So an unsupported atom inside a test is replaced by a **fresh boolean variable**, never dropped: `¬(is_Ok(s) ∧ b₁)` is sound whatever `b₁` is. Only *whole top-level* hypotheses may be dropped (§11.2).
- A guard that cannot be rewritten over selectors contributes nothing, and neither does the negation of its clause.
- `defn_multi` clauses will use the same rule with the parameter as the scrutinee, once `defn_multi` is admitted (§4.4).
- **Exhaustiveness is checked by Core Lint** (§9.2); HM's check does not count, because HM is untrusted. Refinements do not relax it in Phase 2; a clause that is unreachable under refinements gets a warning (§13.4), not removal.

### 10.4 `:when` guards

A guard on a single-clause `defn` adds its facts to the body's `Φ`: the body only runs when the guard is true, and otherwise BEAM raises `function_clause`. Guards are guard-safe expressions, and a crash inside a guard counts as false (§4.2), so the fact added is `def(g) ∧ g` (§4.2). The vacuity check (§11.3) includes the guard: requirements plus guard must be satisfiable. Callers are **not** required to prove the guard: a guard is a runtime check, a refined parameter is a static requirement, and the two stay distinct. Two lints make the relationship visible: a guard that is provable at every call site ("this guard can never fail"), and a guard refuted at some call site ("this call always fails the guard"). Whether a guard should also imply a static precondition is Q9.

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
| `Solver.Z3` | Production. A `Port` to `z3 -in -smt2` with a **fresh context per VC** (`(reset)`), not a long-lived `push`/`pop` stack whose history could change answers. A deterministic **resource limit** (`(set-option :rlimit N)`) rather than a wall-clock timeout, so `unknown` does not depend on machine load. Fixed seeds (`:random-seed 0`, `smt.random_seed 0`, `sat.random_seed 0`), and `(get-value ...)` on `sat` to obtain a counterexample. |
| `Solver.Unknown` | A test double that answers `{:unknown, :test}` to everything. It exists to prove that `unknown` never passes (C13 in §14). |
| `Solver.Script` | A test double with scripted answers per VC id, for deterministic tests of diagnostics without Z3. |

The SMT-LIB lowering (`Refine.SMTLIB`) is a pure function from `%VC{}` to text. It uses deterministic naming (`x_1`, sorted declarations), so the same program yields byte-identical queries, which makes answers cacheable by `(query_digest, solver identity)`.

**Language and dependency rules.** This is Elixir code that talks to an external executable over a port, like invoking `erlc`. It adds no Hex dependency and no NIF, and no Python/Go/Node. It does add an **external tool requirement** for programs that use refinements, which `AGENTS.md` ("Do not add dependencies") does not anticipate. That needs an explicit owner decision (Q2). Modules with no obligations (§4.4) never start the solver.

### 11.2 Trust boundary

| Solver answer | Meaning | Action |
|---|---|---|
| `unsat` for `hyps ∧ ¬goal` | obligation valid | accept |
| `sat` | obligation may fail | error, with an optional "for example" built from the model, labelled as an example |
| `unknown`, timeout, solver crash, unparsable output, solver missing | nothing is known | **error**: "could not verify …" |

Rules:

- **Soundness direction.** Every approximation in the lowering may **drop a hypothesis** (lose precision, stay sound) but must **never weaken a goal**. Only **whole top-level** hypotheses may be dropped. An unsupported construct *inside* a hypothesis is replaced by a fresh boolean variable (§10.3), because dropping it under a negation or a disjunction would strengthen the hypothesis. Unsupported constructs in goals make the VC unprovable. A code review checklist item and a unit test per lowering rule enforce this.
- **Trusted components for `valid`:** the VC generator, the primitive specification table, the SMT-LIB lowering, and Z3's `unsat` answers. §19 lists each one.
- **Pinned solver.** The solver identity is recorded in the module interface (§8.1). A different version is allowed but visible.
- **Decidability is scoped.** Linear integer arithmetic with uninterpreted functions and datatypes is decidable. A product of two variables, or division by a variable, puts a VC in nonlinear arithmetic, where Z3 is a semi-decision procedure. Nonlinear VCs are allowed but may answer `unknown`, and the "decidable" claims in §12 and §16 apply to the linear fragment only.
- **Provenance is syntactic.** The provenance of a derived fact is the union of the provenance of *every* hypothesis in its VC. That is an over-approximation, but it is stable. Unsat cores would be more precise, but they are neither unique nor stable across solver versions, so they are not used.
- **Counterexamples are advisory.** With uninterpreted measures a model can be spurious, so messages say "for example" and tests never assert on model values. They assert on the deterministic parts: the failing conjunct, the known facts, the location.
- **No solver, no pass.** If `z3` is missing and the module has obligations: "refinement checking needs the `z3` solver, which was not found on PATH". Never skip.
- **Optional second opinion.** A paranoid mode that requires a second solver (cvc5) to agree on `unsat` is possible later (Q13), and is not needed for Phase 2.
- **Caching.** A cache of `valid` answers keyed by query digest and solver identity is an optimization only. Whether CI may run without a solver, using a cache, is Q7. A cache is a trusted component.

### 11.3 Vacuity and reachability checks

A proof under an impossible hypothesis proves nothing, so the pass also asks satisfiability questions:

- **Vacuous requirements (error).** For each refined `defn`, check that `p1 ∧ … ∧ pn` is satisfiable. If not: "the requirements of `f` can never be met". A contract whose precondition is `false` must look suspicious, not verified.
- **Unreachable branches (warning).** For each `if` branch and `match` clause, check that `⟦Γ;Φ⟧` is satisfiable. If not: "this branch can never run, given the requirements on `x`".
- **Contradictory facts at an obligation (error).** If the hypotheses at an obligation are unsatisfiable while the function's requirements are satisfiable, the obligation is only vacuously valid. It is an **error unless the branch is marked `(unreachable)`**, a form that is itself the obligation `false` under the same hypotheses. Contradictions are exactly how a wrong axiom or a mistreated guard would otherwise turn into accepted code (C19, C20), so they must be deliberate, never accidental.
- **Vacuous guarantees (error).** For each refined function, check that its result refinement is satisfiable under its requirements. A guarantee that can never hold, such as `{v | v > 0 ∧ v < 0}`, verifies only because the function never returns; it is reported.

---

## 12. Measures (assignment item 10)

Measures arrive at the end of Phase 2. Before that, the only measure is the built-in `len` over lists, with the facts of §4.3.

**Proposed form** (provisional syntax):

```scheme
(defmeasure phase [j :Job] :Phase
  [(Job _ p _) p])

(defmeasure depth [t :Tree] :int
  [(Leaf)       0]
  [(Node l _ r) (+ 1 (if (> (depth l) (depth r)) (depth l) (depth r)))])
```

**Discipline:**

1. **One argument**, whose type is a nominal sum or record (or `List`, for built-ins only).
2. **Result sort** in the logic: `Int`, `Bool`, or an `(adt Name)` sort: a branded record, sum or enum (§5.2).
3. **One clause per constructor**, exhaustive, non-overlapping, no guards, flat patterns (constructor with variable or `_` fields).
4. **Bodies are in the logic fragment:** literals, pattern variables, arithmetic, boolean operators, `if` (lowered to `ite`), constructor applications, selectors, previously defined measures, and **self-recursion only on fields of the matched constructor** that have the same type.
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
           ^ `at` requires (< (length xs) (len xs)) for its argument `i`
  note: the other requirement, (>= (length xs) 0), holds
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
error: could not verify this requirement within its resource limit
  at line 9
    (run cap job)
             ^ `run` requires (le (. job :required) (. cap :scope))
  hint: split the condition, or check it at runtime with (le? ...)
```

The domain-specific rendering in the brief ("required: production.write / available: repo.read") comes from optional per-alias explanations (§16.5), not from special cases in the checker.

### 13.4 Warnings

- unreachable branch (§11.3);
- guard can never fail / always fails (§10.4);

Not a warning: a refinement on a sort outside the logic ("refinements on `Float` are not supported") is an **error** (C18), and so are contradictory facts at an obligation without `(unreachable)` and vacuous guarantees (§11.3).

---

## 14. Conformance suite (assignment item 12)

The core programs, C1–C10, each with its expected outcome (§14.2 adds C14–C20). Programs that need more than Phase 0 and Phase 2.1 say so where they are listed. The expected diagnostic text is normative only for the parts listed (message line, caret target, failing conjunct); notes and hints may change wording.

| # | Program | Expected |
|---|---|---|
| C1 | `safe-div` called under a branch fact | **compiles** |
| C2 | `safe-div` called with no fact about the divisor | **error:** requirement not met, conjunct `(!= n 0)` |
| C3 | `clamp0` correct | **compiles** |
| C4 | `clamp0` returns `x` in the negative branch | **error:** result does not satisfy `(>= r 0)` |
| C5 | `max2` correct; `max2-bad` swapped | first **compiles**; second **errors on both conjuncts**: `(>= v x)` in the then-branch and `(>= v y)` in the else-branch |
| C6 | recursive `at` over lists; caller guards with `length`; off-by-one caller | `at` and guarded caller **compile**; `(at xs (length xs))` **error** on `(< (length xs) (len xs))` |
| C7 | contradictory precondition | **error:** requirements can never be met |
| C8 | an undecoded `Dyn` value feeding a refined parameter | **error:** this value has type Dyn |
| C9 | `(and (!= y 0) (> (safe-div x y) 1))` | **compiles**: Core defines `and` as short-circuit (§4.3). On today's emitters it also *runs* correctly only on `:elixir` until P0-4 (D5). |
| C10 | truncating division | `half-up` **compiles**; `half-floor` **error** (would compile under SMT `div`) |

Three infrastructure properties accompany the suite:

- **C11 (erasure):** for the accepted parts of C1, C3, C5, C6, C9 and C10, the evaluator gives the same result before and after erasure, and the backend output for the refined program equals that for the refinement-stripped program: Core Erlang and Elixir AST on the old emitters, the abstract format after Phase 1b.
- **C12 (modularity, needs Phase 1b interfaces):** C1's `safe-div` in module `A`, `avg` in module `B`: `B` compiles against `A`'s interface without re-checking `A`'s body; changing `safe-div`'s requirement to `(> d 1)` makes `B` fail on rebuild, because `avg`'s guard `(> n 0)` does not establish it. (A change to `(> d 0)` would not: the guard still implies it.)
- **C13 (unknown ≠ proved):** with `Solver.Unknown`, C1 fails with "could not verify".

The programs (provisional syntax, §4.4):

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

**Test harness.** Each program is a test file under `test/refine/conformance/` with an expectations header, run on the backends in use for accepted programs: the old emitters through the bridge until Phase 1b (§21.1), then the single lowering. Diagnostic assertions match the normative parts only. The suite runs with `Solver.Z3` when available and fails (it does not skip) when it is not, unless the test is tagged as solver-free (C13).

### 14.1 Core and backend conformance

The refinement programs above test the checker. These properties test the architecture:

| # | Property | Expected |
|---|---|---|
| K1 | **Differential, pure.** For every accepted program in the corpus, `evaluator(P) = BEAM(P)`, values and crash reasons. | equal; any difference is classified per §6.3 |
| K2 | **Replay.** An effectful program (`send`, `receive`, `external`) run on BEAM with recording and replayed on the evaluator gives the same result. A trace edited so that a `receive` gets a message its selector rejects is refused. | same result; tampered trace rejected |
| K3 | **`Dyn`.** A raw `(external ...)` result (type `Dyn`) used as `Int` is rejected; matching on `(decode :int ...)` with `Ok`/`Err` clauses is accepted. An `extern` declared with result `:int` returns an `Int`: when the foreign function returns something else, the call crashes with a decode error instead of producing a mistyped value (§15.3). | reject / accept / decode crash |
| K4 | **Erasure.** C11, run on every accepted program in the corpus, not only the conformance programs. | equal |
| K5 | **Core Lint catches the elaborator.** Fault injection: a test build of the elaborator that mistypes one construct at a time (the D15 class, a leaked binding, is the first). Each faulty elaboration must be rejected by Core Lint with a message that names it as a compiler bug. The real D15 is fixed in Phase 0 (P0-14), so it cannot serve as the permanent test. | Lint error for every injected fault |
| K6 | **Canonical codec.** Round trip; injectivity (property-based); golden digests of a fixed interface are identical under two OTP releases. | pass |
| K7 | **Admission.** The Vaisto loader refuses a module whose `VSTY` chunk has one byte flipped, refuses a stripped module, and loads a correctly signed one. | refuse / refuse / load |
| K8 | **Opacity and sealing.** Constructing or matching on `Accepted` outside `Vaisto.Admission` is rejected. A hand-built tuple with the shape of an `Accepted`, sent to a process whose protocol carries an `Accepted`, fails to decode (no authenticator). | Lint error; decode failure |
| K9 | **Evidence.** Using `(tests-passed r)` as a fact without going through `passed?` cannot be proved. A non-privileged module that declares `passed?`'s trusted result refinement is rejected. | not provable / rejected |

### 14.2 Programs for the type algebra

| # | Program | Expected |
|---|---|---|
| C14 | **Rows and brands.** `(defn get-x [r] (. r :x))` applied to `(Point 1 2)` returns `1` on the evaluator and on BEAM (row evidence, §4.1; D24 today). `(defn deploy [j :Accepted] ...)` applied to `(Executed 1)` is rejected even though the rows have the same fields. | runs / rejected |
| C15 | **Typed receive.** A process with protocol `<Add: Int \| Get: (Pid Dyn)>` (a reply pid is `(Pid Dyn)`, because a specific protocol cannot be decoded, §4.6) receives `(Add 1)` from Vaisto code, and a raw Erlang `Pid ! {'Add', <<"x">>}`. The second is stray: it is never bound to a typed clause and is handled by the stray policy. | typed clause runs once; stray handled |
| C16 | **Crash as an effect.** `safe-div` with its precondition verified has an effect row without `Crash` and is reported total. `(try (div x y) [catch [:error e 0]])` type-checks with `Crash` handled. `head` on an unrefined list keeps `Crash` in the row. The effect-row parts need Phase 4; the `try` part works from Phase 1a. | total / handled / partial |
| C17 | **Reflection.** The two constant `run` calls of §16.4: the `Read` job compiles, the `Write` job fails with the rendered required/available lines. A `run` call on non-constant authorities without a `le?` check fails. | compiles / error / error |
| C18 | **Lawless instances stay out.** A refinement over a `Float` (`{v :float \| (> v 0.0)}`) is rejected with "refinements on Float are not supported". A refinement using `Num`'s `+` at type `Int` is accepted. | rejected / accepted |
| C19 | **Guard definedness.** `(match n [x :when (> (div 10 x) 1) 1] [x :when (< (div 10 x) 2) 2] [_ (safe-div 1 n)])`. The third clause's obligation `n ≠ 0` **fails**: at `n = 0` both guards crash, count as false, and the third clause runs. Without `def(g)` it would pass vacuously. | error on `(!= n 0)` |
| C20 | **Only terminating functions enter the logic.** `(defn weird [x :int] {v :int \| (and (> v 0) (< v 0))} (weird x))` verifies under partial correctness but is reported as a vacuous guarantee, and it is not liftable. A client refinement that mentions `(weird 0)` is rejected. | error; not liftable |

| # | Property | Expected |
|---|---|---|
| K10 | **Embedding–projection laws** (§4.6), for generated decoders over random values and random `Dyn` terms: `down(up v) = Ok v`; and `down d = Ok v ⇒ up v = d`. | pass |
| K11 | **The bridge commutes** (§21.1): for every program checked in Phase 2.1, `adapter(HM(P)) = erase(elaborate_refined(P))`. | equal, or reported as a bridge bug |
| K12 | **Oracle independence** (§4.3): every primitive has a test in which evaluator and BEAM, or evaluator and SMT, use different implementations. | pass |

---

## 15. Migration risks: `:any`, externs, unknown calls, lambda fallback (assignment item 13)

The new architecture turns every one of these from "a hole the refinement checker must work around" into "a thing that does not exist in Core".

### 15.1 `:any`

`:any` has no Core counterpart. Each source in the inventory (§15.5) becomes one of three things:

- **A type variable.** Untyped parameters already are one (`freshen_any_params`).
- **A type error.** A heterogeneous list, an unknown call, a failed join.
- **An explicit `Dyn`**, decoded before use. Interop results, model output, untyped messages.

The Phase 0 adapter (§9.3) emits `Dyn` wherever today's typed AST has `:any`, and Core Lint reports every use site. That report is the Phase 1a work list, generated rather than hand-maintained.

**Risk:** existing programs lean on `:any` (untyped `defn_multi`, empty lists, interop). Programs that only pass such values through are unaffected; programs that use them at concrete types will need a `decode` or a real type. That pressure is intended.

### 15.2 Unknown qualified calls

A call to a function that no interface or extern declares cannot be elaborated into Core, so it is an error. This finally matches DESIGN.md:235 ("Calling without declaring = compiler error"). In Phase 0 the adapter emits `Dyn` for such calls and Core Lint reports them; the transition from warning to error happens when HM elaborates to Core directly (Phase 1a).

### 15.3 Externs

An `extern` declaration becomes a typed wrapper over the `external` effect:

- **Argument types** are checked statically on the Vaisto side, as today but without the silent fallback (D8).
- **The declared result type is a decode target.** The raw `external` effect returns `Dyn`; the wrapper decodes it to the declared type on every call and crashes with a decode error when the foreign function returns something else. A wrong declaration becomes a crash with a clear reason, never a mistyped value. The result is ordinary typed data, not `Dyn`, so declared externs stay convenient.
- **Argument refinements** are allowed. They only add obligations for callers, which is always sound.
- **Result refinements** are allowed **only as decode targets** (runtime-checked when executable). A result refinement that cannot be checked at runtime is rejected: it would be an unchecked assumption about foreign code, which is exactly an `assume`.
- **Externs cannot return sealed types.** A sealed decoder verifies an authenticator that only a Vaisto issuer can produce, and Core Lint rejects extern result types that contain a sealed type (§4.6).

### 15.4 Lambda fallback

It disappears. Lambdas are typed by the same elaborator as everything else (§9.1), so `fallback_lambda/3` and the prefix-based `infer_should_fallback?/1` classification go away. With them goes the risk that a genuine type error, misclassified as an inference limitation, silently turns into `:any`.

### 15.5 `:any` source inventory

From the escape-hatch audit of `type_checker.ex`, `infer.ex`, `unify.ex`, `parser.ex` and `tc_ctx.ex`. Counts are distinct code sites. In the target design every row becomes a type variable, a type error, or an explicit `Dyn` (§15.1). The last column says how a value from that source is handled **in the interim**, before Phase 1a removes `:any`.

| Category | Sites | Examples | Interim treatment (before `Dyn`) |
|---|---|---|---|
| Missing annotations | 11 | untyped params (`parser.ex:1056,1059`); no return type (944-970); first-pass legacy signatures (3415, 3419) | Refined functions must annotate every refined position; unrefined positions are `true`. |
| `defn_multi` | 21 | pass-1 signature `{:fn, [:any], :any}` (3518); all pattern variables `:any` (2482-2517) | Excluded from Phase 2 (§4.4). |
| Join fallback | 4 | `join_types(_, _) -> :any` (3916); empty list (3892) | Opaque; P0-3 rejects the heterogeneous-list case. |
| Unknown qualified calls | 2 | 713; `infer.ex:326` | Opaque; becomes an error in Phase 1a (§15.2). |
| Externs (trust sites) | 3 | 725, 728; `infer.ex:313` (arguments never unified inside lambdas) | Parameter refinements allowed; result types and refinements are decode targets (§15.3). |
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

## 16. The standard library as algebraic theories, and the capability example (assignment item 14)

### 16.1 The method

Everything in this section is **library**, built from the four primitives and the module system. The only new theory it needs is the type invariants and abstract measures of opaque types (§4.1, §4.8). Each domain is written the way algebra is written:

- a **signature**: an opaque or sealed type plus the functions, predicates and abstract measures exported on it;
- **laws** over that signature;
- one or more **models**: concrete representations, hidden behind the type, that satisfy the laws.

Clients reason **only in the theory**: their refinements mention the exported symbols, and the checker may use the laws and the type invariant. They never see a representation. This is the ordinary algebraic-specification discipline (abstract data types with equations), and opacity gives it for free. Inside the defining module a predicate is concrete. At the module boundary its body is hidden along with the representation, so outside it is an uninterpreted symbol constrained only by the exported laws.

**Library forms.** None has runtime meaning:

- **`defpred`** names a pure predicate: concrete inside the module, abstract outside it. It may carry an `:explain` string (§16.5).
- **`deflaw`** states a closed law over exported symbols. Inside the module it is an **obligation** about the model; outside it is an **axiom**.
- **`definvariant`** states a type's invariant (§4.1). Every constructor and every function that returns the type must establish it; clients assume it.

**Which functions may appear in the logic.** An exported function becomes a logic symbol, as `meet`, `compose` and `plus` do, only if its effect row is empty (no `Crash`) and it is **proved to terminate**: non-recursive, or structurally recursive in the discipline of §12. Its axiom is guarded by its precondition: `pre(x) ⇒ post(x, f(x))`. A function verified only for partial correctness never becomes a symbol. Otherwise a function that never returns could export an impossible result refinement, such as `{v | v > 0 ∧ v < 0}`, as the axiom `false` (C20).

**Runtime accessors.** An abstract measure is logic-only. When runtime code needs the value, the module exports an accessor whose result refinement ties it to the measure: `(defn scope-of [c :Capability] {s :A/Authority | (= s (scope c))} ...)`.

**How clients use laws.** A client VC is quantifier-free. The checker instantiates each law on the ground terms of the VC before handing it to Z3, in **one round** over the terms of the original VC, never over terms that instantiation itself created, so it always terminates (`meet-greatest` would otherwise keep producing new `meet` terms). Instantiating a true law is always sound. Whether the instances drawn from the ground terms suffice for *completeness* is checked per domain in Phase 3 (Q27); when they do not, the answer is an honest `unknown`.

**Law status.** Every exported law and invariant carries a status, recorded in the interface:

- `proved` by the solver inside the module;
- `proved-lean` (§17.4);
- `tested` by generated property tests;
- `assumed`.

Clients see the status, and a build policy can demand `proved` for a domain (Q31). A `tested` or `assumed` law is a visible, auditable assumption, never a silent one.

| Domain | Structure | Laws | Deciding client obligations | Models |
|---|---|---|---|---|
| Authority | bounded meet-semilattice: `meet`, with `le a b ⇔ meet a b = a` | `meet` idempotent, commutative, associative; `le` a partial order | law instantiation on ground terms; constants by reflection | record of booleans; grants over hierarchical resources |
| Capability | a sealed pair of an actor and an `Authority`, minted only by the issuer | scope never grows under `attenuate` and `delegate` | from result refinements plus the authority laws | one |
| Budget | ordered commutative monoid `(ℕᵏ, +, 0, ≤ pointwise)` | monoid laws; `≤` compatible with `+` | linear integer arithmetic | resource records |
| Transition | the free category on admissible steps | identity; associativity | endpoint laws by the solver; path equality in Lean | step lists with endpoints |
| Provenance | the free join-semilattice on sources (§4.7) | associativity, commutativity, idempotence | by construction | finite sets |
| Protocol | a sum of message constructors | — | exhaustiveness | variant types (§4.6) |

### 16.2 Authority: a sealed meet-semilattice with a privileged issuer

**Authority must not be mintable.** If anyone can construct an `Authority`, or call a function that returns the top element, then no refinement about attenuation means anything. So `Authority` is **sealed** (§4.1, §4.6), and the only functions that create authority from nothing (`top`, `grant`) are exported **only to the issuer**: a module listed as privileged in the pinned build configuration, alongside observers (Q14). Every other module can only *narrow* authority it was handed.

```scheme
(ns Vaisto.Authority)

(deftype Action (Read) (Write) (Admin))
(deftype Grant [resource :string action :Action])
(deftype sealed Authority [grants (List Grant)])        ; model: grants over string-named resources

;; public signature
(defpred le [a :Authority b :Authority]
  :explain "requires authority not available here"
  ...)                                                  ; concrete here, abstract outside
(defn meet [a :Authority b :Authority] {m :Authority | (and (le m a) (le m b))} ...)
(defn le? [a :Authority b :Authority] {r :bool | (iff r (le a b))} ...)

;; issuer-only: creating authority from nothing
(defn top [] :Authority ...)                                   ; :issuer-only
(defn grant [resource :string action :Action] :Authority ...)  ; :issuer-only

(deflaw le-reflexive  [a :Authority] (le a a))
(deflaw le-transitive [a :Authority b :Authority c :Authority]
  (=> (and (le a b) (le b c)) (le a c)))
(deflaw meet-greatest [a :Authority b :Authority c :Authority]
  (=> (and (le c a) (le c b)) (le c (meet a b))))
```

- **No join is exported.** Union is where authority grows.
- **Laws mention only exported symbols.** `meet` is exported to the logic as an abstract function symbol constrained by its result refinement, so it may appear in a law. It qualifies (§16.1) because it is pure, non-recursive and deterministic: attenuation computes the next MAC, it does not consult the issuer (§4.6).
- **Two models, one theory:**
  - **Record of booleans**, a product of two-element lattices: laws decidable, status `proved`.
  - **Grants over string-named resources** (`"repo:vaisto/src"` under `"repo:vaisto"`): the concrete `le` checks path prefixes and action order, over a **canonical** representation (the grants sorted and deduplicated, so that equal authority has equal content). The logic has no strings (§4.4), so these laws have status `tested` until Phase 8 proves them in Lean.

  Clients cannot tell the models apart.

**Reflection for constants.** When both arguments of `le?` are closed constants, the checker may run `(le? a b)` in the reference evaluator at compile time and add `(le a b)`, or its negation, as a `Static` fact. The conditions:

- `le?` is total in the sense of §4.4 (an empty effect row, and proved to terminate), and compile-time evaluation runs under a **fuel limit**, so a mistake cannot hang the compiler;
- its result refinement holds, with its status recorded in the fact's provenance;
- the arguments are closed.

A closed constant of a sealed type can only appear in the issuer's own code, or in code the issuer handed a constant to, so reflection does not bypass sealing. Reflection evaluates `le?` on the canonical *content*; the authenticator plays no part in `le`.

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

**In Liquid Core the answer is brands** (§4.1). `Accepted` is `{#Adm.Accepted: Unit, id: Int}` and `Executed` is `{#Adm.Executed: Unit, id: Int}`. `deploy` requires the first row, so probe (a) with an `Executed` value is a type error, while a genuinely row-polymorphic `get-id` still accepts both. The surface fixes P0-1 (D1) and P0-21 (D22) remain necessary so that HM produces the right Core; Core Lint rejects what HM gets wrong in the meantime.

Three mechanisms, and only the second involves the logic:

- **Phases are brands.** The phase lives in the type.
- **Relations between values are refinements.** Ownership, "produced by this job" and "budget did not grow" depend on runtime values. For example, `(defn attach [j :Job c {k :Credential | (= (owner k) (. j :id))}] ...)`, with `owner` an abstract measure of the sealed `Credential`.
- **"Only admission produces `Accepted`" is sealing.** `Accepted` is sealed to the admission module (§4.1).

**Transitions form the free category on admissible steps.**

- **Objects** are states.
- **Generating morphisms** are single steps whose evidence justifies them.
- **Morphisms** are paths: composable sequences of steps.
- **Identity** is the empty path, and **composition** is concatenation, defined when endpoints match.

`Transition` is opaque, with **abstract measures** `before` and `after` and a **type invariant** saying its steps form a valid path (§4.1):

```scheme
(ns Vaisto.Transition)
(deftype sealed Transition [from :State to :State steps (List Step)])   ; sealed: the invariant is not executable

(defmeasure before [t :Transition] :State ...)   ; exported as abstract symbols
(defmeasure after  [t :Transition] :State ...)
(defmeasure chain-ok [steps (List Step)] :bool           ; one argument, structural
  [[] true]
  [[s | rest] (and (valid-step (. s :before) (. s :op) (. s :after) (. s :evidence))
                   (if (is Nil rest) true (= (. s :after) (. (head rest) :before)))
                   (chain-ok rest))])
(definvariant Transition                                  ; assumed by clients
  (and (chain-ok (. t :steps))
       (if (is Nil (. t :steps)) (= (. t :from) (. t :to))
           (= (. t :from) (. (head (. t :steps)) :before)))))

(defn step [b :State op :Operation a :State e {x :Evidence | (valid-step b op a x)}]
  {t :Transition | (and (= (before t) b) (= (after t) a))} ...)
(defn id [s :State] {t :Transition | (and (= (before t) s) (= (after t) s))} ...)
(defn compose [p :Transition q {t :Transition | (= (before t) (after p))}]
  {r :Transition | (and (= (before r) (before p)) (= (after r) (after q)))}
  ...)                                                  ; concatenation, inside the module

(deflaw endpoints-associate [p :Transition q :Transition r :Transition]
  (=> (and (= (before q) (after p)) (= (before r) (after q)))
      (and (= (before (compose (compose p q) r)) (before (compose p (compose q r))))
           (= (after  (compose (compose p q) r)) (after  (compose p (compose q r)))))))
```

- **A consumer holding any `Transition` knows it is a valid path**, because the invariant is assumed of every value of the type. `chain-ok` is an ordinary one-argument structural measure (§12). Its definition uses `valid-step`, so `step`'s precondition establishes the invariant of a one-step path by a single unfolding. `Transition` is **sealed**, not merely opaque, because `valid-step` concerns observed evidence and cannot be checked by a decoder. Refinements over `before` and `after` are writable without seeing the representation.
- **`compose` is implemented inside the module** by concatenation, and must re-establish the invariant: concatenating two valid paths with matching endpoints gives a valid path. That needs induction over the step list, so the invariant obligation for `compose` has status `proved-lean` (or `tested` before Phase 8), recorded in the interface.
- **Associativity at the level of endpoints** (the law above) is decidable, so its status is `proved`. Equality of the composed step lists is associativity of list concatenation, which needs induction: `proved-lean`.

This is the brief's `ValidTransition(before, job, result, after)` made exact, and the typed path through operational state as a free category rather than an analogy.

### 16.4 The capability example

```scheme
(ns Vaisto.Capability)
(import Vaisto.Authority :as A)

(deftype sealed Capability [actor :atom scope :A/Authority])
(defmeasure scope [c :Capability] :A/Authority ...)      ; abstract outside, logic-only
(defn scope-of [c :Capability] {s :A/Authority | (= s (scope c))} ...)   ; runtime accessor

(defn mint [actor :atom a :A/Authority] :Capability ...)  ; :issuer-only

;; delegation cannot invent authority
(defn delegate [parent :Capability child {c :Capability | (A/le (scope c) (scope parent))}]
  {r :Capability | (and (= r child) (A/le (scope r) (scope parent)))}
  child)

;; attenuation only narrows; no join exists to undo it
(defn attenuate [cap :Capability extra :A/Authority]
  {c :Capability | (A/le (scope c) (scope cap))}
  ...)                                                    ; builds (meet (scope cap) extra), inside the module

(deftype Job [id :int required :A/Authority])
(defn run [cap :Capability job {j :Job | (A/le (. j :required) (scope cap))}] :Result ...)
```

Demonstrations:

1. **Constants, decided by reflection.** In the issuer's own configuration code, `(run (mint :ci (A/grant "repo:vaisto" (Read))) (Job 1 (A/grant "repo:vaisto/src" (Read))))` **compiles**: the evaluator decides `le?` on the two constant authorities. The same call with `(Write)` in the job is a **compile error**:

   ```text
   error: requires authority not available here
     at line 14
       (run ci-cap write-job)
                   ^ `run` requires (le (. write-job :required) (scope ci-cap))
     note: required: repo:vaisto/src write
     note: available: repo:vaisto read
   ```

2. **Runtime data, the realistic case.** `(if (A/le? (. job :required) (scope-of cap)) (run cap job) (deny job))` compiles; `(run cap job)` outside such a check does not. *Check once at the boundary, carry the fact statically.* `le?` is executable, `le` is a proposition, and a refinement connects them.
3. **Chains.** `(delegate (attenuate cap extra) c2)` verifies. `attenuate`'s result refinement gives `le(scope(a), scope(cap))`, `delegate`'s precondition gives `le(scope(c2), scope(a))`, and the instantiated `le-transitive` gives `le(scope(c2), scope(cap))`. None of this depends on which model of `Authority` is in use.
4. **Minting is impossible outside the issuer.** `(mint ...)`, `(A/top)` and `(A/grant ...)` are rejected in ordinary modules. So is decoding a `Capability` from `Dyn` without its authenticator (§4.6).

It needs values and refinements over records (Phase 2), static sealing (Phase 1a) and runtime authentication (Phase 3), and nothing from the effect *type system*. Decoding a sealed value from a message asks the issuer, which is an effect handled at runtime.

### 16.5 Explanations

`:explain` on `defpred` is display text only; it cannot change what is checked. The "required / available" lines come from conjunct splitting (§13.2) where the predicate is transparent. For an abstract predicate decided by reflection, they come from rendering the two constants the evaluator already has.

### 16.6 Budgets: an ordered commutative monoid

```scheme
(deftype opaque Budget [cpu :int tokens :int net :int])
(definvariant Budget (and (>= (. b :cpu) 0) (>= (. b :tokens) 0) (>= (. b :net) 0)))
(defpred within [a :Budget b :Budget] ...)              ; pointwise <=, abstract outside
(defn plus [a :Budget b :Budget] :Budget ...)            ; pure, non-recursive: liftable (§16.1)
(defn spend [b :Budget c {k :Budget | (within k b)}]
  {r :Budget | (and (within r b) (= (plus r c) b))}     ; r = b - c, pointwise
  ...)
```

The invariant (every component is non-negative) and `within c b` make `b − c` non-negative, so `spend`'s body can build a valid `Budget`; draft 2's version could not. The laws (monoid, and `≤` compatible with `+`) are linear integer arithmetic, so their status is `proved`. Refinements guarantee monotonicity **along each path**. They do not stop the same budget value being spent twice on two branches. That is what an affine discipline would add, it would be new core theory, and it is deferred (§17.5).

### 16.7 Staging

| Step | Needs | Unlocks |
|---|---|---|
| `defpred`, `deflaw`, `definvariant`, law status, instantiation, reflection | Phase 2 records; invariants of opaque types | the method of §16.1 |
| Authority, with both models, and an issuer | the above; static sealing (Phase 1a); runtime authentication (Phase 3) | §16.2 |
| Capability example | Authority | §16.4, the first demo that justifies the project |
| Phases and transitions | brands, opacity, invariants | §16.3 |
| Budgets | the above | §16.6 |
| Evidence-backed transitions | Phase 5 observers | `valid-step` over observed facts |

All of this is Phase 3 (§21). Everything except evidence-backed transitions works without the effect system.

---

## 17. Contracts, operators, intent, and theorems

### 17.1 Task-contract operators and `Ctx` (brief §13, §33)

Operators are library code over the effect algebra, not compiler special cases. They ship as library code in Phase 7; the Phase column below says when each operator's *refinement contract* becomes expressible.

**The headline check survives.** Today's `prompt_output_mismatch` is row unification: a `generate :extract T` rejects a prompt whose output lacks `T`'s fields. In Core it becomes a row-polymorphic signature. For a record `T` with fields `x₁: τ₁ … xₙ: τₙ`, the elaborator instantiates `generate : (forall ((ρ Row)) (-> ((p (Prompt In {x₁: τ₁, …, xₙ: τₙ | ρ})) (d (Decodable T))) Model T))`. The row is spelled out per `T`, so no type operator is needed, and the hidden `Decodable T` dictionary carries the runtime schema and the decoder (§4.6). The prompt's output row must contain `T`'s fields and may contain more, and missing fields are an ordinary row type error with the same field-level diff. The check moves from a special case in the checker into the type of a library function. Today only `generate` exists, inside `pipeline`, with unfinished payload threading (TODO at `type_checker.ex:1524-1531`).

| Operator | Built on | Refinement contract it can expose | Kind of any facts | Phase |
|---|---|---|---|---|
| `retrieve` | `external`, or an observer | none on the documents; external data is at best observed | `Observed` via an observer | 7 |
| `rerank` | pure code or `model-call` | length-preserving, `len(out) = len(in)`; being a permutation is not expressible in the logic | `Static` | 3 (pure); 4 (model-based) |
| `generate` | `model-call` | none about content: model output is a **candidate** and arrives as `Dyn` | — | never |
| `extract` | pure `decode` or `model-call` | a result refinement if the extractor is checked Vaisto code (`decode` into a refined type) | `Static` | 7 |
| `verify` | a checked predicate, or an observer | the bridge from observation to fact; its confidence update stays stochastic | `Static` or `Observed` | 5 |
| `tool` | `external` | **requirements on the request** (the capability example applies: calling a tool requires authority); the response is `Dyn` unless an observer returns a witness | `Static` (request), `Observed` (response) | 3 / 5 |
| `branch` | pure code | a payload predicate refines each arm (§10.1); a predicate over `conf` gives **no** facts, deliberately | `Static` | 2 |
| `map` | pure code | `len(out) = len(in)`; per-element refinements need nested refinements, not yet admitted | `Static` | 2 |
| `parallel` | process effects | each branch starts from the input's facts; **no fact relates one sibling's output to another's** | `Static` | 6 |
| `fold` | pure code | needs an explicit invariant, a refinement on the accumulator checked at the start and after each step | `Static` | after refined lambdas (later than 2.2) |
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
- **Identity by digest.** An accepted result names the contract by its **contract digest**: the canonical bytes (§5.1) of the contract's *semantic* content only, meaning its types, requirements, guarantees and laws, never a file name. It also covers the semantic digests of every theory the contract mentions, such as `Authority`'s laws and abstract predicates, so changing an imported theory changes the contract's identity. It is deliberately **not** the interface digest, which also covers build provenance (solver version, import digests). Otherwise a Z3 upgrade or an unrelated dependency change would un-accept every result.
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
| Refinement soundness | if every VC is valid, values satisfy their refinements (§4.4) |
| Erasure | `eval(materialize(e)) = eval(erase(materialize(e)))` for verified pure `e` (§7.2) |
| Effect lowering | lowering preserves the sequence of effect requests and the use of their results |
| Replay determinism | same program, input and trace give the same result (§4.5); later, the same for a valid causal history |
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
| Core Lint, including brand and row rules | M1, M22 | 0 |
| Decoder and receive-guard generator (embedding–projection laws) | M16, M25 | 1a |
| Row-evidence generator | M22 | 1a |
| Canonical codec and digests | M2 | 0 |
| Primitive semantics table | M5 | 0 |
| Predicate translator (surface predicates to IR; faithfulness is its job, not HM's) | M6 | 2 |
| Refinement checker: VC generation, branch and pattern rules, SMT-LIB lowering | M7, M8, M9 | 2 |
| Materialization of runtime checks, before erasure | M12 | 2 |
| Decoding entry points (wrapper generator) | M20 | 2 |
| Pinned build configuration: observers, issuer, `external` allowlist | M15, M19 | 1a |
| Erasure | M12 | 2 |
| Measure checker and constructor strengthening | M11 | 2 |
| `defpred` inliner and `deflaw` checking | M21 | 3 |
| Interface writer and reader | M18 | 1b |
| Effect handlers on BEAM: their faithfulness to reality ("performed the request and reported its result") | M14 | 4 |
| Privileged observers and their declared axioms | M15 | 5 |
| Admission loader and the build's signing key | M19 | 5 |
| Lean model and proof-checker configuration | — | 8 |

**Not trusted.** None of these is ever a source of facts.

- **The HM elaborator.** Core Lint checks its output.
- **The backend** (lowering, OTP compiler, BEAM) as a *compiler*: the harness tests it against the evaluator. The handlers' faithfulness to reality is a separate, trusted item above.
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
| Library laws with status `tested` or `assumed` | client proofs in that domain | anything beyond the status says | the status is recorded in the interface and visible to clients; a build policy may require `proved` (M24) |
| Test runner, tool receipt, kernel observer, remote executor | the specific observation it issued | anything else, including the property the observation is *about* | privileged observer, runtime-unforgeable receipts (§4.7) |

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
| A received message has its protocol type | anyone (on BEAM anyone can send) | the receive guards, decoding | the stray policy handles the rest (M25) |
| A library law holds | the library author | the solver, Lean, or tests, per its status | clients read the status (M24) |
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
| Soundness assumption | Facts are true on the path where they are added; every obligation in §4.4 is generated. |
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
| Soundness assumption | Core's evaluation order (§4.2, §4.3), not any backend's. |
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
| Soundness assumption | The live handler performs the requested effect and reports its result faithfully. Replay determinism itself is not assumed: it follows from the fold being unique (§4.5). |
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
| Failure mode | A forgery route: construction outside the module, `Dyn`, externs, a forged interface, a hand-built tuple at runtime. Closed by opacity (§4.1), authenticator checks in sealed decoders (§4.6), the extern rule (§15.3), the privileged `exit` (§4.5) and interface checks (§8.3). |
| Diagnostic | "`TestReceipt` can only be produced by `Vaisto.Tests`". |
| Test strategy | K9; one compile-error test per forgery route; runtime lookup test. |
| Creates / establishes / checks | The observer creates; its run against reality establishes; admission checks. |

### M16. `Dyn` and `decode`

| | |
|---|---|
| Soundness assumption | Generated decoders satisfy the embedding–projection laws of §4.6, including executable refinements. |
| Trusted component | The decoder generator. |
| Runtime representation | Generated decode functions. |
| Erasure | Not erased: `decode` is a runtime check by design. |
| Failure mode | A decoder that accepts a wrong value lets a mistyped value into typed code. |
| Diagnostic | Statically: "`x` has type Dyn; decode it first, e.g. (as Int x)". At runtime: a decode error naming the type and the offending value. |
| Test strategy | K3; K10 (both embedding–projection laws on random values and random `Dyn` terms). |
| Creates / establishes / checks | A foreign source creates a value; `decode` establishes its type; Lint checks that nothing skips `decode`. |

### M17. Opaque types

| | |
|---|---|
| Soundness assumption | Core Lint enforces construction and inspection privacy; for sealed types, the authenticator cannot be forged (a protected issuer table or a signature). |
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
| Soundness assumption | External callers use the exported name. The internal version, which Vaisto callers use, must also be exported for cross-module calls, so its protection is A4's whole-node assumption, not the wrapper. |
| Trusted component | Wrapper generator. |
| Runtime representation | Each exported refined function gets a wrapper that decodes its arguments, including executable refinements, before calling the internal version. |
| Erasure | Not erased; opt-in. |
| Failure mode | None silent: a refined function whose parameter refinements are not executable **gets no entry point at all**, so non-Vaisto callers cannot reach it through a wrapper that would skip the check. |
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
| Diagnostic | `:explain` text; "the path-equality form of associativity could not be checked automatically; its status is `proved-lean`"; laws over the string-resource model are reported the same way. |
| Test strategy | §16.2 laws as conformance programs; recursion rejection. |
| Creates / establishes / checks | The library author creates; the solver or Lean establishes; the checker records the result. |

### M22. Brands and row evidence

| | |
|---|---|
| Soundness assumption | A brand label is produced only by its type's constructor; row evidence points at the field it claims to. |
| Trusted component | Core Lint (brand and row rules); the evidence generator. |
| Runtime representation | The brand is the record's tuple tag; evidence is a hidden accessor argument. |
| Erasure | Not erased: evidence is how the program runs. |
| Failure mode | Evidence for the wrong offset: silent wrong field. Caught by K1 against the evaluator, which accesses by label. |
| Diagnostic | "`deploy` needs an `Accepted`, found an `Executed` (same fields, different type)". |
| Test strategy | C14; K1 on every row-polymorphic program; property test: evidence for random rows reads the labelled field. |
| Creates / establishes / checks | The elaborator creates evidence; the representation establishes it; the harness checks. |

### M23. Proof by reflection

| | |
|---|---|
| Soundness assumption | The evaluated function is pure and total, and its result refinement holds (with the law status recorded). |
| Trusted component | The reference evaluator (already trusted); the status of the reflected refinement. |
| Runtime representation | None; compile time only. |
| Erasure | n/a |
| Failure mode | A `tested` refinement that is actually false makes a false constant fact. The fact's provenance names the status. |
| Diagnostic | Required/available lines rendered from the evaluated constants (§16.5). |
| Test strategy | C17; reflection refused for effectful, partial or non-constant calls. |
| Creates / establishes / checks | The checker creates; the evaluator establishes; the law status bounds the trust. |

### M24. Algebraic theories and law status

| | |
|---|---|
| Soundness assumption | Every exported law holds in the module's model, to the degree its status says: `proved`, `proved-lean`, `tested` or `assumed`. |
| Trusted component | The solver for `proved`; Lean for `proved-lean`; nothing for `tested` or `assumed`, which are visible assumptions. |
| Runtime representation | None; laws are compile time. |
| Erasure | Removed. |
| Failure mode | A false `tested` law makes client proofs unsound for that domain. A policy can require `proved`. |
| Diagnostic | "`Vaisto.Authority/le-transitive` is tested, not proved; this build requires proved laws for Authority". |
| Test strategy | Each law's obligation checked inside its module; generated property tests for `tested` laws; §16 examples as conformance programs. |
| Creates / establishes / checks | The library author creates; the solver, Lean or tests establish; clients check the status. |

### M25. Typed receive and stray messages

| | |
|---|---|
| Soundness assumption | Receive guards implement `down_M` (§4.6). |
| Trusted component | The decoder and guard generator. |
| Runtime representation | Guards on each receive clause; the stray policy. |
| Erasure | Not erased. |
| Failure mode | A missing guard lets a mistyped message bind to a typed clause. |
| Diagnostic | Runtime: "stray message in `worker` (protocol `WorkerMsg`): {…}" under the log policy. |
| Test strategy | C15; K10 on protocol decoders; foreign-send tests. |
| Creates / establishes / checks | Anyone can create a message; the guards establish its type; the process's stray policy handles the rest. |

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
| Q8 | Refinements on record fields (data invariants) | **resolved for opaque types** (§4.1); invariants on transparent record fields remain later work | — |
| Q9 | Should `:when` guards imply static preconditions? | open | no; add the two lints of §10.4 |
| Q10 | Unreachable branch: warning or error? | open | warning |
| Q11 | Canonical encoding for digests | **resolved**: canonical S-expressions (§5.1) | residual: exact leaf hints |
| Q12 | Hot code loading of refined modules | **partly resolved**: the admission loader (§8.4); raw loading is out of scope | — |
| Q13 | Second solver cross-check | open | not in the first refinement phases |
| Q14 | Where privileged observers are declared | open | build configuration |
| Q15 | Strict or short-circuit `and`/`or` | **resolved**: Core defines short-circuit (§4.3) | — |
| Q16 | Row polymorphism versus nominal phases | **resolved**: brands (§4.1) | — |
| Q17 | How witness types resist `:any` | **resolved**: `Dyn` converts to nothing; sealed types decode only with a verified authenticator (§4.6, §4.7) | — |
| Q18 | New modules and a new compiler back half versus AGENTS.md | **open, owner decision**; larger than in draft 1 | accept: a separate core is the point of the design |
| Q19 | Effect polymorphism for higher-order functions | **resolved**: effect rows reuse the row algebra (§4.5) | — |
| Q20 | Polymorphism in Core: explicit type abstraction and application, or let-schemes | **resolved**: explicit (§4.1, §9.2) | — |
| Q21 | When to retire each old emitter | **resolved**: at harness parity on the whole corpus (§7.3) | — |
| Q22 | Processes: plain Erlang processes, or GenServer by default | open; typing is settled (§4.6) | plain processes in Core; OTP behaviours as library |
| Q23 | Derived facts: a fourth origin, or premise sets | **resolved**: premise sets (§4.7) | — |
| Q24 | Is `observe` a separate effect or a privileged use of `external`? | **resolved**: separate (§4.5), so privilege is visible in the effect row | — |
| Q25 | Evaluator implementation: Elixir now; later extracted from the Lean model? | open | Elixir now; revisit after Phase 8 |
| Q26 | Default stray-message policy (§4.6) | open | log and drop; crash as an opt-in for strict processes |
| Q27 | Does instantiating the semilattice laws on the ground terms of a VC suffice for completeness (§16.2)? | open, to verify in Phase 3 | if not, `unknown` is reported, never a false proof |
| Q28 | Eager or lazy decoding of large message payloads (§4.6) | open | eager for tags and scalars; measure before optimizing lists |
| Q29 | `Pid` variance: an explicit restriction coercion, or invariance only (§4.1) | open | invariance first |
| Q30 | Anonymous rows: keep Erlang maps, or tagged tuples with a synthetic tag (§4.1) | open | maps, for interoperability; inline accessors when the representation is known |
| Q31 | Which law statuses a build accepts per domain (§16.1) | open, owner decision | `proved` for authority and budgets in production builds |
| Q32 | Module-qualified runtime tags for records (§4.1): a representation change visible to Erlang code | open | qualify, and give Erlang callers an accessor instead of bare-tag matching |
| Q33 | Issuer keys and availability: where the MAC key lives, how it rotates, and what sealed decoding does when the issuer is down (§4.6) | open | key held by the issuer process only; decoding fails closed |
| Q34 | Surface syntax of the `(unreachable)` marker (§11.3) | open | a form that is itself an obligation `false` |
| Q35–Q40 | The type system: required signatures, local `let` generalization, surface quotients, definitional equivalences, grades, `Float` discreteness | open | see `liquid-types.md` §14 |

---

## 21. Roadmap

### 21.1 The bridge: a commuting square

Refinements do not have to wait for the new backend. A refined program has two images:

```text
                 HM
   Surface ────────────► typed AST ──── old emitters ───► BEAM   (runs the program)
      │                      │
      │ refined elaboration  │ Phase 0 adapter
      ▼                      ▼
 VerifiedCore ── erase ──►  Core⁻                                (checks the program)
```

The square **commutes** when `adapter(HM(P)) = erase(elaborate_refined(P))`, as Core terms up to renaming. When it does, the Core that was verified is, after erasure, exactly the program the old emitters run. So refinements can be checked through Core while programs still run through today's backends, and the evaluator-versus-BEAM harness (§6.3) covers those backends too. The square is checked for every program as part of the harness. A program for which it fails **does not build**.

**Gates.** The bridge leans on A2 for the old emitters, which this document itself shows violate it for some constructs. So Phase 2.1:

1. requires P0-4 (`and`/`or`, D5), P0-10 and the Elixir part of P0-14 for every construct a refined program uses;
2. restricts refined code, and callers of refined functions, to constructs on which the harness shows both old emitters agreeing with the evaluator. **Externs are excluded**, because the old emitters do not decode their results;
3. makes a failed square a build error, never "runs unchecked".

Before Phase 1a the square commutes almost by construction, since the refined elaboration *is* the adapter plus the side table. What it then checks is that the refinements were attached to the right terms. It is the parity restriction that carries A2. The bridge needs one parser change. `(defn f [y {d :int | p}] ...)` becomes the plain `(defn f [y :int] ...)` that HM sees, plus a sibling `(refine-sig f ...)` form that only the refined elaboration reads. This is draft 1's side-table design (Appendix B), kept as a temporary bridge. The bridge is retired when HM elaborates directly to Core (Phase 1a) and the new lowering takes over (Phase 1b).

### 21.2 Phases

| Phase | Name | Delivers | Depends on |
|---|---|---|---|
| **0** | Foundation | the written Core spec (§4 as a standalone document), including the full type algebra; canonical codec; reference evaluator with pure and scripted handlers; Core Lint; the typed-AST-to-Core adapter; differential harness; parser round-trip tests; the defect fixes marked "keep, Phase 0" in §21.3 | — |
| **2.1** | Refinements, first step | `Int`/`Bool`/`List`, primitive specs, branch facts, Z3, diagnostics; C1–C7, C9–C11, C13, C19, C20 (C8 needs `Dyn` from 1a; C12 needs 1b); runs on today's emitters through the gated bridge (§21.1) | 0, with P0-4, P0-10, P0-14 |
| **1a** | The type algebra in Core | `Dyn` replaces `:any`; brands, opacity and **static** sealing; row evidence (fixes D24); `crash` as an effect and `try` as its handler; typed receive by decoding; **a bidirectional elaborator with required signatures replaces HM and `Infer`** (`liquid-types.md` §8, §9); Core version 1 with kind `Prop` | 0 |
| **2.2** | Refinements over records and sums | branded records, sums, enums in the logic; row selectors | 1a, 2.1 |
| **2.3** | Measures | §12 | 2.2 |
| **3** | Library | the method of §16.1; **runtime authentication** for sealed library types (issuer keys, MAC chains); authority, phases, transitions, budgets; reflection; **the capability demo** | 2.2 |
| **1b** | The new backend | lowering to the abstract format; old emitters retired at parity; canonical interfaces and digests (§8) | 1a |
| **4** | Effects | effect rows and inference; the handler interface on BEAM; trace recording; single-process replay (K2) | 1a |
| **5** | Evidence and admission | observers, sealed witnesses, `Observed` facts; `VSTY` admission and the loader | 3, 4, 1b |
| **6** | Process systems | causal histories; multi-process replay; distributed interface identity | 4, 1b |
| **7** | Contracts and pipelines | `defcontract`, operators and `Ctx` as library over `model-call` and `external` | 3, 4, 5 |
| **8** | Lean | formal model of Liquid Core; language theorems; library laws outside the decidable fragment (`associativity`, the string-resource `Authority` model) | 0; runs in parallel |

The numbering is historical: 2.1 comes before 1a because it depends only on Phase 0.

- **Shortest path to the first refinement demo** (`safe-div`, `clamp`, bounded index): Phase 0 with P0-4, P0-10 and P0-14, then Phase 2.1. The backend replacement is not on this path.
- **Shortest path to the capability demo:** Phase 0 → 1a → 2.1 → 2.2 → 3. Neither the new backend (1b) nor effects (4) are needed.
- **Cross-module refinements** (conformance C12) need canonical interfaces, so they wait for 1b. Before that, refinements are checked within one module.

### 21.3 Defect fixes

Draft 1's Phase 0 items are still the acceptance tests for today's defects. The last column says what the current architecture does with each.

| # | Fixes | Change | Acceptance test | Under this draft |
|---|---|---|---|---|
| P0-1 | D1, D2 | Resolve named and parameterized type annotations in `collect_defn_signature` and `check_impl_s({:defn, ...})`, reusing `resolve_named_type`. | `(defn idp [p :Point] :Point p)` called with `(Point 1 2)` type-checks; calling it with `(Other 1)` fails with a type mismatch naming both types. Same for `(Result :int :string)`. | **keep**, Phase 0: HM stays as the front end |
| P0-2 | D4 | Engine B reads primitives from `TypeEnv`; `TypeEnv`'s `/` becomes `Float`. | The D4 program fails to type-check (declared `:int`, body `Float`). | one-line fix now; superseded by merging the engines (Phase 1a) |
| P0-3 | D3 | A list literal whose element types do not join is an error when the literal meets a declared element type (or always). | `(defn main [] (List :int) [1 "a"])` is rejected. | **keep**, Phase 0 (Core Lint would also catch it) |
| P0-4 | D5 | Both backends implement the same `and`/`or` evaluation (Q15). | Parity test: `(and (!= y 0) (> (div x y) 1))` with `y = 0` returns `false` on both. | old Core backend only, while it lives; the new lowering is short-circuit by definition (§4.3) |
| P0-5 | D6 | Only capitalized call heads (`List`, `Tuple`, user types) are type annotations in return position. | `(defn f [x] (println x) x)` keeps `(println x)` in the body. | **keep**, Phase 0 (parser) |
| P0-6 | D7 | Until `opaque` is implemented, `(deftype opaque ...)` is a parse error ("opaque types are not implemented yet"). | The D7 program is rejected with that message. | **keep** as a stopgap until `opaque` lands (Phase 1a) |
| P0-7 | D8 | Warning for unknown qualified calls; error in strict mode; extern argument mismatches warn. | A test per case. | interim warning now; superseded in Phase 1a: unknown calls cannot elaborate, externs become `external` plus `decode` (§15) |
| P0-8 | D9 | `.vsi` key prefix consistent; export generalized schemes; include guarded `defn`s; do not overwrite built-in class registries on merge; `binary_to_term(..., [:safe])`; module-name check. | Cross-module call `(A/base)` is typed from `A.vsi`; a wrong argument type is an error; `(show 1)` still dispatches to the Show instance after an import. | superseded by canonical interfaces (Phase 1b, §8.1); fix the key prefix now only if cross-module work cannot wait |
| P0-9 | D10 | Dependency resolver matches imports to graph keys; cycles are reported. | Integration test passes deterministically under 50 random seeds; a two-module cycle **built from files** returns `{:error, :circular_dependency}`. | **keep, do first**: cheap, and it is the flaky test |
| P0-10 | D11, D12 | CoreEmitter compiles guarded `defn`; Elixir emitter compiles field access. | Both programs run on both backends; added to the parity suite. | superseded by the single backend (§7.3); fix only if the old emitters must live long |
| P0-11 | D14 | `apply_subst_to_ast` handles guarded `defn`. | A polymorphic guarded function has no free tvars in its typed AST. | **keep**, Phase 0 (cheap) |
| P0-12 | D13 | Located typed AST under option A, behind a flag, with the corpus property test. | The property holds across the test corpus. | superseded by Core spans (§9.4) |
| P0-13 | §15.4 | One test per `infer_should_fallback?` prefix showing a genuine type error still surfaces. | Tests pass. | superseded by merging the engines (§9.1) |
| P0-14 | D15 | Scope `let`, `try` and `receive` bindings lexically in the checker **and** in the Elixir emitter (the Core Erlang emitter already does). | The D15 program is rejected (`+` on a String). | **keep**, Phase 0 |
| P0-15 | D16 | Check the declared return type with the threaded substitution (`unify_types_s`), not `types_unifiable?`. | The D16 program is rejected. | **keep**, Phase 0 |
| P0-16 | D17 | `check_numeric_op` unifies a tvar operand with the other operand's type. | `(defn f [x] (+ x "s"))` is rejected; `(g 1.5)` is `Float` or rejected. | **keep**, Phase 0 |
| P0-17 | D18 | Sum constructors keep their declared concrete field types; only real type parameters become tvars. | `(Ok "s")` is rejected for `(deftype R (Ok :int) (Err :string))`. | **keep**, before Phase 2.2 records |
| P0-18 | D19 | Higher-order builtins unify the function's parameter with the list element type. | `(map (fn [x] (++ x "a")) [1 2])` is rejected. | **keep**, Phase 0 |
| P0-19 | D20 | Lambda parameters accept annotations like `defn` parameters. | `(fn [x :int] x)` has type `Int -> Int`. | **keep**, Phase 1a (with the engine merge) |
| P0-20 | D21 | Field access on a tvar unifies it with the recorded row. | `(get-x 5)` is rejected. | **keep**, Phase 0 |
| P0-21 | D22 | A `match` against constructor patterns unifies the scrutinee with the patterns' type, which also turns on exhaustiveness for type-variable scrutinees. | `(f (Y 1))` from D22 is rejected; a partial match on an unannotated parameter is reported as non-exhaustive. | **keep**, before Phase 3 workflow phases |
| P0-22 | D23 | `deftype` rejects a type-parameter list on records, with a clear message, until parameterized records exist. | `(deftype Job p [id :int])` is a parse error. | **keep**, Phase 0 (parser) |

---

## Appendix A. Reproduction

All commands were run in a checkout of `0ec7a82` (`lib/` and `std/` identical to `origin/main`) with `mix run -e`. They assume:

```elixir
check = fn src -> Vaisto.TypeChecker.check(Vaisto.Parser.parse(src)) end
```

```elixir
# D1: user-type param annotations
Vaisto.TypeChecker.check(Vaisto.Parser.parse("""
(deftype Point [x :int y :int])
(defn idp [p :Point] :Point p)
(defn main [] :Point (idp (Point 1 2)))
"""))
# => {:error, [%Error{message: "type mismatch", ...}]}   rendered: expected `Point`, found `Point`

# D2: parameterized annotations reach the unifier as raw AST
check.("(deftype Result (Ok v) (Err e))\n(defn f [r (Result :int :string)] :int 1)\n(defn main [] :int (f (Ok 1)))")
# => {:error, [%Error{message: "type mismatch"}]}  expected `{:call, :Result, ...}`, found `Result`

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
# => {:ok, :module, {:module, [{:deftype, :opaque, {:product, [{:Password, :any}, {{:bracket, ...}, :any}]}, ...}, ...]}}

# D8: unknown qualified call and extern mismatch accepted
check("(defn main [] :int (Nope/thing 1 2))")                                  # => {:ok, ...}
check("(extern erlang:abs [:int] :int) (defn main [] :int (erlang:abs \"x\"))")  # => {:ok, ...}

# D10: dependency order follows atom creation order
for n <- ["Elixir.Qz3", "Elixir.Qy2", "Elixir.Qx1"], do: String.to_atom(n)
graph = %{:"Elixir.Qx1" => %{file: "x", imports: []},
          :"Elixir.Qy2" => %{file: "y", imports: [{:Qx1, nil}]},       # imports as the parser stores them
          :"Elixir.Qz3" => %{file: "z", imports: [{:Qy2, nil}]}}
Vaisto.Build.DependencyResolver.topological_sort(graph)  # => order [Qz3, Qy2, Qx1]

# D11 / D12: backend gaps
Vaisto.Runner.run("(defn pos [x :int :when (> x 0)] :int x)\n(defn main [] :int (pos 5))", backend: :core)
# => {:error, %Error{message: "compilation error"}}   (:elixir => {:ok, 5})
Vaisto.Runner.run("(deftype Point [x :int y :int])\n(defn main [] :int (. (Point 1 2) :x))", backend: :elixir)
# raises FunctionClauseError   (:core => {:ok, 1})
```

D15–D24:

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
                                                                       # => {:error, [%Vaisto.Error{message: "non-exhaustive pattern match"}]}
# D23: a record "type parameter" is misparsed
Vaisto.Parser.parse("(deftype Job p [id :int])")
# => {:deftype, :Job, {:product, [{:p, :any}, {{:bracket, ...}, :any}]}, %Loc{}}

# §16.3 phase probes (a, b, c, d, d2, f) use the same helpers with
# (deftype Executed [id :int]) (deftype Accepted [id :int]); results are in the §16.3 table.
```

D24 (row-polymorphic field access on a record):

```elixir
src = "(deftype Point [x :int y :int])\n(defn get-x [r] (. r :x))\n(defn main [] :int (get-x (Point 1 2)))"
Vaisto.TypeChecker.check(Vaisto.Parser.parse(src))  # => {:ok, ...}
Vaisto.Runner.run(src, backend: :core)              # raises BadMapError
Vaisto.Runner.run(src, backend: :elixir)            # raises FunctionClauseError
```

Division semantics (Z3 5.1.0 and OTP 29):

```text
z3:     (div -7 2) = -4    (mod -7 2) = 1          ; SMT-LIB, Euclidean
erlang: -7 div 2   = -3    -7 rem 2   = -1         ; truncating
        7 div -2   = -3    7 rem -2   = 1
z3 with the ite-based tdiv/trem encoding: -3, -1, -3, 1   ; matches Vaisto (§4.3), which Erlang also implements
```

The encoding. SMT-LIB's `div` and `mod` are Euclidean; `tdiv` and `trem` are Vaisto's truncating operators:

```smt2
(define-fun tdiv ((x Int) (y Int)) Int
  (ite (>= x 0) (ite (> y 0) (div x y) (- (div x (- y))))
                (ite (> y 0) (- (div (- x) y)) (div (- x) (- y)))))
(define-fun trem ((x Int) (y Int)) Int (- x (* y (tdiv x y))))
; checked on Z3 5.1.0: -7,2 -> -3,-1   7,-2 -> -3,1   -7,-2 -> 3,-1
```

OTP guidance for language implementors (§7.1), read from OTP 29's own documentation:

```erlang
{ok, {docs_v1, _, _, _, #{<<"en">> := MD}, _, _}} = code:get_doc(compile),
[_, After] = string:split(MD, <<"## Recommendations for Language Implementors">>).
%% "Core Erlang: ... Primops can be added, deleted, or changed in any major release
%%  without notice. Note that by generating Core Erlang directly, it is possible to
%%  construct code that the Core-to-BEAM backend has never encountered before, and
%%  there are no guarantees that the final BEAM code will be safe."
%% "Our recommendation is to use either the abstract format or Erlang source code."
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

---

## Appendix C. Reviews of drafts 2 and 3, and what changed

Draft 2 was reviewed three ways: a read by the author, an adversarial programming-languages review, and a fact-check of every claim against the code. Each finding and its resolution:

| # | Finding in draft 2 | Resolution in draft 3 |
|---|---|---|
| R1 | Core's grammar did not cover today's language: no rows, typed PIDs, maps, `Unit`, float/`:num` arithmetic, tuple construction or `try`, although the rules table used `try`. Existing tests exercise all of them. | Core types are one algebra (§4.1): unit, labelled products (rows) and sums, functions with effect rows, named types as branded rows, `Pid M`, `Map`, `Dyn`. Terms include tuples, records, `select`, injections and `handle-crash` (§4.2). `:num` is a lawful/lawless class. |
| R2 | Typed messages and `Dyn` contradicted each other: a BEAM receiver cannot tell who sent a message. | `Dyn` has an embedding–projection pair per decodable type; a typed `receive` *is* the projection, and undecodable messages are stray (§4.6). No assumption about senders is needed. |
| R3 | The roadmap put the first refinement demo behind the backend replacement. | A commuting-square bridge, checked per program, lets refinements run on today's emitters right after Phase 0 (§21.1). Phase 1 is split into 1a (the type algebra in Core) and 1b (the new backend). |
| R4 | The verdict avoided "native to the existing compiler". | §0 answers each of the three questions separately: native to the language, not to the compiler. |
| R5 | The authority example did not scale beyond a fixed set of scopes. | Authority is an abstract meet-semilattice; clients reason in the theory, models carry laws with a recorded status, and constants are decided by reflection through the evaluator (§16.2). String-named resources work without strings in the logic. |
| R6 | §0 oversold the harness relative to §6.4. | §0.4 states which defect classes the harness finds and which it does not. |
| R7 | `evaluator(P, I, T) = BEAM(P, I, T)` had the direction wrong. | `BEAM(P, I) = (R, T)`, then `evaluator(P, I, replay T) = R`. |
| R8 | For arithmetic, the "independent" oracle ran on the operators it checked. | Primitives are implemented from their definitions wherever divergence is plausible, and every primitive is checked by two parties that do not share an implementation (§4.3, K12). |
| R9 | A2 read as if testing established it. | A2 is stated as an assumption for which the harness gives evidence (§4.4). |
| R10 | Canonical S-expression hints were written `[i]`. | Hints are length-prefixed: `[1:i]` (§5.1). |
| R11 | Row-polymorphic field access was assumed to work. | It does not (D24, reproduced); row evidence fixes it (§4.1, C14). |
| R12 | Replay determinism was asserted. | It is derived: computations are the free algebra over `Σ`, handlers are algebras, running is the unique fold (§4.5). |
| R13 | D15 said "the emitters scope lexically". The Elixir emitter does not, so D15 is also a backend disagreement (`2` on `:elixir`, a crash on `:core`). | D15, P0-14 (now also fixes the Elixir emitter), §0 and §6.4 corrected; re-verified by execution. |
| R14 | C12's example change `(> d 0)` would not break `B`; C5's bad program fails two conjuncts, not one. | C12 uses `(> d 1)`; C5 names both conjuncts. |
| R15 | Wrong citations and types: `unify_call_poly_s` binds a tvar to `:any` at 3304 (not "without binding"); `(if c :read :write)` has type `{:atom, :read}`; recursive `len` is `Any -> Any`. | Corrected in §1.2, §1.3, §1.11; re-verified by execution. |
| R16 | §1.1 missed entry points (LSP diagnostics, inlay hints, `Vaisto.check/1`, `TypeChecker.infer/2`, `Backend.compile/4`) and misplaced the `.vsi` write. | §1.1 lists them; §1.4 notes `TypeChecker.infer/2`. |
| R17 | Appendix A used an undefined `check` and `graph`, showed the wrong D7 result, and never wrote out the promised division encoding; "ediv" conventionally names Euclidean division. | `check` and `graph` defined; D2 and D7 fixed; the `tdiv`/`trem` encoding written out and the operators renamed throughout. |
| R18 | Internal inconsistencies: C11 defined three ways; "both backends" in a suite that runs before the new backend; §11.3 said "error" and "warning" for the same case; Q20, Q21, Q24 were already decided; §2 said only observers hold axioms; operator phases disagreed with the roadmap. | Each reconciled (§7.2, §14, §11.3, §13.4, §20, §2, §17.1). |
| R19 | D9 understated: after an import, polymorphic `+`/`<` lose their instances and exported schemes would fail in the qualified-call clause. D10's "cycles never reported" held only for graphs built from files. | D9 and D10 rows, and P0-9's acceptance test, corrected. |
| R20 | **Blocker:** the library minted authority freely. Plain `Capability`/`Authority` records and a public `top` meant anyone could hold everything. `delegate` promised nothing, and `Transition` consumers got no validity fact. | `Authority` and `Capability` are **sealed**, `top`/`grant`/`mint` are issuer-only, `delegate` has a real postcondition, and transitions carry abstract measures and a type invariant. Invariants of opaque types are admitted as small new theory (§4.1, §4.8, §16). |
| R21 | `external` was a universal escape hatch: kill the observer, recreate its table, load code past the loader. | `external` is privileged and allowlisted in the pinned configuration; admission holds the table's `tid` and checks its owner (§4.5, §4.7). |
| R22 | The observer axiom could read mutable state, making contradictory facts reachable. | `tests-passed` is a pure function of the immutable witness; authenticity is a separate admission step; contradictions involving `Observed` facts are errors (§4.7). |
| R23 | M20 skipped non-executable predicates, and A4 ignored the exported internal entry points. | No entry point for non-executable refinements; A4 is a whole-node assumption (§4.4, M20). |
| R24 | Erasure was not purely syntactic while `decode` checked refined predicates. | `materialize` makes runtime checks explicit first; erasure then has no exceptions (§7.2). |
| R25 | The concurrency theorem was trivial or false: concurrent sends race and timeouts depend on absence. | Histories record arrival order and timeout counts, signals are producers, and the theorem is realizability. The "any interleaving" claim is withdrawn (§4.5, §5.6). |
| R26 | The calculus could not verify its own examples: no synthesis for selectors, constructors or conditionals. Hoisting under `and` was unsafe. Pattern negation had the wrong polarity. The scope of checked code was undefined. | Selector, constructor and guarded-join synthesis; `and`/`or` are `if`; A-normal form within evaluation contexts, with branch-scoped facts; negation of tests only, with fresh booleans for unsupported atoms; a definition of which code is checked (§4.2, §4.4, §10.3). |
| R27 | Refined functions bound by `let` escaped their obligations, and Core arrows had no parameter names. | The rule covers every non-head occurrence; Core has dependent arrows (§4.1, §4.4). |
| R28 | Elaboration faithfulness was silently trusted; autoloading and hot loading bypass admission by default; interfaces and configuration were forgeable files; "error by default" implied an override. | A trusted predicate translator with print-back tests; embedded mode; signed, write-protected interfaces and configuration; no override; assumptions A6 and A7 added (§4.4, §8.3, §8.4). |
| R29 | The effect model lacked links, exit signals, termination, foreign exceptions and mailbox-using externs; OTP behaviours invert control. | All added to `Σ` and to traces; OTP behaviours are foreign drivers entering through decoding entry points (§4.5). |
| R30 | Canonical format gaps: floats, canonical integers, pids, refs, funs. | Specified (§5.1). |
| R31 | `decode` into refined types nested a refinement in `Result`; improper lists; bare-name tags collide across modules. | `decode` returns the erased type and adds the fact on `Ok`; proper lists only; module-qualified brands (§4.1, §4.4, §4.6, Q32). |
| R32 | Predicates could contain partial operators; guard semantics undefined; crash reasons could not match; type variables unspecified; Core Lint's checklist incomplete; solver answers depended on load and history. | Total predicates by construction; guard-safe guards where a crash means false; normalized crash reasons; type variables as uninterpreted sorts; a full Lint checklist; resource limits and fresh contexts, syntactic provenance, nonlinear caveat (§4.2, §4.4, §9.2, §11). |
| R33 | Library examples were ill-formed: `spend` unverifiable, `depth` used a non-measure, laws mentioned ordinary functions, `rerank` claimed a permutation, K5 relied on a defect Phase 0 fixes, the contract digest included build provenance. | Each fixed (§12, §16, §17.1, §17.2, K5). |
| R34 | While fixing R20 it became clear that banning `decode` on *all* opaque types would stop opaque values crossing processes, since a typed `receive` is decoding. | Abstraction and authentication are separated: opaque types decode only through their own module; sealed types also verify an authenticator (§4.6). |
| R35 | Re-review of draft 3: a guard that crashes counts as false, but its negation assumed it was evaluated. A program verified and then divided by zero. | A guard means `def(g) ∧ g` everywhere (§4.2, §10.3); C19. |
| R36 | Functions verified only for partial correctness were lifted into the logic, so a non-returning function could export the axiom `false`; reflection could hang. | Only terminating, effect-free functions are liftable, their axioms are guarded by their preconditions, and reflection has a fuel limit. Contradictory facts at an obligation are errors unless marked `(unreachable)`, and vacuous guarantees are errors (§4.4, §11.3, §16.1); C20. |
| R37 | Opaque invariants were assumed of values decoded from forged messages. | Opaque decoders check the invariant; a type with a non-executable invariant must be sealed (`Transition` is) (§4.6, §16.3). |
| R38 | The Phase 2.1 bridge relied on A2 for backends the document shows break it, and a failed square meant "runs unchecked". | Gated on P0-4, P0-10 and P0-14, parity-tested constructs only, no externs; a failed square is a build error (§21.1). |
| R39 | Table-based authenticators made `meet` effectful and broke the laws and reflection; `==` on sealed values exposed the model. | Deterministic, macaroon-style MAC chains; laws and reflection on canonical content; no raw `==` on opaque or sealed types outside their module (§4.6, §16.2). |
| R40 | No phase delivered sealing before the capability demo needed it. | Static sealing in Phase 1a, runtime authentication in Phase 3 (§21.2). |
| R41 | Deep decoding cannot be written as receive guards; C15's protocol was not decodable. | Typed receive selects on tags and guards, then decodes after consumption; strays re-enter with the remaining timeout; C15 uses `(Pid Dyn)` (§4.6, §14.2). |
| R42 | `exit` was unprivileged, so any worker could kill an observer or issuer. | `exit` is privileged like `external` (§4.5, §9.2). |
| R43 | The `generate` signature used a type operator the algebra lacks; transitions used an inexpressible `valid-path`; measures were used at runtime; the IR could not express the logic; `Raised` outcomes were untyped. | Per-type signatures with a `Decodable` dictionary; one-argument structural `chain-ok`; runtime accessors; abstract, type-variable, field and lifted-function forms in the IR; typed `Outcome` in computations and traces (§4.5, §5.2, §16, §17.1). |
| R44 | Recording arrival order was unspecified; the contract digest ignored imported theories; row fields and selectors were not identified; law instantiation could loop. | A privileged tracer via `erlang:trace/3`; theory digests in the contract digest; a field/selector axiom at instantiation; one-round instantiation (§4.4, §4.5, §16.1, §17.2). |
| R45 | Stale text: the pipeline and theorems without `materialize`; "drop unsupported hypotheses"; solver-start and exhaustiveness wording; missing `badarg`; `==` on floats; "in 2s"; test counts; callee names in C6; the associativity status; the §8.5 handshake; Q8 and Q17. | Each corrected. |
