# The algebra of types in Liquid Vaisto

**Status: design.** This document replaces the RFC's decision that Hindley–Milner stays as an untrusted elaborator (`liquid-vaisto-rfc.md` §0.5, §9.1). It defines the type theory that Liquid Core is moving to, and the front end that elaborates into it. `liquid-core.md` remains the normative definition of Core version 0. Where this document goes further, it describes version 1 and later, and says so.

The aim is a type system that is algebra all the way down. There are no inference heuristics and no special cases. Every rule is read off a universal property, and every identification of two things is a stated equivalence.

## 0. Summary

Five decisions, and one deferred:

1. **Types form a free algebra, and every type is a set.** The type formers are the algebraic structure listed in §3, and nothing else. In the language of homotopy type theory, every type is a *set*: equality between two values is a proposition, carrying no information beyond its truth (§2).
2. **Typing rules are universal properties, read bidirectionally.** For each type former, its introduction forms are *checked* against a known type and its elimination forms *synthesize* one (§8). One rule table is both the elaborator and Core Lint.
3. **Inference is local unification.** A most general unifier is a coequalizer (§9). It is used only to choose the type arguments at an elimination site, inside one definition. Top-level definitions carry signatures, and Hindley–Milner is retired.
4. **The logic is the propositions.** A refinement is a Σ-type over a family of propositions, so erasing it is an embedding (§5.1). Decidable equality is discreteness (§4.3). Quotients are set-level higher inductive types (§5.3), and facts that come from outside are propositional truncations (§5.5).
5. **Sameness of types is equivalence.** Univalence is adopted as a principle, not an axiom (§4.2). Two types are identified definitionally only when the equivalence between them is the identity on BEAM representations. Every other equivalence is an explicit transport.
6. **Deferred: grades.** One semiring grades both how often a variable is used and what a computation costs (§10). This comes after refinements and effect rows.

From homotopy type theory, Vaisto takes the **set-level fragment**: propositions, sets, set quotients, propositional truncation, and univalence as a design principle. It takes no higher paths, no univalent universe, and no cubical computation (§11).

## 1. Why Hindley–Milner goes

Hindley–Milner's guarantee is principal types: every let-polymorphic program has a most general type, found without annotations. Each feature Vaisto needs beyond that breaks the guarantee:
- subsumption by refinement (§5.1);
- higher-rank polymorphism for handlers and for capabilities that must not escape;
- grades (§10);
- quotient eliminators that carry obligations (§5.3).

The guarantee is also what failed in practice. The differential harness ran over the 133 programs in the test suite, and Core Lint rejected 25 programs that Hindley–Milner had accepted (PR #11):
- in 12, Hindley–Milner generalized a type it never constrained. `(defn double [x] (* x 2))` became `∀a. a → Int` (D17);
- in 9, it did not tie a scrutinee to its patterns' type (D22);
- in 4, it gave up and produced `:any`.

When global inference cannot decide, it either overgeneralizes or degrades to `:any`, and neither is visible to the programmer.

What survives from Hindley–Milner is its algebraic core, unification, used locally (§9).

## 2. Types are sets

A type `A` is a **set** when, for all `a, b : A`, the identity type `a =_A b` is a proposition: any two proofs that `a` equals `b` are themselves equal (HoTT book, §3.1). In the book's numbering this makes `A` a 0-type.

**Every Vaisto type is a set.** Values are BEAM terms, and nothing at runtime distinguishes one proof of an equality from another. So uniqueness of identity proofs holds, equality proofs carry no information, and there are no higher paths.

Three levels of the hierarchy are used:

| h-level | Name | Characterization | In Vaisto |
|---|---|---|---|
| −2 | contractible | exactly one element | `Unit` |
| −1 | proposition | any two elements are equal | refinement predicates, facts, laws, invariants |
| 0 | set | equality is a proposition | every type of values |

**Propositions have no runtime content.** A proposition has at most one element up to equality, so forgetting which element was given loses nothing. That is the semantic reason refinements erase (RFC §7.2): erasure forgets only proofs of propositions. The kind `Prop` (§5.2) makes this a checked property of the syntax.

## 3. The algebra of types

```text
A, B ::= 0 | 1 | A + B | A × B | A → B          the bicartesian closed structure
       | μX. F X                                initial algebra of a polynomial functor (§3.3)
       | Π(x : A). B x                          dependent functions, refined fragment only
       | Σ(x : A). P x                          P a family of propositions: a refinement (§5.1)
       | ∀(a : κ). A                            κ ∈ {Type, Prop, Row, Eff}, later Grade
       | ⟨ℓ₁ : A₁, …, ℓₙ : Aₙ | ρ⟩               rows (§3.2)
       | A / R                                  set quotient (§5.3)
       | ‖A‖                                    propositional truncation (§5.5)
       | Dyn                                    the set of BEAM terms (§6)
       | T_{Σ,E} A                              computations over an effect theory (§7)
```

In the syntax of Core version 0 (`liquid-core.md` §3):

| Former | Core |
|---|---|
| `1` | `Unit` |
| `+` with `μ` | named sums, such as `(List τ)` and declared types |
| `×` | `(Tuple …)` and records |
| `→` | `(-> (…) ε τ)`; the effect row is the theory of §7 |
| `Π` | `(pi ((x τ)…) ε τ)` |
| `Σ` over `Prop` | `(refine x τ p)` |
| `∀` | `(forall ((a κ)…) τ)` |
| rows | `(Record (row …))` and the brand rows of named records |
| `0` | the result type of `crash` (§7.1) |
| `A / R`, `‖A‖` | new in version 1 |

### 3.1 Laws of the algebra

Up to isomorphism, the types form a commutative semiring with exponentials:

```text
A + 0 ≅ A        A × 1 ≅ A        A × (B + C) ≅ A × B + A × C
(A + B) → C ≅ (A → C) × (B → C)   A → (B × C) ≅ (A → B) × (A → C)   (A × B) → C ≅ A → (B → C)
```

These are **equivalences, not equalities** (§4.2). Their two sides are represented differently on BEAM: a pair is a tuple, and a function of a pair is not a curried function. So Vaisto never identifies them silently. Each is available as an explicit transport, and the laws are property tests of those transports against the evaluator (§8.2).

### 3.2 Rows

A **row** is a finite map from labels to types. Formally it is a set quotient: lists of `(label, type)` pairs with distinct labels, modulo permutation. Two rows are therefore equal exactly when they have the same fields. `liquid-core.md` §10.3 states this as an implementation rule; here it is a fact about the quotient.

Disjoint union `⊎` makes rows a **partial commutative monoid** with unit `∅`, defined when the labels are disjoint. `⟨ℓ : A | ρ⟩` is `{ℓ : A} ⊎ ρ`. Row polymorphism quantifies over this monoid. Instantiating a row variable is the monoid operation, which is the splicing of `liquid-core.md` §10.3.

A named record is its **branded row**: `Point ≃ ⟨#Point : 1, x : Int, y : Int⟩`. The equivalence is the identity on representations, so it is definitional (§4.2). Effect rows have the same structure, over effect labels (§7.2).

### 3.3 Inductive types are initial algebras

A declared sum, recursion included, is the initial algebra `μX. F X` of a **polynomial functor**. That is a sum, over the constructors, of products of fields:

```text
List A = μX. 1 + A × X        Tree A = μX. A + X × X
```

The constructors form the algebra map `F(μF) → μF`. Initiality says that for every other `F`-algebra `α : F B → B` there is exactly one homomorphism `fold α : μF → B`. Structural recursion is exactly such a fold, so:
- the functions that terminate by structure are the folds (RFC §12);
- only folds may become symbols in the logic (RFC §16.1, liftability).

### 3.4 Processes are coalgebras

A process `(process p s₀ :m₁ e₁ … :mₙ eₙ)` with state `S` defines a **coalgebra**, a Mealy machine:

```text
step : S → Π(m : M). S × R_m
```

Its behaviour lives in the final coalgebra of that functor, and two processes behave the same exactly when they are **bisimilar** (Rutten 2000). A typed PID `(Pid M)` is a handle on such a coalgebra, indexed by its protocol `M`. That is the reading at the level of types. Operationally, a process is a computation over the `Proc` effect (§7). Session types, a later step, refine the protocol `M` from a set of messages into an order of messages.

## 4. Equality

### 4.1 Three equalities

1. **Definitional equality**, `A ≡ B` and `a ≡ b`, is decided by Core Lint, syntactically. It is kept small and decidable:
   - renaming of bound variables;
   - rows up to permutation;
   - a named record and its branded row;
   - `pi` and `->` once the parameter names are dropped;
   - substitution of explicit type arguments.

   There is no type-level computation beyond that, and no η-rule for functions.
2. **Propositional equality**, `a =_A b`, is the identity type. It is a proposition, because `A` is a set. It is written `(eq a b)` in refinements and appears in laws.
3. **Equivalence**, `A ≃ B`, is a pair of maps `f : A → B` and `g : B → A` with `g ∘ f = id` and `f ∘ g = id`. It relates types.

### 4.2 Univalence, as a principle

In homotopy type theory, the univalence axiom makes `(A = B) ≃ (A ≃ B)`: equivalent types are equal. Vaisto adopts the principle that **every identification of types is an equivalence**, and makes it operational in two ways:

- **Definitional, when transport is the identity.** Each definitional equality of §4.1 is an equivalence whose two maps are the identity on BEAM representations:
  - permuting a row leaves the same Erlang map or tagged tuple;
  - a record's tag *is* its brand;
  - a `pi` and its arrow are the same function.

  Identifying such types definitionally is sound, because transporting a value along the equivalence does nothing at runtime.
- **Explicit, when transport does work.** `(Tuple A B) ≃ ⟨0 : A, 1 : B⟩`, but one is a tuple and the other a map. The equivalence is then a function the program calls, and the semiring laws of §3.1 are transports of the same kind. This is why RFC §4.1 allows "no implicit isomorphisms": an implicit isomorphism would hide a conversion.

So whatever is invisible at runtime is identified, and whatever costs a conversion is visible in the program. Adding a definitional equality requires showing that its transport is the identity on representations (Q38).

### 4.3 Decidable equality is discreteness

A type `A` has **decidable equality** when, for all `a, b : A`, either `a = b` or `¬(a = b)`. Hedberg's theorem: a type with decidable equality is a set (HoTT book, Theorem 7.2.5). Such a type is called **discrete**.

The primitive `eq` decides equality exactly on the discrete types:

```text
(eq a b) = true  ≃  a =_A b      for A discrete
```

On BEAM, `eq` is `=:=`, which compares representations. Representation is injective on discrete types (§6), so `=:=` decides the identity type. This is what `liquid-core.md` §10.11 calls *ground data*. The rule there now has a reason: ground data is discreteness, computed structurally.

- **Discrete:** `Int`, `Float`, `String`, `Atom`, `Unit`, `Ref` and `Pid`.
- **Preserved by** `0`, `+`, `×`, `μ`, rows with discrete fields, and quotients by a decidable relation.
- **Not discrete:** `→`. Equality of functions is extensional, and not decidable.
- **A type variable is discrete only when a decider for it is in scope.**

**The decider is the `Eq` class.** An `Eq a` dictionary is a function `eq_a : a × a → Bool` together with the law `eq_a x y = true ≃ x = y`. So polymorphic equality is not a primitive at all: it is a witness of discreteness, passed as an argument.
- A *derived* instance satisfies the law by construction, because it decides identity structurally.
- A hand-written instance is a decider only once its law is proved.

That is RFC §4.2's rule ("only a lawful instance's operations enter the logic") stated as a theorem about what such instances are.

`Float` is discrete as *representations*: `0.0` and `-0.0` are different values (`liquid-core.md` §7). IEEE equality is a separate relation, and it is not the identity type.

### 4.4 Deciders and reflection

For a proposition `P`, a **decider** is a boolean `d` with `(d = true) ≃ P`. RFC §10.1's

```scheme
(defn le? [a :Authority b :Authority] {r :bool | (iff r (le a b))} ...)
```

is exactly a decider for the proposition `(le a b)`. Branching on a decider is case analysis on `P + ¬P`: the `then` branch receives `P` as a fact, the `else` branch `¬P`. The brief's distinction between `Bool` and `Prop` is the distinction between a set with two elements and a proposition, and reflection is the bridge between them.

## 5. Propositions, refinements, quotients and truncation

### 5.1 A refinement is a Σ-type over propositions

```text
{x : A | p}   ≔   Σ(x : A). P x        where P : A → Prop
```

Each `P x` is a proposition, so the first projection `π₁ : Σ(x : A). P x → A` is an **embedding**: `(x, p) = (y, q) ≃ (x = y)` (HoTT book, Lemma 3.5.1). Four consequences:

- **A refined value is a value together with an erasable proof.** Erasure is `π₁`, and it loses nothing.
- **Subtyping by refinement is the only subtyping in Vaisto.** `{x : A | P} ≤ {x : A | Q}` is the embedding given by `P ⇒ Q`.
- **Refinements never change representations**, because erasure is the identity on BEAM terms.
- **The validity of `P ⇒ Q` is decidable** whenever the propositions are written in the logic of RFC §5.2: linear integer arithmetic, uninterpreted functions and datatypes, without quantifiers. Z3 decides it (RFC §11).

The dependent function type `Π(x : A). B x` is used only where `B` mentions `x` inside refinements: `(pi ((x τ)…) ε τ)`.

### 5.2 `Prop` is a kind

The kinds are `Type` (sets), `Prop` (propositions), `Row` and `Eff`, with `Grade` later. A refinement's predicate is checked at kind `Prop`:
- it is pure, total and effect-free;
- it uses only admitted symbols.

RFC §4.4 states these well-formedness conditions; here they are what kinding means. Laws, invariants and the hypotheses of a verification condition are all of kind `Prop`.

### 5.3 Quotients are set-level higher inductive types

Given a type `A` and an equivalence relation `R : A → A → Prop`, the **set quotient** `A / R` (HoTT book, §6.10) has:
- a constructor `[_] : A → A / R`;
- for each `R a b`, an equality `[a] = [b]`;
- truncation to a set.

A function out of `A / R` is a function `f : A → B` together with a proof that `R a b ⇒ f a = f b`. This is the quotient's eliminator, which Lean calls `Quot.lift`.

Five structures the RFC already uses are quotients, and become one mechanism:

| Quotient | Of | By | RFC |
|---|---|---|---|
| Rows | lists of fields | permutation | §4.1; §3.2 here |
| Provenance | lists of sources | permutation and duplication: the free join-semilattice | §4.7 |
| Free models of a library theory | terms | the theory's laws (§5.4) | §16.1 |
| Sealed values | content × authenticator | same content | §4.6 |
| Causal histories | sequences of events | swapping independent events: Mazurkiewicz traces | §4.5 |

The provenance quotient is the type of Kuratowski-finite sets, which is the free join-semilattice (Frumin et al. 2018). The RFC's choice of the free join-semilattice for provenance is that type.

**Representations.** A quotient value is represented on BEAM by one of its representatives. The eliminator's obligation is what keeps that choice invisible: no function may observe which representative it was given. The obligation is discharged by the solver, or carried as a law with a status (RFC §16.1: proved, proved in Lean, tested, assumed).
- **Crossing into `Dyn`.** For a quotient to be decoded from `Dyn` or embedded into it (§6), it needs a **normal form**: a map `n : A → A` with `R a b ⇔ n a = n b`. Its representation is then `n a`.
- **Without a normal form**, a quotient crosses process boundaries only through its own module, as an opaque or sealed type. RFC §4.6 already says this, and it now has a reason.

**Sealed values.** The program carries a representative: the content together with its authenticator, the MAC. The theory, and therefore the logic, reasons about the quotient, where two values with the same content are equal. Opacity guarantees that clients only ever apply operations that respect the relation. A function on `Authority` that inspected the MAC would not respect it. That design rule is now a type rule rather than a convention.

### 5.4 Library theories are free models

RFC §16.1 builds the library as algebraic theories: a signature, laws, and hidden models. In this language:
- A **theory** `T = (Σ, E)` is a set of operations and a set of equations.
- Its **free model** `F_T(X)` is a higher inductive type. Each operation is a constructor, each law is an equality constructor, and the whole is truncated to a set.
- A **model** is a `T`-algebra.
- A function out of the free model is the unique homomorphism a model determines, and it is well defined exactly when the model satisfies `E`. This is the quotient obligation of §5.3 again.

An opaque type exports the theory and hides the model. This is algebraic specification of abstract data types, read in type theory.

### 5.5 Truncation separates facts from witnesses

The **propositional truncation** `‖A‖` is `A` with all of its elements identified: a proposition that says `A` is inhabited without saying by what (HoTT book, §3.7).

An observation (RFC §4.7) has two parts:
- the *receipt*, which is data: an element of a sealed set;
- the *fact* that tests passed, which is the proposition `‖Σ(r : Receipt). passed r‖`.

The logic may use the fact that a passing receipt exists, but never which receipt it was. The witness stays in the program, and the fact goes to the logic.

A boolean `tests-passed = true` written by ordinary code is data. It has no route to the proposition except through a decider produced by the observer (§4.4). This is the RFC's rule that a claim is not a fact, now as a statement about types.

## 6. `Dyn`: every discrete type is a retract

`Dyn` is the set of BEAM terms. Each discrete type `A` comes with two maps:

```text
up_A   : A → Dyn                      an embedding: injective on representations
down_A : Dyn → A + DecodeError

retraction:  down_A ∘ up_A = inl
projection:  down_A d = inl v  ⇒  up_A v = d
```

Together these make `A` a **retract of `Dyn`** in the Kleisli category of the error monad, and `up_A` a split monomorphism there. Its image is exactly the set of terms that decode. These are the embedding–projection laws of RFC §4.6 (New and Ahmed 2018).

`Dyn` is not a top type with subtyping into it. It is an object that every discrete type embeds into, by explicit maps.

Discreteness and decodability coincide. A function type has no injective representation up to extensional equality, so it does not embed. One predicate therefore gates `eq`, `up` and `decode`.

## 7. Effects are theories, handlers are models

### 7.1 Operations and the empty type

An effect is an **algebraic theory** (Plotkin and Power 2003). An operation with argument type `P` and result type `R` has *arity* `R`: its continuation receives an `R`. Computations over a signature `Σ` are its free model, the free monad `T_Σ`. With equations `E`, they form the quotient `T_{Σ,E}`, a higher inductive type.

`crash : Reason → 0` has result type `0`, so its arity is empty: it has no continuation. Three consequences:
- `crash r` followed by anything is `crash r`. This is a *consequence of arity 0*, not an extra equation. RFC §4.5 calls it "the single equation"; it is in fact the definition of sequencing at an operation with no continuation.
- The free model over `crash` alone is `A + Reason`. That is why the outcome of a Core program is `(ok v)` or `(crash ρ)` (`liquid-core.md` §6.1).
- A crash checks against every type. Its result is in `0`, and `0` has exactly one map into every type, the *absurd* map. So `liquid-core.md` §10.5's rule that a crash "checks against any type, synthesizes none" is the eliminator of `0`.

### 7.2 Handlers, rows of theories, histories

- **A handler is a model.** Running a computation is the unique homomorphism out of the free model. When the theory has equations, the handler must satisfy them, or the homomorphism is not well defined: this is the quotient obligation of §5.3. Replay determinism (RFC §4.5) is the uniqueness of that homomorphism.
- **An effect row is a row of theories**, combined by their **sum**: the operations of all of them, with no laws relating them. Commutation between independent effects would be their *tensor* (Hyland, Plotkin and Power 2006). Vaisto does not assume it, because two `send`s do not commute.
- **A history of one process is a sequence of events. A history of a system is a trace:** a sequence of events modulo swapping independent ones, which is an element of the free partially commutative monoid (Mazurkiewicz). The RFC's realizability theorem (§4.5) is the statement that replay is well defined on that quotient: it gives the same result for every ordering of independent events.

## 8. Typing is the universal properties, read bidirectionally

### 8.1 The rule table

**Introduction forms are checked** against a type the context supplies. **Elimination forms synthesize** a type from the thing eliminated (Pfenning's recipe; Dunfield and Krishnaswami 2021). A type former's universal property gives its two rules and its two laws.

| Former | Introduce (⇐) | Eliminate (⇒) | β-law | η-law |
|---|---|---|---|---|
| `A × B`, records | `tuple`, `record` | `select` | `(select (record … (l e) …) l) = e` | `(record (l (select v l)) …) = v` |
| `A + B`, sums | `inj` | `match` | a `match` on `(inj C v)` takes the `C` clause | `(match v [(C x…) (inj C x…)] …) = v` |
| `A → B` | `fn` | `app` | `(app (fn ((x τ)) e) v) = e[v/x]` | `(fn ((x τ)) (app f x)) = f`, extensionally |
| `∀(a : κ). A` | a definition with a signature | `inst` | `(inst Λa.e T) = e[T/a]` | |
| `μF` | constructors | `match`, structural recursion | `fold α ∘ in = α ∘ F(fold α)` | `fold` is unique |
| `1` | `(unit)` | | | `v = (unit)` |
| `0` | | `absurd`; `crash` (§7.1) | | |
| `Σ` over `Prop` | prove `P` holds, by the solver | `π₁`, with `P` added as a fact | `π₁ (v, p) = v` | `(π₁ v, π₂ v) = v` |
| `A / R` | `[a]` | `lift`, with the respect obligation | `lift f [a] = f a` | |
| `‖A‖` | `\|a\|` | elimination into a proposition | | |
| `Dyn` | `up` | `decode` | `down (up v) = ok v` | `up v = d` when `down d = ok v` |
| `T_{Σ,E} A` | `return`, `perform` | `handle` | `handle (return v) = ret v`; `handle (perform op k) = op_h (handle ∘ k)` | the handler is unique |

**Modes.** Checking receives a type, and synthesis returns one. When a synthesized type meets an expected one, they must be definitionally equal (§4.1), or the first must be a refinement of the second (§5.1). There is no other subsumption.

**One table, two implementations.** The elaborator implements this table and produces Core. Core Lint implements the same table again, independently, and re-checks every elaboration. The harness then compares both with the evaluator. Core Lint already is this table restricted to version 0 (`liquid-core.md` §10), which is why the soundness tests of `liquid-core.md` §10.15 pass. The change is that the front end becomes the same algebra.

### 8.2 Laws as tests

Each β-law and each η-law is a property of the reference evaluator, and is tested on generated terms the way `liquid-core.md` §10.15 tests soundness. In the Lean model (RFC Phase 8) the same laws are theorems. A law the evaluator breaks is a bug in the evaluator or in the specification, and the specification decides which.

## 9. Inference is local unification

The only inference that remains is choosing the arguments at an elimination site: the type, row and effect arguments of an `inst`, and the dictionaries for class constraints. It runs inside one definition.

- **Unification is algebraic.** Unifying two type terms over the free algebra of §3 computes their most general unifier. In the category of substitutions, a most general unifier is a **coequalizer** of the two terms (Goguen 1989). Rows unify by Rémy's method, with effect rows the same. The result is unique up to renaming, and the procedure terminates.
- **Signatures on definitions.** Every top-level definition, and every recursive binding, carries a signature. Its signature is its contract.
- **Local `let`s are not generalized** unless annotated with a `∀` type ("Let should not be generalised", Vytiniotis et al. 2010; GHC's `MonoLocalBinds`).
- **Lambdas need no annotations** where they are checked against a known arrow. Checking pushes the expected type inward. In `(map (fn [x] (* x 2)) xs)`, the signature of `map` and the type of `xs` determine the lambda's parameter type before the lambda is checked. That removes the fallback lambda (RFC §15.4) by construction.
- **Class constraints are resolved at the elimination site**, like type arguments. The dictionary is the evidence, chosen by deterministic instance lookup with no overlapping instances. Row evidence (RFC §4.1) works the same way.
- **There is no `:any`.** Where local inference cannot determine a type, the result is an error at that place. Its message may *suggest* a type computed by the old engine, but the old engine never decides.
- **Undetermined is not unconstrained.** Two cases arise at the end of a definition, and they differ in whether a choice matters:
  - an Int/Float operator (`+`, `<`, unary `-`) whose operands' type is still open, as in `(let [f (fn [a b] (+ a b))] (f 3 4))`, is chosen once the definition's other constraints have solved it. If nothing solves it, that is an error, because Int and Float mean different programs;
  - a type that nothing constrains, such as the error type of `(Ok 42)` when only the `Ok` branch is used, is chosen as `Unit`. By parametricity no choice can change what the program does, so this is not a type that inference failed to find. The surface has no syntax for annotating a `let`, so requiring an annotation would leave the program unwritable.

**Surface syntax (implemented, slice 1).**
- A signature annotates every parameter and the result: `(defn f [x :int ys (List :a)] :a ...)`.
- A lowercase keyword that is not a primitive type is a type variable, quantified over the signature, as `defclass` already reads it (owner decision, 2026-10-03).
- A function type is `(Fn :a :b :c)`: two parameters of types `a` and `b`, and a result of type `c`.
- Multi-clause definitions have no syntax for a signature yet.

## 10. Later: one semiring for usage and cost

A **grade semiring** `(R, +, ·, 0, 1)` annotates variables with how they are used, and computations with what they consume (Atkey 2018; McBride 2016; Orchard et al. 2019, the Granule language).

**Usage**, the semiring `{0, 1, ω}`:
- `0` means erased. Propositions are graded `0` automatically, because they have no runtime content (§2); refinement erasure becomes a special case.
- `1` means used exactly once. Capabilities and tokens that must not be duplicated or dropped are graded `1`.
- `ω` means unrestricted: ordinary data.

**Cost.** The budget theory of RFC §16.6, the ordered commutative monoid `(ℕᵏ, +, 0, ≤)`, becomes a grade on effects: a graded monad. Sequencing adds the costs, and branching takes their pointwise maximum. A pipeline's type then bounds its cost, and a contract's `:budget` becomes a check on grades. The task-contracts promise of bounded cost becomes a static check where today it is a runtime one.

The two compose as a product semiring. Grades come after refinements (Phase 2) and need the effect rows of Phase 4.

## 11. What Vaisto does not take from homotopy type theory

- **No higher paths.** Every type is a set (§2). BEAM values carry no higher structure, and higher paths would have no runtime meaning.
- **No univalent universe in the implementation.** Univalence is a principle about which types may be identified (§4.2). It needs no axiom, because each definitional identification is checked to be the identity on representations.
- **No cubical computation.** The fragment Vaisto uses has a simple, decidable checking story. It also has a direct model in Lean 4: `Prop` with definitional proof irrelevance, `Subtype` for §5.1, `Quot` for §5.3, and `Squash` or `Nonempty` for §5.5. That model is RFC Phase 8.

## 12. Consequences for the roadmap

| Phase | Before | With this document |
|---|---|---|
| 1a | engines merged, with Hindley–Milner elaborating directly to Core | **a bidirectional elaborator** implementing §8 with local unification (§9) and required signatures (§13). The current `TypeChecker`, `TypeSystem.Infer` and the Phase 0 adapter retire when it reaches parity on the corpus |
| 1a (spec) | | **Core version 1**: kind `Prop`, `Σ` over propositions for `refine`, `Π`, `absurd` for `0`; the definitional equalities of §4.1, closed |
| 2.x | refinements in Core | unchanged in plan; refinements are §5.1 |
| 3 | library theories, sealed types | free models (§5.4) and quotients (§5.3); sealed values as quotients by authenticator; `Eq` as a discreteness witness (§4.3) |
| 4 | effect rows, handlers, replay | theories with equations, and handlers as models that satisfy them (§7) |
| 6 | histories and realizability | histories as trace quotients (§7.2) |
| 8 | Lean model | the set-level fragment, encoded directly (§11) |
| later | | grades (§10), after Phase 4 |

## 13. What it costs

**Signatures on top-level definitions.** Today, 54 of the 166 definitions in the test suite carry a full signature, and 2 of the 552 in `std/`, `examples/` and `src/` do. Every multi-clause definition lacks one. So most existing code needs a signature added.

The migration can be mechanized. The old engine infers a type for each definition, and a migration tool can insert it as a *suggested* signature, which the new checker then verifies. Definitions where Hindley–Milner was wrong are exactly the ones whose suggestions fail to check: D17 and D22 surface as errors instead of being written into the code. `src/Vaisto/`, the proof-of-concept compiler that no longer type-checks (ADR-002), is out of scope for the migration.

**Pitch.** "Hindley–Milner inference: safety without annotation tax" becomes "signatures at definitions, inference inside". Rust is the precedent: it infers types inside functions and requires them on functions.

**Gains:**
- error messages are local;
- every rule is one row of §8;
- there is one algebra across the elaborator, Lint and the specification;
- refinements, higher rank, quotients and grades can be added without breaking anything, because no principal-types guarantee has to be kept.

## 14. Open questions

| # | Question | Recommendation |
|---|---|---|
| Q35 | Required signatures: on every top-level definition, or only on exported, refined or effectful ones? | every top-level definition: the signature is the contract. Migrate with suggested signatures (§13). **Adopted** in the elaborator (2026-10-03) |
| Q36 | Local `let` generalization | only with an explicit `∀` annotation. **Adopted**: no generalization, and no `∀` annotation syntax yet |
| Q37 | User-defined quotients in the surface language, such as multisets and finite sets | library-only in Phase 3; surface syntax later |
| Q38 | Which equivalences are definitional | the closed list of §4.1. Adding one requires showing its transport is the identity on representations |
| Q39 | Grade semiring: usage only, or usage × cost? | usage first; cost after Phase 4 |
| Q40 | Discreteness of `Float`: identity of representations, or IEEE equality? | representations, which is what `=:=` is; IEEE equality is a separate relation |

## References

- The Univalent Foundations Program. *Homotopy Type Theory: Univalent Foundations of Mathematics.* 2013. §3.1 sets, §3.5 subtypes (Lemma 3.5.1), §3.7 propositional truncation, §4 equivalences, §6.10 set quotients, §7.1 n-types, §7.2 Hedberg's theorem (Theorem 7.2.5).
- M. Hedberg. A coherence theorem for Martin-Löf's type theory. *Journal of Functional Programming*, 1998.
- J. Dunfield and N. Krishnaswami. Bidirectional typing. *ACM Computing Surveys*, 2021.
- J. Goguen. What is unification? A categorical view of substitution, equation and solution. 1989.
- D. Rémy. Type inference for records in a natural extension of ML. 1993.
- D. Vytiniotis, S. Peyton Jones and T. Schrijvers. Let should not be generalised. *TLDI*, 2010.
- G. Plotkin and J. Power. Algebraic operations and generic effects. *Applied Categorical Structures*, 2003.
- G. Plotkin and M. Pretnar. Handlers of algebraic effects. *ESOP*, 2009.
- M. Hyland, G. Plotkin and J. Power. Combining effects: sum and tensor. *Theoretical Computer Science*, 2006.
- M. New and A. Ahmed. Graduality from embedding-projection pairs. *ICFP*, 2018.
- D. Frumin, H. Geuvers, L. Gondelman and N. van der Weide. Finite sets in homotopy type theory. *CPP*, 2018.
- A. Mazurkiewicz. Trace theory. *Advances in Petri Nets*, 1987.
- J. Rutten. Universal coalgebra: a theory of systems. *Theoretical Computer Science*, 2000.
- R. Atkey. Syntax and semantics of quantitative type theory. *LICS*, 2018.
- C. McBride. I got plenty o' nuttin'. 2016.
- D. Orchard, V.-B. Liepelt and H. Eades. Quantitative program reasoning with graded modal types. *ICFP*, 2019.
- N. Vazou, E. Seidel, R. Jhala, D. Vytiniotis and S. Peyton Jones. Refinement types for Haskell. *ICFP*, 2014.
