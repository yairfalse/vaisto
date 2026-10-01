# Liquid Core, version 0

This is the normative definition of Liquid Core, the language that Vaisto programs mean. The RFC (`liquid-vaisto-rfc.md`) explains why Core exists and how it is staged; this document says exactly what Core is. Where the two differ, this document wins, and §11 lists every difference.

Three consumers implement it:

- the **canonical codec** (`Vaisto.Liquid.Canonical`) implements §2;
- the **reference evaluator** (`Vaisto.Liquid.Eval`) implements §6 to §9, and is the executable form of this document;
- **Core Lint** (`Vaisto.Liquid.Lint`) implements the static semantics, §10.

Version 0 covers what Phase 0 of the roadmap needs: the full type grammar, the pure fragment with crashes, and effects as requests to a handler. Refinements and decoding out of `Dyn` are written down here as syntax only, and say so where they appear.

## 1. Notation

Core is written as trees (§2). Grammars use the readable form: `(if e e e)` is a node of four children whose first is the symbol `if`. `x`, `f`, `l`, `C`, `T`, `a` range over symbols; `n` over integers; `r` over floats; `s` over byte strings; `…` means zero or more of the preceding item, `…₁` one or more.

## 2. Trees

### 2.1 The data

A **tree** is a leaf or a node.

- A **leaf** is a **symbol** (a name), an **integer** (unbounded), a **float** (IEEE-754 binary64, finite) or a **string** (a sequence of bytes).
- A **node** is a finite sequence of trees.

In Elixir a symbol is an atom, an integer an integer, a float a float, a string a binary, and a node a proper list. `true`, `false` and `nil` are symbols. Nothing else is a tree: tuples, maps, pids, references and functions are not.

### 2.2 Canonical bytes

`canon(t)` is the canonical byte encoding, a form of Rivest's canonical S-expressions:

```text
canon(symbol)  = len(b) ":" b                 b = the symbol's UTF-8 name
canon(integer) = "[1:i]" len(d) ":" d          d = decimal digits, "-" for negatives
canon(string)  = "[1:s]" len(s) ":" s
canon(float)   = "[1:f]" "16:" h               h = the 64 bits as 16 lowercase hex digits
canon(node)    = "(" canon(child)… ")"
len(x)         = the byte length of x, in decimal
```

Decimals (lengths and integers) have no leading zeros, and the integer zero is `0`, never `-0`. The float encoding gives `0.0` and `-0.0` different bytes, and every float exactly one encoding.

**Decoding** accepts exactly the outputs of `canon`: a non-canonical length or integer, an unknown hint, a float that is not finite, unbalanced parentheses or trailing bytes are errors. Symbols decode to atoms. Because atoms are never garbage collected, a decoder given untrusted input decodes only to atoms that already exist, and reports an unknown symbol as an error. Trusted inputs may ask for new atoms to be created.

**Properties.** `decode(canon(t)) = t` for every tree; `canon` is injective; and `canon` depends on nothing but the tree, so its output is the same under every OTP release.

### 2.3 Metadata and digests

A node whose first child is the symbol `meta` is **metadata**: source spans, documentation. `strip(t)` removes every metadata node that is a child of another node, recursively.

```text
digest(t) = SHA-256(canon(strip(t))), written as 64 lowercase hex digits
```

So two trees that differ only in metadata have the same digest, and reformatting a source file never changes the identity of what it defines.

### 2.4 The readable form

The readable form is for people and agents. `parse(render(t)) = t` for every tree.

```text
tree    ::= leaf | "(" tree… ")"
integer ::= "-"? digit…₁                       no leading zeros; no "-0"
float   ::= an Erlang float in short form      e.g. 1.5, -0.0, 1.0e300; always has "." or "e"
string  ::= '"' (char | escape)… '"'           escape ::= \" | \\ | \xHH;  (one byte)
symbol  ::= plain | "|" (char | escape)… "|"   escape ::= \| | \\ | \xHH;
```

A **plain** symbol is a non-empty run of letters, digits and the characters `! $ % & * + - . / : < = > ? @ ^ _ ~ #` that does not begin like a number (a digit, or `-` or `+` followed by a digit) and is not a single `.`. Every other symbol is written between bars. `render` writes a string's bytes as they are when they are valid UTF-8 and printable, and with `\xHH;` otherwise. Whitespace separates leaves, and `;` starts a comment that runs to the end of the line.

## 3. Types

### 3.1 Kinds

There are three kinds: `Type`, `Row` (finite maps from labels to types) and `Eff` (sets of effect labels).

### 3.2 Grammar

```text
τ ::= Int | Float | String | Atom | Ref | Dyn | Unit      base types
    | (tvar a)                                           a type variable
    | (Tuple τ…₁)                                        positional product
    | (Record ρ)                                         anonymous record: a row
    | (-> (τ…) ε τ)                                      function with effect row ε
    | (pi ((x τ)…) ε τ)                                  dependent function (§3.4)
    | (T τ…)                                             named type T, applied to its parameters
    | (Pid τ)                                            a process accepting protocol τ
    | (Map τ τ)                                          homogeneous finite map
    | (forall ((a k)…₁) τ)                               k ∈ Type | Row | Eff
    | (refine x τ p)                                     refinement: the values of τ satisfying p

ρ ::= (row (l τ)… tail)                                  tail ::= closed | (rvar a)
ε ::= (eff L… tail)                                      tail ::= closed | (evar a)
```

Labels in a row, and effect labels in an effect row, are distinct. Two rows are equal when they have the same labels with equal types and the same tail; the order in which labels are written does not matter, and the canonical order is sorted by the canonical bytes of the label.

A named type is always a node, even with no parameters: `(Bool)`, `(Point)`, `(List Int)`. The symbols `Int`, `Float`, `String`, `Atom`, `Ref`, `Dyn`, `Unit`, `tvar`, `Tuple`, `Record`, `->`, `pi`, `Pid`, `Map`, `forall`, `refine`, `row` and `eff` are reserved and cannot name a type.

`Float`, `String`, `Dyn`, functions, pids and maps are types of values but not sorts of the refinement logic (RFC §4.4). `(refine x τ p)` is syntax only in version 0.

### 3.3 Declarations

```text
decl ::= (type T (a…) (sum (C τ…)…₁))        a named sum: constructor C carries fields τ…
       | (type T (a…) (record (l τ)…))       a named record
```

A constructor carries a **sequence** of fields, so `(Pair 1 2)` is `(inj P Pair 1 2)`, not an injection of a tuple (§11, item 1).

Three named types are built in, and a module may not declare them:

```text
(type Bool () (sum (true) (false)))
(type List (a) (sum (Nil) (Cons (tvar a) (List (tvar a)))))
(type Reason () (sum (badarith) (badarg) (no_match) (bad_decode) (user Dyn) (raised Atom Dyn)))
```

### 3.4 Dependent functions

In `(pi ((x₁ τ₁) … (xₙ τₙ)) ε τ)`, a later `τᵢ` and the result `τ` may mention earlier `xⱼ` inside refinements. Without refinements the names are unused, and `(-> (τ₁ … τₙ) ε τ)` means the same type. The two forms have different heads because their parameter lists cannot be told apart by shape: `((x Int))` is one named parameter, and `((List Int))` is one unnamed parameter of type `(List Int)`.

## 4. Terms

### 4.1 Grammar

```text
e ::= x                                   variable
    | n | r | s                           Int, Float and String literals
    | (atom a)                            Atom literal
    | (unit)                              the value of Unit
    | (fn ((x τ)…) e)                     function
    | (app e e…)                          application
    | (let ((x τ e)) e)                   one binding
    | (letrec ((f τ (fn …))…₁) e)         mutually recursive functions
    | (inst e τ…₁)                        explicit instantiation of a polymorphic term
    | (tuple e…₁)
    | (record τ (l e)…)                   a record of type τ: every field, in any order
    | (select e l)                        field l of a record; l an integer selects from a tuple
    | (inj τ C e…)                        constructor C of the named sum type τ
    | (match e clause…₁)
    | (if e e e)
    | (prim op e…)                        a primitive (§7)
    | (perform op e…)                     an effect request (§8)
    | (handle-crash e ((x (Reason)) e))   run e; on a crash, bind its reason to x
    | (up τ e) | (decode τ e)             into and out of Dyn (syntax only in version 0)

clause ::= (p e) | (p (when g) e)
g      ::= e, restricted to guard-safe forms (§6.6)
```

In `(record τ …)`, `τ` is a named record type `(T σ…)` or an anonymous record type `(Record (row (l σ)… closed))`. In `(inj τ C …)`, `τ` is a named sum type `(T σ…)`. Constructors and records name their full type, so that Core Lint never has to work out a type argument: `(inj (List Int) Nil)` is the empty list of integers.

`do` is `let` with an unused binder; `and` and `or` are `if` (§6.5); a list literal is nested `(inj (List τ) Cons …)` ending in `(inj (List τ) Nil)`.

### 4.2 Patterns

```text
p ::= _ | x | n | r | s | (atom a) | (unit)
    | (tuple p…₁)
    | (record τ (l p)…)                   τ a named record type; any subset of its fields
    | (inj τ C p…)                        one pattern per field of C
    | (as x p)
```

A pattern binds each variable at most once. `_` matches anything and binds nothing.

### 4.3 Modules

```text
module ::= (module M (core-version 0) item…)
item   ::= decl | (def f τ (fn …))
```

The `def`s of a module form one `letrec`: each may refer to any other. Every top-level definition is a function; a constant is a function of no arguments.

## 5. Values and their representation on BEAM

A value is the result of evaluating a term. Each value has exactly one representation as a BEAM term, and the evaluator computes with the representations directly, so the result of a Core program and the result of the BEAM program it was lowered to can be compared as terms.

| Type | Value | Representation |
|---|---|---|
| `Int` | an integer | the integer |
| `Float` | a finite binary64 | the float |
| `String` | a byte string | the binary |
| `Atom` | `(atom a)` | the atom `a` |
| `Unit` | `(unit)` | the atom `nil` |
| `(Tuple τ₁ … τₙ)` | `(tuple v₁ … vₙ)` | `{v₁, …, vₙ}` |
| `(Record ρ)` | `(record (Record ρ) (l v)…)` | the map `#{l => v, …}` |
| named record `(T σ…)` | `(record (T σ…) (l v)…)` | `{T, v₁, …, vₙ}`, fields in declaration order |
| named sum `(T σ…)`, constructor `C` | `(inj (T σ…) C v…)` | `{C, v…}`; a constructor without fields is `{C}` |
| `(Bool)` | `true`, `false` | the atoms `true`, `false` |
| `(List τ)` | `Nil`, `(Cons h t)` | `[]`, `[h | t]` |
| `(Reason)` | `badarith` … `(user v)`, `(raised c v)` | the atom for a constructor without fields; `{user, v}`, `{raised, c, v}` |
| `(Map κ τ)` | a finite map | the map |
| `(-> …)` | a closure | a function; never compared (§9) |

The tag of a named record or sum is today the bare name `T` or `C`. The RFC's module-qualified brands (RFC §4.1, Q32) change this in a later version.

### 5.1 Value typing

`v ⊨ τ` says that the BEAM term `v` is the representation of a value of the closed type `τ`. It is the table above read as a relation:

| `τ` | `v ⊨ τ` when `v` is |
|---|---|
| `Int`, `Float`, `String`, `Atom` | an integer, a float, a binary, an atom |
| `Unit` | `nil` |
| `Ref`, `(Pid τ)` | a reference, a pid |
| `Dyn` | any term |
| `(Tuple τ₁ … τₙ)` | a tuple `{v₁, …, vₙ}` with each `vᵢ ⊨ τᵢ` |
| `(Record (row (l τ)… tail))` | a map with a key `l` for each label, its value `⊨ τ`; with no other keys when the tail is `closed` |
| a named record `(T σ…)` | `{T, v₁, …, vₙ}` with each field's value `⊨` its declared type, parameters replaced by `σ…` |
| a named sum `(T σ…)` | `{C, v…}` for a constructor `C` of `T`, each field `⊨` its type; `(Bool)`, `(List τ)` and `(Reason)` as the table above |
| `(Map κ τ)` | a map whose keys `⊨ κ` and values `⊨ τ` |
| `(-> (τ…) ε τ)` | a closure or function of as many parameters |
| `(forall … τ)`, `(tvar a)` | `v ⊨ τ` with every type variable read as "any term" |

The soundness property of §10.15 is stated with it, and the differential harness checks with it that what a backend returns is a value of the program's type, which catches a backend that gets a representation wrong even when the evaluator was not run.

## 6. Evaluation

### 6.1 Results

Evaluating a closed term under a handler (§8) ends in one of two **outcomes**:

- `(ok v)`: the term produced the value `v`;
- `(crash ρ)`: the term performed `crash` with the reason `ρ`, a value of `(Reason)`, and no `handle-crash` caught it.

A term that does not end is not given an outcome. Evaluation is defined on terms that Core Lint accepts. On any other term the evaluator stops with an internal error, which is a bug in whatever produced the term, never a crash of the program.

### 6.2 Order

Evaluation is call-by-value and strictly left to right: the function before its arguments, arguments in order, tuple and constructor fields in order, record fields **in the order written**, and a primitive's operands in order. `if` and `match` evaluate only the chosen branch. When a sub-term crashes, nothing to its right is evaluated.

### 6.3 Environments and functions

An environment maps variables to values. `(fn ((x τ)…) e)` evaluates to a **closure** of its parameters, body and the current environment. `(app f a₁ … aₙ)` evaluates `f`, then `a₁ … aₙ`, then the closure's body in its own environment extended with its parameters bound to the arguments. `(let ((x τ e₁)) e₂)` evaluates `e₁`, then `e₂` with `x` bound. In `(letrec ((f τ (fn …))…) e)`, every `fᵢ` is bound, inside every function of the group and inside `e`, to the closure of its own `fn`. Types are erased: `(inst e τ…)` and `(up τ e)` evaluate to the value of `e`.

### 6.4 Data

`(tuple e…)`, `(record …)` and `(inj …)` build the value of their fields, represented as §5 says. `(select e l)` evaluates `e` and returns:

- for a named record, the field labelled `l`;
- for an anonymous record, the value under the key `l`;
- for a tuple and an integer `l`, the element at position `l`, counting from 0.

`select` needs no type information at run time: the value says which case applies. This is how a row-polymorphic function reads a field of whatever record it is given (RFC §4.1, D24).

### 6.5 Control

`(if c e₁ e₂)` evaluates `c`, then `e₁` if it is `true` and `e₂` if it is `false`. Because only one branch runs, `(and a b)` is `(if a b false)` and `(or a b)` is `(if a true b)`, and short-circuiting follows from the grammar.

`(match e clause…)` evaluates `e` to `v`, then tries the clauses in order. A clause `(p e')` is chosen when `p` matches `v`; `(p (when g) e')` when `p` matches and `g`, evaluated with the pattern's bindings, gives `true`. The body of the first chosen clause is evaluated with the pattern's bindings. When no clause is chosen, the `match` crashes with `no_match`.

Matching is structural. A variable matches anything and binds it; a literal matches a value that is **exactly** equal (`0.0` does not match `-0.0`); `(tuple p…)` a tuple of the same size whose elements match; `(record τ (l p)…)` a record of `τ` whose named fields match; `(inj τ C p…)` a value built by `C` whose fields match; `(as x p)` what `p` matches, binding `x` to all of it.

### 6.6 Guards

A guard may use only variables, literals, `select`, `tuple`, `inj`, `if` and the primitives of §7 other than `concat`: no application, `perform`, `match` or `handle-crash`. These are the operations a BEAM guard can perform. **A guard that crashes counts as false**: the clause is not chosen, and the next one is tried.

### 6.7 Crashes

`(perform crash e)` evaluates `e` to a reason and crashes with it. A primitive whose condition in §7 holds crashes with the reason given there. `(handle-crash e ((x (Reason)) e'))` evaluates `e`; if that ends in `(ok v)` the result is `(ok v)`, and if it ends in `(crash ρ)` the result is that of `e'` with `x` bound to `ρ`. A crash inside `e'` is not caught by the same `handle-crash`.

## 7. Primitives

Every primitive is defined here. The evaluator implements each from this table. The lowering must reproduce it on BEAM, compensating wherever Erlang behaves differently.

| `op` | Operands | Crashes when | Result |
|---|---|---|---|
| `add` `sub` `mul` | `Int Int` | never | `x + y`, `x − y`, `x · y`, exact |
| `neg` | `Int` | never | `−x` |
| `div` | `Int Int` | `y = 0`: `badarith` | `tdiv(x, y)`: the quotient rounded **toward zero** |
| `rem` | `Int Int` | `y = 0`: `badarith` | `trem(x, y) = x − y · tdiv(x, y)`: the sign of `x` |
| `lt` `le` `gt` `ge` | `Int Int` | never | `x < y`, `x ≤ y`, `x > y`, `x ≥ y` |
| `fadd` `fsub` `fmul` | `Float Float` | the result is not finite: `badarith` | IEEE-754 binary64 |
| `fneg` | `Float` | never | `−x` |
| `fdiv` | `Float Float` | `y = 0.0` or `y = −0.0`, or the result is not finite: `badarith` | IEEE-754 binary64 |
| `flt` `fle` `fgt` `fge` | `Float Float` | never | IEEE-754 comparison (`−0.0 < 0.0` is false) |
| `int-to-float` | `Int` | the integer is out of binary64 range: `badarg` | the nearest binary64 |
| `eq` `ne` | two values of one type, not a function type | never | exact equality of representations (Erlang `=:=`) and its negation |
| `not` | `(Bool)` | never | `¬x` |
| `concat` | `String String` | never | concatenation |
| `length` | `(List τ)` | never | the number of `Cons` cells |
| `empty?` | `(List τ)` | never | `true` exactly for `Nil` |
| `head` | `(List τ)` | the list is `Nil`: `badarg` | the first field of the `Cons` |
| `tail` | `(List τ)` | the list is `Nil`: `badarg` | the second field of the `Cons` |

Exact equality distinguishes `0.0` from `-0.0`, and an `Int` from a `Float` of the same value. Comparisons are defined on `Int` and on `Float` only.

**Independence of the oracle.** Where an implementation could plausibly differ from the definition, the evaluator implements the definition, not the Erlang operator it will be compared with: `tdiv` and `trem` from their equations, `and`/`or` as `if`, and the list primitives by recursion over `Cons` cells. Where a difference is implausible (bignum `+`, `−`, `·`, comparison, IEEE arithmetic), it may use the host operator; other checks (the SMT encoding, law tests) cover those (RFC §4.3).

## 8. Effects and handlers

`(perform op e…)` evaluates its operands, then asks the **handler** to answer the request `(op v…)`. The operations, their argument and result types, and which of them are privileged are RFC §4.5's signature `Σ`. `crash` is the one operation a handler never sees (§6.7).

A handler answers each request with an **outcome of the request**: `(ok v)`, after which evaluation continues with `v` as the value of the `perform`; or `(raised c v)`, after which the `perform` crashes with the reason `(raised c v)`. A run is therefore a fold of the program's computation tree over the handler, and two runs whose handlers give the same answers to the same requests in the same order have the same outcome (RFC §4.5).

Version 0 defines two handlers:

- the **pure handler** answers no request: a `perform` other than `crash` is an internal error;
- the **scripted handler** holds a list of expected requests with their outcomes. It answers the `n`-th request with the `n`-th outcome when the request equals the `n`-th expected one, and otherwise stops with "the script does not fit". A script that is not used up is also reported.

## 9. Comparing results

The differential harness (RFC §6.3) compares a Core program's outcome with that of the BEAM program it was lowered to.

- Two `(ok v)` outcomes agree when the representations are equal as terms (`=:=`), after every function inside them has been replaced by the same placeholder: functions are never compared.
- Two `(crash ρ)` outcomes agree when the reasons are equal. On BEAM, a reason is read from the raised exception as follows:

| BEAM exception | Reason |
|---|---|
| `error:badarith` | `badarith` |
| `error:badarg` | `badarg` |
| `error:{case_clause, _}`, `error:if_clause`, `error:function_clause`, `error:{badmatch, _}`, `error:{try_clause, _}` | `no_match` |
| an exception the lowering raised for `(perform crash (inj (Reason) user v))` | `(user v)` |
| any other `C:R` | `(raised C R)` |

An `(ok …)` and a `(crash …)` never agree.

## 10. Static semantics

Core Lint (`Vaisto.Liquid.Lint`) implements this section. It checks a module and **infers nothing**: every binder carries its type and every instantiation is explicit, so each rule below is a check, never a search. The evaluator assumes a module that passes (§6.1).

### 10.1 Judgments

- `Δ` holds the type variables in scope, each with its kind; `D` the declarations, built-in and the module's; `Γ` maps variables to types.
- `Δ ⊢ τ : k`: `τ` is a well-formed type of kind `k` (§10.2).
- `Δ;Γ ⊢ e ⇒ τ`: `e` **synthesizes** `τ` (§10.4).
- `Δ;Γ ⊢ e ⇐ τ`: `e` **checks** against `τ` (§10.5).
- `Δ ⊢ p : τ ⊣ Γ'`: pattern `p` matches values of `τ` and binds `Γ'` (§10.9).

Lint is bidirectional only so that a term that never returns, `(perform crash e)`, can take whatever type its context expects. That is checking against a given type, not inference.

### 10.2 Well-formed types

| Type | Well formed when |
|---|---|
| a base type | always, of kind `Type` |
| `(tvar a)` | `a : Type` is in `Δ` |
| `(Tuple τ…₁)` | each `τ` is a `Type` |
| `(Record ρ)` | `ρ` is a `Row` |
| `(row (l τ)… tail)` | labels distinct; each `τ` a `Type`; `tail` is `closed` or `(rvar a)` with `a : Row` in `Δ` |
| `(-> (τ…) ε τ)` | parameters and result are `Type`s; `ε` is an `Eff`. `(pi …)` likewise, with distinct names; in version 0 it equals the `->` type without the names |
| `(eff L… tail)` | labels distinct; `tail` is `closed` or `(evar a)` with `a : Eff` in `Δ` |
| `(T σ…)` | `T` is declared with as many parameters, each `σ` a `Type` |
| `(Pid τ)`, `(Map τ τ)` | the components are `Type`s |
| `(forall ((a k)…₁) τ)` | binders distinct; `τ` well formed with them added to `Δ` |
| `(refine x τ p)` | not in version 0 |

A **brand** is a label that begins with `#`. Brands appear only in rows: the unfolding of a named record (§10.3), and rows written to instantiate a row variable with one. A declared field label may not begin with `#`.

### 10.3 Type equality

`τ ≡ τ'` is structural, with these exceptions:

1. **Rows** are equal when they have the same labels with equal types and equal tails, whatever the order in which labels are written.
2. **`forall`** types are equal up to renaming their binders, which must have the same kinds in the same order.
3. **Arrows** are equal when their parameters and results are. Effect rows are not compared in version 0; they are checked from Phase 4 (RFC §21.2).
4. **A named record is its branded row.** `(T σ…)`, for a record `T` with parameters `a…` and fields `(l τ)…`, equals `(Record (row (#T Unit) (l τ[σ/a])… closed))`, its **unfolding**. A named type is unfolded only when the other side is a `Record`, so equality always terminates. Two named types are equal only when their names and arguments are.
5. `Dyn` equals only `Dyn`.

**Substitution** replaces `(tvar a)` by a type, and `(rvar a)` or `(evar a)` by a row or an effect row, **splicing** its labels into the enclosing row. A splice that duplicates a label makes the type ill formed.

### 10.4 Synthesis

| Term | Synthesizes |
|---|---|
| `x` | `Γ(x)` |
| `n`, `r`, `s`, `(atom a)`, `(unit)` | `Int`, `Float`, `String`, `Atom`, `Unit` |
| `(fn ((x τ)…) e)` | `(-> (τ…) (eff closed) τ')` when each `τ` is well formed and `e ⇒ τ'` with the parameters in `Γ` |
| `(app f a₁ … aₙ)` | `τ`, when `f ⇒ (-> (τ₁ … τₙ) ε τ)` and each `aᵢ ⇐ τᵢ` |
| `(let ((x τ e₁)) e₂)` | what `e₂` synthesizes with `x : τ`, when `e₁ ⇐ τ` (§10.7 for a polymorphic `τ`) |
| `(letrec ((f τ (fn …))…) e)` | what `e` synthesizes with every `f : τ`, when every `fn` checks against its `τ` with every `f` in scope; the names are distinct |
| `(inst e σ…)` | `τ[σ/a]`, when `e ⇒ (forall ((a k)…) τ)` with as many binders, each `σ` of kind `k`: a type, a `(row …)` or an `(eff …)` |
| `(tuple e…)` | `(Tuple τ…)` |
| `(record τ (l e)…)` | `τ`, when `τ` is a named record or a `(Record (row … closed))`, the labels are exactly its fields, and each `e` checks against its field's type |
| `(select e l)` | the type of field `l` when `e` synthesizes a named record, or a `Record` whose row has `l`; the `l`-th element type when `e ⇒ (Tuple …)` and `l` is an integer in range |
| `(inj τ C e…)` | `τ`, when `τ` is a named sum with a constructor `C` of as many fields, and each `e` checks against its field's type |
| `(match e clause…₁)` | the clauses' common type (§10.6), when `e ⇒ τₛ`, each pattern types against `τₛ` (§10.9), each guard checks against `(Bool)` and is guard-safe (§6.6), and the unguarded clauses are exhaustive (§10.8) |
| `(if c e₁ e₂)` | the branches' common type, when `c ⇐ (Bool)` |
| `(prim op e…)` | the result type in §10.10, when each operand checks against its type there |
| `(perform op e…)` | the result type of `op` below, when each operand checks against its type |
| `(handle-crash e ((x (Reason)) h))` | the common type of `e` and of `h` (with `x : (Reason)`) |
| `(up τ e)` | `Dyn`, when `e ⇐ τ` and `τ` is ground data (§10.11) |
| `(decode τ e)` | not in version 0 |

The operations of version 0 are a subset of RFC §4.5's signature:

| Operation | Operands | Result |
|---|---|---|
| `now` | none | `Int` |
| `random` | none | `Int` |
| `unique` | none | `Ref` |
| `external` | `Atom Atom (List Dyn)` | `Dyn` |
| `crash` | `(Reason)` | checks against any type; synthesizes none |

### 10.5 Checking

`e ⇐ τ` holds when:

- `e` is `(perform crash e')` and `e' ⇐ (Reason)`;
- `e` is an `if`, a `match`, a `handle-crash`, a `let` or a `letrec`, and the same rule as in §10.4 holds with `τ` checked at every branch or body instead of synthesized;
- `e` is `(fn ((x τ₁)…) b)`, `τ ≡ (-> (τ₁ …) ε τᵣ)`, and `b ⇐ τᵣ`;
- otherwise, `e ⇒ τ'` and `τ' ≡ τ`.

### 10.6 Branches

The branches of an `if`, the clause bodies of a `match`, and the body and handler of a `handle-crash` have a **common type**: the type of the first one that synthesizes, against which the others are checked. If none synthesizes, as when every branch crashes, the term has no type in synthesis position and must be checked.

### 10.7 Polymorphic bindings

A `let`, `letrec` or `def` whose type is `(forall ((a k)…) σ)` checks its right-hand side against `σ` with the binders added to `Δ`. This is how a polymorphic function is made; `inst` is how it is used. A polymorphic `let` must bind a **value**: a `fn`, a literal, `(atom a)`, `(unit)`, a variable, an `inst` of a value, or a `tuple`, `record` or `inj` of values. So a received message can never be given the type `(forall ((a Type)) (tvar a))` (RFC §9.2).

### 10.8 Exhaustiveness

The unguarded clauses of every `match` must match every value of the scrutinee's type. A clause with a guard does not count, since its guard may be false. Lint decides this with the usefulness algorithm (Maranget, *Warnings for pattern matching*, 2007), over these constructor sets:

- a named sum, `(Bool)`, `(List τ)` and `(Reason)`: their constructors, a finite set;
- a tuple, a named record and `Unit`: one constructor each;
- `Int`, `Float`, `String` and `Atom`: infinitely many literals, so literal patterns never cover the type;
- any other type: no constructor patterns at all, only variables and `_`.

A non-exhaustive `match` is reported with an example of a value no clause matches.

### 10.9 Patterns

| Pattern | Types against `τ` when | Binds |
|---|---|---|
| `_` | always | nothing |
| `x` | always | `x : τ` |
| `n`, `r`, `s`, `(atom a)`, `(unit)` | `τ` is `Int`, `Float`, `String`, `Atom`, `Unit` | nothing |
| `(tuple p…)` | `τ ≡ (Tuple τ…)` of the same length, each `p` against its element | theirs |
| `(record τ' (l p)…)` | `τ'` is a named record, `τ' ≡ τ`, the labels are distinct fields of it | theirs |
| `(inj τ' C p…)` | `τ'` is a named sum, `τ' ≡ τ`, `C` is its constructor with one pattern per field | theirs |
| `(as x p)` | `p` types against `τ` | `x : τ` and those of `p` |

No variable is bound twice in one pattern. Patterns over anonymous records are not in version 0.

### 10.10 Primitive types

| `op` | Operands | Result |
|---|---|---|
| `add` `sub` `mul` `div` `rem` | `Int Int` | `Int` |
| `neg` | `Int` | `Int` |
| `lt` `le` `gt` `ge` | `Int Int` | `(Bool)` |
| `fadd` `fsub` `fmul` `fdiv` | `Float Float` | `Float` |
| `fneg` | `Float` | `Float` |
| `flt` `fle` `fgt` `fge` | `Float Float` | `(Bool)` |
| `int-to-float` | `Int` | `Float` |
| `eq` `ne` | `τ τ`, the first operand's type, which is ground data (§10.11) | `(Bool)` |
| `not` | `(Bool)` | `(Bool)` |
| `concat` | `String String` | `String` |
| `length` | `(List τ)` | `Int` |
| `empty?` | `(List τ)` | `(Bool)` |
| `head` | `(List τ)` | `τ` |
| `tail` | `(List τ)` | `(List τ)` |

For `eq`, `ne` and the list primitives, the first operand synthesizes the `τ` the rest are checked against.

### 10.11 Ground data types

A type is **ground data** when it contains no arrow and no type, row or effect variable, including inside the declarations of the named types it mentions. `eq`, `ne` and `up` are defined on ground data only.

- **No arrow:** a function has no equality on which the evaluator and BEAM agree. A closure is not a BEAM fun, and BEAM compares funs by their code and environment, which the lowering is free to change. A function also has no canonical representation, so it cannot be embedded into `Dyn` (RFC §4.6).
- **No variable:** at a type variable, the operation would have to work for every instantiation, functions included. Polymorphic equality is instead an argument: a dictionary of the `Eq` class, the way RFC §4.2 elaborates classes. Only the derived, structural instance is lawful and may enter the logic.

### 10.12 Binders

Within a definition, or a term checked on its own, every binder is distinct: the parameters of every `fn`, the variables of `let`, `letrec`, `handle-crash` and patterns. No binder reuses the name of a definition of the module. The name `_` binds nothing and may repeat. So a name always means one binding, and a fact about it in the refinement checker can never refer to a shadowed binding (RFC §5.4). An elaborator meets this by renaming.

### 10.13 Modules

A module `(module M (core-version 0) item…)` passes when:

- its type names are distinct, and none is built in or reserved (§3.2);
- in each declaration the parameters are distinct, the constructor names or field labels are distinct, no field label is a brand, and every field type is well formed with `Δ` the declaration's parameters;
- its definition names are distinct, each definition's type is well formed with `Δ` empty, its binders are distinct (§10.12), and each `fn` checks against its type (§10.7) with every definition in `Γ`.

Each problem is reported with the name of the definition it is in and the nearest enclosing `(meta (span …))`.

### 10.14 Not checked in version 0

Effect rows, A-normal form, opacity and sealing, privilege, and the well-formedness of refinements (RFC §9.2). Each arrives with the feature it protects; until then no Core term can need them.

### 10.15 Soundness

Core Lint and the evaluator are both trusted, and they must agree. For a closed term `e`:

- if `e ⇒ τ`, evaluating `e` under the pure handler ends in `(ok v)` with `v ⊨ τ` (§5.1), or in `(crash ρ)` with `ρ ⊨ (Reason)`, or does not end;
- if `e` synthesizes no type (it never returns), evaluating it ends in a crash or does not end;
- in no case does the evaluator stop with an internal error (§6.1).

This is type safety: progress and preservation, stated as one property of the two implementations. Version 0 does not prove it; the Lean model of Phase 8 is where it becomes a theorem (RFC §17.4). Until then it is tested three ways:

1. **Generated terms.** Random closed terms are built from the rules of §10.4 and a type, and Lint must accept every one at exactly that type: the generator and Lint agree on what is well typed.
2. **Safety.** Every generated term is evaluated, and the property above must hold.
3. **Mutants.** Each generated term is corrupted at random: a subterm replaced, a constructor, label, primitive or variable changed, a child dropped. Lint must reject the mutant, or the mutant must satisfy the property at the type Lint gives it. A mutant that Lint accepts and that then goes wrong is a hole in Lint. This is RFC K5, fault injection, with the faults chosen at random instead of by hand.

## 11. Differences from the RFC

1. **Constructors carry field sequences.** RFC §4.1 writes a sum label with one type, so a two-field constructor carries a tuple. Here `(inj τ C e…)` takes one argument per field, which is what the representation does (`{C, v₁, v₂}`, not `{C, {v₁, v₂}}`).
2. **Float arithmetic can crash.** RFC §4.3 says `+.`, `-.` and `*.` never crash. BEAM has no infinities: `1.0e308 * 10.0` raises `badarith` (verified on OTP 29). §7 makes a non-finite result a crash.
3. **Named types are always nodes** (`(Bool)`, not `Bool`), and Unit's value is written `(unit)`, so a symbol in a term is always a variable and a symbol in a type is always a base type.
4. **The unit value is the atom `nil`**, the representation today's backends use.
5. **Built-in sums keep Erlang's representations.** `Bool` is `true`/`false`, `List` is Erlang lists, and `Reason`'s constructors without fields are bare atoms, as BEAM's own error reasons are. Declared sums are tagged tuples.
6. **Constructors and records name their full type**: `(inj (List Int) Nil)`, `(record (Point) …)`. RFC §4.2 writes `(inj T C e)` and `(record T? …)`, which leaves a type argument to be inferred; Core Lint infers nothing (§10).
7. **No anonymous sums yet.** RFC §4.1 has the closed labelled sum `<C₁: τ₁ | …>`; version 0 has only named sums, which is all the surface language produces.
8. **Primitive equality is on ground data only** (§10.11). RFC §4.3 defines `==` on "two values of the same type". At a function type the evaluator and BEAM cannot agree, and at a type variable the type might be a function, so both need the `Eq` dictionary RFC §4.2 already describes.

## 12. Defects this definition exposes

Each is a disagreement between a backend and §6 to §9. The differential harness reports them; the RFC's defect list continues the numbering.

| # | Program | Expected (this document) | `:core` | `:elixir` |
|---|---|---|---|---|
| D25 | `(== 0.0 -0.0)`; `(== 1 1.0)` | `false`; rejected by Core Lint (operands of different types) | `false`; `false` | `true`; `true` |
| D26 | a `match` with no matching clause | `(crash no_match)` | `if_clause` | `{case_clause, _}`, which §9 reads as `no_match` |

D26 is not a disagreement of outcomes: §9 reads both as `no_match`. It is listed because the reason a program's own `handle-crash` sees differs between the backends.
