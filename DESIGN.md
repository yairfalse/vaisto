# Vaisto Design Document

Decisions made during design sessions. This is the source of truth for language direction.

---

## Identity

Vaisto is a **distributed systems language** that catches bugs at compile time.

- Not a Lisp (uses S-expressions because they're practical)
- Not an FP language (uses immutable data because it's practical)
- A tool for pragmatic engineers tired of 3 AM pages

**Target user:** Myself. Pragmatic infra engineer who wants types without ceremony.

**The pitch:** "Catch distributed system bugs at compile time."

---

## Core Decisions

| Area | Decision | Rationale |
|------|----------|-----------|
| Syntax | S-expressions | Minimal, parseable, cool, structure is visible |
| Types | ML-style inference (Hindley-Milner) | Safety without annotation tax |
| Runtime | BEAM | Fault tolerance, distribution, hot reload for free |
| Macros | None | Tight, opinionated — users can't extend the language |
| Error style | Rust + Go | Short, exact, no jargon, no theory leaking |

---

## Why S-Expressions

Not because Lisp. Because:

1. **Minimal** — grammar fits on a napkin
2. **Small** — parser is a weekend, not months
3. **Cool** — signals intentional design
4. **Structural** — compiler sees what you see

---

## Why No Macros

1. Type checking stays tractable
2. BEAM/OTP already has the right primitives
3. Tooling becomes possible (LSP, formatter)
4. Faster path to "done"

Users get: `process`, `supervise`, `def`, `deftype`, `match`
Users don't get: `defmacro`

---

## Error Messages

Voice: Rust's clarity + Go's brevity.

```
counter.va:3:12: type error: expected Int, found String

    :increment (+ state "oops")
                        ^^^^^^

  state is Int (from line 1: initial value 0)
  hint: use (string:to_int "oops")?
```

Principles:
- **Where** — file:line:col
- **What** — expected X, found Y
- **Show** — point at the problem
- **Why** — one line of context if inference is involved
- **Hint** — actionable suggestion when obvious fix exists

Never:
- Academic jargon ("unification failed")
- Type variables in user-facing messages
- Paragraphs of explanation

---

## Pattern Matching

Yes. Explicit `match` expression.

```scheme
(match msg
  {:add n} (+ state n)
  :reset 0
  _ state)
```

- Exhaustiveness checking: compiler error if cases are missing
- Wildcard `_` as escape hatch
- Start with `match` only — function head patterns can come later

---

## Typed PIDs

**Process IDs carry their message type.**

The problem: if `pid` is untyped, the compiler can't check what messages it accepts.

```scheme
(def pid (spawn counter 0))
(! pid :increment)    ; ok?
(! pid "garbage")     ; should fail, but how does compiler know?
```

The solution: PIDs are typed by the messages they accept.

```scheme
(def pid (spawn counter 0))   ; pid : (Pid CounterMsg)
(! pid :increment)            ; ok — :increment is a CounterMsg
(! pid "garbage")             ; compile error — String is not CounterMsg
```

### How It Works

The compiler infers the message type from the process definition:

```scheme
(process counter 0
  :increment (+ state 1)
  {:add n} (+ state n)
  :get state)
```

Inferred message type: `CounterMsg = :increment | {:add Int} | :get`

Spawning returns `(Pid CounterMsg)`. Sending checks the message against that type.

### Why This Matters

This is the "type fortress" extended to process boundaries. Without typed PIDs, type safety ends at `spawn`. With them, the compiler catches message mismatches between processes — the core promise of Vaisto.

---

## Data Structures

**Typed records for safety, untyped maps as escape hatch.**

```scheme
; Typed — compiler knows the shape
(deftype User {:name String :age Int})
(def u (User {:name "jo" :age 30}))
(get u :name)      ; String
(get u :wat)       ; compile error

; Untyped — you're on your own
(def config {:port 8080 :debug true})  ; (Map Atom Any)
(get config :port)   ; Any
```

---

## Type Visibility

**Open by default, `opaque` when you need protection.**

```scheme
; Open — anyone can construct and read
(deftype Point {:x Int :y Int})

; Opaque — only this module can construct
(deftype opaque Password {:hash String})

(def make [plaintext]
  (Password {:hash (sha256 plaintext)}))
```

Outside modules can pass opaque types around but can't construct or inspect them.

---

## Module System

### Convention

```
my_project/
  lib/
    main.va           ; module: main
    http.va           ; module: http
    utils/
      math.va         ; module: utils:math
```

### Rules

| Decision | Answer |
|----------|--------|
| File structure | `lib/` convention |
| Naming | Path = module name |
| Access | Namespaced by default (`math:add`) |
| Selective import | Optional (`(import math [add])`) |
| Visibility | Everything public, `opaque` for protected types |
| Circular imports | Forbidden |
| Naming collisions | Compiler error |
| Build order | Automatic resolution |

### Examples

```scheme
; Namespaced
(import utils:math)
(utils:math:add 1 2)

; Selective
(import utils:math [add])
(add 1 2)

; Collision = error
(import http [get])
(import map [get])   ; compiler error
```

---

## Interop

**Extern declarations required to call Erlang/Elixir.**

```scheme
(extern erlang:io:format [String (List Any)] Atom)
(extern erlang:file:read_file [String] (Tuple Atom Binary))

(io:format "hello ~s~n" ["world"])
(file:read_file "/tmp/data.txt")
```

Calling without declaring = compiler error.

Being called from Erlang/Elixir = just works (compiles to `.beam`).

---

## Standard Library

Minimal — enough to write a typed process.

### Primitives
- Arithmetic: `+` `-` `*` `/`
- Comparison: `==` `!=` `<` `>` `<=` `>=`
- Process: `!` (send) `?` (receive) `spawn`

### Lists
- `head`, `tail`, `cons`
- `length`
- `map`, `filter`, `fold`

### Strings
- `concat`, `length`, `split`

### Debug
- `print`, `inspect`

### Not in stdlib
- HTTP, JSON, files, database, crypto
- Those are packages or Erlang interop

---

## Cross-Service Contracts

**Future work. Not in v1.**

Vision: `defcontract` for cross-service boundaries.

```scheme
(defcontract user-events {:user_id Int :action Atom})
```

Compiler emits contracts. CI compares across services. Mismatch = build failure.

For now: shared types by convention, or accept runtime risk at service boundaries.

---

## What Vaisto Is Not

- A Lisp for Lisp purists (no macros, no homoiconicity worship)
- An FP language for FP enthusiasts (no monads, no category theory)
- A general-purpose language (focused on distributed systems)
- A research project (using proven techniques: HM inference, BEAM, S-exprs)

---

## What Vaisto Is

A focused tool for building distributed services where:

- Types catch contract errors at compile time
- Supervision is built into the syntax
- The BEAM handles fault tolerance
- Code is small and auditable

```scheme
(process counter 0
  {:add n} (+ state n)
  :reset 0
  :get state)

(supervise :one_for_one
  (counter 0))
```

---

## Open Questions (Decide When We Hit Them)

- Pattern matching in function heads (vs. explicit `match` only)
- Row polymorphism for flexible typed maps
- Package manager (Hex? Own?)
- Frameworks

---

## Changelog

- **2026-01-14**: Initial design session. Core decisions locked in.
- **2026-01-14**: Added typed PIDs, actionable error hints.
