# ADR-002: Compiler Implementation Strategy

## Status

Accepted

## Context

Vaisto needs a compiler. The key architectural question: what language should the compiler be written in?

Three options exist:

1. **Self-hosted** — Write the compiler in Vaisto itself
2. **Elixir/Erlang** — Use BEAM languages (current implementation)
3. **Rust** — Use a systems language like Gleam does

We already have:
- A working Elixir-based compiler (~2000 lines)
- A proof-of-concept self-hosted compiler (~1700 lines of Vaisto)

### Research on other BEAM languages

| Language | Compiler written in | Self-hosted? |
|----------|-------------------|--------------|
| Erlang   | Erlang            | Yes          |
| Elixir   | Erlang            | No           |
| Gleam    | Rust              | No           |
| LFE      | Erlang            | No           |
| Caramel  | OCaml             | No           |

Only Erlang self-hosts. The Elixir core team explicitly chose not to self-host, citing:
- Increased maintenance burden
- Higher barrier for new contributors
- Bootstrapping complexity
- Erlang is already required for BEAM anyway

The Joxa language (a BEAM Lisp) went self-hosted and later reported that "the cost of maintenance went up considerably" and it "made it much harder for folks not familiar with the infrastructure to get up to speed."

### Self-hosting mechanics

Self-hosted compilers use staged bootstrapping (e.g., Rust):

```
Stage 0: Pre-built binary (downloaded/checked in)
    ↓ compiles
Stage 1: Compiler built from source
    ↓ compiles
Stage 2: Compiler rebuilt with stage 1
    ↓ compiles
Stage 3: Compiler rebuilt with stage 2 (should match stage 2)
```

This requires maintaining pre-built binaries in the release chain forever.

### Performance considerations

For real-world adoption, compilation speed matters:

- Elixir compiler: ~100-500ms per file
- Rust compiler: ~1-10ms per file

At 500+ files, the difference between 30 seconds and 3 seconds significantly impacts developer experience.

## Decision

### Primary compiler: Elixir (current)

Keep the Elixir implementation as the primary compiler. Reasons:

1. **Already working** — No rewrite needed
2. **Fast iteration** — Can evolve language design quickly
3. **Native BEAM access** — Direct use of `:cerl` and BEAM tools
4. **Lower contributor barrier** — Elixir is more accessible than Vaisto

### No self-hosting

The Vaisto-written compiler remains a **proof-of-concept**, not the production compiler. Reasons:

1. **No dependency win** — BEAM is required regardless
2. **Bootstrap complexity** — Must maintain pre-built binaries
3. **Debugging difficulty** — Can't debug compiler with buggy compiler
4. **Industry precedent** — Elixir, Gleam, LFE all chose not to self-host

### Future: Rust rewrite (when design stabilizes)

Once the language design is proven with real users, consider rewriting in Rust for:

1. **Compilation speed** — Critical for large codebases
2. **Error handling** — Rust's type system catches compiler bugs at compile time
3. **Industry validation** — Gleam's Rust compiler is fast and reliable

This should happen *after*:
- Language syntax is stable
- Type system design is finalized
- First 100+ users validate the design
- Clear performance bottlenecks are identified

## Consequences

### Positive

- Ship faster with existing Elixir compiler
- Lower barrier for contributors
- Language design can evolve without rewriting compiler
- Self-hosted compiler serves as complex test case
- Clear path to performance (Rust) when needed

### Negative

- Compilation speed limited by BEAM performance
- Two codebases if Rust rewrite happens (temporary)
- Self-hosted compiler may bit-rot if not maintained

### Neutral

- Self-hosted compiler remains as demo/test case
- Rust rewrite is optional, not committed

## Alternatives Considered

### A. Self-host now

Rejected. The romantic appeal of "compiler compiles itself" doesn't outweigh:
- Bootstrap complexity
- Maintenance burden
- No practical benefit on BEAM

The self-hosted compiler proves Vaisto's capability without being the production compiler.

### B. Rewrite in Rust immediately

Rejected. Premature optimization:
- Language design may still change
- 6-12 months of work before shipping anything new
- Elixir compiler is "good enough" for early adoption

### C. Switch to Erlang

Rejected. No meaningful benefit over Elixir:
- Same runtime performance
- Less pleasant syntax
- Smaller ecosystem

## References

- [Elixir core discussion on self-hosting](https://groups.google.com/g/elixir-lang-core/c/K9i3uBSPKZI)
- [Rust bootstrapping guide](https://rustc-dev-guide.rust-lang.org/building/bootstrapping/what-bootstrapping-does.html)
- [Gleam compiler (Rust)](https://github.com/gleam-lang/gleam)
- [Wikipedia: Bootstrapping compilers](https://en.wikipedia.org/wiki/Bootstrapping_(compilers))
