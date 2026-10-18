# AGENTS.md

Operating manual for AI coding agents (Codex CLI, Claude Code, etc.) working
in this repo. CLAUDE.md is the architecture reference; this file is the
working contract.

## Stack

Vaisto is a statically-typed Scheme-like language compiling to BEAM.
Implementation is Elixir. Build with `mix`. Two dependencies: `jason`,
`toml`. Refinement checking also needs the `z3` executable on PATH (owner
decision Q2): it is reached over a Port, not a dependency, and only programs
with refinements start it. No CI, no formatter config, no Credo, no Dialyzer.

## Conventions you must follow

- AST nodes are tuples with `%Loc{}` as the **final** element
  (e.g. `{:defn, name, params, body, ret_type, %Loc{}}`).
- Elaborated output is canonical, typed Liquid Core (Elixir lists), not a
  second surface AST. Core metadata may retain source information.
- The parser **raises** on syntax errors. Wrap with
  `Vaisto.Compilation.parse/2` for `{:error, ...}` returns.
- Type checking uses `Vaisto.Elab` with explicit definition signatures and
  local inference. Thread its context; do not reintroduce HM or a fallback.
- Use `Vaisto.Elab.Types.unify/4` for unification; thread both its
  substitution and fresh counter. Do not write bespoke field comparison.
- Every elaborated module must pass independent Core Lint before emission.
- Errors use `Vaisto.Errors`/`Vaisto.Error` with source spans and filenames; rendered
  by `Vaisto.ErrorFormatter` (Rust-style with source context).
- "Did you mean?" hints use Jaro distance — see
  `Vaisto.Errors.undefined_variable` for the pattern.
- Mirror the closest existing pattern. For new top-level forms, model on
  `parse_defn` / `parse_deftype` in `lib/vaisto/parser.ex`.

## Constraints you must respect

- `mix test` must stay green after every change.
- Do not add dependencies.
- Do not introduce new abstractions, helper modules, or compatibility
  shims unless required to keep a single function readable.
- Do not skip git hooks (`--no-verify`, `--no-gpg-sign`).
- Match surrounding code style — there is no `.formatter.exs`.
- Do not commit unless the task brief explicitly asks for it.

## Workflow expectation

For any non-trivial task:
1. Inspect the relevant files first; do not start coding from
   assumptions.
2. Post a short plan before implementing.
3. For changes touching the type checker or parser, post the diff for
   the production code before adding tests.
4. After implementing, summarize changed files and verification output.

## Verification commands

- `mix deps.get` — install dependencies (one-time).
- `mix test` — run the full suite. The refinement tests
  (`test/refine/`) need `z3` on PATH and fail without it.
- `mix test path/to/file_test.exs` — run one file.
- `mix test path/to/file_test.exs:LINE` — run one test.
- `mix escript.build` — build the `vaistoc` CLI.
