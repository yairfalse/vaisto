# AGENTS.md

Operating manual for AI coding agents (Codex CLI, Claude Code, etc.) working
in this repo. CLAUDE.md is the architecture reference; this file is the
working contract.

## Stack

Vaisto is a statically-typed Scheme-like language compiling to BEAM.
Implementation is Elixir. Build with `mix`. Two dependencies: `jason`,
`toml`. No CI, no formatter config, no Credo, no Dialyzer.

## Conventions you must follow

- AST nodes are tuples with `%Loc{}` as the **final** element
  (e.g. `{:defn, name, params, body, ret_type, %Loc{}}`).
- Typed AST nodes carry types instead of locations.
- The parser **raises** on syntax errors. Wrap with
  `Vaisto.Compilation.parse/2` for `{:error, ...}` returns.
- Type checking uses the ctx-threaded `check_s` / `check_impl_s` form,
  not the legacy `check`. `TcCtx` threads substitution and tvar counter.
- Use `Vaisto.TypeSystem.Unify` for unification; do not write bespoke
  field comparison.
- Errors are `Vaisto.Errors` constructors carrying `%Span{}`; rendered
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
- `mix test` — run the full suite (~1,200 tests).
- `mix test path/to/file_test.exs` — run one file.
- `mix test path/to/file_test.exs:LINE` — run one test.
- `mix escript.build` — build the `vaistoc` CLI.
