# Repository Guidelines

## Project Structure & Module Organization
- `Main.hs` exposes the CLI wired to example parsing and synthesis.
- `Jsyn.hs` contains the DSL, type inference, and search routines; most feature work lands here.
- `Spec.hs` keeps the Hspec suite and golden assertions that exercise evaluation and synthesis paths.
- `Benchmarks.hs` runs Criterion microbenchmarks for the hottest operations.
- `tests/` stores JSON fixtures; golden snapshots for `defaultGolden` live next to their source files.
- `jsyn.cabal` defines shared dependencies and enables `-Wall`; adjust it whenever modules or tooling change.

## Build, Test, and Development Commands
- `cabal build` compiles the executable and catches type errors early.
- `cabal run jsyn -- tests/test8.json` drives the CLI end-to-end against a sample fixture.
- `cabal test` executes the Hspec suite; set `HSPEC_UPDATE=1` to refresh golden snapshots after intentional output changes.
- `cabal bench` invokes the Criterion benchmarks defined in `Benchmarks.hs` to watch performance trends.
- `cabal repl jsyn` opens a REPL scoped to the executable for quick experimentation.

## Coding Style & Naming Conventions
- Follow the established two-space indentation for `do` blocks and case expressions; keep imports explicit or qualified as in `Jsyn.hs`.
- Use `CamelCase` for data types and constructors (`JsonExample`, `SynthRes`) and lower camel case for functions (`runSynth`), reserving underscores for fixtures.
- Group language pragmas at the top of each module and attach instance derivations via `deriving` clauses.
- Treat compiler warnings as actionable; `-Wall` is enabled, so resolve or justify them before committing.

## Testing Guidelines
- Add new example pairs under `tests/` and load them with `readJsonExamples` when expanding coverage.
- Organize assertions with `describe`/`it`, following the `TestTask` template to cover evaluation and inferred types together.
- Use golden tests (`defaultGolden`) to lock expected JavaScript output; regenerate snapshots with `HSPEC_UPDATE=1 cabal test` when behavior changes on purpose.
- Document limitations in comments (see `Spec.hs`) and mark expected failures with `pendingWith` when introducing regression cases.

## Commit & Pull Request Guidelines
- Write commit subjects in the imperative mood (e.g., “Add boolean combinators”), mirroring the existing history, and keep them under 72 characters.
- Reference affected modules or fixtures in the body, and call out golden updates or benchmark shifts.
- For pull requests, include a concise summary, reproduction steps, new tests or fixtures, and screenshots or diffs if snapshots move.
- Ensure local `cabal test` (and `cabal bench` when relevant) run cleanly before requesting review.
