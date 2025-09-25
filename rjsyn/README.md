# rjsyn

A Rust rewrite of the original `jsyn` JSON program synthesiser. It infers types from JSON input/output examples, searches the DSL space, and emits an executable JavaScript program.

## Prerequisites

- Rust toolchain (rustup + cargo)
- JSON example file structured as an array of `{ "input": ..., "output": ... }` pairs

## Build & Test

```sh
cargo build
cargo test
```

## Run the CLI

Two subcommands are available:

- `synth [--lang <language>] <examples file>` synthesises a program from example pairs. By default it prints the program's debug representation; pass `--lang javascript` to emit runnable JS.
- `infer <json file>` infers and prints the abstract type of a JSON value.

Examples:

```sh
# synthesise and print debug tree
cargo run -- synth tests/test1.json

# synthesise and emit JavaScript
cargo run -- synth --lang javascript tests/test1.json

# infer the type of a JSON value stored in value.json
cargo run -- infer value.json
```

The default synthesis timeout is 2 seconds (`Duration::from_micros(2_000_000)`). Edit `src/main.rs` to adjust.

## Snapshot Tests

Snapshot tests live under `tests/`. After intentional output changes, refresh snapshots with:

```sh
INSTA_ACCEPT=overwrite cargo test --test synth
```
