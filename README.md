# Tlon

A game about markets on top of lotteries, written in Haskell.

## Development Flow

This repo uses:

- `nix` to pin the toolchain and provide a reproducible shell
- `cabal` for the normal edit/build/run/test loop
- plain Haskell package structure: `src/` for the library, `app/` for the executable, `test/` for the test suite

In practice, the flow is:

1. Enter the Nix shell to get the exact compiler and tools for this repo.
2. Use Cabal while developing.
3. Use `nix build` or `nix run` when you want to exercise the packaged flake output.

## Prerequisites

- [Nix](https://nixos.org/download/) with flakes enabled

The checked-in flake provides outputs for:

- `x86_64-linux`
- `aarch64-linux`
- `x86_64-darwin`
- `aarch64-darwin`

## Enter The Shell

```sh
nix develop
```

This shell provides the full Haskell toolchain, so no separate global GHC or Cabal install is needed.

## Cabal Inner Loop

Once inside `nix develop`, use Cabal for day-to-day work:

```sh
cabal build all
cabal run tlon
cabal test
```

Useful variants:

```sh
cabal build lib:tlon
cabal build exe:tlon
cabal build exe:tlon-web
cabal build test:tlon-test
cabal repl tlon
```

What those targets map to:

- `lib:tlon`: the library in `src/`
- `exe:tlon`: the executable in `app/Main.hs`
- `exe:tlon-web`: the web executable in `app/WebMain.hs`
- `test:tlon-test`: the test suite in `test/Spec.hs`

## Web Development

The browser-facing implementation is a server-rendered web app built from the same Haskell package.

Common dev-mode commands:

```sh
just web 8080
just web-watch 8080
```

In dev mode, the public server is a local proxy. `just web 8080` builds the local Agentation bundle, starts the Haskell `tlon-web` backend on port 8081, and starts the proxy on `http://127.0.0.1:8080`. Open the proxy URL, not the backend URL.

`just web-watch 8080` rebuilds and restarts that same proxy-backed dev server when Haskell, static, client, package, or script files change.

The proxy serves the local Agentation bundle and injects its mount into HTML responses. This keeps Agentation out of the Haskell app code while still making it available during local development.

To intentionally run the plain packaged web app without the dev proxy, use Cabal directly:

```sh
cabal run tlon-web -- --port 8080
```

Current web flow:

- create a one-player trading dashboard with optional NPC flow
- optionally reserve NPC seats at lobby creation
- share per-player URLs if using more than one human seat
- start the game once all human seats are filled
- stage limit orders over listed `SeriesId` pairs
- stage per-offering purchases for active instrument offerings
- submit turns manually, or let timed rounds auto-resolve if a timer was configured

Player pages currently surface:

- inventory as series positions
- active offerings
- listed markets
- staged trading actions
- latest round report, instrument activity, and round history

## Fast Feedback Tools

These tools are available inside the Nix shell:

| Tool | Version | Purpose |
|------|---------|---------|
| GHC | 9.10.3 | Haskell compiler |
| Cabal | 3.16.0 | Build system |
| HLS | 2.12.0 | Language server for editors |
| HLint | 3.10 | Linting |
| Fourmolu | 0.19.0 | Source formatting |
| ghcid | 0.8.9 | Fast reload loop |
| cabal-fmt | 0.1.12 | `.cabal` file formatting |

Common commands:

```sh
ghcid --command "cabal repl tlon"
hlint .
fourmolu -i app/Main.hs src/Tlon.hs test/Spec.hs
cabal-fmt -i tlon.cabal
```

## Nix Package Flow

Use these when you want to build or run the flake output itself rather than working through Cabal:

```sh
nix build .#tlon
nix run .#tlon
nix run .#tlon-web
```

This was verified locally:

- `nix build .#tlon` builds the packaged derivation
- `nix run .#tlon` runs the executable and prints `Welcome to Tlon.`

## Troubleshooting

If Cabal gets into a bad local build state, clear the workspace build artifacts and retry:

```sh
rm -rf dist-newstyle
nix develop --command cabal build all
```

I hit one such case locally where `cabal build all` failed with an existing `package.conf.inplace` path while `cabal test` still succeeded. Removing `dist-newstyle` is the normal reset for that situation.

## Project Layout

```text
flake.nix        # Nix flake: pinned toolchain, dev shell, package output
tlon.cabal       # Cabal package definition
src/             # Library source
app/             # Executable source
test/            # Test source
dist-newstyle/   # Local Cabal build artifacts
```

## Next Design Notes

- Web interface sketch: [WEB.md](./WEB.md)

## Current Stack

- `nixpkgs`: `nixos-25.11`
- `ghc`: `9.10.3`
- `cabal-install`: `3.16.0`
