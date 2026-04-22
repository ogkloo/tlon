# Web Interface Sketch

This document sketches a simple first web interface for Tlon that fits the current repo and toolchain.

## Current Status

The repo now has a working first-pass web implementation rather than just a sketch. The live shape still matches the broad approach in this document:

- a single Haskell web server
- server-rendered HTML via Lucid
- in-memory game state
- partial updates and polling instead of a separate SPA

Implemented so far:

- lobby creation with per-player links
- optional NPC seats at game creation
- manual round submission, plus optional timed auto-resolution
- staged player actions for limit orders and lottery tickets
- latest round report, inventory, holdings, and history views
- debug-only controls behind a `--debug` flag, including `Reset All Games`
- watched local development through `just web-watch 8080 [--debug]`

The rest of this document remains useful as rationale and architectural context, but some sections describe the intended first pass that now exists in code.

The main constraint is that the game engine is already pure Haskell and the project is already distributed through Nix and Cabal. The first web layer should preserve that rather than introducing a separate frontend build system immediately.

## Recommendation

Build the first web interface as a single Haskell web server with server-side rendered HTML and small progressive enhancement. Do not start with a separate SPA.

That means:

- the game engine stays in `src/Tlon/...`
- a new web executable owns in-memory game state and HTTP routes
- HTML is rendered on the server
- updates are driven with normal form posts and small polling or `htmx`-style partial refreshes

This is the lowest-friction path to something usable.

## Why This Shape

It matches the current system well:

- pure engine logic is already written in Haskell
- the repo already expects development through `nix develop`
- Cabal and Nix can package another executable cleanly
- there is no current need for a rich client-side state machine

It also avoids front-loading a lot of moving parts:

- no Node toolchain
- no bundler
- no separate API versioning problem on day one
- no duplicated view models between backend and frontend

## Suggested v1 Web Stack

### Server

- `wai`
- `warp`
- `scotty`

`scotty` is enough for a small route table and simpler than starting with `servant`. If the project later wants a more explicit API surface, the engine can stay unchanged and the HTTP layer can be replaced.

### Rendering

- `lucid`

`lucid` keeps HTML generation typed and local to Haskell. That fits the current repo better than template files or a separate frontend app.

### Data / Serialization

- `aeson`
- `text`
- `bytestring`

Even if the first UI is mostly server-rendered HTML, JSON endpoints for round reports and game state snapshots are worth having from the start.

### Concurrency / State

- `stm`

Use a `TVar ServerState` to hold running games and submitted orders. The engine remains pure; the web layer is just responsible for collecting input, sequencing rounds, and storing snapshots.

### Optional Small Frontend Enhancement

- vendored `htmx` static asset, or no JS at all in the first pass

If you want dynamic tables and partial refreshes without building a SPA, `htmx` is a good fit. I would vendor the asset into the repo rather than depending on a CDN.

## Suggested Package Changes

### Cabal

Add a second executable, something like:

- `exe:tlon-web`

This executable should depend on:

- `base`
- `containers`
- `text`
- `bytestring`
- `aeson`
- `stm`
- `wai`
- `warp`
- `scotty`
- `lucid`

The existing `tlon` executable can stay as the CLI/debug runner.

### Nix

The current `callCabal2nix` packaging approach is still fine. Once the web executable exists:

- `nix build .#tlon` can continue to build the package
- `nix run .#tlon-web` should start the local web server

If you want cleaner flake outputs, expose both:

- `packages.tlon-cli`
- `packages.tlon-web`

and make one of them the default.

For development, the shell should add no new global tools unless they are actually needed. For this approach, the current Haskell dev shell is still sufficient.

## Suggested Repo Shape

```text
app/Main.hs                     -- existing CLI entry point
app/WebMain.hs                  -- new web server entry point
src/Tlon/...                    -- pure engine
src/Tlon/Web/Server.hs          -- route table and server bootstrap
src/Tlon/Web/View.hs            -- HTML rendering with Lucid
src/Tlon/Web/Api.hs             -- JSON encoders / response shapes
src/Tlon/Web/State.hs           -- TVar-backed server state
static/                         -- vendored htmx, small CSS, icons
```

Keep the web code out of the core engine. The boundary should stay:

- core engine: pure round stepping and reports
- web layer: HTTP, sessions, game registry, form decoding, rendering

## Minimal Interaction Model

The first web UI should support exactly these workflows:

1. Create a new local game instance
2. View the current round state
3. View holdings, redemption table, and previous round report
4. Submit one or more orders for a chosen player
5. Resolve the round
6. View fills, redemptions, grants, eliminations, and winner state

That is enough to exercise the engine without inventing auth, persistence, or multiplayer synchronization yet.

## Proposed HTTP Shape

### HTML routes

- `GET /`
  - game list / create game form
- `POST /games`
  - create a game, redirect to `/games/:gameId`
- `GET /games/:gameId`
  - main game screen
- `POST /games/:gameId/orders`
  - submit one player's round orders
- `POST /games/:gameId/resolve`
  - resolve the round and redirect back to the game screen

### JSON routes

- `GET /api/games/:gameId/state`
- `GET /api/games/:gameId/report`

The HTML routes are the primary product. The JSON routes are there so the interface can grow later without reworking the server shape.

## State Model

For a first pass, keep games in memory:

```haskell
data ServerState = ServerState
  { runningGames :: Map GameId RunningGame
  }

data RunningGame = RunningGame
  { currentState :: GameState
  , stagedOrders :: Map EntityId [Order]
  , history :: [RoundReport]
  }
```

This is intentionally local and disposable.

It is enough for:

- local development
- short-lived game sessions
- debugging the round engine through a browser

Do not add a database in the first web pass.

## Rendering Notes

The game page should prioritize scannable tables, not decorative layout.

Sections:

- round header with winner / status
- redemption table
- per-entity holdings table
- staged order entry form
- previous round report
- fills / expirations / redemptions / grants

This game wants dense information more than marketing chrome.

## Distribution

### Local development

Use:

```sh
nix develop
cabal run tlon-web
```

or:

```sh
nix develop --command cabal run tlon-web
```

### Packaged local run

Use:

```sh
nix run .#tlon-web
```

### Later deployment

Once the single-process web executable works locally, deployment can stay simple:

- build the binary with Nix
- run it behind a reverse proxy if needed
- keep static assets served by the same executable at first

There is no reason to split frontend hosting and backend hosting before the interface proves itself.

## What I Would Not Do Yet

- no SPA
- no React/Vite/TypeScript toolchain
- no websocket transport
- no database
- no user accounts
- no distributed game workers

Those all may become appropriate later, but they are the wrong first move for this codebase.

## Phase Order

1. Add `tlon-web` executable and minimal `scotty` server
2. Render a read-only game page from a seeded default game
3. Add order submission forms
4. Add round resolution and report display
5. Add small progressive enhancement for partial refreshes
6. Only then decide whether a richer API or client app is justified
