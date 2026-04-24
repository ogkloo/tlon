# Web Interface Notes

The web interface is currently a server-rendered Haskell app for exercising the market engine through a browser. It is intentionally simple: one process, in-memory state, Lucid-rendered HTML, htmx partial updates, and optional timed rounds.

The current UI is a compact trading dashboard rather than a full multiplayer game client.

## Current Shape

Implemented pieces:

- local web executable: `tlon-web`
- server-rendered pages in `Tlon.Web.View`
- in-memory game registry in `Tlon.Web.State`
- routes in `Tlon.Web.Server`
- JSON snapshots in `Tlon.Web.Api`
- lobby creation with configurable human and NPC seats
- per-player URLs
- manual submissions or timed auto-resolution
- one-player dashboard defaults: one human trader and three NPCs
- limit-order staging over listed `SeriesId` pairs
- per-offering purchase staging
- player inventory as series positions
- active offerings, listed markets, latest report, activity, and history panels
- debug-only reset/advance controls when `tlon-web` is run with `--debug`

## Development Commands

Run locally through Cabal:

```sh
nix develop --command cabal run tlon-web -- --debug --port 8080
```

or through the repo helpers:

```sh
just web 8080
just web-watch 8080 --debug
```

The server defaults to:

```text
http://127.0.0.1:8080
```

## UI Principles

The web UI should remain a thin dashboard over the current market model.

Prefer showing:

- current series positions
- active offerings
- listed market pairs
- staged player actions
- latest round report
- issued/settled lottery ticket series
- recent history

Avoid rebuilding old default-game concepts into the UI. In particular, the UI should not center redemption tables, survival stakes, elimination, or a special lottery-menu concept.

Lottery ticket series should appear like other tradable series once issued and listed. The order form should work over `SeriesId`s rather than asset-specific choices.

## Current User Flow

1. Create a game from `/`, usually with one human trader and a few NPCs.
2. Start once all human seats are filled.
3. On the player page, inspect inventory, offerings, markets, and latest activity.
4. Stage limit orders over listed series pairs.
5. Stage purchases for active offerings.
6. Submit the round.
7. Inspect fills, expirations, issuances, settlements, and history.

NPCs and the Government market maker generate additional default actor inputs each round, so report order counts include more than just human-staged orders.

## API Notes

The JSON endpoints are:

- `GET /api/games/:gameId/state`
- `GET /api/games/:gameId/report`

Generic market and holdings fields now use series vocabulary:

- `seriesId`
- `baseSeriesId`
- `quoteSeriesId`

Base instrument terms may still expose an `assetId`, because base series are backed by primitive assets.

The API is useful for inspection but should not be treated as stable yet. Reports still include lottery-specific fields and survival-shaped fields from the older report model, even though the default game no longer applies survival effects.

## Architecture Boundary

The web layer should collect inputs and render snapshots. It should not shape the core model.

The boundary should stay:

- core: pure market stepping and report construction
- default game: scenario rules, offerings, actor input generation
- web: HTTP, in-memory sessions, forms, rendering, JSON encoding

## Known Cleanup

Before treating the web surface as stable:

- continue manual dashboard review
- improve report/event names so survival-shaped fields disappear from default reports
- decide how to present NPC/government actor activity without overwhelming the single-player dashboard
- decide whether the market-rule editor belongs behind debug controls only
- decide whether old helper scripts such as `scripts/show-lottery-scenario.hs` should be kept
