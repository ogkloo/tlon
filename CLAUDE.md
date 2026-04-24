# Agent instructions

In dev mode, restarting the web server or rebuilding the browser app means using the local Agentation proxy workflow, not running `tlon-web` directly on the public port.

Run the dev server with:

```sh
nix develop -c ./scripts/run-web.sh --port 8080
```

or, from inside `nix develop`:

```sh
just web 8080
```

For watch mode, use:

```sh
just web-watch 8080
```

This workflow builds `static/agentation.bundle.js`, starts the Haskell `tlon-web` backend on port 8081, and starts `scripts/agentation-proxy.mjs` on the requested public port, usually 8080. Open the public proxy URL, not the backend URL.

The proxy serves `/static/agentation.bundle.js`, forwards other requests to the backend, and injects the Agentation mount into HTML responses. Keep this as a local development layer. Do not add Agentation mounts, bundle routes, npm dependencies, or debug-only Agentation behavior to tracked Haskell application code just to make dev mode work.

If port 8080 is already in use, stop the existing local web/proxy process before restarting. If you need to verify the setup, check that port 8080 is a `node scripts/agentation-proxy.mjs` listener, port 8081 is `tlon-web`, and `curl http://127.0.0.1:8080/` contains `agentation-root`.

Use direct `cabal run tlon-web -- --port PORT` only when intentionally testing the plain packaged app without Agentation.
