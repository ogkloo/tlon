#!/usr/bin/env sh

set -eu

script_dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
repo_root=$(dirname -- "$script_dir")
cd "$repo_root"

public_port=8080
backend_port=8081

if [ "${1:-}" = "--port" ] && [ -n "${2:-}" ]; then
  public_port="$2"
elif [ -n "${1:-}" ]; then
  public_port="$1"
fi

"$script_dir/build-agentation.sh"

cabal run tlon-web -- --debug --port "$backend_port" &
backend_pid=$!
TLON_WEB_PUBLIC_PORT="$public_port" TLON_WEB_BACKEND_PORT="$backend_port" node scripts/agentation-proxy.mjs &
proxy_pid=$!

cleanup() {
  kill "$backend_pid" "$proxy_pid" 2>/dev/null || true
}
trap cleanup INT TERM EXIT

wait "$backend_pid" "$proxy_pid"
