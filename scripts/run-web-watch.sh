#!/usr/bin/env sh

set -eu

cabal run tlon-web -- "$@"
