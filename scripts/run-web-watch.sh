#!/usr/bin/env sh

set -eu

./scripts/build-agentation.sh
cabal run tlon-web -- "$@"
