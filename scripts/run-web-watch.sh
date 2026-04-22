#!/usr/bin/env sh

set -eu

npm run build:agentation
cabal run tlon-web -- "$@"
