#!/usr/bin/env sh

set -eu

if [ ! -d node_modules/agentation ] || [ ! -d node_modules/esbuild ] || [ ! -d node_modules/react ] || [ ! -d node_modules/react-dom ]; then
  echo "Missing Agentation dependencies. Run: npm install" >&2
  exit 1
fi

npx esbuild \
  client/agentation-entry.jsx \
  --bundle \
  --format=iife \
  --platform=browser \
  --outfile=static/agentation.bundle.js
