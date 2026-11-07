#!/usr/bin/env bash

# Builds the wasm interpreter and assembles the static site into ./_site
set -euo pipefail
cd "$(dirname "$0")/.."

dune build --profile release web/main.bc.wasm.js

test -d _build/default/web/main.bc.wasm.assets \
  || { echo "missing main.bc.wasm.assets directory"; exit 1; }

rm -rf _site
mkdir -p _site
cp web/index.html web/app.js web/worker.js _site/
cp _build/default/web/main.bc.wasm.js _site/
cp -r _build/default/web/main.bc.wasm.assets _site/
chmod -R u+w _site
touch _site/.nojekyll

echo "Site ready in ./_site"
