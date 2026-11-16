#!/usr/bin/env bash

set -euo pipefail
cd "$(dirname "$0")/.."

dune build --profile release web/main.bc.wasm.js

test -d _build/default/web/main.bc.wasm.assets \
  || { echo "missing main.bc.wasm.assets directory"; exit 1; }

rm -rf _site
mkdir -p _site
cp web/index.html web/app.js web/worker.js _site/
cp -r web/vendor _site/

mkdir -p _site/samples
export LC_ALL=C
shopt -s nullglob
samples=(sample/*.mao)
for f in "${samples[@]}"; do cp "$f" _site/samples/; done
{
  printf '['
  sep=''
  for f in "${samples[@]}"; do
    name=$(basename "$f")
    name=${name//\\/\\\\}
    name=${name//\"/\\\"}
    printf '%s"%s"' "$sep" "$name"
    sep=','
  done
  printf ']\n'
} > _site/samples/index.json

cp _build/default/web/main.bc.wasm.js _site/
cp -r _build/default/web/main.bc.wasm.assets _site/
chmod -R u+w _site
touch _site/.nojekyll

echo "Site ready in ./_site"
