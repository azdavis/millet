#!/bin/sh

set -eu
cd "$(git rev-parse --show-toplevel)"

vscode='extensions/vscode'
out="$vscode/out"

mkdir -p "$out"
rm -rf "$out/lang-srv"

cargo build
cp target/debug/old-lang-srv "$out/lang-srv"

cd "$vscode"
if ! [ -e node_modules ]; then
  npm install
fi
npm run build
