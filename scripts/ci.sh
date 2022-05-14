#!/bin/sh

set -eu

info() {
  echo "==> $1"
}

need_cmd() {
  if ! command -v "$1" >/dev/null; then
    echo "command '$1' not found, but it is needed to $2. try $3 to install"
    exit 1
  fi
}

info 'checking for required commands'

need_cmd 'git' 'know where the top level dir is' 'https://git-scm.com'
cd "$(git rev-parse --show-toplevel)"

rustup='https://rustup.rs'
need_cmd 'rustc' 'compile rust' "$rustup"
need_cmd 'cargo' 'manage rust projects' "$rustup"
need_cmd 'rustfmt' 'format rust' "$rustup"

node='https://nodejs.org/en/download'
need_cmd 'node' 'run node scripts' "$node"
need_cmd 'npm' 'install node deps' "$node"

# using cargo build here because the sometimes-recommended cargo test --no-run doesn't seem to run
# build scripts.
info 'building rust'
cargo build

info 'building vscode extension'
./scripts/mk-vscode-ext.sh

info 'checking rust formatting'
cargo fmt -- --check

info 'checking sml defn refs in statics'
./scripts/ck-sml-defn.sh

info 'running clippy'
cargo clippy

info 'running tests'
cargo test
