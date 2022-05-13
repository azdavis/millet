#!/bin/sh

set -eu
cd "$(git rev-parse --show-toplevel)"

info() {
  echo "==> $1"
}

# using cargo build here because the sometimes-recommended cargo test --no-run doesn't seem to run
# build scripts.
info 'build'
cargo build

info 'check formatting'
cargo fmt -- --check

info 'check sml defn refs'
./scripts/ck-sml-defn.sh

info 'run clippy'
cargo clippy

info 'run tests'
cargo test
