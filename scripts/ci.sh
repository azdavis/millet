#!/bin/sh

set -eu

cd "$(git rev-parse --show-toplevel)"

info() {
  echo "==> $1"
}

info 'build'
cargo test --no-run

info 'check formatting'
cargo fmt -- --check

info 'check sml defn refs'
./scripts/ck-sml-defn.sh

info 'run clippy'
cargo clippy

info 'run cargo tests'
cargo test

info 'run tests in tests/'
./scripts/run-test.sh tests/*
