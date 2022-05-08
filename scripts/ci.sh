#!/bin/sh

set -eux

cd "$(git rev-parse --show-toplevel)"
cargo test --no-run
cargo fmt -- --check
./scripts/ck-sml-defn.sh
cargo clippy
cargo test
./scripts/run-test.sh tests/*
