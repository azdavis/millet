#!/bin/sh

set -eu
cd "$(git rev-parse --show-toplevel)"

seq 1 89 > want.tmp

RE='SML Definition \(([[:digit:]]+)\)'
git grep -hoE "$RE" crates/old-statics/src | sed -E "s#$RE#\1#" | sort -nu > got.tmp

diff want.tmp got.tmp
rm want.tmp got.tmp
