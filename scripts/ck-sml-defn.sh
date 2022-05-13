#!/bin/sh

set -eu
cd "$(git rev-parse --show-toplevel)"

NUM_RULES=89
seq 1 "$NUM_RULES" > want.tmp

RE='SML Definition \(([[:digit:]]+)\)'
git grep -hoE "$RE" crates/statics/src | sed -E "s#$RE#\1#" | sort -nu > got.tmp

diff want.tmp got.tmp

echo "all and only rules 1-$NUM_RULES were referenced"
rm want.tmp got.tmp
