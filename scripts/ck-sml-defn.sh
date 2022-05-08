#!/bin/sh

set -eu

if [ "$#" -ne 0 ]; then
  echo "usage: $0"
  exit 1
fi

mk_temp_dir() {
  while true; do
    name="/tmp/$(od -A n -N 8 -t u /dev/random | tr -d ' \n')"
    if mkdir -m 700 "$name" 2>/dev/null; then
      echo "$name"
      break
    fi
  done
}

cd "$(git rev-parse --show-toplevel)"
temp="$(mk_temp_dir)"
trap "rm -rf $temp" EXIT

NUM_RULES=89
i=1
while [ "$i" -le "$NUM_RULES" ]; do
  echo "$i" >>"$temp/want"
  i=$((i + 1))
done

git grep -ho 'SML Definition ([[:digit:]]*)' crates/statics/src |
  tr -d 'A-Za-z ()' |
  sort -nu >"$temp/got"

comm -23 "$temp/want" "$temp/got" >"$temp/want_not_got"
if [ -s "$temp/want_not_got" ]; then
  echo "the following rules were not referenced:"
  cat "$temp/want_not_got"
  exit 1
fi

comm -13 "$temp/want" "$temp/got" >"$temp/got_not_want"
if [ -s "$temp/got_not_want" ]; then
  echo "the following rules were unexpectedly referenced:"
  cat "$temp/got_not_want"
  exit 1
fi

echo "all and only rules 1-$NUM_RULES were referenced"
