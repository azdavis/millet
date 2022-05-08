#!/bin/sh

set -eu

usage() {
cat <<EOF
usage:
  $0 [options] <file>...

options:
  -h
    show this help
  -q
    be quiet, do not mention individual passing tests
  -g
    generate new expected output from resulting output
EOF
exit 1
}

generate=false
quiet=false
while getopts 'hgq' opt; do
  case "$opt" in
  (g) generate=true ;;
  (q) quiet=true ;;
  (*) usage ;;
  esac
done
shift "$((OPTIND - 1))"

if [ "$#" -eq 0 ]; then
  usage
fi

cd "$(git rev-parse --show-toplevel)"
cargo build --bin cli
export NO_COLOR=1
export MILLET="$PWD/target/debug/cli"

old_pwd="$PWD"
passed=0
failed=0
skipped=0
generated=0
for x in "$@"; do
  cd "$x"
  if [ -f skip ]; then
    if [ -s skip ]; then
      echo "$x: expected skip empty, got non-empty"
      failed=$((failed + 1))
    else
      if ! "$quiet"; then
        echo "$x: skipped"
      fi
      skipped=$((skipped + 1))
    fi
  elif [ -f run.sh ]; then
    if sh -eu run.sh; then
      if ! "$quiet"; then
        echo "$x: ok"
      fi
      passed=$((passed + 1))
    else
      echo "$x: expected run.sh to exit zero, got non-zero"
      failed=$((failed + 1))
    fi
  elif [ -f ast.sml ]; then
    if ! "$MILLET" --just-ast ast.sml >out.tmp; then
      echo "$x: expected success, got failure"
      failed=$((failed + 1))
    elif "$generate"; then
      mv out.tmp out.txt
      if ! "$quiet"; then
        echo "$x: generated"
      fi
      generated=$((generated + 1))
    elif diff out.txt out.tmp; then
      rm out.tmp
      if ! "$quiet"; then
        echo "$x: ok"
      fi
      passed=$((passed + 1))
    else
      echo "$x: expected lhs, got rhs"
      failed=$((failed + 1))
    fi
  elif [ -f ok.sml ]; then
    if ! "$MILLET" --quiet ok.sml >out.tmp; then
      echo "$x: expected success, got failure"
      failed=$((failed + 1))
    elif [ -s out.tmp ]; then
      echo "$x: expected no output, got output"
      failed=$((failed + 1))
    else
      rm out.tmp
      if ! "$quiet"; then
        echo "$x: ok"
      fi
      passed=$((passed + 1))
    fi
  elif [ -f err.sml ]; then
    set +e
    "$MILLET" err.sml >out.tmp
    ec="$?"
    set -e
    if [ "$ec" -ne 1 ]; then
      echo "$x: expected exit 1, got exit $ec"
      failed=$((failed + 1))
    elif "$generate"; then
      mv out.tmp out.txt
      if ! "$quiet"; then
        echo "$x: generated"
      fi
      generated=$((generated + 1))
    elif diff out.txt out.tmp; then
      rm out.tmp
      if ! "$quiet"; then
        echo "$x: ok"
      fi
      passed=$((passed + 1))
    else
      echo "$x: expected lhs, got rhs"
      failed=$((failed + 1))
    fi
  else
    echo "$x: don't know how to run"
    failed=$((failed + 1))
  fi
  cd "$old_pwd"
done

echo "test result: $passed passed, $failed failed, $skipped skipped, $generated generated"
if [ "$failed" -ne 0 ]; then
  exit 1
fi
