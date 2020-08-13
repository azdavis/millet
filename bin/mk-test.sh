#!/bin/sh

set -eu

usage() {
cat <<EOF
usage:
  $0 [options] <file>...

options:
  -h
    show this help
  -e
    open the new test files in EDITOR
EOF
exit 1
}

edit=false
while getopts 'he' opt; do
  case "$opt" in
  (e) edit=true ;;
  (*) usage ;;
  esac
done
shift "$((OPTIND - 1))"

if [ "$#" -eq 0 ]; then
  usage
fi

for x in "$@"; do
  mkdir -p "$x"
  touch "$x/ok.sml"
  if "$edit"; then
    "$EDITOR" "$x/ok.sml"
  fi
done
