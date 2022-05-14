#!/bin/sh

need_cmd() {
  if ! command -v "$1" >/dev/null; then
    echo "need $1 to $2. try $3 to install"
    exit 1
  fi
}

need_cmd 'git' 'know where the top level dir is' 'https://git-scm.com'
cd "$(git rev-parse --show-toplevel)"

rustup='https://rustup.rs'
need_cmd 'rustc' 'compile rust' "$rustup"
need_cmd 'cargo' 'manage rust projects' "$rustup"
need_cmd 'rustfmt' 'format rust' "$rustup"

node='https://nodejs.org/en/download'
need_cmd 'node' 'run node scripts' "$node"
need_cmd 'npm' 'install node deps' "$node"

cargo build
cd extensions/vscode
if ! [ -e node_modules ]; then
  npm install
  npm build
fi
