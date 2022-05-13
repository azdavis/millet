#!/bin/sh

set -eu
cd "$(git rev-parse --show-toplevel)"

OUT="extensions/vscode/out"

cargo build
mkdir -p "$OUT"
rm -rf "$OUT/lang-srv"
cp target/debug/lang-srv "$OUT/lang-srv"

rm -rf "$OUT/lang-srv"
cat <<EOF > "$OUT/lang-srv"
#!/bin/sh
rm -rf inp.log out.log
tee inp.log | "$PWD/$OUT/lang-srv" | tee out.log
EOF
chmod +x "$OUT/lang-srv"

cd extensions/vscode
if ! [ -e node_modules ]; then
  npm install
fi
npm run build
