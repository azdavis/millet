#!/bin/sh

set -eu

if [ "$#" -ne 0 ]; then
  echo "usage: $0"
  exit 1
fi

OUT="extensions/vscode/out"

cargo build
mkdir -p "$OUT"
rm -rf "$OUT/millet-ls-impl"
cp target/debug/millet-ls "$OUT/millet-ls-impl"

rm -rf "$OUT/millet-ls"
cat <<EOF > "$OUT/millet-ls"
#!/bin/sh
rm -rf inp.log out.log
tee inp.log | "$PWD/$OUT/millet-ls-impl" | tee out.log
EOF
chmod +x "$OUT/millet-ls"

cd extensions/vscode
if ! [ -e node_modules ]; then
  npm install
fi
npm run build
