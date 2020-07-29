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
rm -rf inp.txt out.txt
tee inp.txt | "$PWD/$OUT/millet-ls-impl" | tee out.txt
EOF
chmod +x "$OUT/millet-ls"

cd extensions/vscode
npm run build
