#!/bin/sh

set -eu

if [ "$#" -ne 0 ]; then
  echo "usage: $0"
  exit 1
fi

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
