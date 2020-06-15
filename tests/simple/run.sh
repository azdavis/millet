cargo run -- --show-ast inp.sml >out.tmp 2>/dev/null
diff out.txt out.tmp
