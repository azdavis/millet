cargo run --quiet -- --show-ast inp.sml >out.tmp
diff out.txt out.tmp
