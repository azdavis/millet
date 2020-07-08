! NO_COLOR=1 cargo run --quiet --bin millet -- inp.sml >out.tmp
diff out.txt out.tmp
rm out.tmp
