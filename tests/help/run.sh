NO_COLOR=1 cargo run --quiet --bin millet -- --help >out.tmp
diff ../../crates/cli/src/help.txt out.tmp
rm out.tmp
