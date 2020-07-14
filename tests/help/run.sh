"$MILLET" --help >out.tmp
diff ../../crates/cli/src/help.txt out.tmp
rm out.tmp
