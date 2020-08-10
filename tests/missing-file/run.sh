if "$MILLET" does-not-exist.sml >out.tmp; then exit 1; fi
diff out.txt out.tmp
rm out.tmp
