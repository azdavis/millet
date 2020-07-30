"$MILLET" --quiet one.sml two.sml >out.tmp
if [ -s out.tmp ]; then exit 1; fi
rm out.tmp
