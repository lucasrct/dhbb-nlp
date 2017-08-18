for f in ../../dhbb/text/*.text; do
    BASE=$(basename $f .text)
    awk 'BEGIN { text=0; } $0 ~ /^#/ {next} text>1 {print} /^---$/ { text = text + 1; }' $f > $BASE.raw ;
done
