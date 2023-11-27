#!/bin/bash
# Expects lines of the format:
# {"C1": "None", "C2": "REX", ...}
IN=json/avg-ratios.json
if [ ! -e $IN ]
then
  echo Please run json/proofs.sh to generate $IN first.
  exit 1
fi

jq -s --raw-output '.[] | [[.[]] | .[:2][], (.[2:][] * 10 | round / 10)] | @tsv' $IN |
sed 's/\t/ | /g'
