#!/bin/bash
# Expects lines of the format:
# {"C1": "None", "C2": "None", "C3": "REX", ...}
IN=json/infs.json
if [ ! -e $IN ]
then
  echo Please run json/proofs.sh to generate $IN first.
  exit 1
fi

SCRIPT='transpose | .[0][], (.[3:][] | .[0] / .[1] * 10 | round / 10)'
cat $IN |
jq -s \
' [[[.[0][]], [.[1][]]] | '"$SCRIPT"']'\
',[[[.[2][]], [.[3][]]] | '"$SCRIPT"']'\
',[[[.[4][]], [.[5][]]] | '"$SCRIPT"']' |
jq -s --raw-output '.[] | @tsv' |
sed 's/\t/ | /g'
