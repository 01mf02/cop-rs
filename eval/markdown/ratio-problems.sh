#!/bin/bash
IN=json/problems.json
if [ ! -e $IN ]
then
  echo Please run json/proofs.sh to generate $IN first.
  exit 1
fi

SCRIPT='transpose | .[0][], (.[2:][] | .[1] / .[0] * 1000 | round / 10)'
cat $IN |
jq -s \
' [[[.[0][]], [.[1][]]] | '"$SCRIPT"']'\
',[[[.[0][]], [.[2][]]] | '"$SCRIPT"']'\
',[[[.[3][]], [.[4][]]] | '"$SCRIPT"']' |
jq -s --raw-output '.[] | @tsv' |
sed 's/\t/ | /g'
