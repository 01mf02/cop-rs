#!/bin/bash

SETS={TPTP-v6.3.0,bushy,chainy,miz40-deps.a15,flyspeck-top}

echo -n "Any but (R)EX"
for i in `eval echo $SETS`; do echo -n " |" $(ls solved/$i/10s/* | grep -v "cutsr\?ex" | xargs cat | sort -n | uniq | wc -l); done
echo

echo -n "REX and REI"
for i in `eval echo $SETS`; do echo -n " |" $(cat solved/$i/10s/*--cutsre? | sort -n | uniq | wc -l); done
echo

echo -n "Any"
for i in `eval echo $SETS`; do echo -n " |" $(cat solved/$i/10s/* | sort -n | uniq | wc -l); done
echo
