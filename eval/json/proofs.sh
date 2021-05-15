#!/bin/bash
# enable recursive globs, such as **/*.stats
shopt -s globstar

SETS={TPTP-v6.3.0,bushy,chainy,miz40-deps.a15,flyspeck-top}
OUTS=$(eval echo o/$SETS/10s)
#SETS=bushy
#OUTS=$(eval echo o/$SETS/1s)

COMP=meancop--conj
REX=meancop--conj--cutsrex
REI=meancop--conj--cutsrei

INFS='[.[].infs | add] | add'

# declare associative arrays
declare -A COMPREX COMPREI REXREI
for s in $OUTS; do
  echo Obtaining data for $s ...
  # problems which COMP solves by giving a REX proof
  COMPREX[$s]=$(cd $s/$COMP && for i in **/*.p; do cmp --quiet $i.o ../$REX/$i.o && echo $i.stats; done)
  # problems which COMP solves by giving a REI proof
  COMPREI[$s]=$(cd $s/$COMP && for i in **/*.p; do cmp --quiet $i.o ../$REI/$i.o && echo $i.stats; done)
  # problems which REX solves by giving a REI proof
  REXREI[$s]=$(cd $s/$REX && for i in **/*.p; do cmp --quiet $i.o ../$REI/$i.o && echo $i.stats; done)
done

# Problems solved by C2 that are identically solved by C1.
problems() {

echo -n '{"C1": "None", "C2": "None"'
for s in $OUTS; do echo -n , \"$s\": $(ls $s/$COMP/**/*.stats | wc -l); done
echo "}"

echo -n '{"C1": "REX", "C2": "None"'
for s in $OUTS; do echo -n , \"$s\": $(echo ${COMPREX[$s]} | wc -w); done
echo "}"

echo -n '{"C1": "REI", "C2": "None"'
for s in $OUTS; do echo -n , \"$s\": $(echo ${COMPREI[$s]} | wc -w); done
echo "}"

echo -n '{"C1": "REX", "C2": "REX"'
for s in $OUTS; do echo -n , \"$s\": $(ls $s/$REX/**/*.stats | wc -l); done
echo "}"

echo -n '{"C1": "REI", "C2": "REX"'
for s in $OUTS; do echo -n , \"$s\": $(echo ${REXREI[$s]} | wc -w); done
echo "}"

}

# Inferences taken by C1 for problems solved with identical proofs by C2 and C3.
inferences() {

echo -n '{"C1": "None", "C2": "None", "C3": "REX"'
for s in $OUTS; do echo -n , \"$s\": $(cd $s/$COMP && cat ${COMPREX[$s]} | jaq -s "$INFS"); done
echo "}"

echo -n '{"C1": "REX", "C2": "None", "C3": "REX"'
for s in $OUTS; do echo -n , \"$s\": $(cd $s/$REX  && cat ${COMPREX[$s]} | jaq -s "$INFS"); done
echo "}"

echo -n '{"C1": "None", "C2": "None", "C3": "REI"'
for s in $OUTS; do echo -n , \"$s\": $(cd $s/$COMP && cat ${COMPREI[$s]} | jaq -s "$INFS"); done
echo "}"

echo -n '{"C1": "REI", "C2": "None", "C3": "REI"'
for s in $OUTS; do echo -n , \"$s\": $(cd $s/$REI  && cat ${COMPREI[$s]} | jaq -s "$INFS"); done
echo "}"

echo -n '{"C1": "REX", "C2": "REX", "C3": "REI"'
for s in $OUTS; do echo -n , \"$s\": $(cd $s/$REX  && cat ${REXREI[$s]}  | jaq -s "$INFS"); done
echo "}"

echo -n '{"C1": "REI", "C2": "REX", "C3": "REI"'
for s in $OUTS; do echo -n , \"$s\": $(cd $s/$REI  && cat ${REXREI[$s]}  | jaq -s "$INFS"); done
echo "}"

}

problems > json/problems.json
inferences > json/infs.json
