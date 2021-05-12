#!/bin/bash
SETS={TPTP-v6.3.0,bushy,chainy,miz40-deps.a15,flyspeck-top}
CFGS=meancop--conj{,--cuts{ei,ex},--cutsr{,ei,ex}}
for i in $(eval echo $CFGS)
do
  echo -n $i
  for s in $(eval echo $SETS)
  do
    echo -n " |" $(wc -l < solved/$s/10s/$i)
  done
  echo
done
