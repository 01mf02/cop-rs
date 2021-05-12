#!/bin/bash
# Create plot data showing how many problems are solved up to a certain time.
CFGS=meancop--conj--cutsr{ei,ex}
for s in $(eval echo $CFGS)
do
  for i in `grep -l "% SZS status Theorem" o/bushy/10s/$s/*.p`
  do
    jaq '.user' < $i.time
  done | sort -n | awk '{print $s " " NR}' > plot/bushy-$s
done
