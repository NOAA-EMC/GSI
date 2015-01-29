#!/bin/bash

filepath=/scratch5/$1/para/$2/
YMDH=$3

echo $filepath
echo $YMDH

for f in $(ls ${filepath}*${YMDH}*dayf*)
do
   echo Checking ${f}
   grep -i 'time limit' $f
   grep -i unexpected $f
   grep -i segementation $f
   grep -i ' nan ' $f
done


