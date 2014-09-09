#!/bin/sh
set -xa
n=0

while read inum dummy
do
n=`expr $n + 1`

 echo " $n    $dummy " >> retdata.txt
  
done < tdata.txt
