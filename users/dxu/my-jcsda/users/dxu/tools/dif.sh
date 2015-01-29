#!/bin/bash

dir1=/scratch5/snebuda/check/modnwv/gdas
dir2=/scratch5/snebuda/check/modnwv/gfs

for f in $(find ${dir1} -maxdepth 1 -type f)
do
   echo $f
   diff ${f} ${dir2}
done

