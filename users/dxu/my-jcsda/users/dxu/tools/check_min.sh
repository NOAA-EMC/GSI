#!/bin/bash

for f in $(ls /scratch5/snebuda/par/$@/*gdasanal.dayfile)
do
   substr=gdasanal.dayfile
   file1=${f##*/}
   file2=${file1%%$substr}
   output=$(grep 'Minimization iteration' ${f} | tail -1)
   echo ${file2}  ${output} 
done


