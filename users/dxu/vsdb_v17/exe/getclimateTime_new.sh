#!/bin/ksh
#set -x

if [ $# -lt 2 ] ; then
 echo "Usage : $0 model_or_experiment_name"
 exit
fi

sorc=${NWPROD:-/nwprod}/fix
gbindex=${gbindex:-${NWPROD:-/nwprod}/util/exec/grbindex}

model=`echo $1 |tr "[a-z]" "[A-Z]" `
gdtype=$2

file=g2g.ctl.$model

 i=1
 n=0
 while read LINE 
  do
   
   if [ $i -eq 3 ] ; then
     set -A tobs $LINE
     n=`echo "${tobs[0]} + 3" | bc`
   fi

   if [ $i -gt 3 ] && [ $i -le $n ] ; then
     set -A time $LINE 
     mmdd=`echo ${time[1]} | cut -c 5-8`
     if [ ! -h climat.1959$mmdd ] ; then
       cp $sorc/cmean_1d.1959$mmdd  climat.mean.1959$mmdd  
       $cpygb -B-1 -K-1  -x ${gdtype} climat.mean.1959$mmdd climat.1959$mmdd
       $gbindex climat.1959$mmdd climat.1959$mmdd.indx
     fi  
   fi

  i=`echo "$i + 1" | bc`   

  done < $file

  
  
exit
