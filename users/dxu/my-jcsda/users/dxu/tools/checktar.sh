#!/usr/bin/bash

# Script assumes you're running from the file directory,
# just add a line to change to it if you're not

DDIR=${1}
cd $DDIR

for gfs in `ls -1r gfs* | tail -n +2`
do
   datestr=${gfs:3:8}
   gfsstr="gfs${datestr}"
   gdasstr="gdas${datestr}"

   gfscount=`ls -1 ${gfsstr}* | wc -l`
   gdascount=`ls -1 ${gdasstr}* | wc -l`
   echo "$gfscount $gdascount"
   if [ $gfscount -ne 1 ] || [ $gdascount -ne 4 ]
   then
      continue
   fi

   # Replace this line with whatever tar/etc. commands you want to run,
   # using the $gfsstr and $gdasstr created above!
   echo "$gfsstr $gdasstr"

   DATE=${datestr}

   tar -xvf gdas${DATE}00.tar cnvstat.gdas.${DATE}00
   tar -xvf gdas${DATE}06.tar cnvstat.gdas.${DATE}06
   tar -xvf gdas${DATE}12.tar cnvstat.gdas.${DATE}12
   tar -xvf gdas${DATE}18.tar cnvstat.gdas.${DATE}18
   tar -xvf gfs${DATE}00.tar cnvstat.gfs.${DATE}00 pgbanl.gfs.${DATE}00 "pgbf*.gfs.${DATE}00"

   tar -cvf ${DATE}00-${DATE}18.tar cnvstat.* pgb*

   echo rm cnvstat.* pgb*
   echo rm gdas${DATE}00.tar gdas${DATE}06.tar gdas${DATE}12.tar gdas${DATE}18.tar gfs${DATE}00.tar
   rm cnvstat.* pgb*
   rm gdas${DATE}00.tar gdas${DATE}06.tar gdas${DATE}12.tar gdas${DATE}18.tar gfs${DATE}00.tar

done 
