#!/bin/sh
#set -x
rundir=/stmp/wx23dc/printscores
ndate=${ndate_dir}/ndate
datadir=/global/shared/stat/vrfy
if [ ! -d $rundir ]; then mkdir $rundir || exit 8 ; fi ;
cd $rundir;rm -f dummy *.txt
verdate=2007010612
enddate=2008010512
hr=120
lev=500
anldate=`$ndate -$hr $verdate`
mn=`echo $anldate |cut -c5-6`
dy=`echo $anldate |cut -c7-8`
yr=`echo $anldate |cut -c1-4`
cyc=`echo $anldate |cut -c9-10`

while [ $verdate -le $enddate ]; do
grep -n "120.*500mb" ${datadir}/SCORESs.$verdate > dummy
grep -n "46.*500mb" dummy |head -1 >> nhem$yr$cyc.txt
grep -n "122.*500mb" dummy |head -1 >> shem$yr$cyc.txt
verdate=`$ndate +24 $verdate`
done
