#!/bin/sh
set -x
###############################################################################
## 1. Set date 
## 3. Save directory
###############################################################################
adate=${1:-2013040600}
savedir=${2:-/ptmp/wx23dc/cfsv2}
if [ ! -d $savedir ]; then mkdir -p $savedir || exit 8 ; fi ;

ndate=${ndate_dir}/ndate

tdate=`$ndate -192 $adate`

while [ $tdate -le $adate ]; do
cd $savedir
YYYY=`echo $tdate | cut -c1-4`
MM=`echo $tdate | cut -c5-6`
DD=`echo $tdate | cut -c7-8`
CYC=`echo $tdate | cut -c9-10`

ftype=pgbf
/u/wx20mi/bin/hpsstar getnostage /NCEPPROD/hpssprod/runhistory/cfs${YYYY}/${YYYY}${MM}/${YYYY}${MM}${DD}/monthly/cfs.$ftype.${tdate}.m01.monthly.tar 
tdate=`$ndate 24 $tdate`
done
exit
