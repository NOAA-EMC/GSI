#!/bin/sh
set -x
###############################################################################
## 1. Set date 
## 2. analysis cycle (6hr gdas vs 2.5hr gfs)
## 3. Save directory
###############################################################################
adate=${1:-2012060900}
mod=${2:-nam}
edate=${3:-2012062000}
savedir=${4:-/ptmp/wx23dc/nambufr}

while [ $adate -le $edate ]; do
if [ ! -d $savedir ]; then mkdir -p $savedir/$adate || exit 8 ; fi ;
cd $savedir/$adate
YYYY=`echo $adate | cut -c1-4`
MM=`echo $adate | cut -c5-6`
DD=`echo $adate | cut -c7-8`
CYC=`echo $adate | cut -c9-10`
ndate=${ndate_dir}/ndate

/u/wx20mi/bin/hpsstar getnostage /NCEPPROD/hpssprod/runhistory/rh${YYYY}/${YYYY}${MM}/${YYYY}${MM}${DD}/com_nam_prod_${mod}.${YYYY}${MM}${DD}${CYC}.bufr.tar  

adate=`$ndate +24 $adate`
done
exit
