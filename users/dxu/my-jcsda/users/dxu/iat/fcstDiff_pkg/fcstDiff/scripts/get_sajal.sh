#!/bin/sh
set -x
###############################################################################
## 1. Set date 
## 2. analysis cycle (6hr gdas vs 2.5hr gfs)
## 3. Save directory
###############################################################################
adate=${1:-2011071000}
expt=${2:-prslg1}
savedir=${3:-/ptmp/$USER/$expt}
edate=${4:-2011071000}
if [ ! -d $savedir ]; then mkdir -p $savedir || exit 8 ; fi ;
cd $savedir

while [ $adate -le $edate ]; do
YYYY=`echo $adate | cut -c1-4`
MM=`echo $adate | cut -c5-6`
DD=`echo $adate | cut -c7-8`
CYC=`echo $adate | cut -c9-10`
tag=pgb
ndate=${ndate_dir}/ndate


/u/wx20mi/bin/hpsstar getnostage /NCEPPROD/1year/hpsspara/runhistory/glopara/$expt/${YYYY}${MM}${DD}${CYC}gfs.tar ${tag}f120.gfs.$adate

adate=`$ndate +24 $adate`
done

