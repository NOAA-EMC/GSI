#!/bin/sh
set -x
###############################################################################
## 1. Set date 
## 2. analysis cycle (6hr gdas vs 2.5hr gfs)
## 3. Save directory
###############################################################################
adate=${1:-2012021000}
expt=${2:-slg4}
savedir=${3:-/ptmp/wx23dc/pr$expt}
edate=${4:-2012021000}
if [ ! -d $savedir ]; then mkdir -p $savedir || exit 8 ; fi ;
cd $savedir
while [ $adate -le $edate ]; do
YYYY=`echo $adate | cut -c1-4`
MM=`echo $adate | cut -c5-6`
DD=`echo $adate | cut -c7-8`
CYC=`echo $adate | cut -c9-10`
ndate=${ndate_dir}/ndate

#/u/wx20mi/bin/hpsstar getnostage /NCEPPROD/1year/hpsspara/runhistory/glopara/pr$expt/${YYYY}${MM}${DD}${CYC}gdas.tar ${expt}${adate}gdasanal.dayfile ${expt}${adate}gdasfcst1.dayfile 
/u/wx20mi/bin/hpsstar getnostage /NCEPPROD/1year/hpsspara/runhistory/glopara/pr$expt/${YYYY}${MM}${DD}${CYC}gfs.tar ${expt}${adate}gfsfcst1.dayfile 

adate=`$ndate +06 $adate`
done
#echo "Finished copying" | mail -s "Finished copying all $expt pgb files" dana.carlis@gmail.com

