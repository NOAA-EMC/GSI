#!/bin/sh
set -x
###############################################################################
## 1. Set date 
## 2. analysis cycle (6hr gdas vs 2.5hr gfs)
## 3. Save directory
###############################################################################
adate=${1:-2012100100}
expt=${2:-prgm141con}
savedir=${3:-/ptmp/wx23dc/$expt}
edate=${4:-2012100100}
if [ ! -d $savedir ]; then mkdir -p $savedir || exit 8 ; fi ;
cd $savedir
while [ $adate -le $edate ]; do
YYYY=`echo $adate | cut -c1-4`
MM=`echo $adate | cut -c5-6`
DD=`echo $adate | cut -c7-8`
CYC=`echo $adate | cut -c9-10`
tag=flx
ndate=${ndate_dir}/ndate

/u/wx20mi/bin/hpsstar getnostage /NCEPPROD/1year/hpsspara/runhistory/glopara/$expt/${YYYY}${MM}${DD}${CYC}gdas.tar pgbh00.gdas.$adate pgbh06.gdas.$adate siganl.gdas.$adate cnvstat.gdas.$adate s.$adate prepqc.gdas.$adate prepqa.gdas.$adate 

#/u/wx20mi/bin/hpsstar getnostage /NCEPPROD/1year/hpsspara/runhistory/glopara/$expt/${YYYY}${MM}${DD}${CYC}gfs.tar pgbf12.gfs.$adate
adate=`$ndate +06 $adate`
done
exit
#echo "Finished copying" | mail -s "Finished copying all $expt pgb files" dana.carlis@gmail.com

