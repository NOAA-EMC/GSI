#!/bin/sh
set -x
###############################################################################
## 1. Set date 
## 2. analysis cycle (6hr gdas vs 2.5hr gfs)
## 3. Save directory
###############################################################################
adate=${1:-2012121506}
mod=${2:-gdas}
edate=${3:-2012121506}
while [ $adate -le $edate ]; do
savedir=${4:-/ptmp/wx23dc/gdas/$adate}
if [ ! -d $savedir ]; then mkdir -p $savedir || exit 8 ; fi ;
cd $savedir
YYYY=`echo $adate | cut -c1-4`
MM=`echo $adate | cut -c5-6`
DD=`echo $adate | cut -c7-8`
CYC=`echo $adate | cut -c9-10`
tag=pgrb
ndate=${ndate_dir}/ndate

/u/wx20mi/bin/hpsstar getnostage /NCEPPROD/hpssprod/runhistory/rh${YYYY}/${YYYY}${MM}/${YYYY}${MM}${DD}/com_gfs_prod_${mod}.${YYYY}${MM}${DD}${CYC}.tar ./${mod}1.t${CYC}z.prepbufr 


cp ${mod}1.t${CYC}z.prepbufr prepqc.gdas.$adate
adate=`$ndate +06 $adate`
done
exit
