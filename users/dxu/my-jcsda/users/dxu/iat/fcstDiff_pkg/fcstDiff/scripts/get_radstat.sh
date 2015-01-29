#!/bin/sh
set -x
###############################################################################
## 1. Set date 
## 2. analysis cycle (6hr gdas vs 2.5hr gfs)
## 3. Save directory
###############################################################################
adate=${1:-2009081118}
edate=${2:-2009081200}
mod=${3:-gdas}
savedir=${4:-/ptmp/wx23dc/gdas}
ndate=${ndate_dir}/ndate

if [ ! -d $savedir ]; then mkdir $savedir || exit 8 ; fi ;
cd $savedir

while [ $adate -le $edate ]; do
YYYY=`echo $adate | cut -c1-4`
MM=`echo $adate | cut -c5-6`
DD=`echo $adate | cut -c7-8`
CYC=`echo $adate | cut -c9-10`
tag=radstat

/u/wx20mi/bin/hpsstar getnostage /hpssprod/runhistory/rh${YYYY}/${YYYY}${MM}/${YYYY}${MM}${DD}/com_gfs_prod_${mod}.${YYYY}${MM}${DD}${CYC}.tar ./${mod}1.t${CYC}z.${tag}

mv ${mod}1.t${CYC}z.${tag} ${tag}.$adate
adate=`ndate +06 $adate`
done
exit
