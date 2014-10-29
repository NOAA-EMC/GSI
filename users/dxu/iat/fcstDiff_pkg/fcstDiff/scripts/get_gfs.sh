#!/bin/sh
set -x
###############################################################################
## 1. Set start date 
## 2. Save directory
## 3. End date
###############################################################################
adate=${1:-2011050100}
savedir=${2:-/ptmp/wx23dc/gfs}
edate=${3:-2011052100}
mod=gfs
while [ $adate -le $edate ]; do
if [ ! -d $savedir/$adate ]; then mkdir $savedir/$adate || exit 8 ; fi ;
YYYY=`echo $adate | cut -c1-4`
MM=`echo $adate | cut -c5-6`
DD=`echo $adate | cut -c7-8`
CYC=`echo $adate | cut -c9-10`
tag=pgrb
ndate=${ndate_dir}/ndate
gdate=`$ndate -06 `

cd $savedir/$adate
   /u/wx20mi/bin/hpsstar getnostage /1year/hpssprod/runhistory/rh${YYYY}/${YYYY}${MM}/${YYYY}${MM}${DD}/com_gfs_prod_${mod}.${YYYY}${MM}${DD}${CYC}.pgrb.tar ./${mod}.t${CYC}z.${tag}f00 ./${mod}.t${CYC}z.${tag}f06 ./${mod}.t${CYC}z.${tag}f12 ./${mod}.t${CYC}z.${tag}f18 ./${mod}.t${CYC}z.${tag}f24 ./${mod}.t${CYC}z.${tag}f30 ./${mod}.t${CYC}z.${tag}f36 ./${mod}.t${CYC}z.${tag}f42 ./${mod}.t${CYC}z.${tag}f48 ./${mod}.t${CYC}z.${tag}f54 ./${mod}.t${CYC}z.${tag}f60 ./${mod}.t${CYC}z.${tag}f66 ./${mod}.t${CYC}z.${tag}f72 ./${mod}.t${CYC}z.${tag}f78 ./${mod}.t${CYC}z.${tag}f84 ./${mod}.t${CYC}z.${tag}f90 ./${mod}.t${CYC}z.${tag}f96 ./${mod}.t${CYC}z.${tag}f102 ./${mod}.t${CYC}z.${tag}f108 ./${mod}.t${CYC}z.${tag}f114 ./${mod}.t${CYC}z.${tag}f120 ./${mod}.t${CYC}z.${tag}f126 ./${mod}.t${CYC}z.${tag}f132 ./${mod}.t${CYC}z.${tag}f138 ./${mod}.t${CYC}z.${tag}f144 ./${mod}.t${CYC}z.${tag}f150 ./${mod}.t${CYC}z.${tag}f156 ./${mod}.t${CYC}z.${tag}f162 ./${mod}.t${CYC}z.${tag}f168    
adate=`$ndate +24 $adate`
done
exit

