#!/bin/sh
set -x
###############################################################################
## 1. Set date 
## 2. analysis cycle (6hr gdas vs 2.5hr gfs)
## 3. Save directory
###############################################################################
adate=${1:-2011071000}
mod=${2:-gfs}
savedir=${3:-/ptmp/wx23dc/pra}
edate=${4:-2011071000}
if [ ! -d $savedir ]; then mkdir $savedir || exit 8 ; fi ;

cd $savedir
while [ $adate -le $edate ]; do

YYYY=`echo $adate | cut -c1-4`
MM=`echo $adate | cut -c5-6`
DD=`echo $adate | cut -c7-8`
CYC=`echo $adate | cut -c9-10`
tag=pgb
ndate=${ndate_dir}/ndate
gdate=`$ndate -06 $adate`

#/u/wx20mi/bin/hpsstar get /hpssuser/g01/globstat/pra/pra_pgb${YYYY}${MM}_${CYC}c.tar ${tag}f00.$adate ${tag}f24.$adate ${tag}f48.$adate ${tag}f72.$adate ${tag}f96.$adate ${tag}f120.$adate

/u/wx20mi/bin/hpsstar getnostage /NCEPPROD/1year/hpssprod/runhistory/rh${YYYY}/save/com_gfs_prod_${mod}.${YYYY}${MM}${DD}${CYC}.pgrb.tar ./gfs.t${CYC}z.pgrbf120  

mv gfs.t${CYC}z.pgrbf120 pgbf120.gfs.$adate
adate=`$ndate +12 $adate`
done


