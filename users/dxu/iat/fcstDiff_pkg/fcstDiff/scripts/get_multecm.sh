#!/bin/sh
set -x
###############################################################################
## 1. Set date 
## 2. analysis cycle (6hr gdas vs 2.5hr gfs)
## 3. Save directory
###############################################################################
adate=${1:-2008021512}
mod=${2:-ecm}
savedir=${3:-/ptmp/wx23dc/$mod}
if [ ! -d $savedir ]; then mkdir $savedir || exit 8 ; fi ;
#list="2008011100 2008011212 2008020300 2008030312 2008031800 2008031812 2008042512 2008042600 2008052200 2008062512"
#for adate in $list; do
YYYY=`echo $adate | cut -c1-4`
MM=`echo $adate | cut -c5-6`
DD=`echo $adate | cut -c7-8`
CYC=`echo $adate | cut -c9-10`
tag=pgb
ndate=${ndate_dir}/ndate
gdate=`$ndate -06 $adate`

cd $savedir

/u/wx20mi/bin/hpsstar getnostage /hpssuser/g01/globstat/ecm/${mod}_PGB${YYYY}${MM}_${CYC}.tar ${tag}anl.${adate} ${tag}f00.${adate} ${tag}f12.${adate} ${tag}f24.${adate} ${tag}f36.${adate} ${tag}f48.${adate} ${tag}f60.${adate} ${tag}f72.${adate} ${tag}f84.${adate} ${tag}f96.${adate} ${tag}f108.${adate} ${tag}f120.${adate}

###/u/wx20mi/bin/hpsstar get /1year/hpssprod/runhistory/rh${YYYY}/${YYYY}${MM}/${YYYY}${MM}${DD}/com_gfs_prod_${mod}.${YYYY}${MM}${DD}${CYC}.pgrb.tar ./${mod}.t${CYC}z.${tag}f00 ./${mod}.t${CYC}z.${tag}f06 ./${mod}.t${CYC}z.${tag}f12 ./${mod}.t${CYC}z.${tag}f18 ./${mod}.t${CYC}z.${tag}f24 ./${mod}.t${CYC}z.${tag}f30 ./${mod}.t${CYC}z.${tag}f36 ./${mod}.t${CYC}z.${tag}f42 ./${mod}.t${CYC}z.${tag}f48 ./${mod}.t${CYC}z.${tag}f54 ./${mod}.t${CYC}z.${tag}f60 ./${mod}.t${CYC}z.${tag}f66 ./${mod}.t${CYC}z.${tag}f72 ./${mod}.t${CYC}z.${tag}f78 ./${mod}.t${CYC}z.${tag}f84 ./${mod}.t${CYC}z.${tag}f90 ./${mod}.t${CYC}z.${tag}f96 ./${mod}.t${CYC}z.${tag}f102 ./${mod}.t${CYC}z.${tag}f108 ./${mod}.t${CYC}z.${tag}f114 ./${mod}.t${CYC}z.${tag}f120

#done
