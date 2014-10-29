#!/bin/sh
set -x
###############################################################################
## 1. Set date 
## 2. analysis cycle (6hr gdas vs 2.5hr gfs)
## 3. Save directory
###############################################################################
adate=${1:-2008021512}
edate=${2:-2008021512}
mod=${3:-ecm}
savedir=${4:-/ptmp/wx23dc/$mod}
if [ ! -d $savedir ]; then mkdir $savedir || exit 8 ; fi ;
tag=pgb
ndate=${ndate_dir}/ndate

cd $savedir
while [ $adate -le $edate ]; do
YYYY=`echo $adate | cut -c1-4`
MM=`echo $adate | cut -c5-6`
DD=`echo $adate | cut -c7-8`
CYC=`echo $adate | cut -c9-10`
if [ $CYC = 00 -o $CYC = 06 ] ; then
CYC=00
/u/wx20mi/bin/hpsstar get /NCEPDEV/hpssuser/g01/globstat/ecm/${mod}_PGB${YYYY}${MM}_${CYC}.tar ${tag}anl.${adate}
elif [ $CYC = 12 -o $CYC = 18 ] ; then
CYC=12
/u/wx20mi/bin/hpsstar get /NCEPDEV/hpssuser/g01/globstat/ecm/${mod}_PGB${YYYY}${MM}_${CYC}.tar ${tag}anl.${adate}
fi
adate=`$ndate 24 $adate`
done
exit
