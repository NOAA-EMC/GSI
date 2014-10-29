#!/bin/sh
set -x
###############################################################################
## 1. Set start date 
## 2. Save directory
## 3. End date
###############################################################################
adate=${1:-2011052600}
savedir=${2:-/ptmp/wx23dc/nosonde}
edate=${3:-2011052600}
mod=gfs
while [ $adate -le $edate ]; do
if [ ! -d $savedir ]; then mkdir $savedir || exit 8 ; fi ;
YYYY=`echo $adate | cut -c1-4`
MM=`echo $adate | cut -c5-6`
DD=`echo $adate | cut -c7-8`
CYC=`echo $adate | cut -c9-10`
tag=pgb
ndate=${ndate_dir}/ndate
gdate=`$ndate -06 `

cd $savedir
/u/wx20mi/bin/hpsstar getnostage /hpssuser/g01/wx20ef/prnosonde/$YYYY$MM$DD${CYC}gfs.tar ${tag}f00.${mod}.${YYYY}${MM}${DD}${CYC} ${tag}f06.${mod}.${YYYY}${MM}${DD}${CYC} ${tag}f12.${mod}.${YYYY}${MM}${DD}${CYC} ${tag}f18.${mod}.${YYYY}${MM}${DD}${CYC} ${tag}f24.${mod}.${YYYY}${MM}${DD}${CYC} ${tag}f30.${mod}.${YYYY}${MM}${DD}${CYC} ${tag}f36.${mod}.${YYYY}${MM}${DD}${CYC} ${tag}f42.${mod}.${YYYY}${MM}${DD}${CYC} ${tag}f48.${mod}.${YYYY}${MM}${DD}${CYC} ${tag}f54.${mod}.${YYYY}${MM}${DD}${CYC} ${tag}f60.${mod}.${YYYY}${MM}${DD}${CYC} ${tag}f66.${mod}.${YYYY}${MM}${DD}${CYC} ${tag}f72.${mod}.${YYYY}${MM}${DD}${CYC} ${tag}f78.${mod}.${YYYY}${MM}${DD}${CYC} ${tag}f84.${mod}.${YYYY}${MM}${DD}${CYC} ${tag}f90.${mod}.${YYYY}${MM}${DD}${CYC} ${tag}f96.${mod}.${YYYY}${MM}${DD}${CYC} ${tag}f102.${mod}.${YYYY}${MM}${DD}${CYC} ${tag}f108.${mod}.${YYYY}${MM}${DD}${CYC} ${tag}f114.${mod}.${YYYY}${MM}${DD}${CYC} ${tag}f120.${mod}.${YYYY}${MM}${DD}${CYC} ${tag}f126.${mod}.${YYYY}${MM}${DD}${CYC} ${tag}f132.${mod}.${YYYY}${MM}${DD}${CYC} ${tag}f138.${mod}.${YYYY}${MM}${DD}${CYC} ${tag}f144.${mod}.${YYYY}${MM}${DD}${CYC} ${tag}f150.${mod}.${YYYY}${MM}${DD}${CYC} ${tag}f156.${mod}.${YYYY}${MM}${DD}${CYC} ${tag}f162.${mod}.${YYYY}${MM}${DD}${CYC} ${tag}f168.${mod}.${YYYY}${MM}${DD}${CYC}
adate=`$ndate +24 $adate`
done
exit

