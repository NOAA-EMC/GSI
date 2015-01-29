#!/bin/sh
set -x
###############################################################################
## 1. Set date 
## 2. analysis cycle (6hr gdas vs 2.5hr gfs)
## 3. Save directory
###############################################################################
adate=${1:-2013101206}
mod=${2:-gdas}
edate=${3:-2013101206}
while [ $adate -le $edate ]; do
savedir=${4:-/ptmp/$USER/gdas/$adate}
if [ ! -d $savedir ]; then mkdir -p $savedir || exit 8 ; fi ;
cd $savedir
YYYY=`echo $adate | cut -c1-4`
MM=`echo $adate | cut -c5-6`
DD=`echo $adate | cut -c7-8`
CYC=`echo $adate | cut -c9-10`
tag=pgrb
ndate=${ndate_dir}/ndate

/nwprod/util/ush/hpsstar getnostage /NCEPPROD/hpssprod/runhistory/rh${YYYY}/${YYYY}${MM}/${YYYY}${MM}${DD}/com_gfs_prod_${mod}.${YYYY}${MM}${DD}${CYC}.tar ./${mod}1.t${CYC}z.sf00 ./${mod}1.t${CYC}z.sf03 ./${mod}1.t${CYC}z.sf06 ./${mod}1.t${CYC}z.sf09 ./${mod}1.t${CYC}z.bf00 ./${mod}1.t${CYC}z.bf03 ./${mod}1.t${CYC}z.bf06 ./${mod}1.t${CYC}z.bf09 ./${mod}1.t${CYC}z.sanl ./${mod}1.t${CYC}z.sfcanl

cp ${mod}1.t${CYC}z.sanl siganl.gdas.$adate
cp ${mod}1.t${CYC}z.sfcanl sfcanl.gdas.$adate
cp ${mod}1.t${CYC}z.sf00 sigf00.gdas.$adate
cp ${mod}1.t${CYC}z.sf03 sigf03.gdas.$adate
cp ${mod}1.t${CYC}z.sf06 sigf06.gdas.$adate
cp ${mod}1.t${CYC}z.sf09 sigf09.gdas.$adate
adate=`$ndate +06 $adate`
done
exit
