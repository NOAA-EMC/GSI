#!/bin/sh
set -x
###############################################################################
## 1. Set date 
## 2. analysis cycle (6hr gdas vs 2.5hr gfs)
## 3. Save directory
###############################################################################
adate=${1:-2007103112}
mod=${2:-gdas}
edate=${3:-2007103112}
savedir=${4:-/ptmp/wx23dc/gdas}
if [ ! -d $savedir ]; then mkdir -p $savedir/$adate || exit 8 ; fi ;
while [ $adate -le $edate ]; do
cd $savedir/$adate
YYYY=`echo $adate | cut -c1-4`
MM=`echo $adate | cut -c5-6`
DD=`echo $adate | cut -c7-8`
CYC=`echo $adate | cut -c9-10`
tag=pgrb
ndate=${ndate_dir}/ndate

/u/wx20mi/bin/hpsstar getnostage /hpssprod/runhistory/rh${YYYY}/${YYYY}${MM}/${YYYY}${MM}${DD}/com_gfs_prod_${mod}.${YYYY}${MM}${DD}${CYC}.tar ./${mod}1.t${CYC}z.${tag}anl 

#./${mod}1.t${CYC}z.${tag}f00 ./${mod}1.t${CYC}z.${tag}f06 ./gdas1.t00z.bf06

cp ${mod}1.t${CYC}z.${tag}anl pgbanl.$adate
#mv ${mod}1.t${CYC}z.${tag}f00 pgbf00.$adate
#mv ${mod}1.t${CYC}z.${tag}f06 pgbf06.$adate
adate=`$ndate +06 $adate`
done
exit
