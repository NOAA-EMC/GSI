#!/bin/sh
set -x
###############################################################################
## 1. Set date 
## 2. analysis cycle (6hr gdas vs 2.5hr gfs)
## 3. Save directory
###############################################################################
adate=${1:-2012061000}
mod=${2:-gfs}
edate=${3:-2012062000}
savedir=${4:-/ptmp/wx23dc/gfsbufr}

while [ $adate -le $edate ]; do
if [ ! -d $savedir/$adate ]; then mkdir -p $savedir/$adate || exit 8 ; fi ;
cd $savedir/$adate
YYYY=`echo $adate | cut -c1-4`
MM=`echo $adate | cut -c5-6`
DD=`echo $adate | cut -c7-8`
CYC=`echo $adate | cut -c9-10`
ndate=${ndate_dir}/ndate

/u/wx20mi/bin/hpsstar getnostage /NCEPPROD/hpssprod/runhistory/rh${YYYY}/${YYYY}${MM}/${YYYY}${MM}${DD}/com_${mod}_prod_${mod}.${YYYY}${MM}${DD}${CYC}.anl.tar ./gfs.t${CYC}z.bufrsnd.tar.gz 

gunzip gfs.t${CYC}z.bufrsnd.tar.gz
tar xvf gfs.t${CYC}z.bufrsnd.tar
rm -f gfs.t${CYC}z.bufrsnd.tar
adate=`$ndate +24 $adate`
done
exit
