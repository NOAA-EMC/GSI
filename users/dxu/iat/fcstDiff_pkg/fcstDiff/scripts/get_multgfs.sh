#!/bin/sh
set -x
###############################################################################
## 1. Set date 
## 2. analysis cycle (6hr gdas vs 2.5hr gfs)
## 3. Save directory
###############################################################################
mod=gfs
savedir=/ptmp/wx23dc/gfs
if [ ! -d $savedir ]; then mkdir $savedir || exit 8 ; fi ;
tag=pgrb
#list="2007092912 2007100212 2007100412 2007100612 2007111912 2007121612 2007122012 2008011212 2008030312 2008031812 2008032012 2008042512 2008051512 2008052212 2008061212 2008062512 2008081912"
list="2009010212 2009010312 2009021012 2009022012 2009022112"
cd $savedir
for adate in $list; do
YYYY=`echo $adate | cut -c1-4`
MM=`echo $adate | cut -c5-6`
DD=`echo $adate | cut -c7-8`
CYC=`echo $adate | cut -c9-10`

#/u/wx20mi/bin/hpsstar get /hpssprod/runhistory/rh${YYYY}/${YYYY}${MM}/${YYYY}${MM}${DD}/com_gfs_prod_${mod}.${YYYY}${MM}${DD}${CYC}.anl.tar ./${mod}.t${CYC}z.${tag}anl 
#mv ${mod}.t${CYC}z.${tag}anl pgbanl.$adate

/u/wx20mi/bin/hpsstar get /1year/hpssprod/runhistory/rh${YYYY}/${YYYY}${MM}/${YYYY}${MM}${DD}/com_gfs_prod_${mod}.${YYYY}${MM}${DD}${CYC}.pgrb.tar ./${mod}.t${CYC}z.${tag}f00 ./${mod}.t${CYC}z.${tag}f12 ./${mod}.t${CYC}z.${tag}f24 ./${mod}.t${CYC}z.${tag}f36 ./${mod}.t${CYC}z.${tag}f48 ./${mod}.t${CYC}z.${tag}f60 ./${mod}.t${CYC}z.${tag}f72 ./${mod}.t${CYC}z.${tag}f84 ./${mod}.t${CYC}z.${tag}f96 ./${mod}.t${CYC}z.${tag}f108 ./${mod}.t${CYC}z.${tag}f120

mv ${mod}.t${CYC}z.${tag}f00 pgbf00.$adate
mv ${mod}.t${CYC}z.${tag}f12 pgbf12.$adate
mv ${mod}.t${CYC}z.${tag}f24 pgbf24.$adate
mv ${mod}.t${CYC}z.${tag}f36 pgbf36.$adate
mv ${mod}.t${CYC}z.${tag}f48 pgbf48.$adate
mv ${mod}.t${CYC}z.${tag}f60 pgbf60.$adate
mv ${mod}.t${CYC}z.${tag}f72 pgbf72.$adate
mv ${mod}.t${CYC}z.${tag}f84 pgbf84.$adate
mv ${mod}.t${CYC}z.${tag}f96 pgbf96.$adate
mv ${mod}.t${CYC}z.${tag}f108 pgbf108.$adate
mv ${mod}.t${CYC}z.${tag}f120 pgbf120.$adate

#./${mod}.t${CYC}z.${tag}f06 ./${mod}.t${CYC}z.${tag}f12 ./${mod}.t${CYC}z.${tag}f18 ./${mod}.t${CYC}z.${tag}f24 ./${mod}.t${CYC}z.${tag}f30 ./${mod}.t${CYC}z.${tag}f36 ./${mod}.t${CYC}z.${tag}f42 ./${mod}.t${CYC}z.${tag}f48 ./${mod}.t${CYC}z.${tag}f54 ./${mod}.t${CYC}z.${tag}f60 ./${mod}.t${CYC}z.${tag}f66 ./${mod}.t${CYC}z.${tag}f72 ./${mod}.t${CYC}z.${tag}f78 ./${mod}.t${CYC}z.${tag}f84 ./${mod}.t${CYC}z.${tag}f90 ./${mod}.t${CYC}z.${tag}f96 ./${mod}.t${CYC}z.${tag}f102 ./${mod}.t${CYC}z.${tag}f108 ./${mod}.t${CYC}z.${tag}f114 ./${mod}.t${CYC}z.${tag}f120

done
