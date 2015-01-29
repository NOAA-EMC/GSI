#!/bin/sh
set -x

mod=gfs
rundir=/global/save/wx23dc/archive/vsdb_data
name1=prgm141xnm
name2=prgm141xnm_org
name3=xnum_orig
list="anom pres sfc"

for dir1 in $list; do
cd $rundir/$dir1/00Z/$name3
adate=2012101400
edate=2012122400
while [ $adate -le $edate ]; do
YYYY=`echo $adate | cut -c1-4`
MM=`echo $adate | cut -c5-6`
DD=`echo $adate | cut -c7-8`
CYC=`echo $adate | cut -c9-10`

sed 's/PRGM141XNM/XNUM_ORIG/g' ${name1}_${YYYY}${MM}${DD}.vsdb > ${name3}_${YYYY}${MM}${DD}.vsdb
#cp ${name1}_${YYYY}${MM}${DD}.vsdb ${name3}_${YYYY}${MM}${DD}.vsdb
adate=`${ndate_dir}/ndate +24 $adate`
done
done
