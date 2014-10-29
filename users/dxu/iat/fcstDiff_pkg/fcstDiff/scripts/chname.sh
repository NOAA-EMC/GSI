#!/bin/sh
set -x

adate=2011053000
edate=2011061500
mod=gfs
savedir=/ptmp/wx23dc/gfs
tag=pgrb
hour="f00 f06 f12 f18 f24 f30 f36 f42 f48 f54 f60 f66 f72 f78 f84 f90 f96 f102 f108 f114 f120 f126 f132 f138 f144 f150 f156 f162 f168"

while [ $adate -le $edate ]; do
cd $savedir/$adate
for fhr in $hour; do
YYYY=`echo $adate | cut -c1-4`
MM=`echo $adate | cut -c5-6`
DD=`echo $adate | cut -c7-8`
CYC=`echo $adate | cut -c9-10`

mv ${mod}.t${CYC}z.${tag}$fhr pgb${fhr}.$adate
done
adate=`${ndate_dir}/ndate  +24 $adate`
done
