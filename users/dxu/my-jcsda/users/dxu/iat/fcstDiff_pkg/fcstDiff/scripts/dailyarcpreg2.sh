#!/bin/sh
set -x

cdate=${1:-2008080112}
hpssdir=/hpssuser/g01/wx23dc/dailyecm
#edate=${2:-2008081512}
dir=${2:-ecm011212}
basedir=/global/noscrub/wx23dc
cd ${basedir}/$dir
ndate=${ndate_dir}/ndate

#while [ $cdate -le $edate ] ; do
YYYY=`echo $cdate | cut -c1-4`
MM=`echo $cdate | cut -c5-6`
DD=`echo $cdate | cut -c7-8`
CYC=`echo $cdate | cut -c9-10`
if [ -s pgbf120.gfs.$cdate ]; then
/u/wx20mi/bin/hpsstar put ${hpssdir}/ecmarc.${cdate}.tar siganl.ecm2.$cdate pgbf*.$cdate prepqc.ecm.$cdate pgbanl.ecm2.$cdate
#cdate=`$ndate +24 $cdate`
else
exit 8
fi
exit
#done
