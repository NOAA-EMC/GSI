#!/bin/sh
set -xa

HOMEDIR=/global/save/glopara/RFC/GSI_Q2FY2012
NWPROD=/nwprod
SCRIPTS=/global/save/glopara/RFC/GSI_Q2FY2012/ush
BASEDIR=$HOMEDIR
#DISK_GLOB=/global/save
sdate=2012100106
edate=2012100112
while [ $sdate -le $edate ] ; do
/global/save/glopara/svn/gfs/trunk/para/ush/horizn.sh $sdate /ptmp/wx23dc/prgm141con/prepqa.gdas.$sdate /ptmp/wx23dc/prgm141con /stmp/wx23dc/gm141con${sdate}gdasvrfy anl
sdate=`${ndate_dir}/ndate  +06 $sdate`
done
exit
