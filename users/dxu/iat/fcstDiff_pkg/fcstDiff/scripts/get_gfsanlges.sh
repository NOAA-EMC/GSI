#!/bin/sh
set -x
###############################################################################
## 1. Set date 
## 2. analysis cycle (6hr gdas vs 2.5hr gfs)
## 3. Save directory
###############################################################################
adate=${1:-2007100500}
mod=${2:-gfs}
edate=${3:-$adate}
savedir=${4:-/ptmp/wx23dc/test}
if [ ! -d $savedir ]; then mkdir -p $savedir || exit 8 ; fi ;
cd $savedir
while [ $adate -le $edate ]; do
YYYY=`echo $adate | cut -c1-4`
MM=`echo   $adate | cut -c5-6`
DD=`echo   $adate | cut -c7-8`
CYC=`echo  $adate | cut -c9-10`
tag=pgrb
ndate=${ndate_dir}/ndate
gdate=`ndate -06 $adate`
GYYYY=`echo $gdate | cut -c1-4`
GMM=`echo   $gdate | cut -c5-6`
GDD=`echo   $gdate | cut -c7-8`
GCYC=`echo  $gdate | cut -c9-10`

/u/wx20mi/bin/hpsstar getnostage /NCEPPROD/hpssprod/runhistory/rh${YYYY}/${YYYY}${MM}/${YYYY}${MM}${DD}/com_gfs_prod_${mod}.${YYYY}${MM}${DD}${CYC}.anl.tar ./${mod}.t${CYC}z.${tag}anl 

/u/wx20mi/bin/hpsstar getnostage /NCEPPROD/hpssprod/runhistory/rh${GYYYY}/${GYYYY}${GMM}/${GYYYY}${GMM}${GDD}/com_gfs_prod_gdas.${GYYYY}${GMM}${GDD}${GCYC}.tar ./gdas1.t${GCYC}z.${tag}f06 

#./${mod}1.t${CYC}z.${tag}f00 ./${mod}1.t${CYC}z.${tag}f06 ./gdas1.t00z.bf06

mv ${mod}.t${CYC}z.${tag}anl grib.gfs.anl.$adate
mv gdas1.t${GCYC}z.${tag}f06 grib.ges.$adate

#/nwprod/util/exec/copygb -g2 -x ${mod}.t${CYC}z.${tag}anl pgbanl.${adate}
#/nwprod/util/exec/copygb -g2 -x gdas1.t${GCYC}z.${tag}f06 pgbf06.${gdate}
adate=`$ndate +06 $adate`
done
echo FINISHED
exit 
