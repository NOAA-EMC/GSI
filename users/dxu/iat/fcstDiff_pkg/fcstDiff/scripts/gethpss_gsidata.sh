#!/bin/sh
set -x
###############################################################################
## 1. Set date of experiment
## 2. analysis cycle (6hr gdas vs 2.5hr gfs)
## 3. Save directory         
###############################################################################
adate=${1:-2011080100}
edate=${2:-2011083100}
mod=${3:-gfs}

while [ $adate -le $edate ]; do
savedir=/ptmp/wx23dc/${mod}/$adate
mkdir -p $savedir
cd $savedir
rm -f $savedir/*
YYYY=`echo $adate | cut -c1-4`
MM=`echo $adate | cut -c5-6`
DD=`echo $adate | cut -c7-8`
CYC=`echo $adate | cut -c9-10`

ndate=${ndate_dir}/ndate
gdate=`$ndate -06 $adate`
GYYYY=`echo $gdate | cut -c1-4`
GMM=`echo $gdate | cut -c5-6`
GDD=`echo $gdate | cut -c7-8`
GCYC=`echo $gdate | cut -c9-10`

if [ "$mod" = "gdas" ]; then
/global/save/wx23dc/scripts/hpsstar getnostage /hpssprod/runhistory/rh${GYYYY}/${GYYYY}${GMM}/${GYYYY}${GMM}${GDD}/com_gfs_prod_${mod}.${GYYYY}${GMM}${GDD}${GCYC}.tar ./gdas1.t${GCYC}z.abias ./gdas1.t${GCYC}z.satang ./gdas1.t${GCYC}z.bf03 ./gdas1.t${GCYC}z.bf06 ./gdas1.t${GCYC}z.bf09 

/global/save/wx23dc/scripts/hpsstar getnostage /hpssprod/runhistory/rh${YYYY}/${YYYY}${MM}/${YYYY}${MM}${DD}/com_gfs_prod_${mod}.${YYYY}${MM}${DD}${CYC}.tar ./gdas1.t${CYC}z.sgm3prep ./gdas1.t${CYC}z.sgesprep ./gdas1.t${CYC}z.sgp3prep ./gdas1.t${CYC}z.prepbufr

cp /global/shared/dump/${adate}/${mod}/*.${adate} $savedir
touch *
elif [ "$mod" = "gfs" ]; then
/global/save/wx23dc/scripts/hpsstar getnostage /NCEPPROD/hpssprod/runhistory/rh${GYYYY}/${GYYYY}${GMM}/${GYYYY}${GMM}${GDD}/com_gfs_prod_gdas.${GYYYY}${GMM}${GDD}${GCYC}.tar ./gdas1.t${GCYC}z.abias ./gdas1.t${GCYC}z.satang ./gdas1.t${GCYC}z.bf03 ./gdas1.t${GCYC}z.bf06 ./gdas1.t${GCYC}z.bf09 
#mv gdas1.t${GCYC}z.abias gfs.t${GCYC}z.abias
#mv gdas1.t${GCYC}z.satang gfs.t${GCYC}z.satang
#mv gdas1.t${GCYC}z.bf03 gfs.t${GCYC}z.bf03
#mv gdas1.t${GCYC}z.bf06 gfs.t${GCYC}z.bf06
#mv gdas1.t${GCYC}z.bf09 gfs.t${GCYC}z.bf09

/global/save/wx23dc/scripts/hpsstar getnostage /NCEPPROD/hpssprod/runhistory/rh${YYYY}/${YYYY}${MM}/${YYYY}${MM}${DD}/com_gfs_prod_${mod}.${YYYY}${MM}${DD}${CYC}.anl.tar ./gfs.t${CYC}z.sgm3prep ./gfs.t${CYC}z.sgesprep ./gfs.t${CYC}z.sgp3prep ./gfs.t${CYC}z.prepbufr

cp /global/shared/dump/${adate}/${mod}/*.${adate} $savedir
touch *
fi
adate=`$ndate +24 $adate`
done
exit 0
