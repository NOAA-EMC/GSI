#!/bin/sh
set -x
###############################################################################
## 1. Set date of experiment
## 2. analysis cycle (6hr gdas vs 2.5hr gfs)
## 3. Save directory         
###############################################################################
adate=${1:-2010031600}
edate=${2:-2010031612}
mod=${3:-gdas}

while [ $adate -le $edate ]; do
savedir=/ptmp/wx23dc/${mod}/$adate
mkdir -p $savedir
cd $savedir
rm -f *
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
  if [ $CYC != "00" ]; then
     cp /com/gfs/prod/${mod}.${YYYY}${MM}${DD}/gdas1.t${GCYC}z.abias .
     cp /com/gfs/prod/${mod}.${YYYY}${MM}${DD}/gdas1.t${GCYC}z.satang .
     cp /com/gfs/prod/${mod}.${YYYY}${MM}${DD}/gdas1.t${GCYC}z.bf03 .
     cp /com/gfs/prod/${mod}.${YYYY}${MM}${DD}/gdas1.t${GCYC}z.bf06 .
     cp /com/gfs/prod/${mod}.${YYYY}${MM}${DD}/gdas1.t${GCYC}z.bf09 .
     cp /com/gfs/prod/${mod}.${YYYY}${MM}${DD}/gdas1.t${CYC}z.sgm3prep .
     cp /com/gfs/prod/${mod}.${YYYY}${MM}${DD}/gdas1.t${CYC}z.sgesprep .
     cp /com/gfs/prod/${mod}.${YYYY}${MM}${DD}/gdas1.t${CYC}z.sgp3prep .
     cp /com/gfs/prod/${mod}.${YYYY}${MM}${DD}/gdas1.t${CYC}z.prepbufr .
  else
     cp /com/gfs/prod/${mod}.${GYYYY}${GMM}${GDD}/gdas1.t${GCYC}z.abias .
     cp /com/gfs/prod/${mod}.${GYYYY}${GMM}${GDD}/gdas1.t${GCYC}z.satang .
     cp /com/gfs/prod/${mod}.${GYYYY}${GMM}${GDD}/gdas1.t${GCYC}z.bf03 .
     cp /com/gfs/prod/${mod}.${GYYYY}${GMM}${GDD}/gdas1.t${GCYC}z.bf06 .
     cp /com/gfs/prod/${mod}.${GYYYY}${GMM}${GDD}/gdas1.t${GCYC}z.bf09 .
     cp /com/gfs/prod/${mod}.${YYYY}${MM}${DD}/gdas1.t${CYC}z.sgm3prep .
     cp /com/gfs/prod/${mod}.${YYYY}${MM}${DD}/gdas1.t${CYC}z.sgesprep .
     cp /com/gfs/prod/${mod}.${YYYY}${MM}${DD}/gdas1.t${CYC}z.sgp3prep .
     cp /com/gfs/prod/${mod}.${YYYY}${MM}${DD}/gdas1.t${CYC}z.prepbufr .
  fi

cp gdas1.t${GCYC}z.abias biascr.$mod.$gdate
cp gdas1.t${GCYC}z.satang satang.$mod.$gdate
cp gdas1.t${GCYC}z.bf03 sfcf03.$mod.$gdate
cp gdas1.t${GCYC}z.bf06 sfcf06.$mod.$gdate
cp gdas1.t${GCYC}z.bf09 sfcf09.$mod.$gdate

cp gdas1.t${CYC}z.sgm3prep siggm3.$mod.$adate
cp gdas1.t${CYC}z.sgesprep sigges.$mod.$adate
cp gdas1.t${CYC}z.sgp3prep siggp3.$mod.$adate
cp gdas1.t${CYC}z.prepbufr prepqc.$mod.$adate
cp /global/shared/dump/${adate}/${mod}/*.${adate} $savedir
touch *
elif [ "$mod" = "gfs" ]; then
   if [ $CYC != "00" ]; then
     cp /com/gfs/prod/gdas.${YYYY}${MM}${DD}/gdas1.t${GCYC}z.abias .
     cp /com/gfs/prod/gdas.${YYYY}${MM}${DD}/gdas1.t${GCYC}z.satang .
     cp /com/gfs/prod/gdas.${YYYY}${MM}${DD}/gdas1.t${GCYC}z.bf03 .
     cp /com/gfs/prod/gdas.${YYYY}${MM}${DD}/gdas1.t${GCYC}z.bf06 .
     cp /com/gfs/prod/gdas.${YYYY}${MM}${DD}/gdas1.t${GCYC}z.bf09 .
     cp /com/gfs/prod/${mod}.${YYYY}${MM}${DD}/gfs.t${CYC}z.sgm3prep .
     cp /com/gfs/prod/${mod}.${YYYY}${MM}${DD}/gfs.t${CYC}z.sgesprep .
     cp /com/gfs/prod/${mod}.${YYYY}${MM}${DD}/gfs.t${CYC}z.sgp3prep .
     cp /com/gfs/prod/${mod}.${YYYY}${MM}${DD}/gfs.t${CYC}z.prepbufr .
   else
     cp /com/gfs/prod/gdas.${GYYYY}${GMM}${GDD}/gdas1.t${GCYC}z.abias .
     cp /com/gfs/prod/gdas.${GYYYY}${GMM}${GDD}/gdas1.t${GCYC}z.satang .
     cp /com/gfs/prod/gdas.${GYYYY}${GMM}${GDD}/gdas1.t${GCYC}z.bf03 .
     cp /com/gfs/prod/gdas.${GYYYY}${GMM}${GDD}/gdas1.t${GCYC}z.bf06 .
     cp /com/gfs/prod/gdas.${GYYYY}${GMM}${GDD}/gdas1.t${GCYC}z.bf09 .
     cp /com/gfs/prod/${mod}.${YYYY}${MM}${DD}/gfs.t${CYC}z.sgm3prep .
     cp /com/gfs/prod/${mod}.${YYYY}${MM}${DD}/gfs.t${CYC}z.sgesprep .
     cp /com/gfs/prod/${mod}.${YYYY}${MM}${DD}/gfs.t${CYC}z.sgp3prep .
     cp /com/gfs/prod/${mod}.${YYYY}${MM}${DD}/gfs.t${CYC}z.prepbufr .
   fi
cp gdas1.t${GCYC}z.abias biascr.$mod.$gdate
cp gdas1.t${GCYC}z.satang satang.$mod.$gdate 
cp gdas1.t${GCYC}z.bf03 sfcf03.$mod.$gdate
cp gdas1.t${GCYC}z.bf06 sfcf06.$mod.$gdate
cp gdas1.t${GCYC}z.bf09 sfcf09.$mod.$gdate
cp gfs.t${CYC}z.sgm3prep siggm3.$mod.$adate
cp gfs.t${CYC}z.sgesprep sigges.$mod.$adate
cp gfs.t${CYC}z.sgp3prep siggp3.$mod.$adate
cp gfs.t${CYC}z.prepbufr prepqc.$mod.$adate

cp /global/shared/dump/${adate}/${mod}/*.${adate} $savedir
touch *
fi
adate=`$ndate +24 $adate`
done
exit 0
