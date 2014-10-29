#!/bin/sh
set -x
###############################################################################
## 1. Set date of experiment
## 2. analysis cycle (6hr gdas vs 2.5hr gfs)
## 3. Save directory         
###############################################################################
adate=${1:-2013082500}
edate=${2:-2013082500}
mod=${3:-gdas}

while [ $adate -le $edate ]; do
savedir=/ptmp/$USER/$adate
mkdir -p $savedir
cd $savedir
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
/nwprod/util/ush/hpsstar getnostage /NCEPPROD/hpssprod/runhistory/rh${YYYY}/${YYYY}${MM}/${YYYY}${MM}${DD}/com_gfs_prod_${mod}.${YYYY}${MM}${DD}${CYC}.tar ./gdas1.t${CYC}z.abias ./gdas1.t${CYC}z.satang ./gdas1.t${CYC}z.sanl ./gdas1.t${CYC}z.sfcanl 

##cp /global/shared/dump/${adate}/${mod}/*.${adate} $savedir
mv gdas1.t${CYC}z.abias biascr.$adate
mv gdas1.t${CYC}z.satang satang.$adate
mv gdas1.t${CYC}z.sfcanl sfcanl.$adate
mv gdas1.t${CYC}z.sanl siganl.$adate
else
echo `NEED TO GIVE A MODEL NAME`
fi
adate=`$ndate +06 $adate`
done
exit 
