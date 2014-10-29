#!/bin/sh
set -x
###############################################################################
## 1. Set date 
## 2. analysis cycle (6hr gdas vs 2.5hr gfs)
## 3. Save directory
###############################################################################
adate=${1:-2012111400}
expt=${2:-prgm141xnm}
savedir=${3:-/ptmp/wx23dc/$expt}
edate=${4:-2012111400}

if [ ! -d $savedir ]; then mkdir -p $savedir || exit 8 ; fi ;
cd $savedir

while [ $adate -le $edate ]; do
YYYY=`echo $adate | cut -c1-4`
MM=`echo $adate | cut -c5-6`
DD=`echo $adate | cut -c7-8`
CYC=`echo $adate | cut -c9-10`
tag=pgb
ndate=${ndate_dir}/ndate
gdate=`$ndate -06 $adate`


/u/wx20mi/bin/hpsstar getnostage /NCEPPROD/1year/hpsspara/runhistory/glopara/${expt}/${adate}gfs.tar siggm3.gfs.$adate sigges.gfs.$adate siggp3.gfs.$adate prepqc.gfs.$adate

/u/wx20mi/bin/hpsstar getnostage /NCEPPROD/1year/hpsspara/runhistory/glopara/${expt}/${gdate}gdas.tar biascr.gdas.$gdate satang.gdas.$gdate sfcf06.gdas.$gdate sfcf03.gdas.$gdate sfcf09.gdas.$gdate radstat.gdas.$gdate 

/u/wx20mi/bin/hpsstar getnostage /NCEPPROD/1year/hpsspara/runhistory/glopara/${expt}/${gdate}gdas.enkf.fcs.tar fcsstat_${gdate}_all sfg_${gdate}_fhr06s_mem001 sfg_${gdate}_fhr06s_mem002 sfg_${gdate}_fhr06s_mem003 sfg_${gdate}_fhr06s_mem004 sfg_${gdate}_fhr06s_mem005 sfg_${gdate}_fhr06s_mem006 sfg_${gdate}_fhr06s_mem007 sfg_${gdate}_fhr06s_mem008 sfg_${gdate}_fhr06s_mem009 sfg_${gdate}_fhr06s_mem010 sfg_${gdate}_fhr06s_mem011 sfg_${gdate}_fhr06s_mem012 sfg_${gdate}_fhr06s_mem013 sfg_${gdate}_fhr06s_mem014 sfg_${gdate}_fhr06s_mem015 sfg_${gdate}_fhr06s_mem016 sfg_${gdate}_fhr06s_mem017 sfg_${gdate}_fhr06s_mem018 sfg_${gdate}_fhr06s_mem019 sfg_${gdate}_fhr06s_mem020 sfg_${gdate}_fhr06s_mem021 sfg_${gdate}_fhr06s_mem022 sfg_${gdate}_fhr06s_mem023 sfg_${gdate}_fhr06s_mem024 sfg_${gdate}_fhr06s_mem025 sfg_${gdate}_fhr06s_mem026 sfg_${gdate}_fhr06s_mem027 sfg_${gdate}_fhr06s_mem028 sfg_${gdate}_fhr06s_mem029 sfg_${gdate}_fhr06s_mem030 sfg_${gdate}_fhr06s_mem031 sfg_${gdate}_fhr06s_mem032 sfg_${gdate}_fhr06s_mem033 sfg_${gdate}_fhr06s_mem034 sfg_${gdate}_fhr06s_mem035 sfg_${gdate}_fhr06s_mem036 sfg_${gdate}_fhr06s_mem037 sfg_${gdate}_fhr06s_mem038 sfg_${gdate}_fhr06s_mem039 sfg_${gdate}_fhr06s_mem040 sfg_${gdate}_fhr06s_mem041 sfg_${gdate}_fhr06s_mem042 sfg_${gdate}_fhr06s_mem043 sfg_${gdate}_fhr06s_mem044 sfg_${gdate}_fhr06s_mem045 sfg_${gdate}_fhr06s_mem046 sfg_${gdate}_fhr06s_mem047 sfg_${gdate}_fhr06s_mem048 sfg_${gdate}_fhr06s_mem049 sfg_${gdate}_fhr06s_mem050 sfg_${gdate}_fhr06s_mem051 sfg_${gdate}_fhr06s_mem052 sfg_${gdate}_fhr06s_mem053 sfg_${gdate}_fhr06s_mem054 sfg_${gdate}_fhr06s_mem055 sfg_${gdate}_fhr06s_mem056 sfg_${gdate}_fhr06s_mem057 sfg_${gdate}_fhr06s_mem058 sfg_${gdate}_fhr06s_mem059 sfg_${gdate}_fhr06s_mem060 sfg_${gdate}_fhr06s_mem061 sfg_${gdate}_fhr06s_mem062 sfg_${gdate}_fhr06s_mem063 sfg_${gdate}_fhr06s_mem064 sfg_${gdate}_fhr06s_mem065 sfg_${gdate}_fhr06s_mem066 sfg_${gdate}_fhr06s_mem067 sfg_${gdate}_fhr06s_mem068 sfg_${gdate}_fhr06s_mem069 sfg_${gdate}_fhr06s_mem070 sfg_${gdate}_fhr06s_mem071 sfg_${gdate}_fhr06s_mem072 sfg_${gdate}_fhr06s_mem073 sfg_${gdate}_fhr06s_mem074 sfg_${gdate}_fhr06s_mem075 sfg_${gdate}_fhr06s_mem076 sfg_${gdate}_fhr06s_mem077 sfg_${gdate}_fhr06s_mem078 sfg_${gdate}_fhr06s_mem079 sfg_${gdate}_fhr06s_mem080

adate=`$ndate +24 $adate`
done
#echo "Finished copying" | mail -s "Finished copying all $expt pgb files" dana.carlis@gmail.com

