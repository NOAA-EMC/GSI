#!/bin/sh
set -x
###############################################################################
## 1. Set date 
## 2. analysis cycle (6hr gdas vs 2.5hr $mod)
## 3. Save directory
###############################################################################
adate=${1:-2012100100}
savedir=${2:-/ptmp/wx23dc/$expt}
edate=${3:-2012100100100100100100100100100100100100100100100100100100100100}
while [ $adate -le $edate ]; do
if [ ! -d $savedir ]; then mkdir -p $savedir/$adate || exit 8 ; fi ;
cd $savedir/$adate
YYYY=`echo $adate | cut -c1-4`
MM=`echo $adate | cut -c5-6`
DD=`echo $adate | cut -c7-8`
CYC=`echo $adate | cut -c9-10`
tag=sig
mod=gdas
ndate=${ndate_dir}/ndate

/u/wx20mi/bin/hpsstar getnostage /NCEPPROD/hpssprod/runhistory/rh${YYYY}/${YYYY}${MM}/${YYYY}${MM}${DD}/com_gfs_prod_gdas.${YYYY}${MM}${DD}${CYC}.tar  ${tag}anl.$mod.$adate ${tag}f06.$mod.$adate ${tag}f12.$mod.$adate ${tag}f18.$mod.$adate ${tag}f24.$mod.$adate ${tag}f30.$mod.$adate ${tag}f36.$mod.$adate ${tag}f42.$mod.$adate ${tag}f48.$mod.$adate ${tag}f54.$mod.$adate ${tag}f60.$mod.$adate ${tag}f66.$mod.$adate ${tag}f72.$mod.$adate ${tag}f78.$mod.$adate ${tag}f84.$mod.$adate ${tag}f90.$mod.$adate ${tag}f96.$mod.$adate ${tag}f102.$mod.$adate ${tag}f108.$mod.$adate ${tag}f114.$mod.$adate ${tag}f120.$mod.$adate  

#${tag}f00.$mod.$adate ${tag}f06.$mod.$adate ${tag}f12.$mod.$adate ${tag}f18.$mod.$adate ${tag}f24.$mod.$adate ${tag}f30.$mod.$adate ${tag}f36.$mod.$adate ${tag}f48.$mod.$adate ${tag}f60.$mod.$adate ${tag}f72.$mod.$adate ${tag}f96.$mod.$adate ${tag}f102.$mod.$adate ${tag}f108.$mod.$adate ${tag}f114.$mod.$adate ${tag}f120.$mod.$adate 
#${tag}f126.$mod.$adate ${tag}f132.$mod.$adate ${tag}f144.$mod.$adate ${tag}f156.$mod.$adate ${tag}f168.$mod.$adate

#/u/wx20mi/bin/hpsstar get /hpssuser/g01/wx23dc/$expt/${YYYY}${MM}${DD}${CYC}$mod.tar ${tag}f144.$mod.$adate
adate=`$ndate +24 $adate`
done
echo "Finished copying" | mail -s "Finished copying all $expt pgb files" dana.carlis@gmail.com

