#!/bin/sh
set -x
###############################################################################
## 1. Set date 
## 2. analysis cycle (6hr gdas vs 2.5hr gfs)
## 3. Save directory
###############################################################################
adate=${1:-2012110900}
expt=${2:-prgm141con}
savedir=${3:-/ptmp/wx23dc/$expt}
edate=${4:-2012110900}
if [ ! -d $savedir ]; then mkdir -p $savedir || exit 8 ; fi ;
cd $savedir
while [ $adate -le $edate ]; do
YYYY=`echo $adate | cut -c1-4`
MM=`echo $adate | cut -c5-6`
DD=`echo $adate | cut -c7-8`
CYC=`echo $adate | cut -c9-10`
tag=sig
ndate=${ndate_dir}/ndate

#/u/wx20mi/bin/hpsstar getnostage /NCEPDEV/hpssuser/g01/wx23dc/$expt/${YYYY}${MM}${DD}${CYC}gfs.tar  ${tag}anl.gfs.$adate ${tag}f00.gfs.$adate ${tag}f03.gfs.$adate ${tag}f09.gfs.$adate ${tag}f15.gfs.$adate ${tag}f21.gfs.$adate ${tag}f27.gfs.$adate ${tag}f33.gfs.$adate ${tag}f39.gfs.$adate ${tag}f42.gfs.$adate ${tag}f45.gfs.$adate ${tag}f51.gfs.$adate ${tag}f54.gfs.$adate ${tag}f57.gfs.$adate ${tag}f63.gfs.$adate ${tag}f66.gfs.$adate ${tag}f69.gfs.$adate ${tag}f75.gfs.$adate ${tag}f78.gfs.$adate ${tag}f81.gfs.$adate ${tag}f84.gfs.$adate ${tag}f87.gfs.$adate ${tag}f90.gfs.$adate ${tag}f93.gfs.$adate ${tag}f99.gfs.$adate ${tag}f105.gfs.$adate ${tag}f111.gfs.$adate ${tag}f114.gfs.$adate ${tag}f117.gfs.$adate 

#/u/wx20mi/bin/hpsstar getnostage /NCEPPROD/1year/hpsspara/runhistory/glopara/$expt/${YYYY}${MM}${DD}${CYC}gfs.tar ${tag}anl.gfs.$adate ${tag}f00.gfs.$adate ${tag}f06.gfs.$adate ${tag}f12.gfs.$adate ${tag}f18.gfs.$adate ${tag}f24.gfs.$adate ${tag}f30.gfs.$adate ${tag}f36.gfs.$adate ${tag}f48.gfs.$adate ${tag}f60.gfs.$adate ${tag}f72.gfs.$adate ${tag}f96.gfs.$adate ${tag}f102.gfs.$adate ${tag}f108.gfs.$adate ${tag}f114.gfs.$adate ${tag}f120.gfs.$adate 
#${tag}f126.gfs.$adate ${tag}f132.gfs.$adate ${tag}f144.gfs.$adate ${tag}f156.gfs.$adate ${tag}f168.gfs.$adate

/u/wx20mi/bin/hpsstar getnostage /NCEPPROD/1year/hpsspara/runhistory/glopara/$expt/${YYYY}${MM}${DD}${CYC}gfs.tar ${tag}anl.gfs.$adate sfcanl.gfs.$adate 

#/u/wx20mi/bin/hpsstar getnostage /NCEPPROD/1year/hpsspara/runhistory/glopara/$expt/${YYYY}${MM}${DD}${CYC}gfs.sigfa.tar ${tag}f117.gfs.$adate ${tag}f120.gfs.$adate ${tag}f123.gfs.$adate ${tag}f126.gfs.$adate ${tag}f129.gfs.$adate ${tag}f132.gfs.$adate

#/u/wx20mi/bin/hpsstar getnostage /NCEPPROD/1year/hpsspara/runhistory/glopara/${expt}/${YYYY}${MM}${DD}${CYC}gfs.tar ${tag}anl.gfs.$adate ${tag}f00.gfs.$adate ${tag}f24.gfs.$adate ${tag}f48.gfs.$adate ${tag}f72.gfs.$adate ${tag}f96.gfs.$adate ${tag}f120.gfs.$adate ${tag}f144.gfs.$adate ${tag}f168.gfs.$adate ${tag}f192.gfs.$adate ${tag}f216.gfs.$adate ${tag}f240.gfs.$adate ${tag}f264.gfs.$adate ${tag}f288.gfs.$adate ${tag}f312.gfs.$adate ${tag}f336.gfs.$adate ${tag}f360.gfs.$adate ${tag}f384.gfs.$adate

#/u/wx20mi/bin/hpsstar get /hpssuser/g01/wx23dc/$expt/${YYYY}${MM}${DD}${CYC}gfs.tar ${tag}f144.gfs.$adate
adate=`$ndate +24 $adate`
done
#echo "Finished copying" | mail -s "Finished copying all $expt pgb files" dana.carlis@gmail.com
#
