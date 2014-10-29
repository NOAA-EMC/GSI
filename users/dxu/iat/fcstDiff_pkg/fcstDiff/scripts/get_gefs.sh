#!/bin/sh
set -x
###############################################################################
## 1. Set date 
## 2. analysis cycle (6hr gdas vs 2.5hr gfs)
## 3. Save directory
###############################################################################
adate=${1:-2006091206}
savedir=${2:-/ptmp/wx23dc/gefs}
if [ ! -d $savedir ]; then mkdir -p $savedir || exit 8 ; fi ;

cd $savedir
rm -rf $savedir/*
YYYY=`echo $adate | cut -c1-4`
MM=`echo $adate | cut -c5-6`
DD=`echo $adate | cut -c7-8`
CYC=`echo $adate | cut -c9-10`
tag=pgrb
ndate=${ndate_dir}/ndate
gdate=`$ndate -06 $adate`
cnvgrib=/nwprod/util/exec/cnvgrib

#for num in avg ; do
for num in c00 p01 p02 p03 p04 p05 p06 p07 p08 p09 p10 p11 p12 p13 p14 p15 p16 p17 p18 p19 p20; do

/u/wx20mi/bin/hpsstar getnostage /NCEPPROD/hpssprod/runhistory/rh$YYYY/$YYYY$MM/$YYYY$MM$DD/com_gens_prod_gefs.$YYYY$MM${DD}_$CYC.pgrb2a.tar ./pgrb2a/ge${num}.t${CYC}z.pgrb2aanl ./pgrb2a/ge${num}.t${CYC}z.pgrb2af00 ./pgrb2a/ge${num}.t${CYC}z.pgrb2af12 ./pgrb2a/ge${num}.t${CYC}z.pgrb2af24 ./pgrb2a/ge${num}.t${CYC}z.pgrb2af36 ./pgrb2a/ge${num}.t${CYC}z.pgrb2af48 ./pgrb2a/ge${num}.t${CYC}z.pgrb2af60 ./pgrb2a/ge${num}.t${CYC}z.pgrb2af72 ./pgrb2a/ge${num}.t${CYC}z.pgrb2af84 ./pgrb2a/ge${num}.t${CYC}z.pgrb2af96 ./pgrb2a/ge${num}.t${CYC}z.pgrb2af108 ./pgrb2a/ge${num}.t${CYC}z.pgrb2af120
mv ./pgrb2a/ge* $savedir
$cnvgrib -g21 ge${num}.t${CYC}z.pgrb2aanl gefs.$adate.pgrbanl.${num#?}
$cnvgrib -g21 ge${num}.t${CYC}z.pgrb2af00 gefs.$adate.pgrbf00.${num#?}
$cnvgrib -g21 ge${num}.t${CYC}z.pgrb2af12 gefs.$adate.pgrbf12.${num#?}
$cnvgrib -g21 ge${num}.t${CYC}z.pgrb2af24 gefs.$adate.pgrbf24.${num#?}
$cnvgrib -g21 ge${num}.t${CYC}z.pgrb2af36 gefs.$adate.pgrbf36.${num#?}
$cnvgrib -g21 ge${num}.t${CYC}z.pgrb2af48 gefs.$adate.pgrbf48.${num#?}
$cnvgrib -g21 ge${num}.t${CYC}z.pgrb2af60 gefs.$adate.pgrbf60.${num#?}
$cnvgrib -g21 ge${num}.t${CYC}z.pgrb2af72 gefs.$adate.pgrbf72.${num#?}
$cnvgrib -g21 ge${num}.t${CYC}z.pgrb2af84 gefs.$adate.pgrbf84.${num#?}
$cnvgrib -g21 ge${num}.t${CYC}z.pgrb2af96 gefs.$adate.pgrbf96.${num#?}
$cnvgrib -g21 ge${num}.t${CYC}z.pgrb2af108 gefs.$adate.pgrbf108.${num#?}
$cnvgrib -g21 ge${num}.t${CYC}z.pgrb2af120 gefs.$adate.pgrbf120.${num#?}
rm -rf ${savedir}/pgrb2a/
rm -f ${savedir}/*pgrb2a*
done


