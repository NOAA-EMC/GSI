#!/bin/sh
set -xa
cyc=06
sdate=2012100106
edate=2012101518
BASEDIR=/global/save/glopara/RFC/GSI_Q2FY2012
while [ $sdate -le $edate ] ; do
/ptmp/wx23dc/prgm141con/fits.sh $sdate /ptmp/wx23dc/prgm141con/prepqa.gdas.$sdate /ptmp/wx23dc/prgm141con /stmp/wx23dc/gm141con${sdate}gdasvrfy 06 00
sdate=`${ndate_dir}/ndate +06 $sdate`
done
exit
