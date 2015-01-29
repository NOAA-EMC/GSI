#!/bin/sh
set -x
###############################################################################
## 1. Set date 
## 2. analysis cycle (6hr gdas vs 2.5hr gfs)
## 3. Save directory
###############################################################################
adate=${1:-2012111000}
pslot=${2:-ops}
edate=${3:-2012111000}
savedir=${4:-/ptmp/$USER/pr$pslot}
if [ ! -d $savedir ]; then mkdir -p $savedir || exit 8 ; fi ;

while [ $adate -le $edate ]; do
cd $savedir
#rm $savedir/*
YYYY=`echo $adate | cut -c1-4`
MM=`echo $adate | cut -c5-6`
DD=`echo $adate | cut -c7-8`
CYC=`echo $adate | cut -c9-10`
tag=pgrb
ndate=${ndate_dir}/ndate
hpsstar=/nwprod/util/ush/hpsstar

$hpsstar getnostage /NCEPDEV/hpssuser/g01/glopara/WCOSS/pr$pslot/${adate}gdas.tar biascr.gdas.$adate satang.gdas.$adate sfcanl.gdas.$adate siganl.gdas.$adate

$hpsstar getnostage /NCEPDEV/hpssuser/g01/glopara/WCOSS/pr$pslot/${adate}gdas.enkf.anl.tar sfcanl_${adate}_mem001 sfcanl_${adate}_mem002 sfcanl_${adate}_mem003 sfcanl_${adate}_mem004 sfcanl_${adate}_mem005 sfcanl_${adate}_mem006  sfcanl_${adate}_mem007  sfcanl_${adate}_mem008  sfcanl_${adate}_mem009  sfcanl_${adate}_mem010  sfcanl_${adate}_mem011  sfcanl_${adate}_mem012  sfcanl_${adate}_mem013  sfcanl_${adate}_mem014  sfcanl_${adate}_mem015  sfcanl_${adate}_mem016  sfcanl_${adate}_mem017  sfcanl_${adate}_mem018  sfcanl_${adate}_mem019  sfcanl_${adate}_mem020  sfcanl_${adate}_mem021  sfcanl_${adate}_mem022  sfcanl_${adate}_mem023  sfcanl_${adate}_mem024  sfcanl_${adate}_mem025  sfcanl_${adate}_mem026  sfcanl_${adate}_mem027  sfcanl_${adate}_mem028  sfcanl_${adate}_mem029  sfcanl_${adate}_mem030  sfcanl_${adate}_mem031  sfcanl_${adate}_mem032  sfcanl_${adate}_mem033  sfcanl_${adate}_mem034  sfcanl_${adate}_mem035  sfcanl_${adate}_mem036  sfcanl_${adate}_mem037  sfcanl_${adate}_mem038  sfcanl_${adate}_mem039  sfcanl_${adate}_mem040  sfcanl_${adate}_mem041  sfcanl_${adate}_mem042  sfcanl_${adate}_mem043  sfcanl_${adate}_mem044  sfcanl_${adate}_mem045  sfcanl_${adate}_mem046  sfcanl_${adate}_mem047  sfcanl_${adate}_mem048  sfcanl_${adate}_mem049  sfcanl_${adate}_mem050  sfcanl_${adate}_mem051  sfcanl_${adate}_mem052  sfcanl_${adate}_mem053  sfcanl_${adate}_mem054  sfcanl_${adate}_mem055  sfcanl_${adate}_mem056  sfcanl_${adate}_mem057  sfcanl_${adate}_mem058  sfcanl_${adate}_mem059  sfcanl_${adate}_mem060  sfcanl_${adate}_mem061  sfcanl_${adate}_mem062  sfcanl_${adate}_mem063  sfcanl_${adate}_mem064  sfcanl_${adate}_mem065  sfcanl_${adate}_mem066  sfcanl_${adate}_mem067  sfcanl_${adate}_mem068  sfcanl_${adate}_mem069  sfcanl_${adate}_mem070  sfcanl_${adate}_mem071  sfcanl_${adate}_mem072  sfcanl_${adate}_mem073  sfcanl_${adate}_mem074  sfcanl_${adate}_mem075  sfcanl_${adate}_mem076  sfcanl_${adate}_mem077  sfcanl_${adate}_mem078  sfcanl_${adate}_mem079  sfcanl_${adate}_mem080


$hpsstar getnostage /NCEPDEV/hpssuser/g01/glopara/WCOSS/pr$pslot/${adate}gdas.enkf.anl.tar siganl_${adate}_mem001  siganl_${adate}_mem002  siganl_${adate}_mem003  siganl_${adate}_mem004  siganl_${adate}_mem005  siganl_${adate}_mem006  siganl_${adate}_mem007  siganl_${adate}_mem008  siganl_${adate}_mem009  siganl_${adate}_mem010  siganl_${adate}_mem011  siganl_${adate}_mem012  siganl_${adate}_mem013  siganl_${adate}_mem014  siganl_${adate}_mem015  siganl_${adate}_mem016  siganl_${adate}_mem017  siganl_${adate}_mem018  siganl_${adate}_mem019  siganl_${adate}_mem020  siganl_${adate}_mem021  siganl_${adate}_mem022  siganl_${adate}_mem023  siganl_${adate}_mem024  siganl_${adate}_mem025  siganl_${adate}_mem026  siganl_${adate}_mem027  siganl_${adate}_mem028  siganl_${adate}_mem029  siganl_${adate}_mem030  siganl_${adate}_mem031  siganl_${adate}_mem032  siganl_${adate}_mem033  siganl_${adate}_mem034  siganl_${adate}_mem035  siganl_${adate}_mem036  siganl_${adate}_mem037  siganl_${adate}_mem038  siganl_${adate}_mem039  siganl_${adate}_mem040  siganl_${adate}_mem041  siganl_${adate}_mem042  siganl_${adate}_mem043  siganl_${adate}_mem044  siganl_${adate}_mem045  siganl_${adate}_mem046  siganl_${adate}_mem047  siganl_${adate}_mem048  siganl_${adate}_mem049  siganl_${adate}_mem050  siganl_${adate}_mem051  siganl_${adate}_mem052  siganl_${adate}_mem053  siganl_${adate}_mem054  siganl_${adate}_mem055  siganl_${adate}_mem056  siganl_${adate}_mem057  siganl_${adate}_mem058  siganl_${adate}_mem059  siganl_${adate}_mem060  siganl_${adate}_mem061  siganl_${adate}_mem062  siganl_${adate}_mem063  siganl_${adate}_mem064  siganl_${adate}_mem065  siganl_${adate}_mem066  siganl_${adate}_mem067  siganl_${adate}_mem068  siganl_${adate}_mem069  siganl_${adate}_mem070  siganl_${adate}_mem071  siganl_${adate}_mem072  siganl_${adate}_mem073  siganl_${adate}_mem074  siganl_${adate}_mem075  siganl_${adate}_mem076  siganl_${adate}_mem077  siganl_${adate}_mem078  siganl_${adate}_mem079  siganl_${adate}_mem080

adate=`$ndate +24 $adate`
done
exit
