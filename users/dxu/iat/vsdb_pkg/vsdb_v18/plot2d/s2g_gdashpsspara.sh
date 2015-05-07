#!/bin/ksh
set -x

## read experimental gdas siganl and sigges from hpss archive 
## and create lat-lon binary files using ss2ggx
# Fanglin Yang

exp=${1:-pr4dev}
edate=${2:-2015030812}
sdate=${3:-2015030218}

USER=${USER:-$LOGNAME}
HPSSDIR=${HPSSDIR:-/5year/NCEPDEV/emc-global/$USER/WCOSS/$exp}

host=`echo $(hostname)|cut -c 1-1`
if [ $host = t -o $host = g ]; then  ;#WCOSS
 rundir=${rundir:-/stmpd2/$USER/hpssgdas_$exp}                                     
 ROTDIR=${ROTDIR:-/ptmpd1/$USER/$exp}
 savedir=${savedir:-/global/noscrub/$USER/archive/$exp}
 s2g=${s2g:-/global/save/Fanglin.Yang/VRFY/vsdb/nwprod/util/exec/ss2ggx}
 HPSSTAR=${HPSSTAR:-/nwprod/util/ush/hpsstar}
 ndate=${ndate:-/nwprod/util/exec/ndate}
elif [ $host = f ]; then   #;ZEUS
 rundir=${rundir:-/scratch2/portfolios/NCEPDEV/stmp/$USER/hpssgdas_$exp}                                     
 ROTDIR=${ROTDIR:-/scratch2/portfolios/NCEPDEV/ptmp/$USER/$exp}
 savedir=${savedir:-/scratch2/portfolios/NCEPDEV/global/noscrub/$USER/archive/$exp}
 s2g=${s2g:-/scratch2/portfolios/NCEPDEV/global/save/Fanglin.Yang/VRFY/vsdb/nwprod/util/exec/ss2ggx}
 HPSSTAR=${HPSSTAR:-/home/Fanglin.Yang/bin/hpsstar_zeus}
 ndate=${ndate:-/scratch2/portfolios/NCEPDEV/global/save/Fanglin.Yang/VRFY/vsdb/nwprod/util/exec/ndate}
fi


JCAP_bin=${JCAP_bin:-254}    ;#output binary file resolution
if [ $JCAP_bin = 1534 ]; then
 lonb=3072; latb=1536; dx=0.117188  ; dy=0.117264
elif [ $JCAP_bin = 1148 ]; then
 lonb=2304; latb=1152; dx=0.156250  ; dy=0.156386
elif [ $JCAP_bin = 574 ]; then
 lonb=1760; latb=880;  dx=0.204545  ; dy=0.204778
elif [ $JCAP_bin = 382 ]; then
 lonb=1152; latb=576;  dx=0.312500  ; dy=0.313043
elif [ $JCAP_bin = 254 ]; then
 lonb=768;  latb=384;  dx=0.468750  ; dy=0.469974
elif [ $JCAP_bin = 126 ]; then
 lonb=384;  latb=190;  dx=0.937500  ; dy=0.952381
elif [ $JCAP_bin = 62 ]; then
 lonb=192;  latb=94;   dx=1.875000  ; dy=1.935484
else
 echo " JCAP_bin=$JCAP_bin not supported, exit"
 exit
fi
idrt=0  ;##0->lat-lon grid

mkdir -p $rundir 
cd $rundir ||exit 8
cdate=$edate
#---------------------------------------
while [ $cdate -ge $sdate ]; do
#---------------------------------------

ymdh=${cdate}
yyyy=`echo $ymdh | cut -c1-4`
mm=`  echo $ymdh | cut -c5-6`
dd=`  echo $ymdh | cut -c7-8`
cyc=`  echo $ymdh | cut -c9-10`

ina=$ROTDIR/siganl.gdas.${ymdh}                                 
ing=$ROTDIR/sigges.gdas.${ymdh}                                 
oua=${savedir}/siganl.gdas.${ymdh}.${lonb}.${latb}.bin
oug=${savedir}/sigges.gdas.${ymdh}.${lonb}.${latb}.bin

if [ ! -s $ina ]; then
 ina=siganl.gdas.${ymdh} 
 ing=sigges.gdas.${ymdh}     
 rm -f $ina $ing
 $HPSSTAR get ${HPSSDIR}/${ymdh}gdas.tar  $ina $ing 
fi

export OMP_NUM_THREADS=4
export MP_SHARED_MEMORY=no
export KMP_STACKSIZE=2048m

date
if [ -s $ina ]; then $s2g $ina $oua tmpa.ctl $idrt ${lonb} ${latb} &;fi
if [ -s $ing ]; then $s2g $ing $oug tmpg.ctl $idrt ${lonb} ${latb} ;fi
date


cdate=`$ndate -6 ${cdate} `
done   # date
#---------------------------------------

exit


