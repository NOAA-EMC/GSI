#!/bin/bash
# submit_gsi_observer.sh
# script to define configuration,
# prepare, and submit GSI observer job
# cory.r.martin@noaa.gov

set -x

#---- user modified variables
# valid time
cycle=2021080100
# path to your GSI clone
#GSIDIR=/work2/noaa/da/cmartin/UFO_eval/geovals/GSI
GSIDIR=/work2/noaa/da/$LOGNAME/GSI_jedi/GSI/
# top level working directory
workdir=/work2/noaa/da/$LOGNAME/ufoeval/GSIobserver/$cycle/
# GDASApp clone
#GDASApp=/work2/noaa/da/cmartin/GDASApp/dev/GDASApp
#GDASApp=/work/noaa/da/eliu/JEDI-GDAS/GDASApp
GDASApp=/work2/noaa/da/$LOGNAME/git/GDASApp
# gfs or gdas
dump=gdas
# restricted data inclusion
rstprod="true"
# 6 for 3d, 1 or 3 for 4d
nhr_bkg=6

# should NOT touch below this line
CRTM_FIX=/apps/contrib/NCEP/libs/hpc-stack-gfsv16/intel-2018.4/crtm/2.3.0/fix/
# many people cannot clone this without gerrit permissions
# plus, I have the C768 berror file here
# so that GSI observer can run at full background res
GSIFIX=/work2/noaa/da/cmartin/UFO_eval/geovals/GSI/fix
#GSIFIX=/work2/noaa/da/nesposito/GSI_fixdir
#GSIFIX=/work/noaa/da/eliu/JEDI-GDAS/GDASApp/ush/ufoeval/gsi/fix
dumpdir=/work/noaa/rstprod/dump
gesroot=/work2/noaa/da/cmartin/UFO_eval/data/para/output_ufo_eval_aug2021/

mkdir -p $workdir
cd $workdir
cat > $workdir/config.sh << EOF
export adate=$cycle
export GSIDIR=$GSIDIR
export workdir=$workdir
export GSIFIX=$GSIFIX
export dump=$dump
export dumpdir=$dumpdir
export gesroot=$gesroot
export CRTM_FIX=$CRTM_FIX
export rstprod=$rstprod
export GSI_background_nhr=$nhr_bkg
export GDASApp=$GDASApp
EOF

#sbatch $GDASApp/ush/ufoeval/gsi/gsi_observer.sh $workdir/config.sh
sbatch $GSIDIR/ush/JEDI/gsi_observer.sh $workdir/config.sh
