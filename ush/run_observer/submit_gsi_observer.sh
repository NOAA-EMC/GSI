#!/bin/bash
# submit_gsi_observer.sh
# script to define configuration,
# prepare, and submit GSI observer job
# cory.r.martin@noaa.gov

set -x

#---- user modified variables
# valid time
cycle=2024021900
# path to your GSI clone
THISDIR=`pwd`
GSIDIR=$THISDIR/../..
# top level working directory
workdir=/work2/noaa/da/$LOGNAME/ufoeval/GSIobserver/orion/$cycle/
# GDASApp clone
#GDASApp=/work2/noaa/da/$LOGNAME/git/GDASApp
GDASApp=/work2/noaa/da/$LOGNAME/git/orion/GDASApp
MACHINE=orion

# gfs or gdas
dump=gdas
# restricted data inclusion
rstprod="true"
# 6 for 3d, 1 or 3 for 4d
nhr_bkg=6

# should NOT touch below this line
CRTM_FIX=/work/noaa/epic/role-epic/spack-stack/hercules/spack-stack-1.6.0/envs/unified-env/install/intel/2021.9.0/crtm-fix-2.4.0.1_emc-2os2hw2/fix
# many people cannot clone this without gerrit permissions
# plus, I have the C768 berror file here
# so that GSI observer can run at full background res
GSIFIX=/work2/noaa/da/cmartin/UFO_eval/geovals/GSI/fix
dumpdir=/work/noaa/rstprod/dump
gesroot=/work2/noaa/da/acollard/UFO_eval/data/para/output_ufo_eval_feb2024_9Aug

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
export MACHINE=$MACHINE
EOF

#sbatch $GSIDIR/ush/run_observer/gsi_observer.sh $workdir/config.sh
sbatch $GSIDIR/ush/run_observer/gsi_observer.sh $workdir/config.sh
