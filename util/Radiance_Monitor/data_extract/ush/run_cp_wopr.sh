#!/bin/sh
set -ax

export RADSTAT_LOCATION=/com2/gfs/prod
export SOURCE_DIR=/com2/verf/prod
export ACCOUNT=dev
export USE_ANL=1
export DO_DIAG_RPT=1
export DO_DATA_RPT=1
export MAIL_TO="Edward.Safford@noaa.gov"
#export MAIL_CC="Russ.Treadon@noaa.gov"
export MAIL_CC="edward.c.safford@gmail.com"
export JOB_QUEUE=dev_shared

me=`hostname | cut -c1`
export package=ProdGSI/util/Radiance_Monitor

export TANK_USE_RUN=1

scripts=/gpfs/${me}d2/emc/da/noscrub/Edward.Safford/${package}/data_extract/ush


#--------------------------------------------------------------------
#  Check for my monitoring use.  Abort if running on prod machine.
#--------------------------------------------------------------------

is_prod=`${scripts}/onprod.sh`
if [[ $is_prod = 1 ]]; then
   exit 10
fi

#--------------------------------------------------------------------
export RADMON_SUFFIX=GFS
export RUN=gdas
export RAD_AREA=glb

TANKverf=/u/Edward.Safford/nbns/stats
NDATE=/nwprod/util/exec/ndate

idate=`${scripts}/find_cycle.pl --run $RUN --cyc 1 --dir ${TANKverf}/${RADMON_SUFFIX}`

#idate=`${idate_scripts}/find_cycle.pl 1 ${TANKverf}`
idate_len=`echo ${#idate}`
if [[ ${idate_len} -ne 10 ]]; then
   exit 1
fi
START_DATE=`${NDATE} +06 $idate`


echo idate, START_DATE = $idate, $START_DATE

/gpfs/${me}d2/emc/da/noscrub/Edward.Safford/${package}/data_extract/ush/Copy_glbl.sh \
   ${RADMON_SUFFIX} ${START_DATE} \
   1>/ptmpp1/Edward.Safford/logs/CopyRad_${RADMON_SUFFIX}.log \
   2>/ptmpp1/Edward.Safford/logs/CopyRad_${RADMON_SUFFIX}.err

set +ax
exit
