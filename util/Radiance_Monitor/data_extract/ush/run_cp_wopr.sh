#!/bin/sh
set -ax

#export RADSTAT_LOCATION=/com2/gfs/prod
export SOURCE_DIR=/gpfs/dell1/nco/ops/com/gfs/prod
export ACCOUNT=dev
export USE_ANL=1
export DO_DIAG_RPT=1
export DO_DATA_RPT=1
export MAIL_TO="Edward.Safford@noaa.gov"
#export MAIL_CC="Russ.Treadon@noaa.gov"
export MAIL_CC="edward.c.safford@gmail.com"
export JOB_QUEUE=dev_shared

me=`hostname | cut -c1`
#package=ProdGSI/util/Radiance_Monitor
package=RadMon

export TANK_USE_RUN=1
export USE_HR=1

scripts=/gpfs/dell2/emc/modeling/noscrub/Edward.Safford/${package}/data_extract/ush

#--------------------------------------------------------------------
export RADMON_SUFFIX=GFS
export RUN=gdas
export RAD_AREA=glb

TANKverf=/u/Edward.Safford/nbns/stats
NDATE=/gpfs/dell1/nco/ops/nwprod/prod_util.v1.1.2/exec/ips/ndate

idate=`${scripts}/nu_find_cycle.pl --run $RUN --cyc 1 --dir ${TANKverf}/${RADMON_SUFFIX}`

idate_len=`echo ${#idate}`
if [[ ${idate_len} -ne 10 ]]; then
   exit 1
fi
START_DATE=`${NDATE} +06 $idate`
#START_DATE=2019062900

echo idate, START_DATE = $idate, $START_DATE

logdir=/gpfs/dell2/ptmp/Edward.Safford/logs/${RADMON_SUFFIX}/${RUN}/radmon

${scripts}/Copy_glbl.sh \
   ${RADMON_SUFFIX} ${START_DATE} \
   1>${logdir}/CopyRad_${RADMON_SUFFIX}.log \
   2>${logdir}/CopyRad_${RADMON_SUFFIX}.err

set +ax
exit
