#!/bin/sh
set -ax

export RADSTAT_LOCATION=/com2/gfs/prod
export SOURCE_DIR=/gpfs/dell2/emc/modeling/noscrub/emc.glopara/monitor/radmon/stats/v16rt0

export ACCOUNT=dev
export USE_ANL=1
export DO_DIAG_RPT=1
export DO_DATA_RPT=1
export MAIL_TO="Edward.Safford@noaa.gov"
#export MAIL_CC="Russ.Treadon@noaa.gov"
export MAIL_CC="edward.c.safford@gmail.com"
export JOB_QUEUE=dev_shared

me=`hostname | cut -c1`
package=ProdGSI/util/Radiance_Monitor
#package=RadMon
export TANK_USE_RUN=1

scripts=/gpfs/dell2/emc/modeling/noscrub/Edward.Safford/${package}/data_extract/ush


#--------------------------------------------------------------------
#  Check for my monitoring use.  Abort if running on prod machine.
#--------------------------------------------------------------------

is_prod=`${scripts}/onprod.sh`
if [[ $is_prod = 1 ]]; then
   exit 10
fi

#--------------------------------------------------------------------
export RADMON_SUFFIX=v16rt0
export RUN=gdas
export RAD_AREA=glb

TANKverf=/u/Edward.Safford/nbns/stats
NDATE=/gpfs/dell1/nco/ops/nwprod/prod_util.v1.1.2/exec/ips/ndate

ldate=`${scripts}/nu_find_cycle.pl --run $RUN --cyc 1 --dir ${TANKverf}/${RADMON_SUFFIX}`
echo "last cycle processed is $ldate"

ldate_len=`echo ${#ldate}`
if [[ ${ldate_len} -ne 10 ]]; then
   exit 1
fi
START_DATE=`${NDATE} +06 $ldate`
#START_DATE=2019070100

day=`echo $START_DATE | cut -c1-8` 
export DATDIR=/gpfs/dell2/emc/modeling/noscrub/emc.glopara/monitor/radmon/stats/v16rt0/gdas.${day}

logs=/gpfs/dell2/ptmp/Edward.Safford/logs/${RADMON_SUFFIX}/${RUN}/radmon

echo ldate, START_DATE = $ldate, $START_DATE

${scripts}/Copy_glbl.sh \
   ${RADMON_SUFFIX} ${START_DATE} \
   1>${logs}/CopyRad_${RADMON_SUFFIX}.log \
   2>${logs}/CopyRad_${RADMON_SUFFIX}.err

exit
