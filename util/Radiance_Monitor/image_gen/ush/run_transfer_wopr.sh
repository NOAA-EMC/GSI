#!/bin/sh

package=ProdGSI/util/Radiance_Monitor
idev=`cat /etc/dev | cut -c1`

scripts=/gpfs/${idev}d2/emc/da/noscrub/Edward.Safford/${package}/image_gen/ush

export PROJECT=GDAS-T2O
export RAD_AREA=glb
export MY_MACHINE=wcoss

export JOB_QUEUE=transfer

shell=sh
. /usrx/local/Modules/default/init/${shell}
module load lsf
export SUB="bsub"

export RADMON_SUFFIX=GFS
export jobname=transfer_${RADMON_SUFFIX}
export RUN=gdas
export TANK_USE_RUN=1

export logfile=/ptmpp1/Edward.Safford/logs/${RADMON_SUFFIX}/${RUN}/radmon/Transfer_wopr.log

if [[ $MY_MACHINE = "wcoss" ]]; then
   $SUB -P $PROJECT -q $JOB_QUEUE -o ${logfile} -M 80 -W 0:45 \
        -R affinity[core] -J ${jobname} -cwd ${PWD} \
        ${scripts}/Transfer.sh ${RADMON_SUFFIX} --run $RUN --area $RAD_AREA
else

   ${scripts}/Transfer.sh ${RADMON_SUFFIX} \
     1>/ptmpp1/Edward.Safford/logs/Transfer_${RADMON_SUFFIX}.log \
     2>/ptmpp1/Edward.Safford/logs/Transfer_${RADMON_SUFFIX}.err

fi

exit
