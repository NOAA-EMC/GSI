#!/bin/sh

export RADMON_SUFFIX=nam

package=ProdGSI/util/Radiance_Monitor
idev=`cat /etc/dev | cut -c1`

scripts=/gpfs/${idev}d2/emc/da/noscrub/Edward.Safford/${package}/image_gen/ush

export PROJECT=NDAS-T2O
export RAD_AREA=rgn
export MY_MACHINE=wcoss
export TANK_USE_RUN=0

data_map=/gpfs/${idev}d2/emc/da/noscrub/Edward.Safford/${package}/util/Radiance_Monitor/parm/data_map.xml
tankdir=/u/Edward.Safford/nbns/stats/regional/${RADMON_SUFFIX}

export logfile=/ptmpp1/Edward.Safford/logs/Transfer_${RADMON_SUFFIX}.log
export JOB_QUEUE=transfer

shell=sh
. /usrx/local/Modules/default/init/${shell}
module load lsf
export SUB=bsub

export jobname=transfer_${RADMON_SUFFIX}

if [[ $MY_MACHINE = "wcoss" ]]; then
   $SUB -P $PROJECT -q $JOB_QUEUE -o ${logfile} -M 80 -W 0:45 \
        -R affinity[core] -J ${jobname} -cwd ${PWD} \
        ${scripts}/Transfer.sh ${RADMON_SUFFIX} --area ${RAD_AREA}

else
   ${scripts}/Transfer.sh ${RADMON_SUFFIX} --area ${RAD_AREA} \
        1>/ptmpp1/Edward.Safford/logs/Transfer_${RADMON_SUFFIX}.log \
        2>/ptmpp1/Edward.Safford/logs/Transfer_${RADMON_SUFFIX}.err

fi

exit
