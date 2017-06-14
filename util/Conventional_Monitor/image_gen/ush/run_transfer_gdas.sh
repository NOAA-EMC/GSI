#!/bin/sh

package=CMon_486
idev=`cat /etc/dev | cut -c1`

scripts=/gpfs/${idev}d2/emc/da/noscrub/${USER}/${package}/util/Conventional_Monitor/image_gen/ush
suffix=GDAS

export PROJECT=GDAS-T2O
export MY_MACHINE=wcoss

export logfile=/ptmpp1/${USER}/logs/${suffix}/ConMon/Transfer_CMon.log
export JOB_QUEUE=transfer

shell=sh
. /usrx/local/Modules/default/init/${shell}
module load lsf
export SUB="bsub"

export CMON_SUFFIX=${suffix}
export jobname=transfer_${CMON_SUFFIX}_cmon

if [[ $MY_MACHINE = "wcoss" ]]; then
   $SUB -P $PROJECT -q $JOB_QUEUE -o ${logfile} -M 80 -W 0:45 \
        -R affinity[core] -J ${jobname} -cwd ${PWD} \
        ${scripts}/Transfer.sh ${CMON_SUFFIX}
else

   ${scripts}/Transfer.sh ${CMON_SUFFIX} \
     1>/ptmpp1/${USER}/logs/${suffix}/ConMon/Transfer.log \
     2>/ptmpp1/${USER}/logs/${suffix}/ConMon/Transfer.err

fi

exit
