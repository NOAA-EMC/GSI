#!/bin/sh

package=ProdGSI/util/Conventional_Monitor


scripts=/gpfs/dell2/emc/modeling/noscrub/${USER}/${package}/image_gen/ush
suffix=GFS
run=gdas

export PROJECT=GFS-DEV
export MY_MACHINE=wcoss_d

logdir=/gpfs/dell2/ptmp/${USER}/logs/${suffix}/${run}/conmon
export logfile=${logdir}/run_transfer_gdas.log

export JOB_QUEUE=dev_transfer

#shell=sh
#. /usrx/local/Modules/default/init/${shell}
#module load lsf
export SUB="bsub"

export CMON_SUFFIX=${suffix}
export jobname=transfer_${CMON_SUFFIX}_cmon

if [[ $MY_MACHINE = "wcoss" || $MY_MACHINE == "wcoss_d" ]]; then
   $SUB -P $PROJECT -q $JOB_QUEUE -o ${logfile} -M 80 -W 0:45 \
        -R affinity[core] -J ${jobname} -cwd ${PWD} \
        ${scripts}/Transfer.sh ${CMON_SUFFIX}
else

   ${scripts}/Transfer.sh ${CMON_SUFFIX} \
     1>/ptmpp1/${USER}/logs/${suffix}/ConMon/Transfer.log \
     2>/ptmpp1/${USER}/logs/${suffix}/ConMon/Transfer.err

fi

exit
