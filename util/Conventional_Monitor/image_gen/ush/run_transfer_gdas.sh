#!/bin/sh

package=ProdGSI/util/Conventional_Monitor


scripts=/gpfs/dell2/emc/modeling/noscrub/${USER}/${package}/image_gen/ush
suffix=GFS
run=gdas


shell=sh
source /usrx/local/prod/lmod/lmod/init/${shell}

MODULEPATH=/usrx/local/prod/lmod/lmod/modulefiles/Core
MODULEPATH=${MODULEPATH}:/usrx/local/prod/modulefiles/core_third
MODULEPATH=${MODULEPATH}:/usrx/local/prod/modulefiles/defs
MODULEPATH=${MODULEPATH}:/gpfs/dell1/nco/ops/nwprod/modulefiles/core_prod
export MODULEPATH=${MODULEPATH}:/usrx/local/dev/modulefiles

module load lsf/10.1
SUB="bsub"

export PROJECT=GFS-DEV
export MY_MACHINE=wcoss_d

logdir=/gpfs/dell2/ptmp/${USER}/logs/${suffix}/${run}/conmon
export logfile=${logdir}/Transfer.log

export JOB_QUEUE=dev_transfer

export jobname=transfer_${suffix}_conmon

if [[ $MY_MACHINE = "wcoss" || $MY_MACHINE == "wcoss_d" ]]; then
   $SUB -P $PROJECT -q $JOB_QUEUE -o ${logfile} -M 80 -W 1:30 \
        -R affinity[core] -J ${jobname} -cwd ${PWD} \
        ${scripts}/Transfer.sh ${suffix} --run ${run}

else			# wcoss_c can probably use the above if condition, 
			# block hera from doing a transfer though.

   ${scripts}/Transfer.sh ${suffix} \
     1>/ptmpp1/${USER}/logs/${suffix}/ConMon/Transfer.log \
     2>/ptmpp1/${USER}/logs/${suffix}/ConMon/Transfer.err

fi

exit
