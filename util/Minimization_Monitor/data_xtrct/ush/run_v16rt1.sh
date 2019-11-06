#!/bin/sh

idev=`cat /etc/dev | cut -c1`
iprod=`cat /etc/prod | cut -c1`

package=MinMon
#package=ProdGSI/util/Minimization_Monitor
suffix=v16rt1
export run=gdas

echo "user = $USER"

export KEEPDATA=YES
export DO_ERROR_RPT=1
export MAIL_TO=""
export MAIL_CC=""

scripts=/gpfs/dell2/emc/modeling/noscrub/${USER}/${package}/data_xtrct/ush
echo "scripts = $scripts"

export jobfile=/gpfs/dell2/emc/modeling/noscrub/${USER}/${package}/nwprod/gdas.v1.0.0/jobs/JGDAS_VMINMON
echo "jobfile = $jobfile"

shell=sh
source /usrx/local/prod/lmod/lmod/init/${shell}

MODULEPATH=/usrx/local/prod/lmod/lmod/modulefiles/Core:/usrx/local/prod/modulefiles/core_third:/usrx/local/prod/modulefiles/defs:/gpfs/dell1/nco/ops/nwprod/modulefiles/core_prod:/usrx/local/dev/modulefiles

module purge
module load ips/18.0.1.163
module load metplus/2.1
module load lsf
module load prod_util/1.1.2

echo NDATE = $NDATE

#export COMIN=/gpfs/hps3/emc/global/noscrub/emc.glopara/archive/prfv3rt1
export COMIN=/gpfs/dell2/emc/modeling/noscrub/emc.glopara/archive/v16rt1

tank=~/nbns/stats/${suffix}
logdir=/gpfs/dell2/ptmp/Edward.Safford/logs/${suffix}/${run}/minmon

ldate=`${scripts}/find_cycle.pl --cyc 1 --dir ${tank} --run ${run}`
echo ldate = $ldate
export PDATE=`${NDATE} +06 $ldate`
#PDATE=2019082806

export PDY=`echo $PDATE|cut -c1-8`
export cyc=`echo $PDATE|cut -c9-10`

#module unload prod_util

export gsistat=${COMIN}/gsistat.${run}.${PDATE}

echo PDATE   = $PDATE
echo gsistat = $gsistat

${scripts}/MinMon_DE.sh ${suffix} --pdate ${PDATE} 1>$logdir/MinMon_DE.log 2>$logdir/MinMon_DE.err


