#!/bin/sh

idev=`cat /etc/dev | cut -c1`
iprod=`cat /etc/prod | cut -c1`

package=
suffix=
echo "user = $USER"

export KEEPDATA=NO
export DO_ERROR_RPT=1
export MAIL_TO=""
export MAIL_CC=""

scripts=/gpfs/hps3/emc/da/noscrub/${LOGNAME}/${package}/util/Minimization_Monitor/data_xtrct/ush
echo "scripts = $scripts"

export jobfile=/gpfs/hps3/emc/da/noscrub/${LOGNAME}/${package}/util/Minimization_Monitor/nwprod/gdas.v1.0.0/jobs/JGDAS_VMINMON
echo "jobfile = $jobfile"

export MODULESHOME=/opt/modules/3.2.6.7

. $MODULESHOME/init/sh

module use -a /gpfs/hps/nco/ops/nwprod/modulefiles

module load prod_util

echo NDATE = $NDATE

export COMIN=/gpfs/hps3/emc/global/noscrub/emc.glopara/archive/prfv3rt1

tank=~/nbns/stats/${suffix}
logdir=/gpfs/hps2/ptmp/Edward.Safford/logs/${suffix}/minmon

ldate=`${scripts}/find_cycle.pl ${suffix} 1 ${tank}`
export PDATE=`${NDATE} +06 $ldate`


export PDY=`echo $PDATE|cut -c1-8`
export cyc=`echo $PDATE|cut -c9-10`

module unload prod_util

export gsistat=${COMIN}/gsistat.gdas.${PDATE}

echo PDATE   = $PDATE
echo gsistat = $gsistat

${scripts}/MinMon_DE.sh ${suffix} ${PDATE} 1>$logdir/MinMon_DE.log 2>$logdir/MinMon_DE.err


