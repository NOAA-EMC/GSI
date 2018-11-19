#!/bin/sh

idev=`cat /etc/dev | cut -c1`
iprod=`cat /etc/prod | cut -c1`

#package=MinMon
package=ProdGSI/util/Minimization_Monitor
suffix=fv3rt1
export run=gdas

echo "user = $USER"

export KEEPDATA=YES
export DO_ERROR_RPT=1
export MAIL_TO=""
export MAIL_CC=""

scripts=/gpfs/gd2/emc/da/noscrub/${USER}/${package}/data_xtrct/ush
echo "scripts = $scripts"

export jobfile=/gpfs/gd2/emc/da/noscrub/${USER}/${package}/nwprod/gdas.v1.0.0/jobs/JGDAS_VMINMON
echo "jobfile = $jobfile"

#export MODULESHOME=/opt/modules/3.2.6.7
#. $MODULESHOME/init/sh

. /usrx/local/Modules/3.2.9/init/ksh
module use /nwprod2/modulefiles
module load prod_util

echo NDATE = $NDATE

#export COMIN=/gpfs/hps3/emc/global/noscrub/emc.glopara/archive/prfv3rt1
export COMIN=/gpfs/dell3/ptmp/emc.glopara/ROTDIRS/prfv3rt1

tank=~/nbns/stats/${suffix}
logdir=/ptmpd1/Edward.Safford/logs/${suffix}/${run}/minmon
#export LOGdir=${logdir}

ldate=`${scripts}/find_cycle.pl --cyc 1 --dir ${tank} --run ${run}`
echo ldate = $ldate
export PDATE=`${NDATE} +06 $ldate`
#PDATE=2018101818

export PDY=`echo $PDATE|cut -c1-8`
export cyc=`echo $PDATE|cut -c9-10`

#module unload prod_util

export gsistat=${COMIN}/gdas.${PDY}/${cyc}/gdas.t${cyc}z.gsistat

echo PDATE   = $PDATE
echo gsistat = $gsistat

${scripts}/MinMon_DE.sh ${suffix} --pdate ${PDATE} 1>$logdir/MinMon_DE.log 2>$logdir/MinMon_DE.err


