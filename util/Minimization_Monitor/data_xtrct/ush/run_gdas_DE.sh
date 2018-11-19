#!/bin/sh

ch=`hostname | cut -c1`

#package=MinMon
package=ProdGSI/util/Minimization_Monitor

suffix=GFS
export RUN=gdas

scripts=/gpfs/${ch}d2/emc/da/noscrub/${USER}/${package}/data_xtrct/ush
export jobfile=/gpfs/${ch}d2/emc/da/noscrub/${USER}/${package}/nwprod/gdas.v1.0.0/jobs/JGDAS_VMINMON

. /usrx/local/Modules/3.2.9/init/sh
module use -a /gpfs/hps/nco/ops/nwprod/modulefiles
module load prod_util

#NDATE=/nwprod/util/exec/ndate


tank=~/nbns/stats/${suffix}
ldate=`${scripts}/find_cycle.pl ${suffix} --cyc 1 --dir ${tank} --run ${RUN}`

export PDATE=`${NDATE} +06 $ldate`
#export PDATE=2018102800

export PDY=`echo $PDATE|cut -c1-8`
export cyc=`echo $PDATE|cut -c9-10`


export COMIN=/gpfs/hps/nco/ops/com/gfs/prod/${RUN}.${PDY}
export gsistat=${COMIN}/${RUN}.t${cyc}z.gsistat

tank=~/nbns/stats/${suffix}
logdir=/ptmpd1/Edward.Safford/logs/${suffix}/${RUN}/minmon

echo PDATE   = $PDATE
echo gsistat = $gsistat

#${scripts}/MinMon_DE.sh ${suffix} ${PDATE}
${scripts}/MinMon_DE.sh ${suffix} --pdate ${PDATE} --run ${RUN} 1>$logdir/MinMon_DE.log 2>$logdir/MinMon_DE.err
