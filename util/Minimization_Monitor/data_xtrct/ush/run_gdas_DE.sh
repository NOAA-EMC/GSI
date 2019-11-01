#!/bin/sh

ch=`hostname | cut -c1`

#package=MinMon
package=ProdGSI/util/Minimization_Monitor

suffix=GFS
net=gfs
export RUN=gdas

scripts=/gpfs/dell2/emc/modeling/noscrub/${USER}/${package}/data_xtrct/ush
export jobfile=/gpfs/dell2/emc/modeling/noscrub/${USER}/${package}/nwprod/gdas.v1.0.0/jobs/JGDAS_VMINMON


NDATE=/gpfs/dell1/nco/ops/nwprod/prod_util.v1.1.0/exec/ips/ndate

tank=~/nbns/stats/${suffix}
ldate=`${scripts}/find_cycle.pl ${suffix} --cyc 1 --dir ${tank} --run ${RUN}`

export PDATE=`${NDATE} +06 $ldate`
#export PDATE=2019083018

export PDY=`echo $PDATE|cut -c1-8`
export cyc=`echo $PDATE|cut -c9-10`


export COMIN=/gpfs/dell1/nco/ops/com/${net}/prod/${RUN}.${PDY}/${cyc} 
export gsistat=${COMIN}/${RUN}.t${cyc}z.gsistat

tank=~/nbns/stats/${suffix}
logdir=/gpfs/dell2/ptmp/Edward.Safford/logs/${suffix}/${RUN}/minmon

echo PDATE   = $PDATE
echo gsistat = $gsistat

${scripts}/MinMon_DE.sh ${suffix} --pdate ${PDATE} --run ${RUN} 1>$logdir/MinMon_DE.log 2>$logdir/MinMon_DE.err
