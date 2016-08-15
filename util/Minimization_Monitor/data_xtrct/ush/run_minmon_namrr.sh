#!/bin/sh

scripts=/gpfs/gd2/emc/da/noscrub/Edward.Safford/MinMon_585/util/Minimization_Monitor/data_xtrct/ush
export jobfile=/gpfs/gd2/emc/da/noscrub/Edward.Safford/MinMon_585/util/Minimization_Monitor/nwprod/nam_minmon.v1.0.0/jobs/JNAM_MINMON

NDATE=/nwprod/util/exec/ndate

export RUN=namrr
export MINMON_SUFFIX=namrr
export PROJECT=NDAS-T2O
export GLB_AREA=0
export CYCLE_INTERVAL=1

export M_TANKverf=~/nbns
tank=~/nbns/stats/regional/namrr
ldate=`${scripts}/find_cycle.pl ${MINMON_SUFFIX} 1 ${tank}`

#export PDATE=2016080300
export PDATE=`${NDATE} +${CYCLE_INTERVAL} $ldate`
export PDY=`echo $PDATE|cut -c1-8`
export cyc=`echo $PDATE|cut -c9-10`

export COMROOT=/ptmpd3/Eric.Rogers/com/namrr/para/
export COM_IN=/ptmpd3/Eric.Rogers/com/namrr/para/

export REGIONAL_RR=1

${scripts}/MinMon_DE.sh ${MINMON_SUFFIX} ${PDATE} 1>log 2>err

