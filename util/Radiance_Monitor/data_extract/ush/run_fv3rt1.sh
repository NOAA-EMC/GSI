#!/bin/sh

suffix=fv3rt1
RUN=gdas
tank=/u/${LOGNAME}/nbns/stats/${suffix}

export RADSTAT_LOCATION=/gpfs/hps3/ptmp/emc.glopara/ROTDIRS/prfv3rt1
export PROJECT=GDAS-T2O
export USE_ANL=1
export JOB_QUEUE=dev_shared
export DO_DIAG_RPT=1
export DO_DATA_RPT=1
export RAD_AREA=glb
export CYCLE_INTERVAL=6
export REGIONAL_RR=0
export TANK_USE_RUN=1

export COMROOT=/com
export jlogfile=/ptmpp1/$LOGNAME/logs/fv3rt1/radmon/DE.log

package=ProdGSI/util/Radiance_Monitor
idev=`cat /etc/dev | cut -c1`
iprod=`cat /etc/prod | cut -c1`

scripts=/gpfs/${idev}d2/emc/da/noscrub/Edward.Safford/${package}/data_extract/ush


#--------------------------------------------------------------------
#  Check for my monitoring use.  Abort if running on prod machine.
#--------------------------------------------------------------------

is_prod=`${scripts}/onprod.sh`
if [[ $is_prod = 1 ]]; then
   exit 10
fi

#--------------------------------------------------------------------
NDATE=/nwprod/util/exec/ndate

ldate=`${scripts}/find_cycle.pl --run $RUN --cyc 1 --dir $tank`
pdate=`${NDATE} +6 ${ldate}`
#pdate=2018041118

pdy=`echo $pdate | cut -c1-8`
cyc=`echo $pdate | cut -c9-10`

export RADSTAT_LOCATION=${RADSTAT_LOCATION}/gdas.${pdy}/${cyc}
echo RADSTAT_LOCATION= $RADSTAT_LOCATION

/gpfs/${idev}d2/emc/da/noscrub/Edward.Safford/${package}/data_extract/ush/VrfyRad_glbl.sh ${suffix} ${pdate} 1>/ptmpp1/Edward.Safford/logs/${suffix}/${RUN}/radmon/VrfyRad.log 2>/ptmpp1/Edward.Safford/logs/${suffix}/${RUN}/radmon/VrfyRad.err

exit
