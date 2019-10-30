#!/bin/bash 

suffix=control_V02
RUN=gdas
tank=/home/${LOGNAME}/nbns/stats/${suffix}

export RADSTAT_LOCATION=/scratch4/NAGAPE/jcsda-datagap/Erin.Jones/ROTDIRS/noscrub/archive/control_V02/radstat

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
#idev=`cat /etc/dev | cut -c1`
#iprod=`cat /etc/prod | cut -c1`

#scripts=/gpfs/${idev}d2/emc/da/noscrub/Edward.Safford/${package}/data_extract/ush
scripts=/scratch4/NCEPDEV/da/noscrub/Edward.Safford/${package}/data_extract/ush

#--------------------------------------------------------------------
#  Check for my monitoring use.  Abort if running on prod machine.
#--------------------------------------------------------------------

#is_prod=`${scripts}/onprod.sh`
#if [[ $is_prod = 1 ]]; then
#   exit 10
#fi

#--------------------------------------------------------------------
#NDATE=/nwprod/util/exec/ndate
NDATE=/home/Edward.Safford/bin/ndate

ldate=`${scripts}/nu_find_cycle.pl --run $RUN --cyc 1 --dir $tank`
echo "ldate = $ldate"

pdate=`${NDATE} +6 ${ldate}`
#pdate=2018120100
echo "processing pdate = $pdate"

pdy=`echo $pdate | cut -c1-8`
cyc=`echo $pdate | cut -c9-10`

export RADSTAT_LOCATION=${RADSTAT_LOCATION}/gdas.${pdy}/${cyc}
echo RADSTAT_LOCATION= $RADSTAT_LOCATION
ptmp_user=/scratch4/NCEPDEV/stmp4/Edward.Safford

${scripts}/VrfyRad_glbl.sh ${suffix} ${pdate} 1>${ptmp_user}/logs/${suffix}/${RUN}/radmon/VrfyRad.log 2>${ptmp_user}/logs/${suffix}/${RUN}/radmon/VrfyRad.err

exit
