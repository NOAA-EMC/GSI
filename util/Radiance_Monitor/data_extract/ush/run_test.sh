#!/bin/sh

suffix=testrad
RUN=gdas
tank=/home/${LOGNAME}/nbns/stats/${suffix}

export RADSTAT_LOCATION=/scratch4/NCEPDEV/da/noscrub/Edward.Safford/test_data
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
export jlogfile=/scratch4/NCEPDEV/stmp4/Edward.Safford/logs/${suffix}/radmon/DE.log

package=ProdGSI/util/Radiance_Monitor
#idev=`cat /etc/dev | cut -c1`
#iprod=`cat /etc/prod | cut -c1`

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
#NDATE=/scratch4/NCEPDEV/global/save/glopara/nwpara/util/exec/ndate

#ldate=`${scripts}/find_cycle.pl --run $RUN --cyc 1 --dir $tank`
#pdate=`${NDATE} +6 ${ldate}`
pdate=2018091700

pdy=`echo $pdate | cut -c1-8`
cyc=`echo $pdate | cut -c9-10`

export RADSTAT_LOCATION=${RADSTAT_LOCATION}/gdas.${pdy}/${cyc}
echo RADSTAT_LOCATION= $RADSTAT_LOCATION
logs=/scratch4/NCEPDEV/stmp4/Edward.Safford/logs/${suffix}

${scripts}/VrfyRad_glbl.sh ${suffix} ${pdate} 1>${logs}/${RUN}/radmon/VrfyRad.log 2>${logs}/${RUN}/radmon/VrfyRad.err

exit
