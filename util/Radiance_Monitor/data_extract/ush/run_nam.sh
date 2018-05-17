#!/bin/sh

export RADSTAT_LOCATION=/com2/nam/prod
export PROJECT=NDAS-T2O
export JOB_QUEUE=dev_shared

export USE_ANL=1
export DO_DIAG_RPT=1
export DO_DATA_RPT=1

export RAD_AREA=rgn
export REGIONAL_RR=1
export CYCLE_INTERVAL=1
export TANK_USE_RUN=0

package=ProdGSI/util/Radiance_Monitor
idev=`cat /etc/dev | cut -c1`
iprod=`cat /etc/prod | cut -c1`

export COMROOT=/com
export jlogfile=/ptmpp1/$LOGNAME/logs/nam/radmon/run_nrx.log

scripts=/gpfs/${idev}d2/emc/da/noscrub/Edward.Safford/${package}/data_extract/ush


#--------------------------------------------------------------------
#  Check for my monitoring use.  Abort if running on prod machine.
#--------------------------------------------------------------------

is_prod=`${scripts}/onprod.sh`
if [[ $is_prod = 1 ]]; then
   exit 10
fi

#--------------------------------------------------------------------


${scripts}/VrfyRad_rgnl.sh nam 1>/ptmpp1/Edward.Safford/logs/nam/radmon/VrfyRad_nam.log 2>/ptmpp1/Edward.Safford/logs/nam/radmon/VrfyRad_nam.err

exit
