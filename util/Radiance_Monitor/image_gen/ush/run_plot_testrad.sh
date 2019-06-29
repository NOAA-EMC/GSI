#!/bin/sh

#
#  This is test plot script for use on theia
#

set -ax
package=ProdGSI/util/Radiance_Monitor
idev=`cat /etc/dev | cut -c1`
iprod=`cat /etc/prod | cut -c1`

#scripts=/gpfs/${idev}d2/emc/da/noscrub/Edward.Safford/${package}/image_gen/ush
scripts=/scratch4/NCEPDEV/da/noscrub/Edward.Safford/${package}/image_gen/ush

export DO_DATA_RPT=0
export DO_DIAG_RPT=0
#export NDATE=/nwprod/util/exec/ndate
export DO_ARCHIVE=0
export JOB_QUEUE=dev_shared
export NUM_CYCLES=360

export REGIONAL_RR=0
export CYCLE_INTERVAL=6
export TANK_USE_RUN=1
export RUN_TRANSFER=0

export RADMON_SUFFIX=testrad
export RUN=gdas

#data_map=/gpfs/${idev}d2/emc/da/noscrub/Edward.Safford/${package}/parm/data_map.xml
data_map=/scratch4/NCEPDEV/da/noscrub/Edward.Safford/${package}/parm/data_map.xml

#tankdir=/gpfs/${idev}d2/emc/da/noscrub/Edward.Safford/nbns/stats/${RADMON_SUFFIX}
tankdir=/home/Edward.Safford/nbns/stats/${RADMON_SUFFIX}

imgdate=`${scripts}/query_data_map.pl ${data_map} ${RADMON_SUFFIX}_${RUN} imgdate`

#idate=`$NDATE +${CYCLE_INTERVAL} $imgdate`

#prodate=`${scripts}/find_cycle.pl --dir ${tankdir} --cyc 1 --run ${RUN}`
prodate=2018091718
echo "imgdate, prodate = $imgdate, $prodate"

if [[ $idate -le $prodate ]]; then

   logdir=/scratch4/NCEPDEV/stmp4/Edward.Safford/logs/${RADMON_SUFFIX}/${RUN}/radmon
   echo " firing CkPlt_glbl.sh with ${RADMON_SUFFIX} $idate"

   ${scripts}/CkPlt_glbl.sh ${RADMON_SUFFIX} $idate 1>${logdir}/CkPlt_${RADMON_SUFFIX}_${RUN}.log 2>${logdir}/CkPlt_${RADMON_SUFFIX}_${RUN}.err

#   rc=`${scripts}/update_data_map.pl ${data_map} ${RADMON_SUFFIX}_${RUN} imgdate ${idate}`

fi


exit
