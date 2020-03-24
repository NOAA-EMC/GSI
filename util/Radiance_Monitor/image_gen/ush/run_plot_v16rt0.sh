#!/bin/sh

module load ips/18.0.1.163
module load metplus/2.1
module load prod_util/1.1.2

package=ProdGSI/util/Radiance_Monitor
suffix=v16rt0
export RUN=gdas

idev=`cat /etc/dev | cut -c1`
iprod=`cat /etc/prod | cut -c1`

scripts=/gpfs/dell2/emc/modeling/noscrub/Edward.Safford/RadMon/image_gen/ush

export USE_STATIC_SATYPE=1
export DO_DATA_RPT=1
export DO_DIAG_RPT=1
#export NDATE=/nwprod/util/exec/ndate
#export DO_ARCHIVE=1
export JOB_QUEUE=dev_shared
export NUM_CYCLES=360
#export MAIL_CC="russ.treadon@noaa.gov, andrew.collard@noaa.gov"
#export MAIL_CC="edward.c.safford@gmail.com"
export MAIL_TO="edward.safford@noaa.gov"

export REGIONAL_RR=0
export CYCLE_INTERVAL=6
export TANK_USE_RUN=1
export RUN_TRANSFER=0

data_map=${scripts}/data_map.xml

TANKverf=/u/Edward.Safford/nbns/stats/${suffix}

#imgdate=`${scripts}/query_data_map.pl ${data_map} ${suffix} imgdate`
#idate=`$NDATE +${CYCLE_INTERVAL} $imgdate`
idate=2018123118

prodate=`${scripts}/nu_find_cycle.pl --cyc 1 --dir ${TANKverf} --run ${RUN}`
echo "imgdate, prodate = $imgdate, $prodate"

logdir="/gpfs/dell2/ptmp/Edward.Safford/logs/${suffix}/${RUN}/radmon"
if [[ $idate -le $prodate ]]; then

   echo " firing CkPlt_glbl.sh"
   ${scripts}/CkPlt_glbl.sh $suffix $idate  \
	1>${logdir}/CkPlt_${suffix}.log \
	2>${logdir}/CkPlt_${suffix}.err

#   rc=`${scripts}/update_data_map.pl ${data_map} ${suffix} imgdate ${idate}`

fi


exit
