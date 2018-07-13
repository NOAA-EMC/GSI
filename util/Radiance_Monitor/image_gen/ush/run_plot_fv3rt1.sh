#!/bin/sh

package=ProdGSI/util/Radiance_Monitor
suffix=fv3rt1
export RUN=gdas

idev=`cat /etc/dev | cut -c1`
iprod=`cat /etc/prod | cut -c1`

scripts=/gpfs/${idev}d2/emc/da/noscrub/Edward.Safford/${package}/image_gen/ush

export USE_STATIC_SATYPE=1
export DO_DATA_RPT=1
export DO_DIAG_RPT=1
export NDATE=/nwprod/util/exec/ndate
#export DO_ARCHIVE=1
export JOB_QUEUE=dev_shared
export NUM_CYCLES=360
#export MAIL_CC="russ.treadon@noaa.gov, john.derber@noaa.gov, andrew.collard@noaa.gov"
export MAIL_CC="russ.treadon@noaa.gov, andrew.collard@noaa.gov"
#export MAIL_CC="edward.c.safford@gmail.com"
export MAIL_TO="edward.safford@noaa.gov"

export REGIONAL_RR=0
export CYCLE_INTERVAL=6
export TANK_USE_RUN=1
export RUN_TRANSFER=1

data_map=/gpfs/${idev}d2/emc/da/noscrub/Edward.Safford/${package}/parm/data_map.xml

TANKverf=/u/Edward.Safford/nbns/stats/fv3rt1

imgdate=`${scripts}/query_data_map.pl ${data_map} fv3rt1 imgdate`
idate=`$NDATE +${CYCLE_INTERVAL} $imgdate`
#idate=2018041118

prodate=`${scripts}/find_cycle.pl --cyc 1 --dir ${TANKverf} --run ${RUN}`
echo "imgdate, prodate = $imgdate, $prodate"

logdir="/ptmpp1/${LOGNAME}/logs/${suffix}/${RUN}/radmon"
if [[ $idate -le $prodate ]]; then

   echo " firing CkPlt_glbl.sh"
   ${scripts}/CkPlt_glbl.sh $suffix $idate  \
	1>${logdir}/CkPlt_fv3rt1.log \
	2>${logdir}/CkPlt_fv3rt1.err

   rc=`${scripts}/update_data_map.pl ${data_map} fv3rt1 imgdate ${idate}`

fi


exit
