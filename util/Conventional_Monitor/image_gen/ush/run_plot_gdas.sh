#!/bin/sh

package=CMon_486
idev=`cat /etc/dev | cut -c1`
iprod=`cat /etc/prod | cut -c1`

scripts=/gpfs/${idev}d2/emc/da/noscrub/${USER}/${package}/util/Conventional_Monitor/image_gen/ush
suffix=GDAS

export DO_DATA_RPT=1
export DO_DIAG_RPT=1
export NDATE=/nwprod/util/exec/ndate
export DO_ARCHIVE=1
export JOB_QUEUE=dev_shared
export NUM_CYCLES=120
#export MAIL_CC="russ.treadon@noaa.gov, john.derber@noaa.gov, andrew.collard@noaa.gov"
export MAIL_CC="edward.c.safford@gmail.com"

export CYCLE_INTERVAL=6

data_map=${scripts}/data_map.xml

tankdir=/gpfs/${idev}d2/emc/da/noscrub/${USER}/nbns/stats/${suffix}

imgdate=`${scripts}/query_data_map.pl ${data_map} ${suffix} imgdate`
idate=`$NDATE +${CYCLE_INTERVAL} $imgdate`

prodate=`${scripts}/find_cycle.pl 1 ${tankdir}`
echo "imgdate, prodate = $imgdate, $prodate"
if [[ $idate -le $prodate ]]; then

   echo " firing CMon_IG.err"
   ${scripts}/CMon_IG.sh ${suffix} $idate  1>/ptmpp1/${USER}/logs/${suffix}/ConMon/CMon_IG.log 2>/ptmpp1/${USER}/logs/${suffix}/ConMon/CMon_IG.err

   rc=`${scripts}/update_data_map.pl ${data_map} ${suffix} imgdate ${idate}`

fi


exit
