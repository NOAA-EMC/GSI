#!/bin/sh

package=ProdGSI/util/Ozone_Monitor
#package=OznMon

suffix=GFS
run=gdas

idev=`cat /etc/dev | cut -c1`
iprod=`cat /etc/prod | cut -c1`

scripts=/gpfs/${idev}d2/emc/da/noscrub/Edward.Safford/${package}/image_gen/ush
data_map=${scripts}/data_map.xml

export NDATE=/nwprod/util/exec/ndate

export MAIL_CC="russ.treadon@noaa.gov, andrew.collard@noaa.gov, haixia.liu@noaa.gov"
#export MAIL_CC="edward.c.safford@gmail.com"
export MAIL_TO="edward.safford@noaa.gov"

export OZN_USE_RUN=1

export CYCLE_INTERVAL=6

tankdir=/gpfs/${idev}d2/emc/da/noscrub/Edward.Safford/nbns/stats/${suffix}
imgdate=`${scripts}/query_data_map.pl ${data_map} ${suffix}_${run} imgdate`
idate=`$NDATE +${CYCLE_INTERVAL} $imgdate`

prodate=`${scripts}/find_cycle.pl -run ${run} -cyc 1 -dir ${tankdir}`
echo "imgdate, idate, prodate = $imgdate, $idate, $prodate"
if [[ $idate -le $prodate ]]; then

   echo " firing OznMon_Plt.sh"
   ${scripts}/OznMon_Plt.sh $suffix -p $idate -r $run  \
      1>/ptmpd1/Edward.Safford/logs/${suffix}/${run}/oznmon/OznMon_Plt.log \
      2>/ptmpd1/Edward.Safford/logs/${suffix}/${run}/oznmon/OznMon_Plt.err

   rc=`${scripts}/update_data_map.pl ${data_map} \
      ${suffix}_${run} imgdate ${idate}`

   echo "rc from update_data_map.pl = $rc"

fi

exit
