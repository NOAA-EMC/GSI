#!/bin/sh

package=ProdGSI/util/Ozone_Monitor
#package=OznMon

suffix=GFS
run=gdas

DO_COMP=1
COMP1=sbuv2_n19
COMP2=ompsnp_npp

idev=`cat /etc/dev | cut -c1`
iprod=`cat /etc/prod | cut -c1`

#scripts=/gpfs/${idev}d2/emc/da/noscrub/Edward.Safford/${package}/image_gen/ush
#scripts=/scratch4/NCEPDEV/da/noscrub/Edward.Safford/${package}/image_gen/ush
scripts=/gpfs/dell2/emc/modeling/noscrub/Edward.Safford/${package}/image_gen/ush

data_map=${scripts}/data_map.xml

#export NDATE=/nwprod/util/exec/ndate
export NDATE=/gpfs/dell1/nco/ops/nwprod/prod_util.v1.1.0/exec/ips/ndate

export MAIL_CC="russ.treadon@noaa.gov, andrew.collard@noaa.gov, haixia.liu@noaa.gov"
export MAIL_TO="edward.safford@noaa.gov"

export OZN_USE_RUN=1

export CYCLE_INTERVAL=6

#tankdir=/gpfs/${idev}d2/emc/da/noscrub/Edward.Safford/nbns/stats/${suffix}
#tankdir=/scratch4/NCEPDEV/da/save/Edward.Safford/nbns/stats/${suffix}
tankdir=/u/Edward.Safford/nbns/stats/${suffix}

imgdate=`${scripts}/query_data_map.pl ${data_map} ${suffix}_${run} imgdate`
idate=`$NDATE +${CYCLE_INTERVAL} $imgdate`
#idate=2019071306

prodate=`${scripts}/find_cycle.pl -run ${run} -cyc 1 -dir ${tankdir}`

logdir=/gpfs/dell2/ptmp/Edward.Safford/logs

echo "imgdate, idate, prodate = $imgdate, $idate, $prodate"
if [[ $idate -le $prodate ]]; then

   echo " firing OznMon_Plt.sh"

   if [[ $DO_COMP -eq 1 ]]; then
      ${scripts}/OznMon_Plt.sh $suffix -p $idate -r $run \
                -c1 $COMP1 -c2 $COMP2 \
         1>${logdir}/${suffix}/${run}/oznmon/OznMon_Plt.log \
         2>${logdir}/${suffix}/${run}/oznmon/OznMon_Plt.err

   else
      ${scripts}/OznMon_Plt.sh $suffix -p $idate -r $run \
         1>${logdir}/${suffix}/${run}/oznmon/OznMon_Plt.log \
         2>${logdir}/${suffix}/${run}/oznmon/OznMon_Plt.err
   fi

   rc=`${scripts}/update_data_map.pl ${data_map} \
      ${suffix}_${run} imgdate ${idate}`

   echo "rc from update_data_map.pl = $rc"

fi

exit
