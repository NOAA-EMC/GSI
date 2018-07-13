#!/bin/sh

package=ProdGSI/util/Ozone_Monitor
#package=OznMon

ozn_suffix=fv3rt1
run=gdas
my_machine=wcoss

if [[ $my_machine = "cray" ]]; then
  
   . /opt/modules/3.2.6.7/init/sh
   module use -a /gpfs/hps/nco/ops/nwprod/modulefiles
   module load prod_util
else
   export NDATE=/nwprod/util/exec/ndate
fi


echo "NDATE = $NDATE"


#idev=`cat /etc/dev | cut -c1`
#iprod=`cat /etc/prod | cut -c1`

scripts=/gpfs/td2/emc/da/noscrub/Edward.Safford/${package}/image_gen/ush
data_map=${scripts}/data_map.xml


export JOB_QUEUE=dev
#export MAIL_CC="russ.treadon@noaa.gov, andrew.collard@noaa.gov, haixia.liu@noaa.gov"
#export MAIL_CC="edward.c.safford@gmail.com"
export MAIL_TO="edward.safford@noaa.gov"

export OZN_USE_RUN=1

export CYCLE_INTERVAL=6

tankdir=/u/Edward.Safford/nbns/stats/${ozn_suffix}
imgdate=`${scripts}/query_data_map.pl ${data_map} ${ozn_suffix}_${run} imgdate`
idate=`$NDATE +${CYCLE_INTERVAL} $imgdate`
#idate=2018030906

logdir=/ptmpp1/Edward.Safford/logs

prodate=`${scripts}/find_cycle.pl -run ${run} -cyc 1 -dir ${tankdir}`
echo "imgdate, idate, prodate = $imgdate, $idate, $prodate"

if [[ $idate -le $prodate ]]; then

   echo " firing OznMon_Plt.sh"
   ${scripts}/OznMon_Plt.sh $ozn_suffix -p $idate -r $run  \
      1>${logdir}/${ozn_suffix}/${run}/oznmon/OznMon_Plt.log \
      2>${logdir}/${ozn_suffix}/${run}/oznmon/OznMon_Plt.err

   rc=`${scripts}/update_data_map.pl ${data_map} \
       ${ozn_suffix}_${run} imgdate ${idate}`

   echo "rc from update_data_map.pl = $rc"

fi

exit
