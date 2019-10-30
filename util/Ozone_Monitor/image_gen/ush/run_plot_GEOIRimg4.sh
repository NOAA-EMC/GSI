#!/bin/sh
set -ax

package=ProdGSI/util/Ozone_Monitor
#package=OznMon

DO_COMP=1
COMP1=sbuv2_n19
COMP2=ompsnp_npp

ozn_suffix=GEOIRimg4
run=gdas

my_machine=wcoss_d


export NDATE=/gpfs/dell1/nco/ops/nwprod/prod_util.v1.1.0/exec/ips/ndate

echo "NDATE = $NDATE"

scripts=/gpfs/dell2/emc/modeling/noscrub/Edward.Safford/${package}/image_gen/ush
data_map=${scripts}/data_map.xml

export JOB_QUEUE=dev
export MAIL_TO="edward.safford@noaa.gov"

export OZN_USE_RUN=1

export CYCLE_INTERVAL=6

tankdir=/u/Edward.Safford/nbns/stats/${ozn_suffix}
#imgdate=`${scripts}/query_data_map.pl ${data_map} ${ozn_suffix}_${run} imgdate`
#idate=`$NDATE +${CYCLE_INTERVAL} $imgdate`

logdir=/gpfs/dell2/ptmp/Edward.Safford/logs

#export SATYPE=`cat ${tankdir}/info/gdas_oznmon_satype.txt`

prodate=`${scripts}/find_cycle.pl -run ${run} -cyc 1 -dir ${tankdir}`
echo "imgdate, idate, prodate = $imgdate, $idate, $prodate"
idate=2019063018

if [[ $idate -le $prodate ]]; then

   echo " firing OznMon_Plt.sh"

   if [[ $DO_COMP -eq 1 ]]; then
      ${scripts}/OznMon_Plt.sh $ozn_suffix -p $idate -r $run \
         	-c1 $COMP1 -c2 $COMP2 \
         1>${logdir}/${ozn_suffix}/${run}/oznmon/OznMon_Plt.log \
         2>${logdir}/${ozn_suffix}/${run}/oznmon/OznMon_Plt.err
   else

      ${scripts}/OznMon_Plt.sh $ozn_suffix -p $idate -r $run \
         1>${logdir}/${ozn_suffix}/${run}/oznmon/OznMon_Plt.log \
         2>${logdir}/${ozn_suffix}/${run}/oznmon/OznMon_Plt.err
   fi

   rc=`${scripts}/update_data_map.pl ${data_map} \
       ${ozn_suffix}_${run} imgdate ${idate}`

   echo "rc from update_data_map.pl = $rc"

fi

exit
