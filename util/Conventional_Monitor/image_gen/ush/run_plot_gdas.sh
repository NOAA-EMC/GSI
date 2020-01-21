#!/bin/sh

package=ProdGSI

idev=`cat /etc/dev | cut -c1`
iprod=`cat /etc/prod | cut -c1`

#scripts=/gpfs/${idev}d2/emc/da/noscrub/${USER}/${package}/util/Conventional_Monitor/image_gen/ush
scripts=/gpfs/dell2/emc/modeling/noscrub/Edward.Safford/ProdGSI/util/Conventional_Monitor/image_gen/ush
echo "scripts = $scripts"

suffix=GFS
run=gdas

export DO_DATA_RPT=1
export DO_DIAG_RPT=1
#export NDATE=/nwprod/util/exec/ndate
export NDATE=/gpfs/dell1/nco/ops/nwprod/prod_util.v1.1.3/exec/ips/ndate
export DO_ARCHIVE=1
export JOB_QUEUE=dev_shared
#export NUM_CYCLES=120
export NUM_CYCLES=30
#export MAIL_CC="russ.treadon@noaa.gov, john.derber@noaa.gov, andrew.collard@noaa.gov"
export MAIL_CC="edward.c.safford@gmail.com"

export CYCLE_INTERVAL=6

data_map=${scripts}/data_map.xml

#tankdir=/gpfs/${idev}d2/emc/da/noscrub/${USER}/nbns/stats/${suffix}
tankdir=/u/${USER}/nbns/stats/${suffix}

imgdate=`${scripts}/query_data_map.pl ${data_map} ${suffix}_${run} imgdate`
idate=`$NDATE +${CYCLE_INTERVAL} $imgdate`

prodate=`${scripts}/find_cycle.pl --cyc 1 --dir ${tankdir} --run ${run}`
echo "imgdate, idate, prodate = $imgdate, $idate, $prodate"
if [[ $idate -le $prodate ]]; then

   logdir=/gpfs/dell2/ptmp/${USER}/logs/${suffix}/${run}/conmon
   echo "logdir = $logdir"
   if [[ ! -d ${logdir} ]]; then
      mkdir -p ${logdir}
   fi

   echo " firing ConMon_IG.sh"
   ${scripts}/ConMon_IG.sh ${suffix} --pdate ${idate} --run ${run} \
	1>${logdir}/ConMon_IG.log \
	2>${logdir}/ConMon_IG.err

   rc=`${scripts}/update_data_map.pl ${data_map} ${suffix}_${run} imgdate ${idate}`

fi


exit
