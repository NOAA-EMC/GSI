#!/bin/sh

package=MinMon
#package=ProdGSI/util/Minimization_Monitor

suffix=v16rt1
run=gdas

idev=`cat /etc/dev | cut -c1`
iprod=`cat /etc/prod | cut -c1`

   shell=sh
   source /usrx/local/prod/lmod/lmod/init/${shell}

   export MODULEPATH=/usrx/local/prod/lmod/lmod/modulefiles/Core:/usrx/local/prod/modulefiles/core_third:/usrx/local/prod/modulefiles/defs:/gpfs/dell1/nco/ops/nwprod/modulefiles/core_prod:/usrx/local/dev/modulefiles
   module load ips/18.0.1.163
   module load metplus/2.1
   module load lsf/10.1
   module load prod_util/1.1.2
   module load GrADS/2.2.0

echo NDATE = $NDATE
ch=`hostname | cut -c1`

#scripts=/gpfs/${ch}d2/emc/da/noscrub/${LOGNAME}/${package}/image_gen/ush
scripts=/gpfs/dell2/emc/modeling/noscrub/${LOGNAME}/${package}/image_gen/ush

#export NDATE=/gpfs/hps/nco/ops/nwprod/prod_util.v1.0.14/exec/ndate
export DO_ARCHIVE=0
export JOB_QUEUE=dev_shared
export DO_ERROR_RPT=1

export MAIL_CC="russ.treadon@noaa.gov, andrew.collard@noaa.gov"
export MAIL_TO="edward.safford@noaa.gov"

data_map=${scripts}/pen_data_map.xml

tankdir=/u/Edward.Safford/nbns/stats/${suffix}

imgdate=`${scripts}/query_data_map.pl ${data_map} ${suffix}_${run} imgdate`
idate=`$NDATE +6 $imgdate`
PDY=`echo $idate | cut -c1-8`
cyc=`echo $idate | cut -c9-10`

ptmp=/gpfs/dell2/ptmp/Edward.Safford

prodate=`${scripts}/find_cycle.pl --run gdas --cyc 1 --dir ${tankdir}`
echo "imgdate, prodate = $imgdate, $prodate"
if [[ $idate -le $prodate ]]; then

   echo " firing MinMon_Plt.sh"
   ${scripts}/MinMon_Plt.sh ${suffix} -p $idate -r $run  \
	1>${ptmp}/logs/${suffix}/${run}/minmon/IG.${run}.${PDY}.${cyc}.log   \
	2>${ptmp}/logs/${suffix}/${run}/minmon/IG.${run}.${PDY}.${cyc}.err

   rc=`${scripts}/update_data_map.pl ${data_map} ${suffix}_${run} imgdate ${idate}`

fi


exit
