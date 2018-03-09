#!/bin/sh

package=ProdGSI
suffix=fv3rt1
run=gdas

idev=`cat /etc/dev | cut -c1`
iprod=`cat /etc/prod | cut -c1`

. /opt/modules/3.2.6.7/init/sh
module use -a /gpfs/hps/nco/ops/nwprod/modulefiles
module use -a /usrx/local/dev/modulefiles
module load GrADS

scripts=/gpfs/hps3/emc/da/noscrub/${LOGNAME}/${package}/util/Minimization_Monitor/image_gen/ush
ptmp=/gpfs/hps2/ptmp/Edward.Safford

export NDATE=/gpfs/hps/nco/ops/nwprod/prod_util.v1.0.14/exec/ndate
export DO_ARCHIVE=0
export JOB_QUEUE=dev_shared
export DO_ERROR_RPT=1

#export MAIL_CC="russ.treadon@noaa.gov, john.derber@noaa.gov, andrew.collard@noaa.gov"
export MAIL_CC="edward.c.safford@gmail.com"

data_map=${scripts}/pen_data_map.xml

tankdir=/u/Edward.Safford/nbns/stats/${suffix}
#tankdir=/gpfs/hps/emc/global/noscrub/emc.glopara/minmon/stats/${suffix}

imgdate=`${scripts}/query_data_map.pl ${data_map} ${suffix}_${run} imgdate`
idate=`$NDATE +6 $imgdate`
PDY=`echo $idate | cut -c1-8`
cyc=`echo $idate | cut -c9-10`

prodate=`${scripts}/find_cycle.pl ${suffix} 1 ${tankdir}`
echo "imgdate, prodate = $imgdate, $prodate"
if [[ $idate -le $prodate ]]; then

   echo " firing MinMon_Plt.sh"
   ${scripts}/MinMon_Plt.sh ${suffix} -p $idate -r $run  \
	1>${ptmp}/logs/${suffix}/minmon/IG.${run}.${PDY}.${cyc}.log   \
	2>${ptmp}/logs/${suffix}/minmon/IG.${run}.${PDY}.${cyc}.err

   rc=`${scripts}/update_data_map.pl ${data_map} ${suffix}_${run} imgdate ${idate}`

fi


exit
