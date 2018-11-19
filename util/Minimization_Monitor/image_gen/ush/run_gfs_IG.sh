#!/bin/sh

set -ax

#package=MinMon
package=ProdGSI/util/Minimization_Monitor

suffix=GFS
run=gfs

idev=`cat /etc/dev | cut -c1`
iprod=`cat /etc/prod | cut -c1`

. /usrx/local/Modules/3.2.9/init/ksh
module use /nwprod2/modulefiles
module load prod_util
module load GrADS

echo NDATE = $NDATE
ch=`hostname | cut -c1`

scripts=/gpfs/${ch}d2/emc/da/noscrub/${LOGNAME}/${package}/image_gen/ush
ptmp=/ptmpd1/Edward.Safford

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

prodate=`${scripts}/find_cycle.pl --run gfs --cyc 1 --dir ${tankdir}`
echo "imgdate, prodate = $imgdate, $prodate"
if [[ $idate -le $prodate ]]; then

   echo " firing MinMon_Plt.sh"
   ${scripts}/MinMon_Plt.sh ${suffix} -p $idate -r $run  \
	1>${ptmp}/logs/${suffix}/${run}/minmon/IG.${run}.${PDY}.${cyc}.log   \
	2>${ptmp}/logs/${suffix}/${run}/minmon/IG.${run}.${PDY}.${cyc}.err

   rc=`${scripts}/update_data_map.pl ${data_map} ${suffix}_${run} imgdate ${idate}`

fi


exit
