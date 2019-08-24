#!/bin/sh

set -xa

OZN_SUFFIX=GFS
run=gdas

NET=gfs
envir=prod

MY_MACHINE=wcoss_d
package="ProdGSI/util/Ozone_Monitor"
#package="OznMon"

if [[ $MY_MACHINE = "cray" ]]; then
   . /opt/modules/3.2.6.7/init/sh
   module use -a /gpfs/hps/nco/ops/nwprod/modulefiles
elif [[ $MY_MACHINE = "wcoss" ]]; then
   shell=sh
   . /usrx/local/Modules/default/init/${shell}
fi

if [[ $MY_MACHINE != "theia" ]]; then
   module load prod_util
fi

if [[ $MY_MACHINE = "theia" ]]; then
   scripts=/scratch4/NCEPDEV/da/noscrub/Edward.Safford/ProdGSI/util/Ozone_Monitor/data_xtrct/ush
elif [[ $MY_MACHINE = "wcoss" ]]; then
   scripts=/gpfs/gd2/emc/da/noscrub/Edward.Safford/${package}/data_xtrct/ush
elif [[ $MY_MACHINE = "wcoss_d" ]]; then
   scripts=/gpfs/dell2/emc/verification/noscrub/Edward.Safford/ProdGSI/util/Ozone_Monitor/data_xtrct/ush
elif [[ $MY_MACHINE = "cray" ]]; then
   scripts=/gpfs/hps3/emc/da/noscrub/Edward.Safford/ProdGSI/util/Ozone_Monitor/data_xtrct/ush
fi

idate=`${scripts}/find_cycle.pl -dir ~/nbns/stats/${OZN_SUFFIX} -cyc 1 -run ${run}`
echo "idate = $idate"

#export START_DATE=2019052300
export START_DATE=`${NDATE} +06 $idate`

PDY=`echo $START_DATE | cut -c1-8`
cyc=`echo $START_DATE | cut -c9-10`

export COM_IN=/gpfs/hps/nco/ops/com/${NET}/${envir}/${run}.${PDY}
#export COM_IN=/scratch4/NCEPDEV/da/noscrub/Edward.Safford/test_data

export oznstat=${COM_IN}/${run}.t${cyc}z.oznstat

export OZN_TANKDIR=/u/${LOGNAME}/nbns
#export OZN_TANKDIR=/home/${LOGNAME}/nbns

#log=/ptmpd1/Edward.Safford/logs/${OZN_SUFFIX}/${run}/oznmon/OznMon_DE.log
log=./log
#err=/ptmpd1/Edward.Safford/logs/${OZN_SUFFIX}/${run}/oznmon/OznMon_DE.err
err=./err

${scripts}/OznMon_DE.sh $OZN_SUFFIX -p $START_DATE -r gdas 1>$log 2>$err
