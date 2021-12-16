#!/bin/sh

set -xa

OZN_SUFFIX=GFS
run=gdas

NET=gfs
envir=prod

MY_MACHINE=wcoss_d
package="ProdGSI/util/Ozone_Monitor"
#package="OznMon"

if [[ $MY_MACHINE = "wcoss_c" ]]; then
   . /opt/modules/3.2.6.7/init/sh
   module use -a /gpfs/hps/nco/ops/nwprod/modulefiles
   module load prod_util
elif [[ $MY_MACHINE = "wcoss_d" ]]; then 
   shell=sh
   . /usrx/local/prod/modules/default/init/${shell}
   module load prod_util/1.1.0
   MODULEPATH=/usrx/local/prod/lmod/lmod/modulefiles/Core
   MODULEPATH=${MODULEPATH}:/usrx/local/prod/modulefiles/core_third
   MODULEPATH=${MODULEPATH}:/usrx/local/prod/modulefiles/defs
   MODULEPATH=${MODULEPATH}:/gpfs/dell1/nco/ops/nwprod/modulefiles/core_prod
   export MODULEPATH=${MODULEPATH}:/usrx/local/dev/modulefiles
fi


if [[ $MY_MACHINE = "hera" ]]; then
   scripts=/scratch4/NCEPDEV/da/noscrub/Edward.Safford/ProdGSI/util/Ozone_Monitor/data_xtrct/ush
elif [[ $MY_MACHINE = "wcoss_d" ]]; then
   scripts=/gpfs/dell2/emc/modeling/noscrub/Edward.Safford/ProdGSI/util/Ozone_Monitor/data_xtrct/ush
elif [[ $MY_MACHINE = "wcoss_c" ]]; then
   scripts=/gpfs/hps3/emc/da/noscrub/Edward.Safford/ProdGSI/util/Ozone_Monitor/data_xtrct/ush
fi

idate=`${scripts}/find_cycle.pl -dir ~/nbns/stats/${OZN_SUFFIX} -cyc 1 -run ${run}`
echo "idate = $idate"

export NDATE=/gpfs/dell1/nco/ops/nwprod/prod_util.v1.1.1/exec/ips/ndate
#export START_DATE=2020010500
START_DATE=`${NDATE} +06 $idate`

PDY=`echo $START_DATE | cut -c1-8`
cyc=`echo $START_DATE | cut -c9-10`


export COM_IN=/gpfs/dell1/nco/ops/com/${NET}/${envir}/${run}.${PDY}/${cyc}
#export COM_IN=/scratch4/NCEPDEV/da/noscrub/Edward.Safford/test_data/${run}.${PDY}/${cyc}

export oznstat=${COM_IN}/${run}.t${cyc}z.oznstat

export OZN_TANKDIR=/u/${LOGNAME}/nbns
#export OZN_TANKDIR=/home/${LOGNAME}/nbns

log=/gpfs/dell2/ptmp/Edward.Safford/logs/${OZN_SUFFIX}/${run}/oznmon/OznMon_DE.log
#log=/ptmpd1/Edward.Safford/logs/${OZN_SUFFIX}/${run}/oznmon/OznMon_DE.log
#log=./log

err=/gpfs/dell2/ptmp/Edward.Safford/logs/${OZN_SUFFIX}/${run}/oznmon/OznMon_DE.err
#err=/ptmpd1/Edward.Safford/logs/${OZN_SUFFIX}/${run}/oznmon/OznMon_DE.err
#err=./err

${scripts}/OznMon_DE.sh $OZN_SUFFIX -p $START_DATE -r gdas 1>$log 2>$err
