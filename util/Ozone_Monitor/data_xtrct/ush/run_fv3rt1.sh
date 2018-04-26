#!/bin/sh

OZN_SUFFIX=fv3rt1
run=gdas

MY_MACHINE=wcoss
package="ProdGSI/util/Ozone_Monitor"

if [[ $MY_MACHINE = "cray" ]]; then
   . /opt/modules/3.2.6.7/init/sh
   module use -a /gpfs/hps/nco/ops/nwprod/modulefiles
elif [[ $MY_MACHINE = "wcoss" ]]; then
   shell=sh
   . /usrx/local/Modules/default/init/${shell}
fi

module load prod_util

scripts=/gpfs/gd2/emc/da/noscrub/Edward.Safford/${package}/data_xtrct/ush

idate=`${scripts}/find_cycle.pl -dir ~/nbns/stats/${OZN_SUFFIX} -cyc 1 -run ${run}`
echo "idate = $idate"

#START_DATE=2018041300
export START_DATE=`${NDATE} +06 $idate`

PDY=`echo $START_DATE | cut -c1-8`
cyc=`echo $START_DATE | cut -c9-10`

export COM_IN=/gpfs/hps3/ptmp/emc.glopara/ROTDIRS/prfv3rt1/${run}.${PDY}/${cyc}
export oznstat=${COM_IN}/${run}.t${cyc}z.oznstat

${scripts}/OznMon_DE.sh $OZN_SUFFIX -p $START_DATE -r gdas 1>log 2>err
