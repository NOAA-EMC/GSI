#!/bin/sh

OZN_SUFFIX=fv3rt1
run=gdas

MY_MACHINE=cray

if [[ $MY_MACHINE = "cray" ]]; then
   . /opt/modules/3.2.6.7/init/sh
   module use -a /gpfs/hps/nco/ops/nwprod/modulefiles
   module load prod_util
fi

scripts=/gpfs/hps3/emc/da/noscrub/Edward.Safford/ProdGSI/util/Ozone_Monitor/data_xtrct

idate=`${scripts}/find_cycle.pl -dir ~/nbns/stats/${OZN_SUFFIX} -cyc 1 -run ${run}`

#START_DATE=2017112306
export START_DATE=`${NDATE} +06 $idate`

PDY=`echo $START_DATE | cut -c1-8`
cyc=`echo $START_DATE | cut -c9-10`

export COM_IN=/gpfs/hps3/ptmp/emc.glopara/ROTDIRS/prfv3rt1/${run}.${PDY}/${cyc}
export oznstat=${COM_IN}/${run}.t${cyc}z.oznstat

${scripts}/OznMon_DE.sh $OZN_SUFFIX -p $START_DATE -r gdas 1>log 2>err
