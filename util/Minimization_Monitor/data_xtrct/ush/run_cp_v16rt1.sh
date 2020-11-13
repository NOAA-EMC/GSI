#!/bin/bash

#package=MinMon
package=ProdGSI/util/Minimization_Monitor

net=v16rt1
run=gdas

echo "user = $USER"

export KEEPDATA=YES
export DO_ERROR_RPT=1
export MAIL_TO=""
export MAIL_CC=""

scripts=/gpfs/dell2/emc/modeling/noscrub/${USER}/${package}/data_xtrct/ush
echo "scripts = $scripts"

shell=bash
source /usrx/local/prod/lmod/lmod/init/${shell}

MODULEPATH=/usrx/local/prod/lmod/lmod/modulefiles/Core:/usrx/local/prod/modulefiles/core_third:/usrx/local/prod/modulefiles/defs:/gpfs/dell1/nco/ops/nwprod/modulefiles/core_prod:/usrx/local/dev/modulefiles

module purge
module load ips/18.0.1.163
module load metplus/2.1
module load prod_util/1.1.2

echo NDATE = $NDATE



tank=~/nbns/stats/${net}

ldate=`${scripts}/find_cycle.pl --cyc 1 --dir ${tank} --run ${run}`
echo ldate = $ldate
pdate=`${NDATE} +06 $ldate`
#pdate=2019070100

echo pdate = $pdate

pdy=`echo $pdate|cut -c1-8`
cyc=`echo $pdate|cut -c9-10`

data_loc=/gpfs/dell2/emc/modeling/noscrub/emc.glopara/monitor/minmon/stats/${net}/${run}.${pdy}
#echo data_loc = $data_loc

tank=${tank}/${run}.${pdy}/${cyc}/minmon
mkdir -p ${tank}

cp ${data_loc}/*${pdate}* ${tank}/.
cp ${data_loc}/gnorm_data.txt ${tank}/.


