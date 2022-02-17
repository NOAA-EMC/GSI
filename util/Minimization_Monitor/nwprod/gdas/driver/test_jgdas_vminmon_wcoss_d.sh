#!/bin/ksh

#BSUB -o gdas_vminmon.o%J
#BSUB -e gdas_vminmon.o%J
#BSUB -J gdas_vminmon
#BSUB -q dev_shared
#BSUB -n 1
#BSUB -R affinity[core]
#BSUB -M 80
#BSUB -W 00:05
#BSUB -P GFS-DEV

set -ax

export PDATE=${PDATE:-2018011112}

#############################################################
# Specify whether the run is production or development
#############################################################
export PDY=`echo $PDATE | cut -c1-8`
export cyc=`echo $PDATE | cut -c9-10`
export job=gdas_vminmon.${cyc}
export pid=${pid:-$$}
export jobid=${job}.${pid}
export envir=para

export DATAROOT=${DATAROOT:-/gpfs/dell2/emc/modeling/noscrub/Edward.Safford/test_data}
export COMROOT=${COMROOT:-/gpfs/dell2/ptmp/Edward.Safford/com}


#############################################################
# Load modules
#############################################################
   shell=ksh
   source /usrx/local/prod/lmod/lmod/init/${shell}

   MODULEPATH=/usrx/local/prod/lmod/lmod/modulefiles/Core
   MODULEPATH=${MODULEPATH}:/usrx/local/prod/modulefiles/core_third
   MODULEPATH=${MODULEPATH}:/usrx/local/prod/modulefiles/defs
   MODULEPATH=${MODULEPATH}:/gpfs/dell1/nco/ops/nwprod/modulefiles/core_prod
   export MODULEPATH=${MODULEPATH}:/usrx/local/dev/modulefiles

   module load ips/18.0.1.163
   module load metplus/2.1
   module load lsf/10.1
   module load prod_util/1.1.2
   module load util_shared/1.1.0
   module load pm5/1.0

   module list


#############################################################
# Set user specific variables
#############################################################
export MINMON_SUFFIX=${MINMON_SUFFIX:-testminmon_gdas}
export NWTEST=${NWTEST:-/gpfs/dell2/emc/modeling/noscrub/Edward.Safford/GSI/util/Minimization_Monitor/nwprod}
export HOMEgdas=${NWTEST}/gdas
export HOMEgfs=${HOMEgfs:-${HOMEgdas}}

export JOBGLOBAL=${HOMEgdas}/jobs
export HOMEminmon=${NWTEST}/minmon_shared

export COM_IN=${COM_IN:-${DATAROOT}}
export M_TANKverf=${COMROOT}/${MINMON_SUFFIX}
export M_FIXgdas=${HOMEgdas}/fix

#############################################################
# Execute job
#############################################################
$JOBGLOBAL/JGDAS_ATMOS_VMINMON

exit

