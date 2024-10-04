#!/bin/ksh

#BSUB -o gdas_conmon.o%J
#BSUB -e gdas_conmon.o%J
#BSUB -J gdas_vconmon
#BSUB -q dev_shared
#BSUB -n 1
#BSUB -R affinity[core]
#BSUB -M 900
#BSUB -W 00:30
#BSUB -P GFS-DEV

set -x

export PDATE=${PDATE:-2020072006}


#############################################################
# Specify versions
#############################################################
export gdas_ver=v15.0.0
export global_shared_ver=v15.0.0
#export gdas_conmon_ver=v1.0.0
#export conmon_shared_ver=v1.0.0

#############################################################
# Load modules
#############################################################
. /usrx/local/prod/lmod/lmod/init/profile

module load lsf/10.1
module load ips/18.0.1.163
module load impi/18.0.1
module load prod_util/1.1.0
module load util_shared/1.1.0
module load grib_util/1.1.1

module list

#############################################################
# Specify whether the run is production or development
#############################################################
export PDY=`echo $PDATE | cut -c1-8`
export CYC=`echo $PDATE | cut -c9-10`
export job=gdas_conmon.${CYC}
export pid=${pid:-$$}
export jobid=${job}.${pid}
export DATAROOT=${DATAROOT:-/gpfs/dell2/emc/modeling/noscrub/Edward.Safford/test_data}
export COMROOT=${COMROOT:-/gpfs/dell2/ptmp/$LOGNAME/com}


#############################################################
# WCOSS environment settings
#############################################################
export POE=YES


#############################################################
# Set user specific variables
#############################################################
export CONMON_SUFFIX=${CONMON_SUFFIX:-test_conmon}

export NWTEST=${NWTEST:-/gpfs/dell2/emc/modeling/noscrub/Edward.Safford/GSI/util/Conventional_Monitor/nwprod}
export HOMEconmon=${HOMEconmon:-${NWTEST}/conmon_shared}

export HOMEgdas_conmon=${HOMEgdas_conmon:-${NWTEST}/gdas_conmon}
export HOMEgfs_conmon=${HOMEgdas_conmon}

export JOBGLOBAL=${HOMEgdas_conmon}/jobs
export C_COM_IN=${C_COM_IN:-${DATAROOT}}

export C_TANKDIR=${C_TANKDIR:-${COMROOT}/${CONMON_SUFFIX}}
if [[ ! -d $C_TANKDIR ]]; then 
   mkdir -p ${C_TANKDIR}
fi

export CONMON_WORK_DIR=${CONMON_WORK_DIR:-/gpfs/dell2/stmp/$LOGNAME/conmon_${CONMON_SUFFIX}}
if [[ ! -d $CONMON_WORK_DIR ]]; then 
   mkdir -p ${CONMON_WORK_DIR}
fi


#############################################################
# Execute job
#############################################################
$JOBGLOBAL/JGDAS_ATMOS_CONMON

exit

