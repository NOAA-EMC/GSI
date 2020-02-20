#!/bin/ksh

#BSUB -o gdas_vconmon.o%J
#BSUB -e gdas_vconmon.o%J
#BSUB -J gdas_vconmon
#BSUB -q dev
#BSUB -n 1
#BSUB -M 160
#BSUB -W 00:50
#BSUB -P GFS-DEV
#BSUB -R "select[mem>160] rusage[mem=160]"

set -x

export PDATE=${PDATE:-2020012006}


#############################################################
# Specify versions
#############################################################
export gdas_ver=v15.0.0
export global_shared_ver=v15.0.0
export gdas_conmon_ver=v1.0.0
export conmon_shared_ver=v1.0.0

#############################################################
# Load modules
#############################################################
. $MODULESHOME/init/ksh

module load prod_util
module load prod_envir

module list

#############################################################
# Specify whether the run is production or development
#############################################################
export PDY=`echo $PDATE | cut -c1-8`
export CYC=`echo $PDATE | cut -c9-10`
export conmon_job=${conmon_job:-gdas_conmon.${CYC}}
export pid=${pid:-$$}
export jobid=${conmon_job}.${pid}
export DATAROOT=/gpfs/hps3/emc/da/noscrub/$LOGNAME/test_data
export COMROOT=/gpfs/hps/ptmp/$LOGNAME/com


#############################################################
# WCOSS environment settings
#############################################################
export POE=YES


#############################################################
# Set user specific variables
#############################################################
export CONMON_SUFFIX=${CONMON_SUFFIX:-test_conmon}
export NWTEST=${NWTEST:-/gpfs/hps3/emc/da/noscrub/${LOGNAME}/ProdGSI/util/Conventional_Monitor/nwprod}
export HOMEconmon=${HOMEconmon:-${NWTEST}/conmon_shared.${conmon_shared_ver}}
export HOMEgdas_conmon=${HOMEgdas_conmon:-${NWTEST}/gdas_conmon.${gdas_conmon_ver}}
export HOMEgfs_conmon=${HOMEgdas_conmon}

export JOBGLOBAL=${HOMEgdas_conmon}/jobs
export C_COM_IN=${C_COM_IN:-${DATAROOT}}


export C_TANKDIR=${C_TANKDIR:-${COMROOT}/${CONMON_SUFFIX}}
if [[ ! -d $C_TANKDIR ]]; then
   mkdir -p ${C_TANKDIR}
fi

export CONMON_WORK_DIR=${CONMON_WORK_DIR:-/gpfs/hps/stmp/$LOGNAME/conmon_${CONMON_SUFFIX}}
if [[ ! -d $CONMON_WORK_DIR ]]; then
   mkdir -p ${CONMON_WORK_DIR}
fi


#############################################################
# Execute job
#############################################################
$JOBGLOBAL/JGDAS_CONMON

exit

