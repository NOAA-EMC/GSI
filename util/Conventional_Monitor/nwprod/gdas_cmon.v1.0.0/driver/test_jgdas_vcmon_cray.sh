#!/bin/ksh

#BSUB -o gdas_vcmon.o%J
#BSUB -e gdas_vcmon.o%J
#BSUB -J gdas_vcmon
#BSUB -q dev
#BSUB -M 80
#BSUB -W 00:40
#BSUB -P GFS-T2O
#BSUB -R "select[mem>80] rusage[mem=80]"

set -x

export PDATE=${PDATE:-2017030606}

#############################################################
# Specify whether the run is production or development
#############################################################
export PDY=`echo $PDATE | cut -c1-8`
export CYC=`echo $PDATE | cut -c9-10`
export cmon_job=${cmon_job:-gdas_vcmon.${CYC}}
export pid=${pid:-$$}
export jobid=${cmon_job}.${pid}
export envir=${envir:-para}


#############################################################
# Specify versions
#############################################################
export gdas_ver=v13.0.0
export global_shared_ver=v13.0.0
export gdas_cmon_ver=v1.0.0
export cmon_shared_ver=v1.0.0


#############################################################
# Load modules
#############################################################
. $MODULESHOME/init/ksh

module load prod_util
module load prod_envir

module list


#############################################################
# WCOSS environment settings
#############################################################
export POE=YES


#############################################################
# Set user specific variables
#############################################################
export DATAROOT=${DATAROOT:-/gpfs/hps/emc/da/noscrub/$LOGNAME/test_data}
#export COMROOT=${COMROOT:-/gpfs/hps/ptmp/$LOGNAME/com}
export PTMP_USER=${PTMP_USER:-/gpfs/hps/ptmp/$LOGNAME}
export CMON_SUFFIX=${CMON_SUFFIX:-testcmon}
export NWTEST=${NWTEST:-/gpfs/hps/emc/da/noscrub/${LOGNAME}/CMon_486/util/Conventional_Monitor/nwprod}
export HOMEgdascmon=${HOMEgdascmon:-${NWTEST}/gdas_cmon.${gdas_cmon_ver}}
export JOBGLOBAL=${JOBGLOBAL:-${HOMEgdascmon}/jobs}
export HOMEcmon=${HOMEcmon:-${NWTEST}/cmon_shared.${cmon_shared_ver}}
export COM_IN=${COM_IN:-${DATAROOT}}

export C_TANKDIR=${C_TANKDIR:-${PTMP_USER}/com/${CMON_SUFFIX}}
if [[ ! -d $C_TANKDIR ]]; then
   mkdir -p ${C_TANKDIR}
fi

export CMON_WORK_DIR=${CMON_WORK_DIR:-/gpfs/hps/stmp/$LOGNAME/${CMON_SUFFIX}_cmon_de}
if [[ ! -d $CMON_WORK_DIR ]]; then
   mkdir -p ${CMON_WORK_DIR}
fi

export CMON_LOG_DIR=${CMON_LOG_DIR:-${PTMP_USER}/logs/jlogfiles}
if [[ ! -d $CMON_LOG_DIR ]]; then
   mkdir -p ${CMON_LOG_DIR}
fi

export jlogfile=${jlogfile:-${CMON_LOG_DIR}/jlogfile.${jobid}}
export utilroot=${utilroot:-$PROD}

#############################################################
# Execute job
#############################################################
$JOBGLOBAL/JGDAS_VCMON

exit

