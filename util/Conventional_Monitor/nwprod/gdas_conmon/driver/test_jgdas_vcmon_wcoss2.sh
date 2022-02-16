#!/bin/ksh -l

#PBS -o gdas_vconmon.out
#PBS -e gdas_vconmon.err
#PBS -N gdas_vconmon
#PBS -q dev
#PBS -l select=1:mem=5000M
#PBS -l walltime=30:00
#PBS -A GFS-DEV

set -x

export PDATE=${PDATE:-2021082318}


#############################################################
# Specify versions
#############################################################
export gdas_ver=v16.1.5
export global_shared_ver=v16.1.5

#############################################################
# Load modules
#############################################################
module purge

module load gcc/11.2.0
module load intel/19.1.3.304
module load libjpeg/9c
module load grib_util/1.2.3

module load prod_util/2.0.10
module load util_shared/1.4.0

module load cray-mpich/8.1.7
module load netcdf/4.7.4

module list

#############################################################
# Specify whether the run is production or development
#############################################################
export PDY=`echo $PDATE | cut -c1-8`
export CYC=`echo $PDATE | cut -c9-10`
export job=gdas_conmon.${CYC}
export pid=${pid:-$$}
export jobid=${job}.${pid}
export DATAROOT=/lfs/h1/ops/canned/com/gfs/v16.2
export COMROOT=/lfs/h2/emc/ptmp/${LOGNAME}

#############################################################
# WCOSS environment settings
#############################################################
export POE=YES


#############################################################
# Set user specific variables
#############################################################
export CONMON_SUFFIX=${CONMON_SUFFIX:-test_conmon}

export NWTEST=/lfs/h2/emc/da/noscrub/Edward.Safford/git/gsi/GSI/util/Conventional_Monitor/nwprod
export HOMEconmon=${HOMEconmon:-${NWTEST}/conmon_shared}

export HOMEgdas_conmon=${HOMEgdas_conmon:-${NWTEST}/gdas_conmon}
export HOMEgfs_conmon=${HOMEgdas_conmon}

export JOBGLOBAL=${HOMEgdas_conmon}/jobs
export C_COM_IN=${C_COM_IN:-${DATAROOT}}

export C_TANKDIR=${C_TANKDIR:-${COMROOT}/${CONMON_SUFFIX}}
if [[ ! -d $C_TANKDIR ]]; then 
   mkdir -p ${C_TANKDIR}
fi

export CONMON_WORK_DIR=${CONMON_WORK_DIR:-/lfs/h2/emc/stmp/$LOGNAME/conmon_${CONMON_SUFFIX}}
if [[ -d $CONMON_WORK_DIR ]]; then  
   rm -rf ${CONMON_WORK_DIR}
fi
mkdir -p ${CONMON_WORK_DIR}


#############################################################
# Execute job
#############################################################
$JOBGLOBAL/JGDAS_ATMOS_CONMON

exit

