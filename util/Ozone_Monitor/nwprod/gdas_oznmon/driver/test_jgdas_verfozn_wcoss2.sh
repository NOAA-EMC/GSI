#!/bin/ksh
#PBS -o gdas_voznmon.out
#PBS -e gdas_voznmon.err
#PBS -N gdas_voznmon
#PBS -q dev
#PBS -l select=1:mem=5000M
#PBS -l walltime=5:00
#PBS -A GFS-DEV

set -ax

##------------------------------------------------------------
##  This is the test driver script for the wcoss2 systems
##  to run the JGDAS_ATMOS_VERFOZN job.
##------------------------------------------------------------

export OZNMON_NEW_HDR=${OZN_NEW_HDR:-0}

#-------------------------------------------------------------
# PDATE settings for this test driver script correspond to 
# available data in the $DATAROOT space (defined below).  
# References to netcdf and binary indicate available files in 
# the test_data space.
#
export PDATE=${PDATE:-2021082400}
export NET=${NET:-gfs}
export RUN=${RUN:-gdas}

export PDY=`echo $PDATE | cut -c1-8`
export cyc=`echo $PDATE | cut -c9-10`
export job=gdas_verfozn.${cyc}
export pid=${pid:-$$}
export jobid=${job}.${pid}
export envir=${envir:-test}
export DATAROOT=/lfs/h1/ops/canned/com/gfs/v16.2
export COMROOT=/lfs/h2/emc/ptmp/${LOGNAME}
export OZN_WORK_DIR=${OZN_WORK_DIR:-/lfs/h2/emc/stmp/${LOGNAME}/oznmon.${pid}}
export DATA=/lfs/h2/emc/stmp/${LOGNAME}/${RADMON_SUFFIX}

if [[ ! -d ${COMROOT}/logs/jlogfiles ]]; then
   mkdir -p ${COMROOT}/logs/jlogfiles
fi

#------------------------------------------------------------
# Specify versions
#
export gdas_oznmon_ver=v2.0.0
export oznmon_shared_ver=v2.0.0


#------------------------------------------------------------
# Load modules
#
module purge
module load prod_util/2.0.10
module load util_shared/1.4.0

module list

#------------------------------------------------------------
# Set user specific variables
#
export OZNMON_SUFFIX=${OZNMON_SUFFIX:-testozn}
export NWTEST=${NWTEST:-/lfs/h2/emc/da/noscrub/Edward.Safford/git/gsi/gfsda.v16.1.5_wcoss2_port/util/Ozone_Monitor/nwprod}
export HOMEgfs=${HOMEgfs:-${NWTEST}/gdas_oznmon.${gdas_oznmon_ver}}
export HOMEgdas_ozn=${HOMEgdas_ozn:-${HOMEgfs}}
export PARMgdas_ozn=${HOMEgdas_ozn}/parm
export FIXgdas_ozn=${FIXgdas_ozn:-${HOMEgdas_ozn}/fix}


export HOMEgfs_ozn=${HOMEgfs_ozn:-${HOMEgdas_ozn}}
export PARMgfs_ozn=${PARMgfs_ozn:-${PARMgdas_ozn}}
export FIXgfs_ozn=${FIXgfs_ozn:-${FIXgdas_ozn}}

export JOBGLOBAL=${JOBGLOBAL:-${HOMEgdas_ozn}/jobs}
export HOMEoznmon=${HOMEoznmon:-${NWTEST}/oznmon_shared.${shared_oznmon_ver}}

export HOMEoznmon=${NWTEST}/oznmon_shared.${oznmon_shared_ver}
export COM_IN=${COM_IN:-$DATAROOT}
export OZN_TANKDIR=${OZN_TANKDIR:-${COMROOT}/${OZNMON_SUFFIX}}


#------------------------------------------------------------
# Execute job
#
${JOBGLOBAL}/JGDAS_ATMOS_VERFOZN

exit

