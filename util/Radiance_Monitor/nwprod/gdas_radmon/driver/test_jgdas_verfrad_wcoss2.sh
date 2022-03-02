#!/bin/ksh

#PBS -o gdas_vradmon.out
#PBS -e gdas_vradmon.err
#PBS -N gdas_vradmon
#PBS -q dev
#PBS -l select=1:mem=5000M
#PBS -l walltime=20:00
#PBS -A GFS-DEV

set -ax

export PDATE=${PDATE:-2021082300}

#############################################################
# Specify whether the run is production or development
#############################################################
export PDY=`echo $PDATE | cut -c1-8`
export cyc=`echo $PDATE | cut -c9-10`
export job=gdas_verfrad.${cyc}
export pid=${pid:-$$}
export jobid=${job}.${pid}
export envir=prod
export DATAROOT=/lfs/h1/ops/canned/com/gfs/v16.2
export COMROOT=/lfs/h2/emc/ptmp/${LOGNAME}

if [[ ! -d ${COMROOT}/logs/jlogfiles ]]; then
   mkdir -p ${COMROOT}/logs/jlogfiles
fi


#############################################################
# Load modules
#############################################################
module load prod_util/2.0.10
module load util_shared/1.4.0

#module load lsf/10.1
#module load ips/18.0.1.163
#module load impi/18.0.1
#module load prod_util/1.1.0
#module load grib_util/1.1.0
#module load util_shared/1.1.0

module list


#############################################################
# Set user specific variables
#############################################################
export RADMON_SUFFIX=test_rad
export DATA=/lfs/h2/emc/stmp/${LOGNAME}/${RADMON_SUFFIX}                # rename this to WORKDIR
if [[ -d ${DATA} ]]; then
   rm -rf ${DATA}
   mkdir -p ${DATA}
fi
export RAD_DATA_IN=${DATA}
export jlogfile=${COMROOT}/logs/jlogfiles/${RADMON_SUFFIX}_jlog

export NWTEST=/lfs/h2/emc/da/noscrub/Edward.Safford/git/gsi/GSI/util/Radiance_Monitor/nwprod

export HOMEgdas=${NWTEST}/gdas_radmon
export HOMEgfs=${HOMEgdas}
export FIXgdas=${FIXgdas:-$HOMEgfs/fix}

export JOBGLOBAL=${HOMEgdas}/jobs
export HOMEradmon=${NWTEST}/radmon_shared
export COM_IN=${DATAROOT}
export TANKverf=${COMROOT}/${RADMON_SUFFIX}

export parm_file=${HOMEgdas}/parm/gdas_radmon.parm

#############################################################
# Execute job
#############################################################
$JOBGLOBAL/JGDAS_ATMOS_VERFRAD

exit

