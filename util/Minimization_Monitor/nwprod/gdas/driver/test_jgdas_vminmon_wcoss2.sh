#!/bin/ksh -l

#PBS -o gdas_vminmon.out
#PBS -e gdas_vminmon.err
#PBS -N gdas_vminmon
#PBS -q dev
#PBS -l select=1:mem=400M
#PBS -l walltime=05:00
#PBS -A GFS-DEV


set -ax

export PDATE=${PDATE:-2021082306}

#############################################################
# Specify whether the run is production or development
#############################################################
export PDY=`echo $PDATE | cut -c1-8`
export cyc=`echo $PDATE | cut -c9-10`
export job=gdas_vminmon.${cyc}
export pid=${pid:-$$}
export jobid=${job}.${pid}
export envir=para

export DATAROOT=${DATAROOT:-/lfs/h1/ops/canned/com/gfs/v16.2}
export DATA=${DATA:-/lfs/h2/emc/stmp/Edward.Safford}
export COMROOT=${COMROOT:-/lfs/h2/emc/ptmp/Edward.Safford/com}


#############################################################
# Load modules
#############################################################
module load prod_util/2.0.10
module load util_shared/1.4.0
module load perl/5.32.0

module list


#############################################################
# Set user specific variables
#############################################################
export MINMON_SUFFIX=${MINMON_SUFFIX:-testminmon_gdas}
export NWTEST=${NWTEST:-/lfs/h2/emc/da/noscrub/Edward.Safford/git/gsi/GSI/util/Minimization_Monitor/nwprod}
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

