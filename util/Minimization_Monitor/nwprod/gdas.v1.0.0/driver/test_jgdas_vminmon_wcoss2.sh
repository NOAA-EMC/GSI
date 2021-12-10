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
# Specify versions
#############################################################
export gdas_ver=v16.2.0
export global_shared_ver=v16.2.0
export gdas_minmon_ver=v1.0.0
export minmon_shared_ver=v1.0.1


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
export NWTEST=${NWTEST:-/lfs/h2/emc/da/noscrub/Edward.Safford/git/gsi/gfsda.v16.1.5_wcoss2_port/util/Minimization_Monitor/nwprod}
export HOMEgdas=${NWTEST}/gdas.${gdas_minmon_ver}
export HOMEgfs=${HOMEgfs:-${HOMEgdas}}

export JOBGLOBAL=${HOMEgdas}/jobs
export HOMEminmon=${NWTEST}/minmon_shared.${minmon_shared_ver}

export COM_IN=${COM_IN:-${DATAROOT}}
export M_TANKverf=${COMROOT}/${MINMON_SUFFIX}
export M_FIXgdas=${HOMEgdas}/fix

#############################################################
# Execute job
#############################################################
$JOBGLOBAL/JGDAS_ATMOS_VMINMON

exit

