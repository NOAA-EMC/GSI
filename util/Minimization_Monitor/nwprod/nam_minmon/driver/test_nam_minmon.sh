#!/bin/ksh

#BSUB -o nam_vminmon.o%J
#BSUB -e nam_vminmon.o%J
#BSUB -J nam_vminmon
#BSUB -q dev_shared
#BSUB -n 1
#BSUB -R affinity[core]
#BSUB -M 80
#BSUB -W 00:10
#BSUB -a poe
#BSUB -P NAM-T2O

set -x

export PDATE=2016062716

#############################################################
# Specify whether the run is production or development
#############################################################
export PDY=`echo $PDATE | cut -c1-8`
export cyc=`echo $PDATE | cut -c9-10`
export job=nam_vminmon.${cyc}
export pid=${pid:-$$}
export jobid=${job}.${pid}
export envir=para
export RUN=namrr
export MINMON_SUFFIX=namrr
export DATAROOT=/gpfs/td2/emc/da/noscrub/Edward.Safford/test_data
export COMROOT=/ptmpp1/$LOGNAME/com


#############################################################
# Specify versions
#############################################################
export gdas_ver=v13.0.0
export global_shared_ver=v13.0.0
export grib_util_ver=v1.0.1
export prod_util_ver=v1.0.2
export util_shared_ver=v1.0.2
export nam_minmon_ver=v1.0.0
export minmon_shared_ver=v1.0.0


#############################################################
# Load modules
#############################################################
. /usrx/local/Modules/3.2.9/init/ksh
module use /nwprod2/modulefiles
module load grib_util/$grib_util_ver
module load prod_util/$prod_util_ver
module load util_shared/$util_shared_ver

module unload ics/12.1
module load ics/15.0.3

module list


#############################################################
# WCOSS environment settings
#############################################################
export POE=YES


#############################################################
# Set user specific variables
#############################################################
export SUFFIX=testminmon
export NWTEST=/gpfs/td2/emc/da/noscrub/Edward.Safford/MinMon_585/util/Minimization_Monitor/nwprod
export HOMEnam=${NWTEST}/nam_minmon.${nam_minmon_ver}
export JOBGLOBAL=${HOMEnam}/jobs
export HOMEminmon=${NWTEST}/minmon_shared.${minmon_shared_ver}
export COM_IN=${DATAROOT}
export M_TANKverf=${COMROOT}/${SUFFIX}


#############################################################
# Execute job
#############################################################
$JOBGLOBAL/JNAM_MINMON

exit

