#!/bin/ksh

#BSUB -o gdas_vcmon.o%J
#BSUB -e gdas_vcmon.o%J
#BSUB -J gdas_vcmon
#BSUB -q dev_shared
#BSUB -n 1
#BSUB -R affinity[core]
#BSUB -M 80
#BSUB -W 00:15
#BSUB -a poe
#BSUB -P GFS-T2O

set -x

export PDATE=2017030600


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
. /usrx/local/Modules/3.2.9/init/ksh
module use /nwprod2/modulefiles
module load grib_util
module load prod_util
module load util_shared

module list

#############################################################
# Specify whether the run is production or development
#############################################################
export PDY=`echo $PDATE | cut -c1-8`
export CYC=`echo $PDATE | cut -c9-10`
export job=gdas_vcmon.${CYC}
export pid=${pid:-$$}
export jobid=${job}.${pid}
export envir=para
ihost=`hostname | cut -c1`
export DATAROOT=/gpfs/${ihost}d2/emc/da/noscrub/Edward.Safford/test_data
export COMROOT=/ptmpp1/$LOGNAME/com

#############################################################
# WCOSS environment settings
#############################################################
export POE=YES


#############################################################
# Set user specific variables
#############################################################
export CMON_SUFFIX=testcmon
export NWTEST=/gpfs/${ihost}d2/emc/da/noscrub/Edward.Safford/CMon_486/util/Conventional_Monitor/nwprod
export HOMEgdascmon=${NWTEST}/gdas_cmon.${gdas_cmon_ver}
export JOBGLOBAL=${HOMEgdascmon}/jobs
export HOMEcmon=${NWTEST}/cmon_shared.${cmon_shared_ver}
export COM_IN=${DATAROOT}

export C_TANKDIR=${COMROOT}/${CMON_SUFFIX}
if [[ ! -d $C_TANKDIR ]]; then 
   mkdir -p ${C_TANKDIR}
fi

export CMON_WORK_DIR=${CMON_WORK_DIR:-/stmpp1/$LOGNAME/${CMON_SUFFIX}_cmon_de}
if [[ ! -d $CMON_WORK_DIR ]]; then 
   mkdir -p ${CMON_WORK_DIR}
fi

export CMON_LOG_DIR=${COMROOT}/logs/jlogfiles
if [[ ! -d $CMON_LOG_DIR ]]; then 
   mkdir -p ${CMON_LOG_DIR}
fi

export jlogfile=${jlogfile:-${CMON_LOG_DIR}/jlogfile.${jobname}.${pid}}


#############################################################
# Execute job
#############################################################
$JOBGLOBAL/JGDAS_VERFCON

exit

