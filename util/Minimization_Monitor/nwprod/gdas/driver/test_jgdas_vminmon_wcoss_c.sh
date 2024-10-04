#!/bin/ksh

#BSUB -o gdas_vminmon.o%J
#BSUB -e gdas_vminmon.o%J
#BSUB -J gdas_vminmon
#BSUB -q dev
#BSUB -M 80
#BSUB -W 00:05
#BSUB -P GDAS-T2O
#BSUB -R "select[mem>80] rusage[mem=80]"

set -x

export PDATE=${PDATE:-2022010406}

#############################################################
# Specify whether the run is production or development
#############################################################
export PDY=`echo $PDATE | cut -c1-8`
export cyc=`echo $PDATE | cut -c9-10`
export job=gdas_vminmon.${cyc}
export pid=${pid:-$$}
export jobid=${job}.${pid}
export envir=para


#############################################################
# Load modules
#############################################################
. $MODULESHOME/init/ksh

module load prod_util
module load pm5

module list


#############################################################
# Set user specific variables
#############################################################
export DATAROOT=/gpfs/hps3/emc/da/noscrub/$LOGNAME/test_data
export COMROOT=/gpfs/hps2/ptmp/$LOGNAME/com
export MINMON_SUFFIX=${MINMON_SUFFIX:-testminmon_gdas}
export NWTEST=/gpfs/hps3/emc/da/noscrub/Edward.Safford/GSI/util/Minimization_Monitor/nwprod
export HOMEgdas=${NWTEST}/gdas
export M_FIXgdas=${M_FIXgdas:-${HOMEgdas}/fix}
export JOBGLOBAL=${HOMEgdas}/jobs
export HOMEgfs=${HOMEgfs:-${HOMEgdas}}
export HOMEminmon=${NWTEST}/minmon_shared
export COM_IN=${DATAROOT}
export M_TANKverf=${COMROOT}/${MINMON_SUFFIX}
export CYCLE_INTERVAL=${CYCLE_INTERVAL:-6}

#############################################################
# Execute job
#############################################################
$JOBGLOBAL/JGDAS_ATMOS_VMINMON

exit

