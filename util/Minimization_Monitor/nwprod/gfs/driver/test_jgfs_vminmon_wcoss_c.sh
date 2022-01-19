#!/bin/ksh

#BSUB -o gfs_vminmon.o%J
#BSUB -e gfs_vminmon.o%J
#BSUB -J gfs_vminmon
#BSUB -q dev_transfer
#BSUB -M 80
#BSUB -W 00:05
#BSUB -P GFS-T2O
#BSUB -R "select[mem>80] rusage[mem=80]"

set -x

export PDATE=${PDATE:-2022010406}

#############################################################
# Specify whether the run is production or development
#############################################################
export PDY=`echo $PDATE | cut -c1-8`
export cyc=`echo $PDATE | cut -c9-10`
export job=gfs_vminmon.${cyc}
export pid=${pid:-$$}
export jobid=${job}.${pid}


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
export DATAROOT=${DATAROOT:-/gpfs/hps3/emc/da/noscrub/$LOGNAME/test_data}
export COMROOT=/gpfs/hps2/ptmp/$LOGNAME/com
export MINMON_SUFFIX=${MINMON_SUFFIX:-testminmon_gfs}
export NWTEST=${NWTEST:-/gpfs/hps3/emc/da/noscrub/Edward.Safford/GSI/util/Minimization_Monitor/nwprod}
export HOMEgfs=${NWTEST}/gfs
export M_FIXgfs=${HOMEgfs}/fix
export JOBGLOBAL=${HOMEgfs}/jobs
export HOMEminmon=${NWTEST}/minmon_shared
export COM_IN=${DATAROOT}
export M_TANKverf=${COMROOT}/${MINMON_SUFFIX}


#############################################################
# Execute job
#############################################################
$JOBGLOBAL/JGFS_ATMOS_VMINMON

exit

