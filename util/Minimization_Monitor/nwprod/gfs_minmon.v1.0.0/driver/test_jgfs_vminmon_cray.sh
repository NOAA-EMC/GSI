#!/bin/ksh

#BSUB -o gfs_vminmon.o%J
#BSUB -e gfs_vminmon.o%J
#BSUB -J gfs_vminmon
#BSUB -q dev
#BSUB -M 80
#BSUB -W 00:05
#BSUB -P GFS-T2O
#BSUB -R "select[mem>80] rusage[mem=80]"

set -x

export PDATE=2016030706

#############################################################
# Specify whether the run is production or development
#############################################################
export PDY=`echo $PDATE | cut -c1-8`
export cyc=`echo $PDATE | cut -c9-10`
export job=gfs_vminmon.${cyc}
export pid=${pid:-$$}
export jobid=${job}.${pid}
export envir=para


#############################################################
# Specify versions
#############################################################
export gfs_ver=v13.0.0
export global_shared_ver=v13.0.0
export grib_util_ver=v1.0.1
export prod_util_ver=1.0.3
export util_shared_ver=v1.0.2
export gfs_minmon_ver=v1.0.0
export minmon_shared_ver=v1.0.0


#############################################################
# Load modules
#############################################################
. $MODULESHOME/init/ksh

module load prod_util/${prod_util_ver}
module load prod_envir
module load pm5

module list


#############################################################
# WCOSS environment settings
#############################################################
export POE=YES


#############################################################
# Set user specific variables
#############################################################
#export DATAROOT=/gpfs/hps/ptmp/$LOGNAME/test_data
export DATAROOT=/gpfs/hps/emc/da/noscrub/$LOGNAME/test_data
export COMROOT=/gpfs/hps/ptmp/$LOGNAME/com
export SUFFIX=testminmon
export NWTEST=/gpfs/hps/emc/da/noscrub/${LOGNAME}/MinMon_546/util/Minimization_Monitor/nwprod
export HOMEgfs=${NWTEST}/gfs_minmon.${gfs_minmon_ver}
export JOBGLOBAL=${HOMEgfs}/jobs
export HOMEminmon=${NWTEST}/minmon_shared.${minmon_shared_ver}
export COM_IN=${DATAROOT}
export M_TANKverf=${COMROOT}/${SUFFIX}


#############################################################
# Execute job
#############################################################
$JOBGLOBAL/JGFS_VMINMON

exit

