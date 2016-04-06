#!/bin/ksh

#BSUB -o gdas_verfrad.o%J
#BSUB -e gdas_verfrad.o%J
#BSUB -J gdas_verfrad
#BSUB -q dev
#BSUB -M 100
#BSUB -W 00:20
#BSUB -P GFS-T2O
#BSUB -R "select[mem>100] rusage[mem=100]"

set -x

export PDATE=2016022500

#############################################################
# Specify whether the run is production or development
#############################################################
export PDY=`echo $PDATE | cut -c1-8`
export cyc=`echo $PDATE | cut -c9-10`
export job=gdas_verfrad.${cyc}
export pid=${pid:-$$}
export jobid=${job}.${pid}
export envir=para

#############################################################
# Specify versions
#############################################################
export gdas_ver=v13.0.0
export global_shared_ver=v13.0.0
export grib_util_ver=v1.0.1
export prod_util_ver=1.0.3
export util_shared_ver=v1.0.2
export gdas_radmon_ver=v2.0.0
export radmon_shared_ver=v2.0.2


#############################################################
# Load modules
#############################################################
. $MODULESHOME/init/ksh

#module use /nwprod2/modulefiles
#module load grib_util/$grib_util_ver
#module load prod_util/$prod_util_ver
#module load util_shared/$util_shared_ver

module load prod_util/${prod_util_ver}
module load prod_envir
module load PrgEnv-intel

module list


#############################################################
# WCOSS environment settings
#############################################################
export POE=YES


#############################################################
# Set user specific variables
#############################################################
export DATAROOT=/gpfs/hps/ptmp/$LOGNAME/test_data
export COMROOT=/gpfs/hps/ptmp/$LOGNAME/com
export SUFFIX=testrad
export NWTEST=/gpfs/hps/emc/da/noscrub/${LOGNAME}/RadMon_545/util/Radiance_Monitor/nwprod
export HOMEgdas=${NWTEST}/gdas_radmon.${gdas_radmon_ver}
export JOBGLOBAL=${HOMEgdas}/jobs
export HOMEradmon=${NWTEST}/radmon_shared.${radmon_shared_ver}
export COM_IN=${DATAROOT}
export TANKverf=${COMROOT}/${SUFFIX}


#############################################################
# Execute job
#############################################################
$JOBGLOBAL/JGDAS_VERFRAD

exit

