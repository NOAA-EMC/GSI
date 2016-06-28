#!/bin/ksh

#BSUB -o namrr_verfrad.o%J
#BSUB -e namrr_verfrad.o%J
#BSUB -J namrr_verfrad
#BSUB -q dev_shared
#BSUB -n 1
#BSUB -R affinity[core]
#BSUB -M 100
#BSUB -W 00:20
#BSUB -a poe
#BSUB -P NDAS-T2O

set -x

export PDATE=2016062712

#############################################################
# Specify whether the run is production or development
#############################################################
export PDY=`echo $PDATE | cut -c1-8`
export cyc=`echo $PDATE | cut -c9-10`
export job=namrr_verfrad.${cyc}
export pid=${pid:-$$}
export jobid=${job}.${pid}
export envir=para
export DATAROOT=/ptmpp1/$LOGNAME/test_data
export COMROOT=/ptmpp1/$LOGNAME/com


#############################################################
# Specify versions
#############################################################
#export gdas_ver=v13.0.0
export global_shared_ver=v13.0.0
export grib_util_ver=v1.0.1
export prod_util_ver=v1.0.2
export util_shared_ver=v1.0.2
export namrr_radmon_ver=v2.0.0
export radmon_shared_ver=v2.0.4


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
export RADMON_SUFFIX=testrad
export NWTEST=/da/noscrub/${LOGNAME}/RadMon_552/util/Radiance_Monitor/nwprod
export HOMEnamrr=${NWTEST}/namrr_radmon.${namrr_radmon_ver}
export JOBregional=${HOMEnamrr}/jobs
export HOMEradmon=${NWTEST}/radmon_shared.${radmon_shared_ver}
export COM_IN=${DATAROOT}
export TANKverf=${COMROOT}/${RADMON_SUFFIX}


#############################################################
# Execute job
#############################################################
$JOBregional/JNAMRR_VERFRAD

exit

