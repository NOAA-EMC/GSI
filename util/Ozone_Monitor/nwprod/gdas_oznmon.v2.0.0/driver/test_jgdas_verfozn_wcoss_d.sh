#!/bin/ksh

#BSUB -o gdas_verfozn.o%J
#BSUB -e gdas_verfozn.o%J
#BSUB -J gdas_verfozn
#BSUB -q dev_shared
#BSUB -n 1
#BSUB -R affinity[core]
#BSUB -M 500
#BSUB -W 00:05
#BSUB -P GFS-DEV

##------------------------------------------------------------
##  This is the test driver script for the wcoss/ibm systems
##  to run the JGDAS_VERFOZN job.
##------------------------------------------------------------

set -x

export OZNMON_NEW_HDR=${OZN_NEW_HDR:-0}

#-------------------------------------------------------------
# PDATE settings for this test driver script correspond to 
# available data in the $DATAROOT space (defined below).  
# References to netcdf and binary indicate available files in 
# the test_data space.
#
#export PDATE=${PDATE:-2019083100}		# netcdf
export PDATE=${PDATE:-2018091706}      	# binary
export NET=${NET:-gfs}
export RUN=${RUN:-gdas}


export PDY=`echo $PDATE | cut -c1-8`
export cyc=`echo $PDATE | cut -c9-10`
export job=gdas_verfozn.${cyc}
export pid=${pid:-$$}
export jobid=${job}.${pid}
export envir=${envir:-test}
export DATAROOT=${DATAROOT:-/gpfs/dell2/emc/modeling/noscrub/${LOGNAME}/test_data}
export COMROOT=/gpfs/dell2/ptmp/${LOGNAME}/com
export OZN_WORK_DIR=${OZN_WORK_DIR:-/gpfs/dell2/stmp/${LOGNAME}/oznmon.${pid}}

#------------------------------------------------------------
# Specify versions
#
export gdas_oznmon_ver=v2.0.0
export oznmon_shared_ver=v2.0.0


#------------------------------------------------------------
# Load modules
#
#. /usrx/local/Modules/3.2.9/init/ksh
#module use /nwprod2/modulefiles

shell=ksh
source /usrx/local/prod/lmod/lmod/init/${shell}

MODULEPATH=/usrx/local/prod/lmod/lmod/modulefiles/Core
MODULEPATH=${MODULEPATH}:/usrx/local/prod/modulefiles/core_third
MODULEPATH=${MODULEPATH}:/usrx/local/prod/modulefiles/defs
MODULEPATH=${MODULEPATH}:/gpfs/dell1/nco/ops/nwprod/modulefiles/core_prod
export MODULEPATH=${MODULEPATH}:/usrx/local/dev/modulefiles

module purge

module load lsf/10.1
module load ips/18.0.1.163
module load impi/18.0.1
module load prod_util/1.1.0
module load grib_util/1.1.0
module load util_shared/1.1.0


module list


#------------------------------------------------------------
# WCOSS environment settings
#
export POE=YES


#------------------------------------------------------------
# Set user specific variables
#
export OZNMON_SUFFIX=${OZNMON_SUFFIX:-testozn}
export NWTEST=${NWTEST:-/gpfs/dell2/emc/modeling/noscrub/Edward.Safford/ProdGSI/util/Ozone_Monitor/nwprod}
export HOMEgdas_ozn=${NWTEST}/gdas_oznmon.${gdas_oznmon_ver}
export PARMgdas_ozn=${HOMEgdas_ozn}/parm
export FIXgdas_ozn=${FIXgdas_ozn:-${HOMEgdas_ozn}/fix}


export HOMEgfs_ozn=${HOMEgfs_ozn:-${HOMEgdas_ozn}}
export PARMgfs_ozn=${PARMgfs_ozn:-${PARMgdas_ozn}}
export FIXgfs_ozn=${FIXgfs_ozn:-${FIXgdas_ozn}}

export JOBGLOBAL=${JOBGLOBAL:-${HOMEgdas_ozn}/jobs}
export HOMEoznmon=${HOMEoznmon:-${NWTEST}/oznmon_shared.${shared_oznmon_ver}}

#export SCRgdas_ozn=${HOMEgdas_ozn}/scripts
#JOBgdas_ozn=${HOMEgdas_ozn}/jobs

export HOMEoznmon=${NWTEST}/oznmon_shared.${oznmon_shared_ver}
export COM_IN=${COM_IN:-$DATAROOT}
export OZN_TANKDIR=${OZN_TANKDIR:-${COMROOT}/${OZNMON_SUFFIX}}


#------------------------------------------------------------
# Execute job
#
${JOBGLOBAL}/JGDAS_VERFOZN

exit

