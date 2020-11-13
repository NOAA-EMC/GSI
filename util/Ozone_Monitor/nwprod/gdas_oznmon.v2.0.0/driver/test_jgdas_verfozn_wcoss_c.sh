#!/bin/ksh

#BSUB -o gdas_verfozn.o%J
#BSUB -e gdas_verfozn.o%J
#BSUB -J gdas_verfozn
#BSUB -q dev
#BSUB -M 100
#BSUB -W 00:05
#BSUB -P GFS-DEV
#BSUB -R "select[mem>100] rusage[mem=100]"

##BSUB -cwd /gpfs/hps/ptmp/Edward.Safford
##BSUB -cwd ${PWD}

##------------------------------------------------------------
##  This is the test driver script for the wcoss/cray systems
##  to run the JGDAS_VERFOZN job.
##------------------------------------------------------------

set -x
export OZNMON_NEW_HDR=${OZN_NEW_HDR:-0}
export PDATE=${PDATE:-2019083100}
export NET=${NET:-gfs}
export RUN=${RUN:-gdas}

export PDY=`echo $PDATE | cut -c1-8`
export cyc=`echo $PDATE | cut -c9-10`
export job=gdas_verfozn.${cyc}
export pid=${pid:-$$}
export jobid=${job}.${pid}
export envir=${envir:-test}

export DATAROOT=${DATAROOT:-/gpfs/hps3/emc/da/noscrub/${LOGNAME}/test_data}
export COMROOT=/gpfs/hps2/ptmp/${LOGNAME}/com
export OZN_WORK_DIR=${OZN_WORK_DIR:-/gpfs/hps2/stmp/${LOGNAME}/oznmon.${pid}}

#------------------------------------------------------------
# Specify versions
#
export gdas_oznmon_ver=v2.0.0
export oznmon_shared_ver=v2.0.0


#############################################################
# Load modules
#############################################################
. $MODULESHOME/init/ksh

module load prod_util
module load util_shared

module list


#------------------------------------------------------------
# WCOSS environment settings
#
export POE=YES

#------------------------------------------------------------
# Set user specific variables
#
export OZNMON_SUFFIX=${OZNMON_SUFFIX:-testozn}
export NWTEST=${NWTEST:-/gpfs/hps3/emc/da/noscrub/Edward.Safford/ProdGSI/util/Ozone_Monitor/nwprod}

export HOMEgdas_ozn=${NWTEST}/gdas_oznmon.${gdas_oznmon_ver}
export PARMgdas_ozn=${HOMEgdas_ozn}/parm
export SCRgdas_ozn=${HOMEgdas_ozn}/scripts
export FIXgdas_ozn=${HOMEgdas_ozn}/fix
JOBgdas_ozn=${HOMEgdas_ozn}/jobs

export HOMEgfs_ozn=${HOMEgfs_ozn:-${HOMEgdas_ozn}}
export PARMgfs_ozn=${PARMgfs_ozn:-${PARMgdas_ozn}}
export FIXgfs_ozn=${FIXgfs_ozn:-${FIXgdas_ozn}}

export HOMEoznmon=${NWTEST}/oznmon_shared.${oznmon_shared_ver}
export COM_IN=${COM_IN:-$DATAROOT}
export OZN_TANKDIR=${OZN_TANKDIR:-${COMROOT}/${OZNMON_SUFFIX}}

#------------------------------------------------------------
# Execute job
#
${JOBgdas_ozn}/JGDAS_VERFOZN

exit

