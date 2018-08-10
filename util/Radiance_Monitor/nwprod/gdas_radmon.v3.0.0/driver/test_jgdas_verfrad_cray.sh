#!/bin/ksh

#BSUB -o gdas_verfrad.o%J
#BSUB -e gdas_verfrad.o%J
#BSUB -J gdas_verfrad
#BSUB -q dev
#BSUB -M 100
#BSUB -W 00:40
#BSUB -P GFS-T2O
#BSUB -R "select[mem>100] rusage[mem=100]"

##BSUB -cwd /gpfs/hps2/ptmp/Edward.Safford
##BSUB -cwd ${PWD}
=======


ulimit -c 0
ulimit -s unlimited

set -x

export MY_MACHINE=cray

export PDATE=${PDATE:-2016100206}

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
export gdas_ver=v15.0.0
export global_shared_ver=v15.0.0

export gdas_radmon_ver=v3.0.0
export radmon_shared_ver=v3.0.0


#############################################################
# Load modules
#############################################################
. $MODULESHOME/init/ksh

module load prod_util
module list

#echo "locating aprun:"
#which aprun

#############################################################
# WCOSS environment settings
#############################################################
export POE=YES


#############################################################
# Set user specific variables
#############################################################
export DATAROOT=${DATAROOT:-/gpfs/hps3/emc/da/noscrub/$LOGNAME/test_data}
export COMROOT=${COMROOT:-/gpfs/hps2/ptmp/$LOGNAME/com}
export RADMON_SUFFIX=${RADMON_SUFFIX:-testrad}

export NWTEST=${NWTEST:-/gpfs/hps3/emc/da/noscrub/${LOGNAME}/ProdGSI/util/Radiance_Monitor/nwprod}
export HOMEgdas=${NWTEST}/gdas_radmon.${gdas_radmon_ver}
export JOBGLOBAL=${HOMEgdas}/jobs

export HOMEradmon=${NWTEST}/radmon_shared.${radmon_shared_ver}
export COM_IN=${DATAROOT}
export TANKverf=${TANKverf:-${COMROOT}/${RADMON_SUFFIX}}


#############################################################
# Execute job
#############################################################
$JOBGLOBAL/JGDAS_VERFRAD

exit

