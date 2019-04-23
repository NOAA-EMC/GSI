#!/bin/ksh

#BSUB -o gdas_verfrad.o%J
#BSUB -e gdas_verfrad.o%J
#BSUB -J gdas_verfrad
#BSUB -q dev_shared
#BSUB -M 5000
#BSUB -W 00:30
#BSUB -P GFS-T2O

=======


ulimit -c 0
ulimit -s unlimited

set -x

export MY_MACHINE=cray

#export PDATE=${PDATE:-2017050406}   # binary
export PDATE=${PDATE:-2018110206}   # NetCDF

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
#export gdas_ver=v15.0.0
export gdas_ver=v3.0.0
#export global_shared_ver=v15.0.0
export global_shared_ver=v3.0.0

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
#export COMROOT=${COMROOT:-/gpfs/hps3/ptmp/$LOGNAME/com}
export COMROOT=/gpfs/hps3/ptmp/$LOGNAME/com
export RADMON_SUFFIX=${RADMON_SUFFIX:-testrad}

export NWTEST=${NWTEST:-/gpfs/hps3/emc/da/noscrub/${LOGNAME}/ProdGSI/util/Radiance_Monitor/nwprod}
export HOMEgdas=${NWTEST}/gdas_radmon.${gdas_radmon_ver}
export HOMEgfs=${HOMEgdas}
export FIXgdas=${FIXgdas:-$HOMEgfs/fix}

export JOBGLOBAL=${HOMEgdas}/jobs
export HOMEradmon=${NWTEST}/radmon_shared.${radmon_shared_ver}
export COM_IN=${DATAROOT}
export TANKverf=${TANKverf:-${COMROOT}/${RADMON_SUFFIX}}

export parm_file=${HOMEgdas}/parm/gdas_radmon.parm

#############################################################
# Execute job
#############################################################
$JOBGLOBAL/JGDAS_VERFRAD

exit

