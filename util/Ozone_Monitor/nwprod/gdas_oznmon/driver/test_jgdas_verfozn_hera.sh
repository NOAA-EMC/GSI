#!/bin/ksh

#SBATCH -o gdas_verfozn.o%j
#SBATCH -J gdas_verfozn
#SBATCH --ntasks=1 --mem=5g
#SBATCH --time=10
#SBATCH --account=fv3-cpu
#SBATCH -D .

set -x

export OZNMON_NEW_HDR=${OZN_NEW_HDR:-0}

export PDATE=${PDATE:-2020032106}	# netcdf
#export PDATE=${PDATE:-2020013000}	# bin
export NET=${NET:-gfs}
export RUN=${RUN:-gdas}

export PDY=`echo $PDATE | cut -c1-8`
export cyc=`echo $PDATE | cut -c9-10`
export job=${job:-gdas_verfozn.${cyc}}
export pid=${pid:-$$}
export jobid=${job}.${pid}
export envir=${envir:-test}
export DATAROOT=${DATAROOT:-/scratch1/NCEPDEV/da/Edward.Safford/noscrub/test_data}
export COMROOT=${COMROOT:-/scratch2/NCEPDEV/stmp3/${LOGNAME}/com}
export OZN_WORK_DIR=${OZN_WORK_DIR:-/scratch2/NCEPDEV/stmp3/${LOGNAME}/oznmon.${pid}}


#------------------------------------------------------------
# Set package definitions. 
#
export OZNMON_SUFFIX=${OZNMON_SUFFIX:-testozn}
export NWTEST=${NWTEST:-/scratch1/NCEPDEV/da/Edward.Safford/noscrub/update/util/Ozone_Monitor/nwprod}
export HOMEgdas_ozn=${HOMEgdas_ozn:-${NWTEST}/gdas_oznmon}
export PARMgdas_ozn=${PARMgdas_ozn:-${HOMEgdas_ozn}/parm}
export FIXgdas_ozn=${FIXgdas_ozn:-${HOMEgdas_ozn}/fix}

export HOMEgfs=${HOMEgfs:-${HOMEgdas_ozn}}
export HOMEgfs_ozn=${HOMEgfs_ozn:-${HOMEgfs}}
export PARMgfs_ozn=${PARMgfs_ozn:-${PARMgdas_ozn}}
export FIXgfs_ozn=${FIXgfs_ozn:-${FIXgdas_ozn}}

export JOBGLOBAL=${JOBGLOBAL:-${HOMEgdas_ozn}/jobs}
export HOMEoznmon=${HOMEoznmon:-${NWTEST}/oznmon_shared}
export COM_IN=${COM_IN:-${DATAROOT}}
export OZN_TANKDIR=${OZN_TANKDIR:-${COMROOT}/${OZNMON_SUFFIX}}

export SUB=${SUB:-/apps/slurm/default/bin/sbatch}
export NDATE=${NDATE:-/home/Edward.Safford/bin/ndate}



#------------------------------------------------------------
#  hera specific hack for no prod_utils module 
#
export MY_MACHINE=hera
prevday=`$NDATE -24 $PDATE`
export PDYm1=`echo $prevday | cut -c1-8`
export PATH=$PATH:${COMROOT}

#------------------------------------------------------------
# Execute job
#
$JOBGLOBAL/JGDAS_ATMOS_VERFOZN

exit

