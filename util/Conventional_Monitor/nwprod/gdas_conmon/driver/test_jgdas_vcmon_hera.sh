#!/bin/ksh -l

#SBATCH -o %x.log
#SBATCH -J gdas_vconmon
#SBATCH --time=00:40:00
#SBATCH --ntasks=1 -p service --mem=4g
#SBATCH -A da-cpu


set -x

#export PDATE=${PDATE:-2020012018}  # binary
export PDATE=${PDATE:-2020040306}   # netcdf

#############################################################
# Specify versions
#############################################################
export gdas_ver=v15.0.0
export global_shared_ver=v15.0.0
#export gdas_conmon_ver=v1.0.0
#export conmon_shared_ver=v1.0.0

module use -a /apps/modules/modulefamilies/intel
module load wgrib2/2.0.8
export WGRIB2=`which wgrib2`
echo WGRIB2 = $WGRIB2

#############################################################
# Specify whether the run is production or development
#############################################################
export PDY=`echo $PDATE | cut -c1-8`
export CYC=`echo $PDATE | cut -c9-10`
export job=gfs_vconmon.${CYC}
export pid=${pid:-$$}
export jobid=${job}.${pid}
export DATAROOT=${DATAROOT:-/scratch1/NCEPDEV/da/Edward.Safford/noscrub/test_data}
export COMROOT=${COMROOT:-/scratch2/NCEPDEV/stmp3/$LOGNAME/com}



#############################################################
# Add nwpara tools to path
#############################################################
#export NWPROD=${NWPROD:-/scratch4/NCEPDEV/global/save/glopara/nwpara/util}
#NWPRODush=${NWPRODush:=${NWPROD}/ush}
#NWPRODexec=${NWPRODexec:=${NWPROD}/exec}
#export PATH=${PATH}:${NWPRODush}:${NWPRODexec}


#############################################################
# Set user specific variables
#############################################################
export CONMON_SUFFIX=${CONMON_SUFFIX:-test_conmon}

export NWTEST=${NWTEST:-/scratch1/NCEPDEV/da/${LOGNAME}/noscrub/GSI/util/Conventional_Monitor/nwprod}
export HOMEconmon=${HOMEconmon:-${NWTEST}/conmon_shared}

export HOMEgdas_conmon=${HOMEgdas_conmon:-${NWTEST}/gdas_conmon}
export HOMEgfs_conmon=${HOMEgdas_conmon}

export JOBGLOBAL=${JOBGLOBAL:-${HOMEgdas_conmon}/jobs}
export C_COM_IN=${C_COM_IN:-${DATAROOT}}

export C_TANKDIR=${C_TANKDIR:-${COMROOT}/${CONMON_SUFFIX}}
if [[ ! -d $C_TANKDIR ]]; then
   mkdir -p ${C_TANKDIR}
fi

export CONMON_WORK_DIR=${CONMON_WORK_DIR:-/scratch2/NCEPDEV/stmp1/${LOGNAME}/conmon_${CONMON_SUFFIX}}
if [[ ! -d $CONMON_WORK_DIR ]]; then
   mkdir -p ${CONMON_WORK_DIR}
fi


#######################################################################
#  hera specific hacks for no prod_utils module
#######################################################################
export NDATE=${NDATE:-/home/Edward.Safford/bin/ndate}
export MY_MACHINE=hera

prevday=`$NDATE -24 $PDATE`
export PDYm1=`echo $prevday | cut -c1-8`


#############################################################
# Execute job
#############################################################
$JOBGLOBAL/JGDAS_ATMOS_CONMON

exit

