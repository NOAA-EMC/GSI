#!/bin/ksh

#SBATCH -o gdas_verfrad.o%j
#SBATCH -J gdas_verfrad
#SBATCH --ntasks=1 --mem=5g
#SBATCH --time=20
#SBATCH --account=fv3-cpu
#SBATCH -D .


set -x

export MY_MACHINE=hera

#export PDATE=${PDATE:-2018091712}	#binary
export PDATE=${PDATE:-2018110206}	#NetCDF

#############################################################
# Specify whether the run is production or development
#############################################################
export PDY=`echo $PDATE | cut -c1-8`
export cyc=`echo $PDATE | cut -c9-10`
export job=gdas_verfrad.${cyc}
export pid=${pid:-$$}
export jobid=${job}.${pid}
export envir=para
export DATAROOT=${DATAROOT:-/scratch1/NCEPDEV/da/Edward.Safford/noscrub/test_data}
export COMROOT=${COMROOT:-/scratch2/NCEPDEV/stmp3/${LOGNAME}/com}


#############################################################
# Specify versions
#############################################################
export gdas_ver=v14.1.0
export global_shared_ver=v14.1.0
export gdas_radmon_ver=v3.0.0
export radmon_shared_ver=v3.0.0


#############################################################
# Set user specific variables
#############################################################

export RADMON_SUFFIX=${RADMON_SUFFIX:-testrad}
export NWTEST=${NWTEST:-/scratch1/NCEPDEV/da/Edward.Safford/noscrub/GSI/util/Radiance_Monitor/nwprod}

export HOMEgdas=${HOMEgdas:-${NWTEST}/gdas_radmon.${gdas_radmon_ver}}
export HOMEgfs=$HOMEgdas
export FIXgdas=${HOMEgdas}/fix

export JOBGLOBAL=${JOBGLOBAL:-${HOMEgdas}/jobs}
export HOMEradmon=${HOMEradmon:-${NWTEST}/radmon_shared.${radmon_shared_ver}}
export COM_IN=${COM_IN:-${DATAROOT}}
export TANKverf=${TANKverf:-${COMROOT}/${RADMON_SUFFIX}}

export SUB=${SUB:-/apps/slurm/default/bin/sbatch}
export NDATE=${NDATE:-/home/Edward.Safford/bin/ndate}

export parm_file=${HOMEgdas}/parm/gdas_radmon.parm


prevday=`$NDATE -24 $PDATE`
export PDYm1=`echo $prevday | cut -c1-8`
						
	
#############################################################
# Execute job
#
$JOBGLOBAL/JGDAS_VERFRAD

exit

