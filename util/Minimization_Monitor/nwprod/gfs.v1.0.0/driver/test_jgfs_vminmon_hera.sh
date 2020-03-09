#!/bin/ksh

#SBATCH -o gfs_verfrad.o%j
#SBATCH -J gfs_verfrad
#SBATCH --ntasks=1 --mem=5g
#SBATCH --time=20
#SBATCH --account=fv3-cpu
#SBATCH -D .

set -x

export PDATE=${PDATE:-2016030706}


#############################################################
# Specify whether the run is production or development
#############################################################
export PDY=`echo $PDATE | cut -c1-8`
export cyc=`echo $PDATE | cut -c9-10`
export job=gfs_vminmon.${cyc}
export pid=${pid:-$$}
export jobid=${job}.${pid}
export envir=para
export DATAROOT=${DATAROOT:-/scratch1/NCEPDEV/da/Edward.Safford/noscrub/test_data}
export COMROOT=${COMROOT:-/scratch2/NCEPDEV/stmp3/$LOGNAME/com}
export STMP_USER=${STMP_USER:-/scratch2/NCEPDEV/stmp3/$LOGNAME}


#############################################################
# Specify versions
#############################################################
export gfs_ver=v1.0.0
export global_shared_ver=v1.0.1


#############################################################
# Set user specific variables
#############################################################
export MINMON_SUFFIX=${MINMON_SUFFIX:-testminmon}

export NWTEST=${NWTEST:-/scratch1/NCEPDEV/da/${LOGNAME}/noscrub/ProdGSI/util/Minimization_Monitor/nwprod}
export HOMEgfs=${HOMEgfs:-${NWTEST}/gfs.${gfs_ver}}
export JOBGLOBAL=${HOMEgfs}/jobs
export HOMEminmon=${HOMEminmon:-${NWTEST}/minmon_shared.${global_shared_ver}}
export COM_IN=${COM_IN:-${DATAROOT}}
export M_TANKverf=${M_TANKverf:-${COMROOT}/${MINMON_SUFFIX}}
export M_FIXgfs=${M_FIXgfs:-${HOMEgfs}/fix}

#######################################################################
#  theia specific hacks for no prod_utils module (ndate)
#######################################################################
export MY_MACHINE=hera
export NDATE=/home/Edward.Safford/bin/ndate

export PERL5LIB="/usr/lib64/perl5:/usr/share/perl5"
export VERBOSE=YES
export KEEPDATA=YES

#############################################################
# Execute job
#############################################################
$JOBGLOBAL/JGFS_VMINMON

exit

