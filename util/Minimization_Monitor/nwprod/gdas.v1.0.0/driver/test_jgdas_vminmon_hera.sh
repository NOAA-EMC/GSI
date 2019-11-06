#!/bin/ksh

#SBATCH -o gdas_verfrad.o%j
#SBATCH -J gdas_verfrad
#SBATCH --ntasks=1 --mem=5g
#SBATCH --time=20
#SBATCH --account=fv3-cpu
#SBATCH -D .

set -x

export PDATE=${PDATE:-2018070418}


#############################################################
# Specify whether the run is production or development
#############################################################
export PDY=`echo $PDATE | cut -c1-8`
export cyc=`echo $PDATE | cut -c9-10`
export job=gdas_vminmon.${cyc}
export pid=${pid:-$$}
export jobid=${job}.${pid}
export envir=para
export DATAROOT=${DATAROOT:-/scratch1/NCEPDEV/da/Edward.Safford/noscrub/test_data}
export COMROOT=${COMROOT:-/scratch2/NCEPDEV/stmp3/$LOGNAME/com}
export STMP_USER=${STMP_USER:-/scratch2/NCEPDEV/stmp3/$LOGNAME}


#############################################################
# Specify versions
#############################################################
export gdas_ver=v1.0.0
export global_shared_ver=v1.0.1

#############################################################
# Add nwpara tools to path
#############################################################
#NWPROD=${NWPROD:-/scratch4/NCEPDEV/global/save/glopara/nwpara/util}
#NWPRODush=${NWPRODush:=${NWPROD}/ush}
#NWPRODexec=${NWPRODexec:=${NWPROD}/exec}
#export PATH=${PATH}:${NWPRODush}:${NWPRODexec}


#############################################################
# Set user specific variables
#############################################################
export MINMON_SUFFIX=${MINMON_SUFFIX:-testminmon}

export NWTEST=${NWTEST:-/scratch1/NCEPDEV/da/${LOGNAME}/noscrub/ProdGSI/util/Minimization_Monitor/nwprod}
export HOMEgdas=${HOMEgdas:-${NWTEST}/gdas.${gdas_ver}}
export HOMEgfs=${HOMEgfs:-${HOMEgdas}}
export JOBGLOBAL=${HOMEgdas}/jobs
export HOMEminmon=${HOMEminmon:-${NWTEST}/minmon_shared.${global_shared_ver}}
export COM_IN=${COM_IN:-${DATAROOT}}
export M_TANKverf=${M_TANKverf:-${COMROOT}/${MINMON_SUFFIX}}
export M_FIXgdas=${M_FIXgdas:-${HOMEgdas}/fix}

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
$JOBGLOBAL/JGDAS_VMINMON

exit

