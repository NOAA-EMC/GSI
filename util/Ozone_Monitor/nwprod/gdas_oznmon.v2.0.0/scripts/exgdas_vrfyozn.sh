#/bin/sh

set -ax

################################################################################
####  UNIX Script Documentation Block
#                      .                                             .
# Script name:         exgdas_vrfyozn.sh
# Script description:  Runs data extract/validation for global ozone diag data
#
# Author:        Ed Safford       Org: NP23         Date: 2012-01-18
#
# Abstract: This script runs the data extract/validation portion of the 
#           OznMon package.  
#
# Script history log:
#
#   Input script positional parameters:
#     1             Current analysis date in yyyymmddhh format
#                   defaults to PDY; required
#     2             cycle time in cc format
#                   defaults to cyc; required
#
#     input data : $oznstat
#
#     output data:  
#
#  Remarks:
#
#    Condition codes
#       0 - no problem encountered
#      >0 - some problem encountered
#
################################################################################
export scr=exgdas_vrfyozn.sh

err=0

#-------------------------------------------------------------------------------
#  Set environment
#
export RUN_ENVIR=${RUN_ENVIR:-nco}
export NET=${NET:-gfs}
export RUN=${RUN:-gdas}
export envir=${envir:-prod}
export COMPONENT=${COMPONENT:-atmos}

#  Command line arguments
export PDY=${1:-${PDY:?}} 
export cyc=${2:-${cyc:?}}

#  Directories
export OZN_WORK_DIR=${OZN_WORK_DIR:-$(pwd)}
export COM_IN=${COM_IN:-${COMROOT}/${NET}/${envir}}
export COMIN=${COMIN:-$COM_IN/${RUN}.${PDY}/${cyc}/$COMPONENT}

export HOMEgdas_ozn=${HOMEgdas_ozn:-${NWROOT}/gdas.${gdas_oznmon_ver}}
export FIXgdas_ozn=${FIXgdas_ozn:-$HOMEgdas/fix}

export HOMEoznmon=${HOMEoznmon:-/${NWROOT}/oznmon_shared.v${shared_oznmon_ver}}
export EXECoznmon=${EXECoznmon:-$HOMEoznmon/exec}
export FIXoznmon=${FIXoznmon:-${HOMEoznmon}/fix}
export USHoznmon=${USHoznmon:-$HOMEoznmon/ush}


#  Filenames
export oznstat=${oznstat:-$COMIN/gdas.t${cyc}z.oznstat}
export satype_file=${satype_file:-$FIXgdas_ozn/gdas_oznmon_satype.txt}

#  Other variables
#export USE_ANL=${USE_ANL:-1}
export PDATE=${PDY}${cyc}
export DO_DATA_RPT=${DO_DATA_RPT:-1}
export NCP=${NCP:-/bin/cp}
export NDATE=${NDATE:-/nwprod/util/exec/ndate}


##################################################################
# ensure work and TANK dirs exist, verify oznstat is available
if [[ ! -d ${OZN_WORK_DIR} ]]; then
   mkdir $OZN_WORK_DIR
fi
cd $OZN_WORK_DIR

if [[ ! -d ${TANKverf_ozn} ]]; then
   mkdir -p $TANKverf_ozn
fi

if [[ -s ${oznstat} ]]; then
   echo ${oznstat} is available
fi

#####################################################################

data_available=0

if [[ -s ${oznstat} ]]; then
   data_available=1                                         

   #------------------------------------------------------------------
   #  Copy data files file to local data directory.  
   #  Untar oznstat file.  
   #------------------------------------------------------------------

   $NCP $oznstat ./oznstat.$PDATE

   tar -xvf oznstat.$PDATE
   rm oznstat.$PDATE

   netcdf=0
   count=`ls diag* | grep ".nc4" | wc -l`
   if [ $count -gt 0 ] ; then
      netcdf=1
      for filenc4 in `ls diag*nc4.gz`; do
         file=`echo $filenc4 | cut -d'.' -f1-2`.gz
         mv $filenc4 $file
      done
   fi
   
   export OZNMON_NETCDF=${netcdf}

   ${HOMEoznmon}/ush/ozn_xtrct.sh
   err=$?

else
   # oznstat file is not available
   err=1
fi


if [[ "$VERBOSE" = "YES" ]]; then
   echo "end exgdas_vrfyozn.sh, exit value = ${err}"
fi


set +x
exit ${err}

