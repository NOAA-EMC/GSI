#!/bin/ksh
################################################################################
# This script runs the enkf post processing in the GDAS cycle.
# Usage: epos.sh
# Imported variables:
#   CONFIG
#   CDATE
#   CDUMP
#   CSTEP
# Configuration variables:
#   DATATMP
#   COMROT
#   NCP
#   NDATE
#   PBEG
#   PERR
#   PEND
################################################################################
set -ux

################################################################################
# Go configure

export CKSH=$(echo $CSTEP|cut -c-4)
export CKND=$(echo $CSTEP|cut -c5-)
set -a;. $CONFIG;set +a
export machine=${machine:-WCOSS}
machine=$(echo $machine|tr '[a-z]' '[A-Z]')
eval export DATA=$DATATMP
cd;rm -rf $DATA||exit 1;mkdir -p $DATA||exit 1;cd $DATA||exit 1
#chgrp ${group_name:-rstprod} $DATA
chmod ${permission:-755} $DATA
#
export HOMEDIR=${HOMEDIR:-/nwprod}
export EXECDIR=${EXECDIR:-$HOMEDIR/exec}
export FIXDIR=${FIXDIR:-$HOMEDIR/fix/fix_am}
export FIXGLOBAL=${FIXGLOBAL:-$FIXDIR}
export SCRDIR=${SCRDIR:-$HOMEDIR/scripts}
export SHDIR=${SHDIR:-$HOMEDIR/bin}
export NWPROD=${NWPROD:-$HOMEDIR}
#
export PBEG=${PBEG:-$SHDIR/pbeg}
export PEND=${PEND:-$SHDIR/pend}
export PERR=${PERR:-$SHDIR/perr}

export FILESTYLE=${FILESTYLEEPOS:-'L'}

if [ $machine = WCOSS ] ; then
   export MPMD=${MPMD_ECEN:-YES}  # YES = run as MPMD
   export POE=${MPMD_ECEN:-YES}
fi

$PBEG

################################################################################
# Set other variables

export PCOP=${PCOP:-$SHDIR/pcop}
export NDATE=${NDATE:-${NWPROD}/util/exec/ndate}
export CYINC=${CYINC:-06}
export CDFNL=${CDFNL:-fnl}
cycle=$(echo $CDATE|cut -c9-10)
cdump=$(echo $CDUMP|tr '[a-z]' '[A-Z]')

export NET=gdas
export RUN=gdas1
export envir=prod
export COMSP=$DATA/
export COMIN=${COMIN:-$COMROT}
export COMOUT=${COMOUT:-$COMROT}
export SENDCOM=${SENDCOM:-YES}
export ENKFPOSTSH=${ENKFPOSTSH:-$SCRDIR/exglobal_enkf_post.sh.sms}
export GETSFCENSMEANEXEC=${GETSFCENSMEANEXEC:-$EXECDIR/getsfcensmean.x}
export GETNSTENSMEANEXEC=${GETNSTENSMEANEXEC:-$EXECDIR/getnstensmean.x}
export GETATMENSMEANEXEC=${GETATMENSMEANEXEC:-$EXECDIR/getsigensmean_smooth.x}
export HYBENSMOOTH=${HYBENSMOOTH:-/$FIXGLOBAL/global_hybens_smoothinfo.l64.txt}

#############################
# Set up the UTILITIES
##############################
export ushscript=${USHGLOBAL:-${NWPROD}/ush}
export utilscript=${USHUTIL:-${NWPROD}/util/ush}
export utilities=${USHUTIL:-${NWPROD}/util/ush}
export jlogfile=${jlogfile:-""}

export pgmout=stdout
export nknd=${CKND:-1}
export FHMIN=$(eval echo \${FHMINEFCS$cycle$cdump:-3}|cut -f$nknd -d,)
export FHMAX=$(eval echo \${FHMAXEFCS$cycle$cdump:-9}|cut -f$nknd -d,)
export FHOUT=$(eval echo \${FHOUTEFCS$cycle$cdump:-3}|cut -f$nknd -d,)


################################################################################
# Compute ensemble means and smooth ensemble forecasts

$ENKFPOSTSH
rc=$?
if [[ $rc -ne 0 ]];then $PERR;exit 1;fi


################################################################################
# Exit gracefully

DO_PRODNAMES=${DO_PRODNAMES:-NO}
PRODNAMES_DIR=${PRODNAMES_DIR:-$COMROT/prod}
SETUPPRODNAMESH=${SETUPPRODNAMESH:-$USHDIR/setup_prodnames.sh}
CNVGRIB2=${CNVGRIB2:-NO}

#
# If requested, make symbolic links to create ops-like /com/gfs files
if [ $DO_PRODNAMES = YES ] ; then
  rcc=0
  if [ ! -s $PRODNAMES_DIR ] ; then
    mkdir -p $PRODNAMES_DIR
    rcc=$?
  fi
  if [[ $rcc -eq 0 ]]; then
     $SETUPPRODNAMESH $COMROT $PRODNAMES_DIR $CDATE enkf $HRKSIG NO          
  else
     echo "EPOS:  ***WARNING*** CANNOT mkdir $PRODNAMES_DIR.  Will NOT run $SETUPPRODNAMESH"
  fi
fi


if [[ $rc -ne 0 ]];then $PERR;exit 1;fi
$PEND
