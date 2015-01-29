#!/bin/ksh
################################################################################
# This script runs ensemble member forecasts for enkf GDAS
# Usage: efcs.sh
# Imported variables:
#   CONFIG
#   CDATE
#   CDUMP
#   CSTEP
# Configuration variables:
#   INFOLEVELFCS
#   PMDLOGANAL
#   FNTSFATMP
#   SMIPCPTMP
#   TMIPCPTMP
#   DATATMP
#   COMIN
#   COMRS
#   COMROT
#   NCP
#   FORECASTSH
#   PBEG
#   PERR
#   PEND
################################################################################
set -ux

################################################################################
# Go configure

set -a;. $CONFIG;set +a
export CKSH=$(echo $CSTEP|cut -c-4)
export CKND=$(echo $CSTEP|cut -c5-)
export machine=${machine:-ZEUS}
machine=$(echo $machine|tr '[a-z]' '[A-Z]')
export CSTEPCOP=efcs
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

export FILESTYLE=${FILESTYLEEFCS:-'L'}

$PBEG

################################################################################
# Set other variables

export PCOP=${PCOP:-$SHDIR/pcop}
export ENKFFCSTSH=${ENKFFCSTSH:-$SCRDIR/exglobal_enkf_fcst.sh.sms}
export FORECASTSH=${FORECASTSH:-$SCRDIR/exglobal_fcst.sh.sms}
export VERBOSE=YES
export CDFNL=${CDFNL:-gdas}

export COMIN=${COMIN:-$COMROT}
export COMOUT=${COMOUT:-$COMROT}

#
export NTHREADS=${NTHREADS_EFCS:-2}
export NTHSTACK=${NTHSTACK_EFCS:-1024000000}

[[ -n ${AMEXECTMP:-""} ]]&&eval export AM_EXEC=$AMEXECTMP
export FCSTEXEC=$AM_EXEC

################################################################################
# Copy in restart and input files

##$PCOP $CDATE/$CDUMP/$CSTEPCOP/ROTI $COMROT $DATA <$RLIST
##rc=$?
##if [[ $rc -ne 0 ]];then $PERR;exit 1;fi

##$PCOP $CDATE/$CDUMP/$CSTEPCOP/OPTI $COMROT $DATA <$RLIST


################################################################################
# Run ensemble forecasts

$ENKFFCSTSH
rc=$?
if [[ $rc -ne 0 ]];then $PERR;exit 1;fi


################################################################################
# Exit gracefully

if [[ $rc -ne 0 ]];then $PERR;exit 1;fi
$PEND
