#!/bin/ksh
################################################################################
# This script retrieves data assimilation files.
# Usage: dcop.sh
# Imported variables:
#   CONFIG
#   CDATE
#   CDUMP
#   CSTEP
# Configuration variables:
#   RLIST
#   DATATMP
#   COMDMP
#   NCP
#   GETGDASSH
#   PBEG
#   PCOP
#   PERR
#   PEND
################################################################################
set -ux

################################################################################
# Go configure

export CKSH=$(echo $CSTEP|cut -c-4)
export CKND=$(echo $CSTEP|cut -c5-)
set -a;. $CONFIG;set +a
eval export DATA=$DATATMP
cd;rm -rf $DATA||exit 1;mkdir -p $DATA||exit 1;cd $DATA||exit 1
#chgrp ${group_name:-rstprod} $DATA
chmod ${permission:-755} $DATA
#
export HOMEDIR=${HOMEDIR:-..}
export SHDIR=${SHDIR:-$HOMEDIR/bin}
export USHDIR=${USHDIR:-$HOMEDIR/ush}
export NWPROD=${NWPROD:-HOMEDIR}
#
export PBEG=${PBEG:-$SHDIR/pbeg}
export PEND=${PEND:-$SHDIR/pend}
export PERR=${PERR:-$SHDIR/perr}
$PBEG

################################################################################
# Set other variables

COMDMPTMP=${COMDMPTMP:-$COMDMP}
COMROTTMP=${COMROTTMP:-$COMROT}
eval export COMDMP=$COMDMPTMP
eval export COMROT=$COMROTTMP
export PCOP=${PCOP:-$SHDIR/pcop}
export NDATE=${NDATE:-$NWPROD/util/exec/ndate}
export USHGLOBAL=${USHGLOBAL:-${USHDIR:-$HOMEDIR/ush}}
export GETGDASSH=${GETGDASSH:-$USHGLOBAL/global_getgdas.sh}
export CYINC=${CYINC:-06}
export CDFNL=${CDFNL:-fnl}
export COMOUT=$DATA
export PREOUT=''
export SUFOUT='.$CDUMP.$CDATE'
export VERBOSE=YES

################################################################################
# Get data assimilation files
  
if [[ $CDATE = ????????00 ]]; then
if [[ $CDUMP = gfs ]];then
  $GETGDASSH $CDATE $CDUMP sfcanl siganl
else
  $GETGDASSH $CDATE $CDUMP biascr satang sfcanl siganl
fi
fi
  
################################################################################
# Copy out data assimilation files
  
mkdir -p $COMDMP
$PCOP $CDATE/$CDUMP/$CSTEP/DMPO $DATA $COMDMP <$RLIST
rc=$?

################################################################################
# Exit gracefully

if [[ $rc -ne 0 ]];then $PERR;exit 1;fi
$PEND
