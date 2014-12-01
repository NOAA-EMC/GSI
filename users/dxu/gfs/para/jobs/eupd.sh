#!/bin/ksh
################################################################################
# This script runs the enkf update (analysis)
# Usage: eupd.sh
# Imported variables:
#   CONFIG
#   CDATE
#   CDUMP
#   CSTEP
# Configuration variables:
#   INFOLEVELANAL
#   PMDLOGANAL
#   FNTSFATMP
#   SMIPCPTMP
#   TMIPCPTMP
#   DATATMP
#   COMIN
#   COMRS
#   COMROT
#   NCP
#   NDATE
#   ENKFUPDSH
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

$PBEG

################################################################################
# Set other variables

export PCOP=${PCOP:-$SHDIR/pcop}
export NDATE=${NDATE:-${NWPROD}/util/exec/ndate}
export ENKFUPDSH=${ENKFUPDSH:-$SCRDIR/exglobal_enkf_update.sh.sms}
export CONVINFO=${CONVINFO:-${FIXGLOBAL}/global_convinfo.txt}
export OZINFO=${OZINFO:-${FIXGLOBAL}/global_ozinfo.txt}
export PCPINFO=${PCPINFO:-${FIXGLOBAL}/global_pcpinfo.txt}
export HYBENSINFO=${HYBENSINFO:-${FIXGLOBAL}/global_hybens_locinfo.txt}

if [ $machine = IBMP6 ] ; then
  export MP_INFOLEVEL=${INFOLEVELUPD:-2}
  export MP_PMDLOG=${PMDLOGANAL:-no}
  export MP_SHARED_MEMORY=${MP_SHARED_MEMORY:-yes}
  export MEMORY_AFFINITY=${MEMORY_AFFINITY:-MCM}
  export MP_LABELIO=${MP_LABELIO:-yes}
  export MP_COREFILE_FORMAT=lite

# Recommended MPI environment variable setttings from IBM
# (Appendix E, HPC Clusters Using InfiniBand on IBM Power Systems Servers)
  export LAPI_DEBUG_ENABLE_AFFINITY=YES
#export LAPI_DEBUG_MTU_4K=YES
  export MP_FIFO_MTU=4K
  export MP_SYNC_QP=YES
  export MP_SHM_ATTACH_THRESH=500000
  export MP_EUIDEVELOP=min
  export MP_USE_BULK_XFER=yes
  export MP_BULK_MIN_MSG_SIZE=64k
  export MP_RC_MAX_QP=8192
  export LAPI_DEBUG_RC_DREG_THRESHOLD=1000000
  export LAPI_DEBUG_QP_NOTIFICATION=no
  export LAPI_DEBUG_RC_INIT_SETUP=yes
elif [ $machine = WCOSS ] ; then
  export MP_EAGER_LIMIT=${MP_EAGER_LIMIT:-65536}
  export MP_COREFILE_FORMAT=${MP_COREFILE_FORMAT:-lite}
# export MP_EUIDEVICE=${MP_EUIDEVICE:-sn_all}
# export MP_EUILIB=${MP_EUILIB:-us}
  export MP_MPILIB=${MP_MPILIB:-mpich2}
  export MP_LABELIO=${MP_LABELIO:-yes}
  export MP_USE_BULK_XFER=${MP_USE_BULK_XFER:-yes}
  export MPICH_ALLTOALL_THROTTLE=${MPICH_ALLTOALL_THROTTLE:-0}
  export MP_COLLECTIVE_OFFLOAD=${MP_COLLECTIVE_OFFLOAD:-no}
  export MP_SINGLE_THREAD=${MP_SINGLE_THREAD:-yes}
  export MP_SHARED_MEMORY=${MP_SHARED_MEMORY:-yes}
  export KMP_STACKSIZE=${KMP_STACKSIZE:-2048m}
  export NTHREADS_ENKF=${NTHREADS_ENKF:-2}
else
  export MPI_BUFS_PER_PROC=${MPI_BUFS_PER_PROC:-256}
  export MPI_BUFS_PER_HOST=${MPI_BUFS_PER_HOST:-256}
  export MPI_GROUP_MAX=${MPI_GROUP_MAX:-256}
  export MPI_MEMMAP_OFF=${MPI_MEMMAP_OFF:-1}
fi

export PREINP=gdas1.t$(echo $CDATE|cut -c9-10)z.
export FILESTYLE=${FILESTYLEEUPD:-'C'}
export VERBOSE=YES
export CYINC=${CYINC:-06}
export GDATE=$($NDATE -$CYINC $CDATE)
export CDFNL=${CDFNL:-gdas}
export GDUMP=${GDUMP:-$CDFNL}
export COMIN=${COMIN:-$COMROT}
export COMOUT=${COMOUT:-$COMROT}



################################################################################
# Define variables for input files

export GBIAS=${GBIAS:-${COMIN}/biascr.$GDUMP.$GDATE}
export GBIASe=${GBIASe:-$COMIN/biascr_int_${CDATE}_ensmean}
export GSATANG=${GSATANG:-${COMIN}/satang.$GDUMP.$GDATE}
export SIGGES=${SIGGES:-${COMIN}/sfg_${GDATE}_fhr06_ensmean}
export SFCGES=${SFCGES:-${COMIN}/bfg_${GDATE}_fhr06_ensmean}
export SIGGESENS=${SIGGESENS:-${COMIN}/sfg_${GDATE}_fhr06s}
export CNVSTAT=${CNVSTAT:-${COMIN}/cnvstat_$CDATE}
export OZNSTAT=${OZNSTAT:-${COMIN}/oznstat_$CDATE}
export RADSTAT=${RADSTAT:-${COMIN}/radstat_$CDATE}



################################################################################
# Make use of updated angle dependent bias file, if it exists.

if [[ -s $GSATANG ]]; then
   export SATANGL=$GSATANG
fi


################################################################################
# Set output data

export ENKFSTAT=${ENKFSTAT:-${COMOUT}/enkfstat_$CDATE}
export SIGANLENS=${SIGANLENS:-${COMOUT}/sanl_$CDATE}

################################################################################
# Run enkf update

export JCAP=${JCAP_ENKF:-254}
export LEVS=${LEVS_ENKF:-64}
export LONB=${LONB_ENKF:-768}
export LATB=${LATB_ENKF:-384}
export LONA=${LONA_ENKF:-512}
export LATA=${LATA_ENKF:-256}


export PGMOUT=stdout
export PGMERR=stderr

$ENKFUPDSH
rc=$?
if [[ $rc -ne 0 ]];then $PERR;exit 1;fi

cat $PGMOUT
cat $PGMERR

################################################################################
# Copy out restart and output files

##$PCOP $CDATE/$CDUMP/$CSTEP/ROTO $DATA $COMROT <$RLIST
##rc=$?
##$PCOP $CDATE/$CDUMP/$CSTEP/OPTO $DATA $COMROT <$RLIST

################################################################################
# Exit gracefully

if [[ $rc -ne 0 ]];then $PERR;exit 1;fi
$PEND
