#!/bin/ksh
################################################################################
# This script runs the analysis.
# Usage: anal.sh
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
#   ANALYSISSH
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
eval export DATA=$DATATMP
cd;rm -rf $DATA||exit 1;mkdir -p $DATA||exit 1;cd $DATA||exit 1
#chgrp ${group_name:-rstprod} $DATA
chmod ${permission:-755} $DATA
export machine=${machine:-WCOSS}
machine=$(echo $machine|tr '[a-z]' '[A-Z]')
#
export HOMEDIR=${HOMEDIR:-/nwprod}
export SCRDIR=${SCRDIR:-$HOMEDIR/scripts}
export FIXDIR=${FIXDIR:-$HOMEDIR/fix/fix_am}
export FIXGLOBAL=${FIXGLOBAL:-$FIXDIR}
export SHDIR=${SHDIR:-$HOMEDIR/bin}
export NWPROD=${NWPROD:-$HOMEDIR}
#
export PBEG=${PBEG:-$SHDIR/pbeg}
export PEND=${PEND:-$SHDIR/pend}
export PERR=${PERR:-$SHDIR/perr}
#             Other option for snoid is weasd (for old snow files)
export snoid=${snoid:-snod}

$PBEG

################################################################################
# Set other variables

export PCOP=${PCOP:-$SHDIR/pcop}
export NDATE=${NDATE:-${NWPROD}/util/exec/ndate}
export WGRIB=${WGRIB:-${NWPROD}/util/exec/wgrib}
export ANALYSISSH=${ANALYSISSH:-$SCRDIR/exglobal_analysis.sh.sms}

if [ $machine = IBMP6 ] ; then
  export MP_INFOLEVEL=${INFOLEVELANAL:-2}
  export MP_PMDLOG=${PMDLOGANAL:-no}
  export MP_SHARED_MEMORY=${MP_SHARED_MEMORY:-yes}
  export MEMORY_AFFINITY=${MEMORY_AFFINITY:-MCM}
  export MP_USE_BULK_XFER=${MP_USE_BULK_XFER:-no}
  export BIND_TASKS=${BIND_TASKS:-NO}
  export MP_LABELIO=${MP_LABELIO:-yes}
  export MP_COREFILE_FORMAT=lite
##TEST
# Environment variables from Carolyn
  export LAPI_DEBUG_ENABLE_AFFINITY=YES
# export LAPI_DEBUG_MTU_4K=YES
  export MP_FIFO_MTU=4K
  export MP_SYNC_QP=YES
  export MP_RFIFO_SIZE=16777216
  export MP_SHM_ATTACH_THRESH=500000
  export MP_EUIDEVELOP=min
#RDMA specific tunables:
  export MP_USE_BULK_XFER=yes
  export MP_BULK_MIN_MSG_SIZE=64k
  export MP_RC_MAX_QP=8192
  export LAPI_DEBUG_RC_DREG_THRESHOLD=1000000
  export LAPI_DEBUG_QP_NOTIFICATION=no
  export LAPI_DEBUG_RC_INIT_SETUP=yes
  export NTHREADS_GSI=${NTHREADS_GSI:-1}
  export OMP_NUM_THREADS=${OMP_NUM_THREADS:-NTHREADS_GSI}
##TEST
elif [ $machine = GAEA ] ; then
  export MPICH_FAST_MEMCPY=${MPICH_FAST_MEMCPY:-"ENABLE"}
  export MPICH_UNEX_BUFFER_SIZE=${MPICH_UNEX_BUFFER_SIZE:-1025000000}
  export MPICH_MAX_SHORT_MSG_SIZE=${MPICH_MAX_SHORT_MSG_SIZE:-8192}
  export MPICH_PTL_UNEX_EVENTS=${MPICH_PTL_UNEX_EVENTS:-64000}
elif [ $machine = WCOSS ] ; then
  export MP_EAGER_LIMIT=${MP_EAGER_LIMIT:-65536}
  export MP_COREFILE_FORMAT=${MP_COREFILE_FORMAT:-lite}
  export MP_EUIDEVELOP=${MP_EUIDEVELOP:-min}
  export MP_MPILIB=${MP_MPILIB=:-mpich2}
  export MP_LABELIO=${MP_LABELIO:-yes}
  export MP_USE_BULK_XFER=${MP_USE_BULK_XFER:-yes}
  export MP_SHARED_MEMORY=${MP_SHARED_MEMORY:-yes}
  export MPICH_ALLTOALL_THROTTLE=${MPICH_ALLTOALL_THROTTLE:-0}
  export MP_COLLECTIVE_OFFLOAD=${MP_COLLECTIVE_OFFLOAD:-yes}
  export KMP_STACKSIZE=${KMP_STACKSIZE:-2048m}
else
  export MPI_BUFS_PER_PROC=${MPI_BUFS_PER_PROC:-256}
  export MPI_BUFS_PER_HOST=${MPI_BUFS_PER_HOST:-256}
  export MPI_GROUP_MAX=${MPI_GROUP_MAX:-256}
  export MPI_MEMMAP_OFF=${MPI_MEMMAP_OFF:-1}
  export NTHREADS_GSI=${NTHREADS_GSI:-1}
fi
export PREINP=gdas1.t$(echo $CDATE|cut -c9-10)z.
export FILESTYLE=${FILESTYLEANAL:-'C'}
export VERBOSE=YES
export CYINC=${CYINC:-06}
export GDATE=$($NDATE -$CYINC $CDATE)
export CDFNL=${CDFNL:-gdas}
export GDUMP=${GDUMP:-$CDFNL}
export CYCLVARS=${CYCLVARS:-""}
COMDMPTMP=${COMDMPTMP:-$COMDMP}
eval export COMDMP=$COMDMPTMP
eval COMDMPG=$(CDATE=$GDATE CDUMP=$GDUMP eval echo $COMDMPTMP)
#
#[[ -n ${COMDMPTMP:-""} ]]&&eval export COMDMP=$COMDMPTMP
#export COMDMPG=${COMDMPG:-$COMDMP}
#[[ -n ${COMDMPTMP:-""} ]]&&export COMDMPG=$(CDATE=$GDATE CDUMP=$GDUMP eval echo $COMDMPTMP)
#

export COMIN_DUMP=${COMIN_DUMP:-$COMDMP}
export COMIN_DUMPG=${COMIN_DUMPG:-$COMDMPG}
export COMIN_GDAS=${COMIN_GDAS:-$COMROT}
export COMIN_ENKF=${COMIN_ENKF:-$COMROT}
export COMOUT_GDAS=${COMOUT_GDAS:-$COMROT}

export CDATE_SKIP=${CDATE_SKIP:-0}
export DOHYBVAR=${DOHYBVAR:-"NO"}

################################################################################
# Copy in restart and input files

##$PCOP $CDATE/$CDUMP/$CSTEP/ROTI $COMROT $DATA <$RLIST
##rc=$?
##if [[ $rc -ne 0 ]];then $PERR;exit 1;fi
export GBIAS=${GBIAS:-$COMIN_GDAS/biascr.$GDUMP.$GDATE}
export GBIASPC=${GBIASPC:-$COMIN_GDAS/biascr_pc.$GDUMP.$GDATE}
export GRADSTAT=${GRADSTAT:-$COMIN_GDAS/radstat.$GDUMP.$GDATE}
export GSATANG=${GSATANG:-$COMIN_GDAS/satang.$GDUMP.$GDATE}
export SFCGES=${SFCGES:-$COMIN_GDAS/sfcf06.$GDUMP.$GDATE}
export NSTGES=${NSTGES:-$COMIN_GDAS/nstf06.$GDUMP.$GDATE}

cycle=$(echo $CDATE|cut -c9-10)
cdump=$(echo $GDUMP|tr '[a-z]' '[A-Z]')
eval mlanl=\${MLANL$cycle$cdump:-0}
if [ $mlanl -gt 0 -a $CDATE -gt $CDATE_SKIP ] ; then
 tsleep=10
 msleep=120
 nsleep=0
 until [[ -s $COMROT/$(basename $SFCGES.LIS) || $((nsleep+=1)) -gt $msleep ]];do
   sleep $tsleep
 done
 if [[ $nsleep -gt $msleep ]] ; then
   echo 'NO SFC FILE FROM LDAS - JOB STOPPED'
   $PERR
   exit 2
 fi
 export SFCGES=$SFCGES.LIS
 ${NCP:-/bin/cp} $COMROT/$(basename $SFCGES) $SFCGES
fi
export SIGGES=${SIGGES:-$COMIN_GDAS/sigges.$CDUMP.$CDATE}
export PREPQC=${PREPQC:-$COMIN_GDAS/prepqc.$CDUMP.$CDATE}

##$PCOP $CDATE/$CDUMP/$CSTEP/OPTI $COMROT $DATA <$RLIST
export SFCG03=${SFCG03:-$COMIN_GDAS/sfcf03.$GDUMP.$GDATE}
export SFCG04=${SFCG04:-$COMIN_GDAS/sfcf04.$GDUMP.$GDATE}
export SFCG05=${SFCG05:-$COMIN_GDAS/sfcf05.$GDUMP.$GDATE}
export SFCG07=${SFCG07:-$COMIN_GDAS/sfcf07.$GDUMP.$GDATE}
export SFCG08=${SFCG08:-$COMIN_GDAS/sfcf08.$GDUMP.$GDATE}
export SFCG09=${SFCG09:-$COMIN_GDAS/sfcf09.$GDUMP.$GDATE}

export NSTG03=${NSTG03:-$COMIN_GDAS/nstf03.$GDUMP.$GDATE}
export NSTG04=${NSTG04:-$COMIN_GDAS/nstf04.$GDUMP.$GDATE}
export NSTG05=${NSTG05:-$COMIN_GDAS/nstf05.$GDUMP.$GDATE}
export NSTG07=${NSTG07:-$COMIN_GDAS/nstf07.$GDUMP.$GDATE}
export NSTG08=${NSTG08:-$COMIN_GDAS/nstf08.$GDUMP.$GDATE}
export NSTG09=${NSTG09:-$COMIN_GDAS/nstf09.$GDUMP.$GDATE}

export SIGG03=${SIGG03:-$COMIN_GDAS/siggm3.$CDUMP.$CDATE}
export SIGG04=${SIGG04:-$COMIN_GDAS/siggm2.$CDUMP.$CDATE}
export SIGG05=${SIGG05:-$COMIN_GDAS/siggm1.$CDUMP.$CDATE}
export SIGG07=${SIGG07:-$COMIN_GDAS/siggp1.$CDUMP.$CDATE}
export SIGG08=${SIGG08:-$COMIN_GDAS/siggp2.$CDUMP.$CDATE}
export SIGG09=${SIGG09:-$COMIN_GDAS/siggp3.$CDUMP.$CDATE}
export GINCIN=${GINCIN:-$COMIN_GDAS/gesfile.gfs.$CDATE}

if [[ "$DOHYBVAR" = "YES" ]]; then
   export SIGGESENS=${SIGGESENS:-$COMIN_ENKF/sfg_${GDATE}_fhr06s}
   export SFCGESENS=${SFCGESENS:-$COMIN_ENKF/bfg_${GDATE}_fhr06}
   export NSTGESENS=${NSTGESENS:-$COMIN_ENKF/nfg_${GDATE}_fhr06}
##   $PCOP $CDATE/$CDUMP/$CSTEP/ENRI $COMROT $DATA <$RLIST
##   rc=$?
##   if [[ $rc -ne 0 ]];then $PERR;exit 1;fi
##   $PCOP $CDATE/$CDUMP/$CSTEP/ENOI $COMROT $DATA <$RLIST
fi


##$PCOP $CDATE/$CDUMP/$CSTEP/DMPI $COMDMP $DATA <$RLIST
##$PCOP $CDATE/$CDUMP/$CSTEP/DMPG $COMDMPG $DATA <$RLIST

[[ -n ${FNTSFATMP:-""} ]]&&eval export FNTSFA=$FNTSFATMP
export FNTSFA=${FNTSFA:-$COMIN_DUMP/sstgrb.$CDUMP.$CDATE}

[[ -n ${FNACNATMP:-""} ]]&&eval export FNACNA=$FNACNATMP
export FNACNA=${FNACNA:-$COMIN_DUMP/icegrb.$CDUMP.$CDATE}

export FNSNOA=${FNSNOA:-$COMIN_DUMP/snogrb.$CDUMP.$CDATE}
export FNSNOG=${FNSNOG:-$COMIN_DUMPG/snogrb.$GDUMP.$GDATE}

[[ -n ${FNSNOAJCAP_TMP:-""} ]]&&eval export FNSNOAJCAP=$FNSNOAJCAP_TMP
export FNSNOAJCAP=${FNSNOAJCAP:-$DMPDIR/$CDATE/${CDUMP}/snogrb_t$JCAP.$CDUMP.$CDATE}
if [ ! -s $FNSNOAJCAP ]; then export FNSNOAJCAP=$DMPDIR/$CDATE/${CDUMP}x/snogrb_t$JCAP.$CDUMP.$CDATE ;fi

[[ -n ${FNSNOGJCAP_TMP:-""} ]]&&eval export FNSNOGJCAP=$FNSNOGJCAP_TMP
export FNSNOGJCAP=${FNSNOGJCAP:-$DMPDIR/$GDATE/${GDUMP}/snogrb_t$JCAP.$GDUMP.$GDATE}
if [ ! -s $FNSNOGJCAP ]; then export FNSNOGJCAP=$DMPDIR/$GDATE/${GDUMP}x/snogrb_t$JCAP.$GDUMP.$GDATE ;fi

export GSNDBF=${GSNDBF:-$COMIN_DUMP/goesnd.$CDUMP.$CDATE}
export GSNDBF1=${GSNDBF1:-$COMIN_DUMP/goesfv.$CDUMP.$CDATE}
export B1HRS2=${B1HRS2:-$COMIN_DUMP/1bhrs2.$CDUMP.$CDATE}
export B1MSU=${B1MSU:-$COMIN_DUMP/1bmsu.$CDUMP.$CDATE}
export B1HRS3=${B1HRS3:-$COMIN_DUMP/1bhrs3.$CDUMP.$CDATE}
export B1HRS4=${B1HRS4:-$COMIN_DUMP/1bhrs4.$CDUMP.$CDATE}
export B1AMUA=${B1AMUA:-$COMIN_DUMP/1bamua.$CDUMP.$CDATE}
export B1AMUB=${B1AMUB:-$COMIN_DUMP/1bamub.$CDUMP.$CDATE}
export B1MHS=${B1MHS:-$COMIN_DUMP/1bmhs.$CDUMP.$CDATE}
export B1SSU=${B1SSU:-$COMIN_DUMP/1bssu.$CDUMP.$CDATE}
export SBUVBF=${SBUVBF:-$COMIN_DUMP/osbuv8.$CDUMP.$CDATE}
export SMIPCP=${SMIPCP:-$COMIN_DUMP/spssmi.$CDUMP.$CDATE}
export TMIPCP=${TMIPCP:-$COMIN_DUMP/sptrmm.$CDUMP.$CDATE}
export AIRSBF=${AIRSBF:-$COMIN_DUMP/airsev.$CDUMP.$CDATE}
export IASIBF=${IASIBF:-$COMIN_DUMP/mtiasi.$CDUMP.$CDATE}
export AMSREBF=${AMSREBF:-$COMIN_DUMP/amsre.$CDUMP.$CDATE}
export SSMITBF=${SSMITBF:-$COMIN_DUMP/ssmit.$CDUMP.$CDATE}
export SSMISBF=${SSMISBF:-$COMIN_DUMP/ssmisu.$CDUMP.$CDATE}
export GPSROBF=${GPSROBF:-$COMIN_DUMP/gpsro.$CDUMP.$CDATE}
export ESHRS3=${ESHRS3:-$COMIN_DUMP/eshrs3.$CDUMP.$CDATE}
export ESAMUA=${ESAMUA:-$COMIN_DUMP/esamua.$CDUMP.$CDATE}
export ESAMUB=${ESAMUB:-$COMIN_DUMP/esamub.$CDUMP.$CDATE}
export SEVIRIBF=${SEVIRIBF:-$COMIN_DUMP/sevcsr.$CDUMP.$CDATE}
export ATMSBF=${ATMSBF:-$COMIN_DUMP/atms.$CDUMP.$CDATE}
export CRISBF=${CRISBF:-$COMIN_DUMP/cris.$CDUMP.$CDATE}
export GOMEBF=${GOMEBF:-$COMIN_DUMP/gome.$CDUMP.$CDATE}
export OMIBF=${OMIBF:-$COMIN_DUMP/omi.$CDUMP.$CDATE}
export MLSBF=${MLSBF:-$COMIN_DUMP/mls.$CDUMP.$CDATE}
export TCVITL=${TCVITL:-$COMIN_DUMP/tcvitl.$CDUMP.$CDATE}
export STATUS=${STATUS:-$COMIN_DUMP/stat01.$CDUMP.$CDATE}
export SATWND=${SATWND:-$COMIN_DUMP/satwnd.$CDUMP.$CDATE}
export OSCATBF=${OSCATBF:-$COMIN_DUMP/oscatw.$CDUMP.$CDATE}
export B1AVHAM=${B1AVHAM:-$COMIN_DUMP/avcsam.$CDUMP.$CDATE}
export B1AVHPM=${B1AVHPM:-$COMIN_DUMP/avcspm.$CDUMP.$CDATE}
export AMSREBF=${AMSREBF:-$COMIN_DUMP/amsre.$CDUMP.$CDATE}
export NSSTBF=${NSSTBF:-nsstbufr.$CDUMP.$CDATE}
export SFCSHPBF=${SFCSHP:-$COMIN_DUMP/sfcshp.$CDUMP.$CDATE}
export TESACBF=${TESACBF:-$COMIN_DUMP/tesac.$CDUMP.$CDATE}
export BATHYBF=${BATHYBF:-$COMIN_DUMP/bathy.$CDUMP.$CDATE}
export TRKOBBF=${TRKOBBF:-$COMIN_DUMP/trkob.$CDUMP.$CDATE}


# If snogrb_t$JCAP files exists, use it.   If it does not exist and
# if USE_JCAP_SNO=YES (default) then abort. Otherwise, revert to snogrb files

if [[ -s $FNSNOAJCAP ]]; then
   eval export FNSNOA=$FNSNOAJCAP
else
   if [ ${USE_JCAP_SNO:-YES} = YES ] ; then
     echo $FNSNOAJCAP 'does not exit'
     exit 111
   fi
fi
if [[ -s $FNSNOGJCAP ]]; then
   eval export FNSNOG=$FNSNOGJCAP
else
   if [ ${USE_JCAP_SNO:-YES} = YES ] ; then
     echo $FNSNOGJCAP 'does not exist'
     exit 222
   fi
fi

export FNOROG=${FNOROG:-$FIXGLOBAL/global_orography.t$JCAP.grb}
export FNMASK=${FNMASK:-$FIXGLOBAL/global_slmask.t$JCAP.grb}
export OROGRAPHY=${OROGRAPHY:-$FIXGLOBAL/global_orography.t$JCAP.grb}
export SLMASK=${SLMASK:-$FIXGLOBAL/global_slmask.t$JCAP.grb}
#
if [ ! -s $FNSNOA ] ; then
 FNSNOA=${FNSNOA}.r2
fi
xx=$($WGRIB -4yr $FNSNOA | awk -F: '{print $4}')
snoid=NONE
for snovar in $xx ; do
 if [ $snovar = SNOD ] ; then
  snoid=SNOD
 elif [ $snovar = WEASD ] ; then
  snoid=WEASD
 fi
done
if [ $snoid = NONE ] ; then
 for snovar in $xx ; do if [ $snovar = SNOWC ] ; then snoid=SNOWC ; fi ; done
fi
#snoid=$($WGRIB -4yr $FNSNOA | awk -F: '{print $4}')

################################################################################
# Turn off snow analysis if it has already been used.
if [ `$WGRIB -4yr ${FNSNOA} 2>/dev/null|grep -i $snoid |\
	  awk -F: '{print $3}'|awk -F= '{print $2}'` -le \
     `$WGRIB -4yr ${FNSNOG} 2>/dev/null |grep -i $snoid  |\
			awk -F: '{print $3}'|awk -F= '{print $2}'` ] ; then
	export FNSNOA=" "
	export CYCLVARS="FSNOL=99999.,FSNOS=99999.,$CYCLVARS"
else
     export SNOW_NUDGE_COEFF=${SNOW_NUDGE_COEFF:-0.}
     export CYCLVARS="FSNOL=${SNOW_NUDGE_COEFF},$CYCLVARS"
fi
#
#  If the snow file contains snow cover, define FNSCVA and remove FNSNOA
#
if [ $snoid = SNOWC ] ; then
#export FNSCVA=$FNSNOA
 export CYCLVARS="FNSCVA=\"$FNSNOA\",$CYCLVARS"
 export FNSNOA=" "
fi

################################################################################
# Make use of updated angle dependent bias file, if it exists.

if [[ -s $GSATANG ]]; then
   export SATANGL=$GSATANG
fi

################################################################################
# Set output data

export SFCANL=${SFCANL:-$COMOUT_GDAS/sfcanl.$CDUMP.$CDATE}
export SIGANL=${SIGANL:-$COMOUT_GDAS/siganl.$CDUMP.$CDATE}
export NSTANL=${NSTANL:-$COMOUT_GDAS/nstanl.$CDUMP.$CDATE}
#
# SFCGCY is a surface file generated by applying global_cycle to SFCGES(sfcf06) 
#
export SFCGCY=${SFCGCY:-$COMOUT_GDAS/sfcgcy.$CDUMP.$CDATE}

export ABIAS=${ABIAS:-$COMOUT_GDAS/biascr.$CDUMP.$CDATE}
export ABIASPC=${ABIASPC:-$COMOUT_GDAS/biascr_pc.$CDUMP.$CDATE}
export RADSTAT=${RADSTAT:-$COMOUT_GDAS/radstat.$CDUMP.$CDATE}
export SSISTAT=${SSISTAT:-$COMOUT_GDAS/ssistat.$CDUMP.$CDATE}
export GSISTAT=${GSISTAT:-$COMOUT_GDAS/gsistat.$CDUMP.$CDATE}
export PCPSTAT=${PCPSTAT:-$COMOUT_GDAS/pcpstat.$CDUMP.$CDATE}
export CNVSTAT=${CNVSTAT:-$COMOUT_GDAS/cnvstat.$CDUMP.$CDATE}
export OZNSTAT=${OZNSTAT:-$COMOUT_GDAS/oznstat.$CDUMP.$CDATE}
export GINCOUT=${GINCOUT:-$COMOUT_GDAS/gesfile.$CDUMP.$CDATE}


################################################################################
# Do not produce radiance diagnostics for GFS dump
#if [[ "$CDUMP" = 'gfs' ]]; then
#  if [[ -n ${SETUP:-""} ]]; then
#     export SETUP="$SETUP,diag_rad=.false.,"
#  else
#     export SETUP="diag_rad=.false.,"
#  fi
#fi

################################################################################
# Run analysis

$ANALYSISSH
rc=$?
if [[ $rc -ne 0 ]];then $PERR;exit 1;fi

################################################################################
# Copy out restart and output files

##$PCOP $CDATE/$CDUMP/$CSTEP/ROTO $DATA $COMROT <$RLIST
##rc=$?

##$PCOP $CDATE/$CDUMP/$CSTEP/OPTO $DATA $COMROT <$RLIST

################################################################################
# Exit gracefully

if [[ $rc -ne 0 ]];then $PERR;exit 1;fi
$PEND
