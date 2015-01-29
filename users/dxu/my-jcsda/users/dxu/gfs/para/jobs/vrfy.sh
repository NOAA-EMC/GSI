#!/bin/ksh
################################################################################
# This script runs the verification step.
# Usage: vrfy.sh
# Imported variables:
#   CONFIG
#   CDATE
#   CDUMP
#   CSTEP
# Configuration variables:
#   DATATMP
#   COMROT
#   HRKTMP
#   HRKROT
#   ATARFILE
#   ALIST
#   HSMTAR
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
export NCP=${NCP:-/bin/cp}
eval export DATA=$DATATMP
cd;rm -rf $DATA||exit 1;mkdir -p $DATA||exit 1;cd $DATA||exit 1
#chgrp ${group_name:-rstprod} $DATA
chmod ${permisstmp:-700} $DATA
#chmod ${permission:-750} $DATA
#
export machine=${machine:-WCOSS}
machine=$(echo $machine|tr '[a-z]' '[A-Z]')
#
export HOMEDIR=${HOMEDIR:-/nwprod}
export SHDIR=${SHDIR:-$HOMEDIR/bin}
export SCRDIR=${SCRDIR:-$HOMEDIR/scripts}
export JOBSDIR=${JOBSDIR:-$HOMEDIR/jobs}
export USHDIR=${USHDIR:-$HOMEDIR/ush}
export NWPROD=${NWPROD:-$HOMEDIR}
export HOMEcfs=${HOMEcfs:-$HOMEDIR}
export USHcfs=${USHcfs:-$USHDIR}
#
PBEG=${PBEG:-$SHDIR/pbeg}
PEND=${PEND:-$SHDIR/pend}
PERR=${PERR:-$SHDIR/perr}
$PBEG

################################################################################
# Set other variables

node=`hostname | cut -d. -f1,1`
mac=`echo $node | cut -c1-1`

export DISK_GLOB=${DISK_GLOB:-/global/save}
export PCOP=${PCOP:-$SHDIR/pcop}
export SUB=${SUB:-$SHDIR/sub}
export NDATE=${NDATE:-$NWPROD/util/exec/ndate}
export PREPQFITSH=${PREPQFITSH:-$USHDIR/global_prepqfit.sh}
export SAVEFITSSH=${SAVEFITSSH:-$USHDIR/global_savefits.sh}
export TRACKERSH=${TRACKERSH:-$USHDIR/global_tracker.sh}
export PUBVRFYSH=${PUBVRFYSH:-$USHDIR/VRFY_PUB.sh}
#
export PRPX=${PRPX:-$NWPROD/exec/prepobs_prepdata}
export PREX=${PREX:-$NWPROD/exec/prepobs_prevents}
export PSTX=${PSTX:-$NWPROD/exec/global_postevents}
export SYNDX=${SYNDX:-$NWPROD/exec/syndat_syndata}
export PRVT=${PRVT:-$HOMEDIR/fix/fix_prep/prepobs_errtable.global}
#
export STORMPSH=${STORMPSH:-$USHDIR/stormp.sh}
export VSDBSH=${VSDBSH:-$USHDIR/vsdbjob.sh}
export VRFYRADSH=${VRFYRADSH:-$USHDIR/VrfyRad_glbl.sh}
export VRFYOZNSH=${VRFYOZNSH:-$USHDIR/VrfyOzn_glbl.sh}
export VRFYFITS=${VRFYFITS:-NO}
export SAVEFITS=${SAVEFITS:-NO}
export VRFYSCOR=${VRFYSCOR:-NO}
export VRFYPRCP=${VRFYPRCP:-NO}
export VRFYRAD=${VRFYRAD:-NO}
export VRFYOZN=${VRFYOZN:-NO}
export SAVERAD=${SAVERAD:-NO}
export SAVEFITS=${SAVEFITS:-NO}
export VRFYTRAK=${VRFYTRAK:-NO}
export VRFYGMPK=${VRFYGMPK:-NO}
export PLTSTORMS=${PLTSTORMS:-NO}
export VSDB_STEP1=${VSDB_STEP1:-NO}
export VSDB_STEP2=${VSDB_STEP2:-NO}
export webhost=${webhost:-rzdm.ncep.noaa.gov}
export webhostid=${webhostid:-$LOGNAME}
export SEND2WEB=${SEND2WEB:-NO}
export WEBDIR=${WEBDIR:-/home/people/emc/www/htdocs/gmb/$webhostid}
ARCHDAY=${ARCHDAY:-2}       # impacts delay for online archive only
BACKDATE=$((ARCHDAY*24))    # impacts delay for online archive only
BACKDATEVSDB=$((BACKDATE+24))
#
export yzdir=${yzdir:-$HOMEDIR/wx20yz}
export nmc30y=${nmc30y:-$yzdir/gvrfy/data/nmc30y.ibmsp}
export cac8ys=${cac8ys:-$yzdir/gvrfy/data/cac8ys.ibmsp}
if [ $mac != v ] ; then
 export cdasdir=${cdasdir:-/global/shared/stat}
else
 export cdasdir=${cdasdir:-$yzdir/gvrfy/data/cdas}
fi
#
export VFCSTDIR=${VFCSTDIR:-$COMROT}
export VANALDIR=${VANALDIR:-$COMROT}
export SCORDIR=${SCORDIR:-$COMROT}
export PRCP_PREFIX=${PRCP_PREFIX:-flx}
[[ $SAVEFITS = YES ]]&&VRFYFITS=YES
export CYINC=${CYINC:-06}
export CDFNL=${CDFNL:-gdas}
export VDUMP=${VDUMP:-gfs}
export GDUMP=${GDUMP:-$CDFNL}
export fdump=${fdump:-gfs}
export adump=${adump:-gdas}
export GDATE=$($NDATE -$CYINC $CDATE)
export COMDMPTMP=${COMDMPTMP:-$COMDMP}
eval export COMDMP=$COMDMPTMP
eval export COMDMPG=$(CDATE=$GDATE CDUMP=$GDUMP eval echo $COMDMPTMP)
#
#[[ -n ${COMDMPTMP:-""} ]]&&eval export COMDMP=$COMDMPTMP
#export COMDMPG=${COMDMPG:-$COMDMP}
#[[ -n ${COMDMPTMP:-""} ]]&&export COMDMPG=$(CDATE=$GDATE CDUMP=$GDUMP eval echo $COMDMPTMP)
#
p=pr$(echo $PSLOT|tr '[A-Z]' '[a-z]')
#
cycle=$(echo $CDATE|cut -c9-10)
cdump=$(echo $CDUMP|tr '[a-z]' '[A-Z]')
nknd=${CKND:-1}
export JCAP=$(eval echo \${JCAPFCST$cycle$cdump:-$JCAP}|cut -f$nknd -d,)
export IO=$(eval echo \${IOPOST$cycle$cdump:-${IO:-360}}|cut -f$nknd -d,)
export JO=$(eval echo \${JOPOST$cycle$cdump:-${JO:-181}}|cut -f$nknd -d,)
export FHMAX=$(eval echo \${FHMAXFCST$cycle$cdump:-9}|cut -f$nknd -d,)
export FHMAX2=$(eval echo \${FHMAXFCST$cycle$cdump:-9}|cut -f2 -d,)
if [ $FHMAX2 -lt $FHMAX ] ; then export FHMAX2=$FHMAX ; fi
export FHMAX2=$(((FHMAX2/24)*24))
export FHOUT=$(eval echo \${FHOUTFCST$cycle$cdump:-9}|cut -f$nknd -d,)
export FHBAK=$(eval echo \${FHBAKFCST$cycle$cdump:-00}|cut -f$nknd -d,)
export VHR=${VHR:-06}
export VBACKUP_PRCP=${VBACKUP_PRCP:-00}

export pgbf_gfs=${pgbf_gfs:-3}     #resolution of gfs pgbf files saved in HPSS archive, 3-1x1,4-0.5x0.5, 5-master
export pgbf_gdas=${pgbf_gdas:-4}   #resolution of gdas pgbf files saved in HPSS archive
export pgbf_grid=$(eval echo \$pgbf_$CDUMP)
if [ $pgbf_grid -eq 193 ] ; then
 export flag_pgb=q
elif [ $pgbf_grid -eq 5 ] ; then
 export flag_pgb=m
elif [ $pgbf_grid -eq 4 ] ; then
 export flag_pgb=h
elif [ $pgbf_grid -eq 3 ] ; then
 export flag_pgb=f
elif [ $pgbf_grid -eq 2 ] ; then
 export flag_pgb=l
else
 export flag_pgb=e
fi
export flag_pgb=${flag_pgb:-f}

myhost=$(hostname)
[[ $myhost = m* ]]&&export HOST=mist
[[ $myhost = d* ]]&&export HOST=dew

################################################################################
# Copy in information
#$PCOP $CDATE/$CDUMP/$CSTEP/DMPI $COMDMP $DATA <$RLIST
#$PCOP $CDATE/$CDUMP/$CSTEP/DMPG $COMDMPG $DATA <$RLIST

for cdm in $(eval echo $COMDMP|tr , ' ') ; do
 if [[ -s $cdm/tcvitl.$CDUMP.$CDATE ]] ; then
  $NCP $cdm/tcvitl.$CDUMP.$CDATE  $DATA/tcvitl.$CDUMP.$CDATE
 fi
done
for cdm in $(eval echo $COMDMPG|tr , ' ') ; do
 if [[ -s $cdm/tcvitl.$GDUMP.$GDATE ]] ; then
  $NCP $cdm/tcvitl.$GDUMP.$GDATE $DATA/tcvitl.$GDUMP.$GDATE
 fi
done

################################################################################
# Make observation fit files.
if [[ $VRFYFITS = YES && $CDUMP = $CDFNL ]] ; then
# export IDVC=$(eval echo \${IDVCFCST${cycle}GFS:-$idvc_a}|cut -f1 -d,)
# if [ $IDVC = 2 -a $idvc_a = 1 ] ; then
#   JIFDIR=$DISK_GLOB/wx20rt/jif
#   export PRPX=$JIFDIR/nwprod/exec/prepobs_prepdata
#   export PREX=$JIFDIR/nwprod/exec/prepobs_prevents
#   export PSTX=$JIFDIR/nwprod/exec/global_postevents
#   export SYNDX=$JIFDIR/nwprod/exec/syndat_syndata
#   export PRVT=$JIFDIR/noaa18/fix/prepobs_errtable.global
#   echo 'This option is not valid in this verification script'
# fi
  if [ $nknd -gt 1 -a $FHBAK -eq 0 ] ; then
    export CDUMPANAL=${CDFNL}$nknd
    export CDUMPFCST=${VDUMP}$nknd
  else
    export CDUMPFCST=$VDUMP
  fi
  export TMPDIR=$PTMP/$LOGNAME/tmpdir
  if [ $machine = IBMP6 -o $machine = ZEUS -o $machine = GAEA ] ; then
    $PREPQFITSH $CDATE $COMROT $DATA
  else
    $PREPQFITSH pr$PSLOT $CDATE $COMROT $ARCDIR $TMPDIR
  fi
fi
  
################################################################################
# Fit to obs archive.
if [[ $SAVEFITS = YES && $CDUMP = $CDFNL ]];then
# mkdir -p $PTMP/$LOGNAME
# cd $PTMP/$LOGNAME
  EXP=$p COMOUT=$COMROT
# if [ $machine = IBMP6 ] ; then
#   $SUB -a $ACCOUNT  -g $GROUP -e 'CDATE,EXP,COMOUT,FIT_DIR,HORZ_DIR,EXECDIR_FITS' -j savefits.$PSLOT.$CDATE -o $DATA/savefitsm.$PSLOT.$CDATE $SAVEFITSSH
# else
#   $SUB -a $ACCOUNT -e 'CDATE=$CDATE,EXP=$EXP,COMOUT=$COMOUT,FIT_DIR=$FIT_DIR,HORZ_DIR=$HORZ_DIR,EXECDIR_FITS=$EXECDIR_FITS' -j savefits.$PSLOT.$CDATE -p 1/1 -o $DATA/savefitsm.$PSLOT.$CDATE $SAVEFITSSH
# fi
#
# just run directly instead of with new job
# -----------------------------------------
  export CDATE=$CDATE
  export EXP=$p
  export COMOUT=$COMROT
  export HORZ_DIR=$HORZ_DIR
  export FIT_DIR=$FIT_DIR
  $SAVEFITSSH

#
  if [ $CYINC -gt 12 ] ; then
    CDATE12=$(echo $CDATE | cut -c1-8)12          ### <------ replace 00Z to 12Z
     EXP=$p COMOUT=$COMROT
    if [ $machine = IBMP6 ] ; then
      $SUB -a $ACCOUNT  -g $GROUP -e 'CDATE12,EXP,COMOUT,FIT_DIR,HORZ_DIR,EXECDIR_FITS' -j savefits.$PSLOT.$CDATE12 -o $DATA/savefitsm.$PSLOT.$CDATE12 $SAVEFITSSH
    else
      $SUB -a $ACCOUNT -e 'CDATE12=$CDATE12,EXP=$EXP,COMOUT=$COMOUT,FIT_DIR=$FIT_DIR,HORZ_DIR=$HORZ_DIR,EXECDIR_FITS=$EXECDIR_FITS' -j savefits.$PSLOT.$CDATE12 -p 1/1 -o $DATA/savefitsm.$PSLOT.$CDATE12 $SAVEFITSSH
    fi
  fi
  cd $DATA
fi

################################################################################
# Extract radiance data files and run integrity checks

if [[ $VRFYRAD = YES && $CDUMP = $CDFNL ]]; then

  EXP=$p COMOUT=$COMROT
  if [ $machine = IBMP6 ] ; then
    $SUB -a $ACCOUNT  -g $GROUP -e 'CDATE,EXP,COMOUT,VRFYRAD_DIR' -j vrfyrad.$PSLOT.$CDATE -o $DATA/vrfyrad.$PSLOT.$CDATE $VRFYRADSH
  else
##  $SUB -a $ACCOUNT -e 'CDATE=$CDATE,EXP=$EXP,COMOUT=$COMOUT,VRFYRAD_DIR=$VRFYRAD_DIR' -j vrfyrad.$PSLOT.$CDATE -p 1/1 -q service -o $DATA/vrfyrad.$PSLOT.$CDATE $VRFYRADSH
    export CDATE=$CDATE
    export EXP=$p
    export COMOUT=$COMROT
    export VRFYRAD_DIR=$VRFYRAD_DIR
    $VRFYRADSH
  fi
fi


################################################################################
# Extract ozone data files

if [[ $VRFYOZN = YES && $CDUMP = $CDFNL ]]; then
  list="ges anl"
  for ozntype in $list; do
     $VRFYOZNSH $PSLOT $ozntype $CDATE $COMROT $OZNDIR/$ozntype
  done
fi


################################################################################
#  Run verification score here
#  Warning -- if CDFNL=gfs, verification is done wrt pgbf00.gfs
#
if [[ $CDUMP = $CDFNL && $CDATE = ????????$VHR ]];then
  if [[ $VRFYSCOR = YES ]];then
    CDATE00=$(echo $CDATE|cut -c1-8)00
#   $PUBVRFYSH $CDATE00 $CDATE00 384 $COMROT $COMROT 24 $COMROT $PSLOT .gfs .$CDFNL
    if [[ $nknd -gt 1 ]] ; then
     $PUBVRFYSH $CDATE00 $CDATE00 384 $VFCSTDIR $VANALDIR 24 $SCORDIR $PSLOT .${fdump}$nknd .$adump $yzdir $cdasdir
    else
     $PUBVRFYSH $CDATE00 $CDATE00 384 $VFCSTDIR $VANALDIR 24 $SCORDIR $PSLOT .$fdump .$adump $yzdir $cdasdir
    fi
  fi
fi

################################################################################
# Make tracks.
if [[ $VRFYTRAK = YES ]];then
  if [[ $nknd -gt 1 ]] ; then
    $TRACKERSH $CDATE $CDUMP $COMROT $DATA $nknd
  else
    $TRACKERSH $CDATE $CDUMP $COMROT $DATA
  fi
fi

################################################################################
# Make gempak.
if [[ $VRFYGMPK = YES && $LOGNAME = glopara && $PSLOT = X ]];then
  RX=MRF$PSLOT
  export AWIPSSH=${AWIPSSH:-$DISK_GLOB/wx20mi/para/nawips.sh}
  $SUB -j nawips.$RX.$CDATE -o $STMP/glopara/nawips.$RX.$CDATE.out $AWIPSSH $CDATE $RX $PARROT
fi

#
# Make tropical storm plots and track plots
#
if [[ $PLTSTORMS = YES && $CDUMP = gfs && $nknd = 1 ]];then
 $STORMPSH $COMROT $CDATE $CDATE $PSLOT $JCAP $FHMAX $FHOUT $SEND2WEB $WEBDIR
 export PARADIR=$COMROT
 export PAREXP=PR$PSLOT
 export STORMTRACKSH=${STORMTRACKSH:-$DISK_GLOB/Shrinivas.Moorthi/track/stormtrack_unix_new.sh}
 $STORMTRACKSH $CDATE $CDATE 0 120 $PSLOT
fi
#
#   Run VSDB step1 script here
#

export vsdbsave=${vsdbsave:-$DISK_GLOB/$LOGNAME/archive/vsdb_data} ;#place where vsdb database is saved
export vsdbhome=${vsdbhome:-$HOMEDIR/vsdb}
export mdlist=${mdlist:-""}
VSDB_START_DATE=${VSDB_START_DATE:-$CDATE}
anltype=${anltype:-gfs}
vlength=${vlength:-$FHMAX2}
xdate=$(echo $($NDATE -$BACKDATEVSDB $CDATE) | cut -c1-8)
if [ $CDUMP = gfs  ] ; then
 if [ $VRFYPRCP = YES -o $VSDB_STEP1 = YES ] ; then
  export gfs_cyc=${gfs_cyc:-1}
  $VSDBSH $xdate $xdate $vlength $cycle pr$PSLOT $VSDB_START_DATE $anltype $gfs_cyc
 fi
fi
################################################################################
# Exit gracefully

#
#   A special archiving for Climate GDAS
#
#if [[ $usrdir = climpara ]] ; then
#if [[ $CDUMP = $CDFNL && $CDATE = ??????0200 ]];then
# CDATE00=$($NDATE -48 $CDATE)
# $DISK_CLIM/climpara/coupled/gdas/tar.sh $CDATE00 $COMROT $HPSSDIR $PSLOT $CDUMP 1 1 1 1 1
#fi
#fi
#
rc=0
if [[ $rc -ne 0 ]];then $PERR;exit 1;fi
$PEND
