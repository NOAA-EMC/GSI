#!/bin/ksh
################################################################################
# This script runs the archive and cleanup.
# Usage: arch.sh
# Imported variables:
#   CONFIG
#   CDATE
#   CDUMP
#   CSTEP
# Configuration variables:
#   DATATMP
#   COMROT
#   COMDAY
#   HRKTMP
#   HRKROT
#   HRKSIG
#   HRKFLX
#   HRKSIGG
#   HRKPGBM
#   HRKVFY
#   HRKDAY
#   HRKOCN_NC
#   HRKOCN_ANL
#   HRKOCN_GRB
#   ALIST
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
export COMROTTMP=${COMROTTMP:-$COMROT}
eval export COMROT=$COMROTTMP
eval export COMDAY=${COMDAY:-$COMROT}
export RESDIR=${RESDIR:-$COMROT/RESTART}
export ARCH_TO_HPSS=${ARCH_TO_HPSS:-YES}
cd;rm -rf $DATA||exit 1;mkdir -p $DATA||exit 1;cd $DATA||exit 1
#chgrp ${group_name:-rstprod} $DATA
chmod ${permission:-755} $DATA
#
export HOMEDIR=${HOMEDIR:-..}
export SHDIR=${SHDIR:-$HOMEDIR/bin}
export USHDIR=${USHDIR:-$HOMEDIR/ush}
export NWPROD=${NWPROD:-$HOMEDIR}
#
PBEG=${PBEG:-$SHDIR/pbeg}
PEND=${PEND:-$SHDIR/pend}
PERR=${PERR:-$SHDIR/perr}
ARCHCFSRRSH=${ARCHCFSRRSH:-$HOMEDIR/ush/cfsrr/hpss.cfsrr.daily.qsub}
CFSRR_ARCH=${CFSRR_ARCH:-YES}
HRKTMP=${HRKTMP:-24}
HRKRES=${HRKRES:-24}
HRKSIG=${HRKSIG:-120}
HRKFLX=${HRKFLX:-192}
HRKSIGG=${HRKSIGG:-120}
HRKPGBM=${HRKPGBM:-48}
HRKROT=${HRKROT:-120}
HRKDAY=${HRKDAY:-${HRKROT:-120}}
HRKVFY=${HRKVFY:-${HRKROT:-120}}
HRKOCN_NC=${HRKOCN_NC:-$HRKSIG}
HRKOCN_ANL=${HRKOCN_ANL:-$HRKROT}
HRKOCN_GRB=${HRKOCN_GRB:-$HRKROT}
HRKTMPGDAS=${HRKTMPGDAS:-$HRKTMP}
HRKTMPGFS=${HRKTMPGFS:-$HRKTMP}

ARCH_GLOBSTAT=${ARCH_GLOBSTAT:-NO}
ARCHCOPY=${ARCHCOPY:-NO}
ARCHSCP=${ARCHSCP:-NO}
ARCHDAY=${ARCHDAY:-2}       # impacts delay for online archive only
BACKDATE=$((ARCHDAY*24))    # impacts delay for online archive only
BACKDATEVSDB=$((BACKDATE+24))
BACKDATEPRCP=$((BACKDATEVSDB+VBACKUP_PRCP))

DO_PRODNAMES=${DO_PRODNAMES:-NO}
PRODNAMES_DIR=${PRODNAMES_DIR:-$COMROT/prod}
SETUPPRODNAMESH=${SETUPPRODNAMESH:-$USHDIR/setup_prodnames.sh}

myhost=$(hostname)
case $myhost in
  g*) export HOST=gyre;;
  t*) export HOST=tide;;
  *) echo unexpected hostname $myhost;HOST="";;
esac

if [[ $ARCHSCP = YES ]];then
  if [[ $HOST = gyre ]]; then
     SCPTO=tide
  elif [[ $HOST = tide ]]; then
     SCPTO=gyre
  else
     SCPTO=undefined
     ARCHSCP=NO
     echo "ARCHSCP turned off.  No valid remote host."
  fi
  ARCHSCPTO=${ARCHSCPTO:-$SCPTO}
fi

$PBEG

################################################################################
# Set other variables

export PCOP=${PCOP:-$SHDIR/pcop}
export NCP=${NCP:-cp}
export SCP=${SCP:-/usr/bin/scp}
export NDATE=${NDATE:-${NWPROD}/util/exec/ndate}
export COPYGB=${COPYGB:-${NWPROD}/util/exec/copygb}
export NCEPPOST=${NCEPPOST:-NO}
export CYINC=${CYINC:-06}
export CDFNL=${CDFNL:-gdas}
export GDATE=$($NDATE -$CYINC $CDATE)
export HPSSTAR=${HPSSTAR:-$HOMEDIR/ush/hpsstar}
export SUB=${SUB:-$HOMEDIR/bin/sub}
export HTAR=${HTAR:-/apps/hpss/htar}
export HSI=${HSI:-/apps/hpss/hsi}

export fhmax_1=${fmax1:-192}
export fhmax_2=${fmax2:-384}
export io_save=${io_save:-144}
export jo_save=${jo_save:-73}
export pgbf_gfs=${pgbf_gfs:-3}     #resolution of gfs pgbf files saved in HPSS archive, 3-1x1,4-0.5x0.5
export pgbf_gdas=${pgbf_gdas:-4}   #resolution of gdas pgbf files saved in HPSS archive
export pgbf_grid=$(eval echo \$pgbf_$CDUMP)
if [ $pgbf_grid -eq 4 ] ; then
 export flag_pgb=h
elif [ $pgbf_grid -eq 3 ] ; then
 export flag_pgb=f
elif [ $pgbf_grid -eq 2 ] ; then
 export flag_pgb=l
fi
export flag_pgb=${flag_pgb:-f}


SDATE=$CDATE
BDATE=$($NDATE -$BACKDATE $CDATE)    # online archive date only


################################################################################# Copy files to online archive
if [[ $ARCHCOPY = YES ]];then
 if [[ ! -s $ARCDIR ]]; then
    mkdir -p $ARCDIR
 fi

 SPECIALARCHSH=${SPECIALARCHSH:-""}
 if [ ! -z $SPECIALARCHSH ]; then
   if [ -s $SPECIALARCHSH ] ; then
     $SPECIALARCHSH
   fi
   rc=$?
   if [[ $rc -ne 0 ]];then $PERR;exit 1;fi
   $PEND
 fi

 export CDATE=$BDATE

# be sure we are in working directory $DATA
 cd $DATA
# rm *

# return code gets checked for any required files (ARCR)
 $PCOP $CDATE/$CDUMP/arch/ARCR $COMROT   $DATA <$RLIST
 rc=$?

#  Comment out next two  pcops so RLIST isn't reprocessed for FIT_DIR and HORZ_DIR
#   - reduces unnecessary errors
#   - at this time at least, no fit or horiz files are listed as ARCR anyway
#     because archiving of these done in global_savefits.sh (called by vrfy.sh)
#  Uncomment if deemed necessary (old setups?  future use?)
# $PCOP $CDATE/$CDUMP/arch/ARCR $FIT_DIR  $DATA <$RLIST
# ((rc+=$?))
# $PCOP $CDATE/$CDUMP/arch/ARCR $HORZ_DIR $DATA <$RLIST
# ((rc+=$?))

# dayfiles may not be stored in COMROT...
#    need to work on this to avoid unnecessary errors
 [ $COMDAY != $COMROT ] && $PCOP $CDATE/$CDUMP/arch/ARCR $COMDAY   $DATA <$RLIST

 ((rc+=$?))

# optional files
 $PCOP $CDATE/$CDUMP/arch/ARCO $COMROT $DATA <$RLIST
 [ $COMDAY != $COMROT ] && $PCOP $CDATE/$CDUMP/arch/ARCO $COMDAY   $DATA <$RLIST

 # Check to see if we only need to save the low resolution pgrb file online
 #   This is done for gfs and gdas pgb files archived via ARCR or ARCO

 if [ $io_save -lt $io_1 -a $jo_save -lt $jo_1 ] ; then
   if [ $NCEPPOST = YES ] ; then
     eval fhmax_cvt=\${fhmax_$fseg}
   elif [ $io_save -lt $io_2 -a $jo_save -lt $jo_2 ] ; then
     fhmax_cvt=$fhmax_2
   else
     fhmax_cvt=$fhmax_1
   fi

   if [ $io_save = 144 -a $jo_save = 73 ] ; then
#    copygbvars="-g2 -i1,1"
     copygbvars="-g2 -i0"
   elif [ $io_save = 360 -a $jo_save = 181 ] ; then
#    copygbvars="-g3 -i2"
     copygbvars="-g3 -i0"
   else
     copygbvars=" "
   fi

## for file in `ls pgbf*.$CDUMP.$CDATE  2>/dev/null` ; do
   for file in `ls pgb${flag_pgb}*.$CDUMP.$CDATE  2>/dev/null` ; do
     fhr=`echo $file |awk -F"." '{print $1}' |cut -c5-`
     if [ $fhr -le $fhmax_cvt ] ; then
       fname=pgbf${fhr}.$CDUMP.$CDATE
       if [ $file != $fname ]; then
          mv $file $fname
       fi
       $COPYGB $copygbvars -x $fname ${fname}.lowres
       sleep 2
       mv ${fname}.lowres $fname
     fi
   done
   if [ -s pgb${flag_pgb}nl.$CDUMP.$CDATE ]; then
     file=pgb${flag_pgb}nl.$CDUMP.$CDATE
     $COPYGB $copygbvars -x $file ${file}.lowres
     sleep 2
     mv ${file}.lowres $file
   fi
   if [ -s pgbanl.$CDUMP.$CDATE ]; then
     file=pgbanl.$CDUMP.$CDATE
     $COPYGB $copygbvars -x $file ${file}.lowres
     sleep 2
     mv ${file}.lowres $file
   fi
 fi

  $NCP *${CDATE}* $ARCDIR/
  CDATE00=$(echo $CDATE|cut -c1-8)
  $NCP $COMROT/*SCORES*${CDATE00}* $ARCDIR/

# CONUS pcp score (rain) file directly copied to $ARCDIR 
# via vsdbjob.sh in vrfy.sh.   Thus, no rain file in $COMROT
##CDATE00=$(echo $($NDATE -${BACKDATEPRCP:-00} $CDATE) |cut -c1-8 )
##$NCP $COMROT/*rain*${CDATE00}* $ARCDIR/

  if [[ $LOGNAME = glopara && $ARCHSCP = YES ]]; then
     $SCP *${CDATE}* $LOGNAME@$ARCHSCPTO:$ARCDIR/
##   list="SCORES rain"
     list="SCORES"
     for file in $list; do
        CDATE00=$(echo $CDATE|cut -c1-8)
        $SCP $COMROT/*${file}*${CDATE00}* $LOGNAME@$ARCHSCPTO:$ARCDIR/
     done
  fi


fi

export CDATE=$SDATE

################################################################################
# Zhu archive.
if [[ $ARCH_GLOBSTAT = YES ]];then
  if [[ $LOGNAME = glopara  ]];then
    if [[ $CDUMP = $CDFNL ]];then
     RUN=GDAS ; if [ $NOANAL = YES ] ; then RUN=GFS ; fi
     RUN=GDAS EXP=$PSLOT CDIR=$COMROT EDIR=$EXPDIR $SUB -a $ACCOUNT \
     -g class1onprod -e 'CDATE,RUN,EXP,CDIR,EDIR' -j garch.FNL$PSLOT.$CDATE \
     -o /global/shared/stat/$EXPDIR/FNL$PSLOT.$CDATE -u globstat@$HOST \
     /global/save/wx20rt/globstat/arch/garch.lls
    else
     RUN=MRF EXP=$PSLOT CDIR=$COMROT EDIR=$EXPDIR $SUB -a $ACCOUNT \
     -g class1onprod-e 'CDATE,RUN,EXP,CDIR,EDIR' -j garch.GFS$PSLOT.$CDATE \
     -o /global/shared/stat/$EXPDIR/GFS$PSLOT.$CDATE -u globstat@$HOST \
     /global/save/wx20rt/globstat/arch/garch.lls
    fi
  fi
fi

###############################################################################

################################################################################
# Archive to tape.

# export CDATE=$BDATE    # commented out because short hrksigg is sometimes required
export CDATE=$SDATE      # so, instead, archive to tape asap
rc=0

SGDATE=$GDATE
if [ $ARCH_TO_HPSS = YES ] ; then
 cycle=$(echo $CDATE|cut -c9-10)
 cdump=$(echo $CDUMP|tr '[a-z]' '[A-Z]')

# GDATE and GDATE00 for SCORES and rain files
 export GDATE=$($NDATE -6 $CDATE)
 export CDATE00=$(echo $CDATE|cut -c1-8)
 export GDATE00=$(echo $GDATE|cut -c1-8)
# cd $DATA

 for a in ARCA ARCB ARCC ARCD ARCE ARCF;do
   NDATA=$DATA/$a
   mkdir -p $NDATA||exit 1;cd $NDATA||exit 1
   rm *
   eval eval afile=\${$a$cycle$cdump:-null}
   if [[ $afile != null ]];then
# make hpss directory if it doesn't exist
     if [ $machine = IBMP6 ] ; then
       $HPSSTAR dir `dirname $afile` > /dev/null 2>&1   #dcs
       [ $? -ne 0 ] && $HPSSTAR mkd `dirname $afile`
     elif [ $machine = ZEUS -o $machine = WCOSS ] ; then
       $HSI mkdir -p ${ATARDIR}
     fi
     $PCOP $CDATE/$CDUMP/arch/$a $COMROT $NDATA <$RLIST
     [ $COMDAY != $COMROT ] && $PCOP $CDATE/$CDUMP/arch/$a $COMDAY $NDATA <$RLIST
     sleep 30
     if [ $machine = IBMP6 ] ; then
       $HPSSTAR put $afile *
       ((rc+=$?))
     elif [ $machine = ZEUS -o $machine = WCOSS ] ; then
     np='1/1/S'
     mem='1024/1'
     tl='3:00:00'
     qq=${CUE2RUNA:-transfer}
     jn=hpsstrans$a$cycle$cdump
     out=$COMROT/${a}${CDATE}${CDUMP}${CSTEP}.transfer
     trans_local=$COMROT/transfer_${a}_${CDATE}${CDUMP}${CSTEP}
     > $trans_local
     echo "export NDATA=$NDATA" >> $trans_local
     echo "export HTAR=$HTAR"   >> $trans_local
     echo "export afile=$afile" >> $trans_local
     echo ""                    >> $trans_local
     echo "cd $NDATA"           >> $trans_local
     echo "$HTAR -cvf $afile *" >> $trans_local
     chmod 755 $trans_local
     en=CONFIG="$trans_local"
     $SUB -e $en -q $qq -a $ACCOUNT -g $GROUP -p $np -r $mem -t $tl -j $jn -o $out $trans_local
     ((rc+=$?))
     fi

   fi
 done
fi

if [[ $CFSRR_ARCH = YES && $CDATE = ????????00 ]];then
 xdate=$($NDATE -24 $CDATE)
 $ARCHCFSRRSH pr$PSLOT $xdate $CDUMP $COMROT
fi

export CDATE=$SDATE
export GDATE=$SGDATE

################################################################################
# Clean up.

# rm old work directories
HRKTMPDIR=$HRKTMP
if [[ $CDUMP = gdas ]] ; then
  HRKTMPDIR=$HRKTMPGDAS
elif [[ $CDUMP = gfs ]] ; then
  HRKTMPDIR=$HRKTMPGFS
fi
rdate=$($NDATE -$HRKTMPDIR $CDATE)
rm -rf $(CDATE=$rdate CDUMP=$CDUMP CSTEP='*' eval ls -d $DATATMP) 2>/dev/null

# define function to check that verifications files are archived online before clean up.
chkarc ()
{
  set -x
    ARCDIR=$1
    for verif_file in `ls $rmfiles 2>/dev/null`
    do
      if [ ! -s $ARCDIR/$verif_file ]; then
        set +x
        echo "****  VERIFICATION FILE $verif_file MISSING FROM $ARCDIR"
        echo "****  WILL ATTEMPT TO MOVE $verif_file TO $ARCDIR NOW"
        echo "****  TAPE ARCHIVE SHOULD BE CHECKED"
        set -x
        mv $verif_file $ARCDIR
      fi
    done
}

## for dayfiles, cd to COMDAY to avoid hitting unix line length limit ("Arg list too long.")
cd $COMDAY
rdate=$($NDATE -$HRKDAY $CDATE)
rm $PSLOT$rdate*dayfile 2>/dev/null

## for other files, cd to COMROT to avoid hitting unix line length limit ("Arg list too long.")

cd $COMROT

# sigma and surface files of age HRKSIG
rdate=$($NDATE -$HRKSIG $CDATE)
rm sigf*.$CDUMP.$rdate* 2>/dev/null
rm sfcf*.$CDUMP.$rdate* 2>/dev/null

# flx files of age HRKFLX  
rdate=$($NDATE -$HRKFLX $CDATE)
rm flxf*.$CDUMP.$rdate* 2>/dev/null
# use touch to prevent system scrubber from removing 
# aged flx files (3 days) that are used for QPF computation 
if [ $CDUMP = gfs ]; then
 cycle=$(echo $CDATE|cut -c9-10)
 keepday=$($NDATE -72 $(date +%Y%m%d)$cycle )
 sdate=$($NDATE +12 $rdate)
 while [ $sdate -le $CDATE ]; do
  if [ -s flxf24.$CDUMP.$sdate ]; then
   flxctime=$(stat -c '%y' flxf24.$CDUMP.$sdate | cut -c 1-10 | sed 's/-//g')$cycle
   if [ $keepday -gt $flxctime ]; then touch flxf*.$CDUMP.$sdate ; fi
  fi
  sdate=$($NDATE +12 $sdate)
 done
fi

# sigma guess files of age HRKSIGG
rdate=$($NDATE -$HRKSIGG $CDATE)
rm sigg*.$CDUMP.$rdate* 2>/dev/null

# gaussin and/or high-resolution pgb files of age HRKPGBM
rdate=$($NDATE -$HRKPGBM $CDATE)
rm pgbm*.$CDUMP.$rdate* 2>/dev/null
rm pgbh*.$CDUMP.$rdate* 2>/dev/null
rm pgbq*.$CDUMP.$rdate* 2>/dev/null

# remove ocean files
if [ $COUP_FCST = YES ] ; then

# remove netcdf ocean files (ocn and ice)
 rdate=$($NDATE -$HRKOCN_NC $CDATE)
 rm ocn_*.$CDUMP.$rdate* 2>/dev/null
 rm ice_*.$CDUMP.$rdate* 2>/dev/null

# remove  ocean analysis files
 rdate=$($NDATE -$HRKOCN_ANL $CDATE)
 rm ocnanl.$CDUMP.$rdate* 2>/dev/null

# remove  ocean forecast grib  files
 rdate=$($NDATE -$HRKOCN_GRB $CDATE)
 rm ocnh*.$CDUMP.$rdate* 2>/dev/null
 rm ocnf*.$CDUMP.$rdate* 2>/dev/null
fi

# remaining CDUMP files except flxf of age HRKROT
rdate=$($NDATE -$HRKROT $CDATE)
#rm *.$CDUMP.$rdate* 2>/dev/null
rm $(ls *.$CDUMP.$rdate* |grep -v flxf ) 2>/dev/null

# verification files of age HRKVFY
rdate=$($NDATE -$HRKVFY $CDATE)
rdate00=$(echo $rdate|cut -c1-8)
rmfiles="SCORES${PSLOT}.$rdate pr${PSLOT}_rain_$rdate00"
# check that they have been archived online.  no check for tape archive.
[[ $ARCHCOPY = YES ]] && chkarc $ARCDIR
rm $rmfiles 2>/dev/null

# check fits safely archived before removal
rmfiles="f*.acar.$rdate f*.acft.$rdate f*.raob.$rdate f*.sfc.$rdate"
FIT_DIR=${FIT_DIR:-$ARCDIR}
chkarc $FIT_DIR
rm $rmfiles 2>/dev/null

rmfiles="*.anl.$rdate"
HORZ_DIR=${HORZ_DIR:-$ARCDIR}
#chkarc ${HORZ_DIR}/anl   # need to pass in modified filename for this to work (do later)
rm $rmfiles 2>/dev/null

rmfiles="*.fcs.$rdate"
#chkarc ${HORZ_DIR}/fcs   # need to pass in modified filename for this to work (do later)
rm $rmfiles 2>/dev/null

#
# Clean the restart files in the RESTART directory
#
cd $RESDIR
rdate=$($NDATE -$HRKRES $CDATE)
rm sig1r*.$CDUMP.$rdate*       2>/dev/null
rm sig2r*.$CDUMP.$rdate*       2>/dev/null
rm sfcr*.$CDUMP.$rdate*       2>/dev/null
if [ $COUP_FCST = YES ] ; then
 rm omrest*.$CDUMP.$rdate.*.tar 2>/dev/null
 rm *.2restart*.$CDUMP.$rdate   2>/dev/null
 rm fluxes_for*.$CDUMP.$rdate.* 2>/dev/null
#rm noah.rst*.$rdate     2>/dev/null
fi

#
# If requested, make symbolic links to create ops-like /com/gfs files
if [ $DO_PRODNAMES = YES ] ; then
  rc=0
  if [ ! -s $PRODNAMES_DIR ] ; then
    mkdir -p $PRODNAMES_DIR
    rc=$?
  fi
  if [[ $rc -eq 0 ]]; then
     $SETUPPRODNAMESH $COMROT $PRODNAMES_DIR $CDATE $CDUMP $HRKSIG
  else
     echo "ARCH:  ***WARNING*** CANNOT mkdir $PRODNAMES_DIR.  Will NOT run $SETUPPRODNAMESH"
  fi
fi

cd $DATA


################################################################################
# Exit gracefully

if [[ $rc -ne 0 ]];then $PERR;exit 1;fi
$PEND
