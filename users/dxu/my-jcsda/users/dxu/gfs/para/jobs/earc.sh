#!/bin/ksh
################################################################################
# This script runs the enkf archive and cleanup.
# Usage: earc.sh
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

set -a;. $CONFIG;set +a  
export CKSH=$(echo $CSTEP|cut -c-4)
export CKND=$(echo $CSTEP|cut -c5-)
export machine=${machine:-ZEUS}
machine=$(echo $machine|tr '[a-z]' '[A-Z]')
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
HRKSIGG=${HRKSIGG:-120}
HRKPGBM=${HRKPGBM:-48}
HRKROT=${HRKROT:-120}
HRKDAY=${HRKDAY:-${HRKROT:-120}}
HRKVFY=${HRKVFY:-${HRKROT:-120}}
HRKOCN_NC=${HRKOCN_NC:-$HRKSIG}
HRKOCN_ANL=${HRKOCN_ANL:-$HRKROT}
HRKOCN_GRB=${HRKOCN_GRB:-$HRKROT}
HRKETMP=${HRKETMP:-24}
HRKENKF=${HRKENKF:-72}

ARCH_GLOBSTAT=${ARCH_GLOBSTAT:-NO}
ARCHCOPY=${ARCHCOPY:-NO}
ARCHSCP=${ARCHSCP:-NO}
ARCHDAY=${ARCHDAY:-2}       # impacts delay for online archive only
BACKDATE=$((ARCHDAY*24))    # impacts delay for online archive only

#----moved to spos.sh  ---------------
#DO_PRODNAMES=${DO_PRODNAMES:-NO}
#PRODNAMES_DIR=${PRODNAMES_DIR:-$COMROT/prod}
#SETUPPRODNAMESH=${SETUPPRODNAMESH:-$USHDIR/setup_prodnames.sh}
#
# If requested, make symbolic links to create ops-like /com/gfs files
#if [ $DO_PRODNAMES = YES ] ; then
#  rc=0
#  if [ ! -s $PRODNAMES_DIR ] ; then
#    mkdir -p $PRODNAMES_DIR
#    rc=$?
#  fi
#  if [[ $rc -eq 0 ]]; then
#     $SETUPPRODNAMESH $COMROT $PRODNAMES_DIR $CDATE enkf $HRKSIG NO
#  else
#     echo "EARC:  ***WARNING*** CANNOT mkdir $PRODNAMES_DIR.  Will NOT run $SETUPPRODNAMESH"
#  fi
#fi
#--------------------------------------------


myhost=$(hostname)
case $myhost in
  c*) export HOST=cirrus;;
  s*) export HOST=stratus;;
  m*) export HOST=mist;;
  d*) export HOST=dew;;
  h*) export HOST=haze;;
  z*) export HOST=zephyr;;
  *) echo unexpected hostname $myhost;HOST="";;
esac

if [[ $ARCHSCP = YES ]];then
  if [[ $HOST = cirrus ]]; then
     SCPTO=stratus
  elif [[ $HOST = stratus ]]; then
     SCPTO=cirrus
  elif [[ $HOST = mist ]]; then
     SCPTO=dew
  elif [[ $HOST = dew ]]; then
     SCPTO=mist
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
export FHOUT=${FHOUT_ENKF:-3}
export FHMAX=${FHMAX_ENKF:-9}
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

export CDUMPE=enkf

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
 $PCOP $CDATE/$CDUMPE/arch/ARCR $COMROT   $DATA <$RLIST
 rc=$?

# dayfiles may not be stored in COMROT...
#    need to work on this to avoid unnecessary errors
 [ $COMDAY != $COMROT ] && $PCOP $CDATE/$CDUMPE/arch/ARCR $COMDAY   $DATA <$RLIST

 ((rc+=$?))

# optional files
 $PCOP $CDATE/$CDUMPE/arch/ARCO $COMROT $DATA <$RLIST
 [ $COMDAY != $COMROT ] && $PCOP $CDATE/$CDUMPE/arch/ARCO $COMDAY   $DATA <$RLIST

  $NCP *${CDATE}* $ARCDIR/
  CDATE00=$(echo $CDATE|cut -c1-8)
  if [[ $LOGNAME = glopara && $ARCHSCP = YES ]]; then
     $SCP *${CDATE}* $LOGNAME@$ARCHSCPTO:$ARCDIR/
  fi


fi

export CDATE=$SDATE


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

 for a in EARCA EARCB EARCC;do
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

# Archive EnKF forecast hours other than FH=6
 export FH=0
 while [ $FH -le $FHMAX ]; do
   if [ $FH -ne 6 ] ; then
     FHR=$FH
     if [ $FH -lt 10 ]; then
       export FHR=0$FH
     fi
     a=EARC${FHR}
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
       count=`ls | wc -l`
       if [ $count -gt 0 ]; then
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
     fi
   fi
   FH=`expr $FH + $FHOUT`
 done
fi

export CDATE=$SDATE
export GDATE=$SGDATE

################################################################################
# Clean up.

# rm old work directories
rdate=$($NDATE -$HRKETMP $CDATE)
rm -rf $(CDATE=$rdate CDUMP=${CDUMP}e CSTEP='*' eval ls -d $DATATMP) 2>/dev/null

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
rm $PSLOT$rdate*dayfile* 2>/dev/null

## for other files, cd to COMROT to avoid hitting unix line length limit ("Arg list too long.")

cd $COMROT

# remove enkf files
if [ $DOENKF = YES ] ; then
 rdate=$($NDATE -$HRKENKF $CDATE)
 rm *${rdate}_ensmean 2>/dev/null
 rm *${rdate}_mem*    2>/dev/null
 rm *${rdate}_all     2>/dev/null
 rm *${rdate}_grp*    2>/dev/null
 rm *${rdate}_fhr*    2>/dev/null
 rm enkfstat_${rdate} 2>/dev/null
 rm sigpairs_${rdate} 2>/dev/null
 rm pertdates_${rdate} 2>/dev/null
fi


cd $DATA


################################################################################
# Exit gracefully

if [[ $rc -ne 0 ]];then $PERR;exit 1;fi
$PEND
