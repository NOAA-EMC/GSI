#!/bin/ksh
# make links for global parallel files using prod names
# must run for each cycle... 
set -xa

if [ $# -lt 5 ]; then
  set +x
  echo " Usage:  $0 COMIN COMPROD CDATE CDUMP HRKSIG"
  echo " Eg:  /global/hires/glopara/pre13a /global/hires/glopara/prod 2008101700 gfs 120"
  echo " exiting"
  set -x
  exit 6
fi

DATA=${TMPDIR:-$STMP/$LOGNAME}/setprodnames
NWPROD=${NWPROD:-/nwprod}

[ -d $DATA ] || mkdir $DATA
cd $DATA


COMIN=${1:-$COMROT}
COMPROD=${2:-${COMPROD:-$COMIN/prod}}
CDATE=${3:-$CDATE}
CDUMP=${4:-gfs}
HRKSIG=${5:-${HRKSIG:-120}}    ;#clean files older than HRKSIG hours

if [ ! -d $COMIN ]; then
  set +x
  echo "ERROR:  $COMIN is not a directory.  Exiting..."
  set -x
  exit
fi


DDATE=`echo $CDATE | cut -c 1-8`
CYC=`echo $CDATE | cut -c 9-10`
export cycle=t${CYC}z
CDATEM=`$NWPROD/util/exec/ndate -$HRKSIG $CDATE`
DDATEM=`echo $CDATEM | cut -c 1-8`

#sh $NWPROD/util/ush/setpdy.sh
#. PDY
#echo $PDYm1
#FDATE=$PDYm1  # want to use date we know should exist in /com IF checking prod files below
#COMOUT=/global/noscrub/glopara


cd $COMIN

# Process GDAS and GFS
if [ $CDUMP = gdas -o $CDUMP = gfs ]; then
 case $CDUMP in
  gdas) NCO_DUMP=gdas;PRE=gdas1;;
  gfs) NCO_DUMP=gfs;PRE=gfs;;
  *) echo UH-OH. $CDUMP;continue;;  # default
 esac

 COMOUT=$COMPROD/com/gfs/para/$CDUMP.$DDATE
 [ -d $COMOUT ] || mkdir -p $COMOUT
 if [ $? -ne 0 ]; then
  set +x; echo error creating $COMOUT;set -x
  exit 7
 fi

 COMOUTM=$COMPROD/com/gfs/para/$CDUMP.$DDATEM
 if [ -d $COMOUTM ]; then rm -r $COMOUTM ;fi         

 for file in *.$CDUMP.$CDATE; do

  echo $file

  base=${file%%.$CDUMP.$CDATE}
  echo $base

  [ -n "$FH" ] && unset FH
  [ -n "$NCO_BASE" ] && unset NCO_BASE

  FOUND=YES

  case $base in
   *.lr) continue;;
   splf*) continue;;
   tcinform_relocate)continue;;
   tcvitals_relocate)continue;;
   atcfunix) continue;;
   prepqc) NCO_BASE=prepbufr;;
   prepq*) continue;;
   cnvstat) NCO_BASE=cnvstat;;
   gsistat) NCO_BASE=gsistat;;
   oznstat) NCO_BASE=oznstat;;
   pcpstat) NCO_BASE=pcpstat;;
   radstat) NCO_BASE=radstat;;
   flxf*) FH=${base#flxf};NCO_BASE=sfluxgrbf$FH;;
   g3dc*) FH=${base#g3dc};NCO_BASE=gcgrbf$FH;;
   logf*) FH=${base#logf};NCO_BASE=logf$FH;;
   pgbqanl) NCO_BASE=pgrbqanl;;
   pgbhanl) NCO_BASE=pgrbhanl;;
   pgbanl) NCO_BASE=pgrbanl;;
   pgblanl) NCO_BASE=pgrblanl;;
   pgbq*) FH=${base#pgbq};NCO_BASE=pgrbq$FH;;
   pgbh*) FH=${base#pgbh};NCO_BASE=pgrbh$FH;;
   pgbf*) FH=${base#pgbf};NCO_BASE=pgrbf$FH;;
   pgbl*) FH=${base#pgbl};NCO_BASE=pgrbl$FH;;
   pgrbl*) FH=${base#pgrbl};NCO_BASE=pgrbf$FH.2p5deg;;
   sfcanl) NCO_BASE=sfcanl;;
   sfcf*) FH=${base#sfcf};NCO_BASE=bf$FH;;
   siganl) NCO_BASE=sanl;;
   sigf*) FH=${base#sigf};NCO_BASE=sf$FH;;
   sigges) NCO_BASE=sgesprep;;
   siggm3) NCO_BASE=sgm3prep;;
   siggp3) NCO_BASE=sgp3prep;;
   biascr) NCO_BASE=abias;;
   biascr_pc) NCO_BASE=abias_pc;;
   satang) NCO_BASE=satang;;
   *) FOUND=NO;;
  esac

  if [ $FOUND = YES ]; then
   if [ $CDUMP = gfs ]; then 
    if [ $base = pgrbqnl -o $base = pgrbq$FH ]; then
     if [ $FH = nl ]; then
       ln -sf $COMIN/$file $COMOUT/$PRE.t${CYC}z.$NCO_BASE
     elif [ $FH -le 240 ]; then
       ln -sf $COMIN/$file $COMOUT/$PRE.t${CYC}z.$NCO_BASE
     else
       ln -sf $COMIN/$file $COMOUT/$PRE.t${CYC}z.$NCO_BASE
     fi
    else
     ln -sf $COMIN/$file $COMOUT/$PRE.t${CYC}z.$NCO_BASE
    fi
   fi
   if [ $CDUMP = gdas ]; then 
     ln -sf $COMIN/$file $COMOUT/$PRE.t${CYC}z.$NCO_BASE
   fi
  else
   echo "NOT FOUND:  $COMIN/$file"
  fi

 done # end for file

 for file in *.$CDUMP.$CDATE.grib2; do

  echo $file

  base=${file%%.$CDUMP.$CDATE.grib2}
  echo $base

  [ -n "$FH" ] && unset FH
  [ -n "$NCO_BASE" ] && unset NCO_BASE

  FOUND=YES

  case $base in
   pgrbqanl) NCO_BASE=pgrb2.0p25.anl;;
   pgrbhanl) NCO_BASE=pgrb2.0p50.anl;;
   pgrbanl) NCO_BASE=pgrb2.1p00.anl;;
   pgrblanl) NCO_BASE=pgrb2.2p50.anl;;
   pgbqnl) NCO_BASE=master.grb2anl;;
   pgrbbqanl) NCO_BASE=pgrb2b.0p25.anl;;
   pgrbbhanl) NCO_BASE=pgrb2b.0p50.anl;; 
   pgrbbfanl) NCO_BASE=pgrb2b.1p00.anl;;
   pgrbq*) FH=${base#pgrbq};NCO_BASE=pgrb2.0p25.f$FH;;
   pgrbh*) FH=${base#pgrbh};NCO_BASE=pgrb2.0p50.f$FH;;
   pgrbf*) FH=${base#pgrbf};NCO_BASE=pgrb2.1p00.f$FH;;
   pgrbl*) FH=${base#pgrbl};NCO_BASE=pgrb2.2p50.f$FH;;
   pgbq*) FH=${base#pgbq};NCO_BASE=master.grb2f$FH;;
   pgrbbq*) FH=${base#pgrbbq};NCO_BASE=pgrb2b.0p25.f$FH;;
   pgrbbh*) FH=${base#pgrbbh};NCO_BASE=pgrb2b.0p50.f$FH;;
   pgrbbf*) FH=${base#pgrbbf};NCO_BASE=pgrb2b.1p00.f$FH;;
   *) FOUND=NO;;
  esac

  if [ $FOUND = YES ]; then
   if [ $CDUMP = gfs ]; then 
    if [ $base = pgrbqnl -o $base = pgrbq$FH ]; then
     if [ $FH = nl ]; then
       ln -sf $COMIN/$file $COMOUT/$PRE.t${CYC}z.$NCO_BASE
     elif [ $FH -le 240 ]; then
       ln -sf $COMIN/$file $COMOUT/$PRE.t${CYC}z.$NCO_BASE
     else
       ln -sf $COMIN/$file $COMOUT/$PRE.t${CYC}z.$NCO_BASE
     fi
    else
     ln -sf $COMIN/$file $COMOUT/$PRE.t${CYC}z.$NCO_BASE
    fi
   fi
   if [ $CDUMP = gdas ]; then 
     ln -sf $COMIN/$file $COMOUT/$PRE.t${CYC}z.$NCO_BASE
   fi
  else
   echo "NOT FOUND:  $COMIN/$file"
  fi

 done # end for grib2 file

fi

# get select dump data
COMDMP=$DMPDIR/$CDATE/$CDUMP
COMDMPx=$DMPDIR/$CDATE/${CDUMP}x
COMDMPy=$DMPDIR/$CDATE/${CDUMP}y

ln -sf $COMDMP/sstgrb.$CDUMP.$CDATE $COMOUT/$PRE.t${CYC}z.sstgrb
ln -sf $COMDMP/icegrb.$CDUMP.$CDATE $COMOUT/$PRE.t${CYC}z.engicegrb
ln -sf $COMDMP/snogrb_t574.$CDUMP.$CDATE $COMOUT/$PRE.t${CYC}z.snogrb_t574
ln -sf $COMDMPx/snogrb_t1534.$CDUMP.$CDATE $COMOUT/$PRE.t${CYC}z.snogrb_t1534



# Create prod look-alike for enkf
if [ $CDUMP = enkf ]; then

 COMOUT=$COMPROD/com/gfs/para/$CDUMP.$DDATE/$CYC
 [ -d $COMOUT ] || mkdir -p $COMOUT
 if [ $? -ne 0 ]; then
  set +x; echo error creating $COMOUT;set -x
  exit 7
 fi

 COMOUTM=$COMPROD/$LOGNAME/com/gfs/para/$CDUMP.$DDATEM/$CYC
 if [ -d $COMOUTM ]; then rm -r $COMOUTM ;fi

 for file in *_${CDATE}*; do
  ln -sf $COMIN/$file $COMOUT/$file
 done

fi

#create GFS station profiles in bufr format
export COMIN=$COMOUT
if [ $CDUMP = gfs ]; then
export pid=$$
export JCAP=${JCAP:-574}
export LEVS=${LEVS:-64}
export LATB=${LATB:-880}
export LONB=${LONB:-1760}
export STARTHOUR=${STARTHOUR:-00}
export ENDHOUR=${ENDHOUR:-180}
mac=`hostname |cut -c1`
if [ $mac = g -o $mac = t ]; then
launcher=mpirun.lsf
$SUB -a $ACCOUNT -e 'GFLUX=$GFLUX,GBUFR=$GBUFR,TOCSBUFR=$TOCSBUFR,launcher=$launcher,STNLIST=$STNLIST,CDATE=$CDATE,COMIN=$COMIN,COMOUT=$COMOUT,DATA=$DATA,BASEDIR=$BASEDIR,JCAP=$JCAP,LEVS=$LEVS,LATB=$LATB,LONB=$LONB,STARTHOUR=$STARTHOUR,ENDHOUR=$ENDHOUR' -q $CUE2RUN -p 4/4/g2  -r 14000/8/2 -t 3:00:00 -j bufr_sounding -o $DATA/bufrsnd.out.$pid $BASEDIR/ush/postsnd.sh
elif [ $mac = f ]; then
launcher=mpiexec_mpt
CUE2RUN=bigmem
KMP_STACKSIZE=2048m
$SUB -a $ACCOUNT -e machine=$machine,GFLUX=$GFLUX,GBUFR=$GBUFR,TOCSBUFR=$TOCSBUFR,KMP_STACKSIZE=$KMP_STACKSIZE,launcher=$launcher,STNLIST=$STNLIST,CDATE=$CDATE,COMROT=$COMROT,COMIN=$COMIN,COMOUT=$COMOUT,DATA=$DATA,BASEDIR=$BASEDIR,JCAP=$JCAP,LEVS=$LEVS,LATB=$LATB,LONB=$LONB,STARTHOUR=$STARTHOUR,ENDHOUR=$ENDHOUR -q $CUE2RUN -p 3/2/g2  -r 36000/3/1 -t 3:00:00 -j bufr_sounding -o $DATA/bufrsnd.out.$pid $BASEDIR/ush/postsnd.sh
fi
fi


exit
