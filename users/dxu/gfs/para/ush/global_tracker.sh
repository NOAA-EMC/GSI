set -x

export CDATE=${1:-?}
export CDUMP=${2:-?}
export COMOUT=${3:-?}
export DATA=${4:-?}
export nknd=${5:-0}

export PSLOT=${PSLOT:-x}
user=$LOGNAME
#
export HOMEDIR=${HOMEDIR:-/global/save/wx23sm/GFS/f2010/trunk/para}
export NWPROD=${NWPROD:-$HOMEDIR}
export USHDIR=${USHDIR:-$HOMEDIR/ush}
export archsyndir=${archsyndir:-/com/arch/prod/syndat}
#
#export DISK_GLOB=${DISK_GLOB:-/global/save}
#export DISK_TRAK=${DISK_TRAK:-$DISK_GLOB}
export GETTRKEXEC=${GETTRKEXEC:-$HOMEDIR/util/exec/gettrk}
export inpdate=$CDATE
export paradir=$COMOUT
export prxtrak=$DATA
export vdir=$DATA

if [ ! -d ${vdir} ]; then
  mkdir -p ${vdir}
fi
if [ ! -d ${prxtrak} ]; then
  mkdir -p ${prxtrak}
fi

cd $vdir

# Always keep cmodel as "para"

export cmodel=para


# TRACKID can be changed to "parx", "pary", "parw", etc....

#
nn=$((`echo $PSLOT | wc -c`-1))
if [ $nn -eq 1 ] ; then
 export TRACKID=`echo par$PSLOT | cut -c1-4`
else
 export TRACKID=pr$PSLOT
# export TRACKID=`echo pr$PSLOT | cut -c1-4`
fi


# You MUST have 22 time levels here.  If you cut back to 120 hours, 
# replace hours 132-168 with 99's.  If you change to 6h resolution for
# the grib files, you must make source code changes to gettrk_main.f
# in subroutines output_all and output_atcf.


export FCSTHRS=' 00 06 12 18 24 30 36 42 48 54 60 66 72 78 84 90 96 102 108 114 120 126 132 138 144 150 156 162 168 174 180 186 192 198 204 210 216 222 228 234 240 99 99 99 99 99 99 99 99 99 99 99 99 99 99 99 99 99 99 99 99 99 99 99 99'

if [[ $CDUMP = gdas ]]; then
  export FCSTHRS=' 00 06 99 99 99 99 99 99 99 99 99 99 99 99 99 99 99 99 99 99 99 99 99 99 99 99 99 99 99 99 99 99 99 99 99 99 99 99 99 99 99 99 99 99 99 99 99 99 99 99 99 99 99 99 99 99 99 99 99 99 99 99 99 99 99' 
fi


export YYYYMMDDHH=${inpdate}
export PDY=` echo ${YYYYMMDDHH} | cut -c1-8`
export cyc=` echo ${YYYYMMDDHH} | cut -c9-10`
if [[ -r tcvitl.$CDUMP.$CDATE ]]; then
  export AUXTCVIT=$DATA/auxtcvit.$CDATE
  export GDATE=$($NWPROD/util/exec/ndate -06 $CDATE)
  cat tcvitl.gdas.$GDATE tcvitl.$CDUMP.$CDATE >$AUXTCVIT
else
  export AUXTCVIT=JUNK_NOFILE
fi

if [ $nknd -gt 1 ] ; then
 export CDUMP=${CDUMP}${nknd}
fi
${PARATRKR:-$USHDIR/paratrkr.sh}
cp $DATA/trak.$TRACKID.atcfunix.$CDATE $COMOUT/atcfunix.$CDUMP.$CDATE
