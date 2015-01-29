#!/bin/ksh
#set -x
#
# This script converts the high resolution (1x1 degree) ECMWF forecast and analysis files
# into GFS "look-alike" files, so that they have the same format and structure of the GFS
# grib files.
#
# The script first checks for the existence of the /dcom/us007003/yyyymmdd/ecmwf directory.
# If this exists, then the ECMWF forecast and analysis files are taken directly
# from there.  This would most likely be the case for very recent dates.  If the
# directory does not exist, then the script checks the HPSS archive for the files
# (contained in the "dcom_us007003" tar files on HPSS), and extracts these into
# the user's /gpfstmp directory (in /gpfstmp/USERID/wgrbbul/ecmwf).
#
# Once the data are found, the program "ecm_gfs_look_alike" is then applied and the resulting
# look-alike files are placed into the user's /ptmp directory, in /ptmp/USERID/global/ecm.
#
# Input includes (1) the number of cycles per day and (2) the range of dates (initial
# final date).  The date(s) have the format yyyymmddhh (yyyy=year; mm=month;
# dd=day; hh=model cycle hour, either 00Z or 12Z).  If only one date is
# given, final date is set to the initial date.  For the number of cycles, if only one (1)
# is requested, then only that particular cycle (00Z or 12Z) is processed each day within
# the range of dates.  If two (2) cycles are requested, then the 00Z and 12Z cycles are
# both processed within the date range, starting with the initial model cycle hour ("hh").
# and ending with the final model cycle hour ("hh").  Thus, if one chooses initial and final
# dates of 2006020112 2006020800, then the starting cycle would be 12Z on 20060201, and
# the ending cycle would be 00Z 20060208 (i.e., you will not get the 00Z cycle on 20060201,
# nor the 12Z cycle on 20060208).
#
if [ $# -lt 2 ]; then
 echo "USAGE:  get_ecm_lookalike.sh NCYC IDATE [FDATE]."
 echo "        NCYC=number of cycles (1 or 2 per day)"
 echo "        IDATE=initial date, yyyymmddhh format (hh must be 00 or 12)"
 echo "        FDATE=final date, yyyymmddhh format (hh must be 00 or 12);"
 echo "              if not specified, it is set to IDATE"
 echo ""
 echo "        The ECMWF look-alike files will be stored in /ptmp/USERID/global/ecm"
 echo ""
 exit 1
fi

ncyc=$1
idate=$2
fdate=${3:-$idate}

curdir=`pwd`

if [ $ncyc -gt 2 ]; then
 echo "Maximum of 2 cycles per day available (00Z and 12Z)."
 exit 1
fi
if [ ${#idate} -lt 10 -o ${#fdate} -lt 10 ]; then
 echo "Date format must be yyyymmddhh"
 exit 1
fi

nhoursx=/nwprod/util/exec/ndate
ecretag=${exe}/misc/ecm_gfs_look_alike
tmpdir=/stmp/${LOGNAME}/ecretag
[ ! -d $tmpdir ] && mkdir -p $tmpdir
#comout=$comout
#[ ! -d $comout ] && mkdir -p $comout

cdate=$idate
ifhr=12
nextcyc=$(( 24/ncyc ))
while [ $cdate -le $fdate ]; do
 fhr=$ifhr
 yy=$(echo $cdate | cut -c1-4)
 ym=$(echo $cdate | cut -c1-6)
 ymd=$(echo $cdate | cut -c1-8)
 md=$(echo $cdate | cut -c5-8)
 cyc=$(echo $cdate | cut -c9-10)
 anl00=./wgrbbul/ecmwf/ecens_DCD${md}0000${md}00001
 anl06=./wgrbbul/ecmwf/ecens_DCD${md}0600${md}06001
 anl12=./wgrbbul/ecmwf/ecens_DCD${md}1200${md}12001
 anl18=./wgrbbul/ecmwf/ecens_DCD${md}1800${md}18001
 tarfile=/NCEPPROD/hpssprod/runhistory/rh${yy}/${ym}/${ymd}/dcom_us007003_${ymd}.tar
 q=$(ls -d /dcom/us007003/${ymd}/wgrbbul/ecmwf)
#
# Check HPSS archive if the /dcom/us007003 directory for this date does
# not exist (i.e., it's not a recent date that's still on /dcom).
#
 if [ -z "$q" ]; then
  cd $tmpdir
  hpsstar get $tarfile $anl00
  $ecretag ${tmpdir}/${anl00} ${comout}/pgbanl.${ymd}00
  hpsstar get $tarfile $anl06
  $ecretag ${tmpdir}/${anl06} ${comout}/pgbanl.${ymd}06
  hpsstar get $tarfile $anl12
  $ecretag ${tmpdir}/${anl12} ${comout}/pgbanl.${ymd}12
  hpsstar get $tarfile $anl18
  $ecretag ${tmpdir}/${anl18} ${comout}/pgbanl.${ymd}18
  cp ${comout}/pgbanl.${ymd}${cyc} ${comout}/pgbf00.${ymd}${cyc}
  while [ $fhr -le 240 ]; do
   fmdh=$($nhoursx +$fhr ${cdate} | cut -c5-10)
   fcst=./wgrbbul/ecmwf/ecens_DCD${md}${cyc}00${fmdh}001
   hpsstar get $tarfile $fcst 
   $ecretag ${tmpdir}/${fcst} ${comout}/pgbf${fhr}.${cdate}
   (( fhr = fhr + 12 ))
  done
#
# For very recent dates, the files are still in the /dcom directory.
#
 else
  $ecretag /dcom/us007003/${ymd}/$anl00 ${comout}/pgbanl.${ymd}00
  $ecretag /dcom/us007003/${ymd}/$anl06 ${comout}/pgbanl.${ymd}06
  $ecretag /dcom/us007003/${ymd}/$anl12 ${comout}/pgbanl.${ymd}12
  $ecretag /dcom/us007003/${ymd}/$anl18 ${comout}/pgbanl.${ymd}18
  cp ${comout}/pgbanl.${ymd}${cyc} ${comout}/pgbf00.${ymd}${cyc}
  while [ $fhr -le 240 ]; do
   fmdh=$($nhoursx +$fhr ${cdate} | cut -c5-10)
   fcst=./wgrbbul/ecmwf/ecens_DCD${md}${cyc}00${fmdh}001
   $ecretag /dcom/us007003/${ymd}/${fcst} ${comout}/pgbf${fhr}.${cdate}
   (( fhr = fhr + 12 ))
  done
 fi
 cdate=$($nhoursx +${nextcyc} ${cdate})
done
#cd $comout
#chgrp rstecm pgb*
#chmod 640 pgb*

cd $curdir

exit
