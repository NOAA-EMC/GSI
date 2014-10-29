#!/bin/ksh
set -x

export sdate=2007071600
export fhr=00
export inchr=03
export last=120

export NEWDATE=$sdate

while [ $fhr -le $last ] ; do

YY=`echo $NEWDATE | cut -c1-4`
MM=`echo $NEWDATE | cut -c5-6`
DD=`echo $NEWDATE | cut -c7-8`
HH=`echo $NEWDATE | cut -c9-10`

echo 'NEWDATE' $NEWDATE
copygb -g2 -x pgbf${fhr}.gfs.$sdate gfsdana.f${fhr}.$sdate

ls -l  gfsdana.f${fhr}.$sdate
err1=$?
if test "$err1" -ne 0
then
echo 'copygb FAILED, EXITTING'
exit
fi
fhr=$(($fhr+$inchr))
if [ $fhr -lt 9 ]
fhr=0$fhr
fi

NEWDATE=`${ndate_dir}/ndate  +${fhr} $sdate`

done
echo "I'm finished"
exit
