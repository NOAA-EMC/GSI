#!/bin/ksh
set -x

export sdate=2007071500
export fhr=00
export inchr=06
export last=120

export NEWDATE=$sdate

while [ $fhr -le $last ] ; do

YY=`echo $NEWDATE | cut -c1-4`
MM=`echo $NEWDATE | cut -c5-6`
DD=`echo $NEWDATE | cut -c7-8`
HH=`echo $NEWDATE | cut -c9-10`

echo 'NEWDATE' $NEWDATE
copygb -g2 -x pgbf00.${YY}${MM}${DD}${HH} pgbanalf00.${YY}${MM}${DD}${HH}

ls -l pgbanalf00.${YY}${MM}${DD}${HH}
err1=$?

if test "$err1" -ne 0
then
echo 'copygb FAILED, EXITTING'
exit
fi

let "fhr=fhr+$inchr"

typeset -Z3 fhr
NEWDATE=`${ndate_dir}/ndate +${fhr} $sdate`

done
echo "I'm finished"
exit
