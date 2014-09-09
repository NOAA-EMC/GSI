#!/bin/ksh
#set -x

indate=$1     ;#e.g 20070101 
nday=$2       ;# days to add or extract

NWPROD=${NWPROD:-/nwprod}
ndate=${ndate:-$NWPROD/util/exec/ndate}

nhours=` expr $nday \* 24 `
if [ $nhours -le 0 ]; then
 NDATE=`$ndate $nhours ${indate}00`
else
 NDATE=`$ndate +$nhours ${indate}00`
fi

yy=`echo $NDATE |cut -c 1-4 `
mm=`echo $NDATE |cut -c 5-6 `
dd=`echo $NDATE |cut -c 7-8 `

set -A mlist none jan feb mar apr may jun jul aug sep oct nov dec
outdate=${dd}${mlist[$mm]}${yy}
 

echo $outdate
