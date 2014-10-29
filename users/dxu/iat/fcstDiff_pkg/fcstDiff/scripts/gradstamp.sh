#!/bin/sh
set -x

indate=${1:-20090101}     ;#e.g 20070101 
nday=${2:-0}       ;# days to add or extract
cyc=${3:-00}        ;##cycle time
ct=`echo $indate | wc -c`

if [ $ct -ge 10 ]; then
cyc=`echo $indate |cut -c 9-10 `
indate=`echo $indate |cut -c 1-8 `
fi
nhours=`expr $nday \* 24`
if [ $nhours -le 0 ]; then
 NDATE=`${ndate_dir}/ndate $nhours ${indate}00`
else
 NDATE=`${ndate_dir}/ndate  +$nhours ${indate}00`
fi

yy=`echo $NDATE |cut -c 1-4 `
mm=`echo $NDATE |cut -c 5-6 `
dd=`echo $NDATE |cut -c 7-8 `

set -A mlist none jan feb mar apr may jun jul aug sep oct nov dec

outdate=${cyc}z${dd}${mlist[$mm]}${yy}
 

echo $outdate
