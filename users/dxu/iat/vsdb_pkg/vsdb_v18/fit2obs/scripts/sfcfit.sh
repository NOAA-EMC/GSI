#!/bin/ksh
if [ $# -ne 8 ] ; then
  echo "Usage: $0 sdate edate hzdir hzname outfile imas iwnd incrx"
  exit 1
fi

set -xeua

sdate=$1
edate=$2
hzdir=$3
hzname=$4
outf=$5
imasx=$6
iwndx=$7
incrx=$8

export inputdir=$hzdir
export iname=$hzname
export outfile=$outf
export imas=$imasx
export iwnd=$iwndx
export incr=$incrx

export syear=`echo $sdate | cut -c1-4`
export smonth=`echo $sdate | cut -c5-6`
export sday=`echo $sdate | cut -c7-8`
export shour=`echo $sdate | cut -c9-10`

export eyear=`echo $edate | cut -c1-4`
export emonth=`echo $edate | cut -c5-6`
export eday=`echo $edate | cut -c7-8`
export ehour=`echo $edate | cut -c9-10`

export idbug=0
${SFCFIT:-$SORC/sfcfit.x}
