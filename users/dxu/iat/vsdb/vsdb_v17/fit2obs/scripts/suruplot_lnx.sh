#!/bin/ksh
if [ $# -ne 7 ] ; then
  echo "Usage: $0 mdl fcshr sdate (yyyymmddhh) edate incr outdir mdldir"
  exit 1
fi

set -xaeu
#
export mdl=$1
export fcshr=$2
export sdate=$3
export edate=$4
export incr=$5
export outdir=$6
export mdldir=$7

export syear=`echo $sdate | cut -c1-4`
export smonth=`echo $sdate | cut -c5-6`
export sday=`echo $sdate | cut -c7-8`
export shour=`echo $sdate | cut -c9-10`
#
export eyear=`echo $edate | cut -c1-4`
export emonth=`echo $edate | cut -c5-6`
export eday=`echo $edate | cut -c7-8`
export ehour=`echo $edate | cut -c9-10`

export idbug=1
#
$SORC/suruplot.x
  ese=$?
  if [ $ese -ne 0 ]
  then
    echo "executable suruplot.x failed"
    exit 1
  fi


cd $outdir

export yyyy=`echo $sdate | cut -c1-4`
export mm=`echo $sdate | cut -c5-6`
export mon=`$SCRIPTS/cmon.sh $mm`
export dd=`echo $sdate | cut -c7-8`
export hh=`echo $sdate | cut -c9-10`

export outname=$mdl.f$fcshr.raob.$sdate.$edate
export outfile=$mdl.f$fcshr.raob

/bin/cp $CTLS/fit_lnx.ctl $outfile.ctl

> out
sed "s?fitname?$outname?g" $outfile.ctl | sed "s?hhzddcmonyyyy?${hh}z${dd}${mon}${yyyy}?g" | sed "s?incr?$incr?g" > out
cp out $outfile.ctl
cat $outfile.ctl
