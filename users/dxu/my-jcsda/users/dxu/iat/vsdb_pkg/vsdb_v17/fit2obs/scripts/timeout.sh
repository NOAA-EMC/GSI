#!/bin/ksh
if [ $# -ne 8 ] ; then
  echo "Usage: $0 mdl fcshr sdate (yyyymmddhh) edate incr outdir mdldir datatype"
  exit 1
fi

set -aeu
#
export mdl=$1
export fcshr=$2
export sdate=$3
export edate=$4
export incr=$5
export outdir=$6
export mdldir=$7
export type=$8   

echo $mdl $fcshr $type

[ $type = raob ] && exec=$SORC/dieraob.x
[ $type = surf ] && exec=$SORC/diesurf.x
[ $type = ship ] && exec=$SORC/dieship.x
[ $type = acft ] && exec=$SORC/dieacft.x
[ $type = acar ] && exec=$SORC/dieacar.x
[ -z "$exec"   ] && { echo unrecognized $type; exit 99; }

export syear=`echo $sdate | cut -c1-4`
export smonth=`echo $sdate | cut -c5-6`
export sday=`echo $sdate | cut -c7-8`
export shour=`echo $sdate | cut -c9-10`
#
export eyear=`echo $edate | cut -c1-4`
export emonth=`echo $edate | cut -c5-6`
export eday=`echo $edate | cut -c7-8`
export ehour=`echo $edate | cut -c9-10`
#
export idbug=1
#
$exec >/dev/null || { echo "executable $exec failed"; exit 1; }

