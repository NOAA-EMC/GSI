#!/bin/ksh

if [ $# -lt 1 ]; then 
  echo " "
  echo " !!! ERROR: You must enter at least 1 argument:"
  echo " !!!"
  echo " !!! USAGE: `basename $0` basin"
  echo " "
  echo "     basin  = al, ep, wp"
  echo " "
  exit 8
fi

basin=$1

export netdir=${netdir:-/stmp/$LOGNAME/tracks/pru12c}
export ndate=$scrdir/ndate.x

for stormfile in `ls -1 a${basin}*.dat`
do

  stormname=` grep CARQ ${stormfile} | tail -1 | cut -c150-159` 
  stormname=` echo $stormname`

  sn_first=`     echo ${stormname} | cut -c1-1` 
  sn_remainder=` echo ${stormname} | cut -c2- | tr '[A-Z]' '[a-z]'`

  stormname=${sn_first}${sn_remainder}

  echo "${stormfile} ${stormname}"

done
