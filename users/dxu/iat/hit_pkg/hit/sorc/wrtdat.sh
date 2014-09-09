#!/bin/ksh

#--------------------------------------------------------------------
# This script was written by Tim Marchok (timothy.marchok@noaa.gov)
#
# This script simply reads in an ascii text output file that 
# contains either track or intensity error data and calls an
# executable that writes that data out in binary format so
# that GrADS can plot it.

set +x

if [ $# -ne 1 ]; then
  set +x
  echo " "
  echo "!!! ERROR: You need at least 1 argument, the input track full path file name"
  echo "!!! USAGE: `basename $0` filename"
  echo " "
  echo "    Try again...."
  echo " "
  exit 8
fi

full_path_file=$1

fname=`  basename ${full_path_file}`
datdir=`  dirname ${full_path_file}`

execdir=${scrdir:-/global/save/wx24fy/VRFY/hurtrack/sorc}
cd $datdir

cat <<paramEOF >${datdir}/wrtdat.input
 &namin INFILE='${fname}'/
paramEOF

${execdir}/wrtdat.x <${datdir}/wrtdat.input

exit
