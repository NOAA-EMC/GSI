#!/bin/sh
set -ax

#
# set_prodate.sh
#
# Replace the protime in the data_map file for the given suffix.  If the 
# suffix is not found, take no action.
#

function usage {
  echo "Usage:  set_prodate.sh suffix data_map_file cycle_time"
  echo "            Suffix is data source identifier that matches data in "
  echo "              the $TANKDIR/stats directory."
  echo "            The data_map_file is the full path to the data_map "
  echo "              file located in the top level parm directory."
  echo "            The full cycle_time (YYYYMMDDHH) that data was last"
  echo "              processed for this data source (suffix)."
}


nargs=$#
if [[ $nargs -ne 3 ]]; then
   usage
   exit 1
fi


suffix=$1
mapfile=$2
newdate=$3
outfile=./tmp_file

if [[ -s $outfile ]]; then 
  rm $outfile
fi
 
while read line; do 

   test_suffix=`echo $line | nawk '{print $1}'`

   if [[ $suffix == $test_suffix ]]; then
      prodate=`echo $line | nawk '{print $5}'`
      sed "/ $suffix /s/${prodate}/${newdate}/1" $mapfile > $outfile
      break
   fi

done < ${mapfile}

s=$(ls -la $outfile | awk '{ print $5}')
echo $s
if [[ $s -gt 0 ]]; then
   cp -f $outfile $mapfile 
#   rm $outfile
else
   echo "$suffix was not found in $mapfile, no update was made"
fi

exit
