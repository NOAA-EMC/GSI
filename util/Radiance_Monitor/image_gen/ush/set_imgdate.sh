#!/bin/sh
set -ax

#
# set_imgdate.sh
#
# Replace the imgtime in the data_map file for the given suffix.  If the 
# suffix is not found, take no action.
#

function usage {
  echo "Usage:  set_imgdate.sh suffix data_map_file cycle_time"
  echo "            Suffix is data source identifier that matches data in "
  echo "              the $TANKDIR/stats directory."
  echo "            The data_map_file is the full path to the data_map "
  echo "              file located in the top level parm directory."
  echo "            The full cycle_time (YYYYMMDDHH) that image files were"
  echo "              created for this data source."
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
      imgdate=`echo $line | nawk '{print $6}'`
      prodate=`echo $line | nawk '{print $5}'`

      # This checks for imgdate and prodate being the same value.  If so
      # then tell sed to change the 2nd value (the imgdate, not the prodate).
      #
      if [[ $imgdate != $prodate ]]; then
         sed "/ $suffix /s/${imgdate}/${newdate}/" $mapfile > $outfile
      else
         sed "/ $suffix /s/${imgdate}/${newdate}/2" $mapfile > $outfile
      fi
      break
   fi

done < ${mapfile}

s=$(ls -la $outfile | awk '{ print $5}')
echo $s
if [[ $s -gt 0 ]]; then
   cp -f $outfile $mapfile 
   rm $outfile
else
   echo "$suffix was not found in $mapfile, no update was made"
fi

exit
