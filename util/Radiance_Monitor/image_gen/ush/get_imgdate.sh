#!/bin/sh
set -ax

#
# get_imgdate.sh
#
# Given a suffix and the data_map file, return the time image files
# were last built for this source.  
#

function usage {
  echo "Usage:  get_imgdate.sh suffix data_map_file"
  echo "            Suffix is data source identifier that matches data in "
  echo "              the $TANKDIR/stats directory."
  echo "            The data_map_file is the full path to the data_map "
  echo "              file located in the top level parm directory."
}


nargs=$#
if [[ $nargs -ne 2 ]]; then
   usage
   exit 1
fi


suffix=$1
file=$2

while read line; do 

   test_suffix=`echo $line | nawk '{print $1}'`
   if [[ $suffix == $test_suffix ]]; then
      imgdate=`echo $line | nawk '{print $6}'`
      break
   fi

done < ${file}

echo $imgdate

exit
