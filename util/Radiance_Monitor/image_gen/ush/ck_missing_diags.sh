#!/bin/sh
set -ax

#-------------------------------------------------------------------------------------
# ck_missing_diags.sh
#
# Check the extracted RadMon data files for a specified cycle and
# report any expected sat/instruments that are missing.  The $TANKdir/info/SATYPE.txt
# file will be used as the list of expected sat/instruments.
#
#-------------------------------------------------------------------------------------

function usage {
  echo "Usage:  ck_missing_diags.sh cycle TANKdir"
  echo "            cycle is in 10 digit format [YYYYMMDDCC]"
  echo "            TANKdir is the location of extracted RadMon files"
}


nargs=$#
if [[ $nargs -ne 2 ]]; then
   usage
   exit 1
fi

SCRIPT="ck_missing_diags.sh"
OUTFILE="missing_diags.txt"

echo "begin $SCRIPT"

pdate=$1
TANKdir=$2

pdy=`echo $pdate | cut -c1-8`

satype="$TANKdir/info/SATYPE.txt"
sat_list=`cat $satype`

if [[ $sat_list = "" ]]; then
   echo "$satype file not found in $SCRIPT"
   exit 2
else
   echo "$satype is good to go"

   #-------------------------------------
   #  Make list of extracted satypes
   #  using time.*.pdate.ieee_d* files
   ext_file_list=`ls -1 $TANKdir/radmon.$pdy/time.*.$pdate.ieee_d*`
   ext_list=""
 
   if [[ $ext_file_list != "" ]]; then

      for var in ${ext_file_list}; do
         #------------------------------------
         #  don't add satname_anl to the list 
         #
         test_anl=`echo $var | grep "_anl"`

         if [[ $test_anl = "" ]]; then
            trim=`basename $var | cut -d. -f2`
            ext_list="$ext_list $trim"
         fi

      done

      #------------------------------------------
      #  compare $sat_list to $ext_list and
      #  generate warnings for anything missing.
      #
      for var in ${sat_list}; do
         test=`echo ${ext_list} | grep ${var}`

         if [[ $test = "" ]]; then
            echo "$var" >> $OUTFILE
         fi

      done


   else 
      echo "WARNING:  unable to build ext_list in $SCRIPT "
      exit 3
   fi
      
fi

echo "end $SCRIPT"
exit
