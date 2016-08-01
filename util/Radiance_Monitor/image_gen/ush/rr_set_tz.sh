#!/bin/sh

#####################################################################################  
#  rr_set_tz.sh 
#
#    Takes a cycle hour as input and sets (exports) the corresponding 
#    rgnHH and rgnTM values for that hour, as used by radpid refresh models (namrr).
#####################################################################################  

function usage {
  echo "Usage:  rr_get_tz.sh hh "
  echo "            hh is the 2 digit hour for a cycle in range 00 to 23"
}

nargs=$#
if [[ $nargs -ne 1 ]]; then
   usage
   exit 1
fi

HH00=$1
echo "rr_set_tz -- HH00 = $HH00"

case $HH00 in
   00) export rgnHH=t00z
       export rgnTM=tm00;;
   01) export rgnHH=t06z
       export rgnTM=tm05;;
   02) export rgnHH=t06z
       export rgnTM=tm04;;
   03) export rgnHH=t06z
       export rgnTM=tm03;;
   04) export rgnHH=t06z
       export rgnTM=tm02;;
   05) export rgnHH=t06z
       export rgnTM=tm01;;
   06) export rgnHH=t06z
       export rgnTM=tm00;;
   07) export rgnHH=t12z
       export rgnTM=tm05;;
   08) export rgnHH=t12z
       export rgnTM=tm04;;
   09) export rgnHH=t12z
       export rgnTM=tm03;;
   10) export rgnHH=t12z
       export rgnTM=tm02;;
   11) export rgnHH=t12z
       export rgnTM=tm01;;
   12) export rgnHH=t12z
       export rgnTM=tm00;;
   13) export rgnHH=t18z
       export rgnTM=tm05;;
   14) export rgnHH=t18z
       export rgnTM=tm04;;
   15) export rgnHH=t18z
       export rgnTM=tm03;;
   16) export rgnHH=t18z
       export rgnTM=tm02;;
   17) export rgnHH=t18z
       export rgnTM=tm01;;
   18) export rgnHH=t18z       		# day directory changes here
       export rgnTM=tm00;;
   19) export rgnHH=t00z
       export rgnTM=tm05;;
   20) export rgnHH=t00z
       export rgnTM=tm04;;
   21) export rgnHH=t00z
       export rgnTM=tm03;;
   22) export rgnHH=t00z
       export rgnTM=tm02;;
   23) export rgnHH=t00z
       export rgnTM=tm01;;
esac

