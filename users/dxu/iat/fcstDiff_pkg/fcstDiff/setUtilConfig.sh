#!/bin/bash
#======================================
# Purpose:
#    Set up utlitity location based on
#    host
# Author: Deyong Xu / RTi@JCSDA
# History:
#    2/11/2015, D. Xu / RTi@JCSDA , initial code.
#
#======================================
set -a 

hnStr=`source getHostname.sh`

# Define two const strings
notSupportStr="NOT_SUPPORTED_HOST"

if [ "${hnStr}" = "${notSupportStr}" ]
then 
   echo "Host `hostname` is not supported!!!"
   echo "Modify getHostname.sh to add this host."
   exit
else
   # Load config file for the host
   source  utilConfig.${hnStr}
fi





