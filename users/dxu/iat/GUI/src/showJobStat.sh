#!/bin/bash 
#====================================================
# Purpose:
#    Used by GUI to show job status
# Author: Deyong Xu / RTi@JCSDA
# History:
#    12/26/2014, D. Xu / RTi@JCSDA , initial code.
#
#====================================================

# Get hostname
# it returns: cardinal  fe    jibb  
#               |      |        |     |
#             cardinal  zeus  jibb 
hnStr=`hostname`

# Define an array of hosts 
declare -a hostArr
declare -a hostArrRetnd
hostArr=('cardinal' 'fe' 'jibb' )
jobStatCmdArr=('squeue ' 'qstat ' 'squeue')
SIZE=${#hostArr[@]}   # array size

# Define two const strings 
emptyStr=""
notSupportStr="Host is NOT supported."

# Initialize to ${emptyStr}
searchStr="${emptyStr}"

# Seach through host array to find a match
for (( i=0; i<$SIZE; i++)); do
   
   searchStr=` echo $hnStr  |grep -e "${hostArr[${i}]}" `

   if [ "${searchStr}" != "${emptyStr}" ]
   then
      # Once a match is found, return and exit.
      echo ${jobStatCmdArr[${i}]} -u $LOGNAME
      ${jobStatCmdArr[${i}]} -u $LOGNAME
      exit
   fi
done 

# Exit when there is no match.
echo ${notSupportStr}
exit

