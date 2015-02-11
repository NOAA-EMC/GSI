#!/bin/bash 
#======================================
# Purpose:
#    Return hostname 
# Author: Deyong Xu / RTi@JCSDA
# History:
#    10/3/2014, D. Xu / RTi@JCSDA , initial code.
#
#======================================

# Get hostname
hnStr=`hostname`

# Define an array of hosts 
declare -a hnSchArr
declare -a hnArr
hnSchArr=('badger' 'cardinal' 'fe' )
hnArr=('badger' 'cardinal' 'zeus' )
SIZE=${#hnSchArr[@]}   # array size

# Define two const strings 
emptyStr=""
notSupportStr="NOT_SUPPORTED_HOST"

# Initialize to ${emptyStr}
searchStr="${emptyStr}"

# Seach through host array to find a match
for (( i=0; i<$SIZE; i++)); do
   
   searchStr=` echo $hnStr  |grep -e "${hnSchArr[${i}]}" `

   if [ "${searchStr}" != "${emptyStr}" ]
   then
      # Once a match is found, return and exit.
      echo "${hnArr[${i}]}"
      exit
   fi
done 

# Exit when there is no match.
echo ${notSupportStr}
exit

