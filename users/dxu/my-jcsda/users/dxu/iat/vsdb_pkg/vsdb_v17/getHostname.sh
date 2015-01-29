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
# it returns: badger cardinal  fe    jibb   s4-gateway
#               |      |        |     |
#             badger cardinal  zeus  jibb   s4-gateway
hnStr=`hostname`

# Define an array of hosts 
declare -a hostArr
declare -a hostArrRetnd
hostArr=('badger' 'cardinal' 'fe' 'jibb' 's4')
hostArrRetnd=('badger' 'cardinal' 'zeus' 'jibb' 's4-gateway')
SIZE=${#hostArr[@]}   # array size

# Define two const strings 
emptyStr=""
notSupportStr="NOT_SUPPORTED_HOST"

# Initialize to ${emptyStr}
searchStr="${emptyStr}"

# Seach through host array to find a match
for (( i=0; i<$SIZE; i++)); do
   
   searchStr=` echo $hnStr  |grep -e "${hostArr[${i}]}" `

   if [ "${searchStr}" != "${emptyStr}" ]
   then
      # Once a match is found, return and exit.
      echo "${hostArrRetnd[${i}]}"
      exit
   fi
done 

# Exit when there is no match.
echo ${notSupportStr}
exit

