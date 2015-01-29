#!/bin/bash 
##################################################
# Purpose:  
#    Find which f90 file is the main GSI code.
# Date: June 9, 2014
# Author:  Deyong Xu, RTI@JCSDA
# Usage:  
#     $ findGSI_main.sh   
# How to check: 
#     $ vi def    # def is log file
# Note: There should be only a few files left.
#       Manually examine files to find main code.
##################################################

files=`ls *.f90 *.F90  `

# Delete file def if existing already
if [ -f def ] 
then 
   rm def
fi


let count=0
for file in $files
do 
   # Get the first line of the file
   line=` head -1 $file `

   # Find match of "module"/"Module"/... 
   len=`expr "$line" : ".*[mM][oO][dD][uU]"`
   if [ $len -eq 0 ]      # didn't find "module"
   then
      # Find match of "subr"/"Subr"/... 
      len=`expr "$line" : ".*[sS][uU][bB][rR]"`
      if [ $len -eq 0 ]      # didn't find "subr"
      then
         # Find match of "func"/"Func"/... 
         len=`expr "$line" : ".*[fF][uU][nN][cC]"`
         if [ $len -eq 0 ]    # dind't find "func"
         then
            echo $file >> def 
            ((count++))
         fi
      fi
   fi
done


echo "$count files are not subroutine/function/module"
