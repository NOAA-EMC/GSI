#!/bin/bash 

###################################################
# Author  : Deyong Xu @ RTI ( deyong.xu@noaa.gov )
# Date    : Feb 14, 2014
# Purpose : Find files with filename pattern
# Usage   : $ ffile.sh  filename_pattern
# Example :  
#    # Find all files whose filename contain "abc"
#    $ ffile.sh  abc
#
###################################################

# Filename pattern is the 1st parameter. 
files=`find . -name "*$1*" `

# declare an integer as counter 
declare -i counter=1

if [ "$files" != "" ]
then
   for file in $files
   do 
      echo "File $counter is :  $file"
      ((counter++))
   done
fi
   
