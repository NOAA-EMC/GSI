#!/bin/bash 

###########################################################
# Author  : Deyong Xu @ RTI ( deyong.xu@noaa.gov )
# Date    : Feb 14, 2014
# Purpose : Find all the files that contain line pattern
#           Specify file_pattern is to speed up searching.
# Usage   : $ fline.sh  line_pattern
#           $ fline.sh  line_pattern   file_pattern  
# Example :  
#    # Find all files whose filename contain "abc"
#    $ fline.sh  abc
#    $ fline.sh  abc  f90
#
###########################################################

# Get line pattern
line_pattern=$1
# Get file pattern
file_pattern=${2:-""}

# List all the files from current directory down
# recursively to nested sub-directories. 
if [ "$file_pattern" != "" ]
then 
   files=`find . -type f -name "*$file_pattern*" |grep -v svn |xargs echo  `
else 
   files=`find . -type f -name "*"               |grep -v svn |xargs echo  `
fi 

# declare an integer as counter 
declare -i counter=1

if [ "$files" != "" ]
then
   for file in $files
   do 
      # Make sure it's ascii file 
      msg=`file $file |grep -e "text\|source"`
      if [ "$msg" != "" ]
      then 
         grapLine=`grep -ie "$line_pattern"  $file`
         if [ "$grapLine" != "" ]
         then
            echo "File $counter is :  $file"
            # Cut down the line if too long 
            echo "     $grapLine " |cut -c1-80
            ((counter++))
         fi
      fi
   done
fi
   
