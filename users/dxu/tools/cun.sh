#!/bin/bash 
#======================================
# Purpose:
#    Save file with timestamp
# Author: Deyong Xu / RTi@JCSDA
# History:
#    12/16/2014, D. Xu / RTi@JCSDA , initial code.
#
#======================================

# Get all the files to be saved from command line
files=$@

# Save file via loop
for file in $files 
do 
   timestamp=`date +%F-%H-%M`
   fileNew=${file}_${timestamp}
   cp -p $file  $fileNew
done

exit

