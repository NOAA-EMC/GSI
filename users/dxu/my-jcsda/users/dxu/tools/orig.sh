#!/bin/bash 

###################################################
# Author  : Deyong Xu @ RTI ( deyong.xu@noaa.gov )
# Date    : Feb 14, 2014
# Purpose : save file with suffix "_orig"
# Usage   : $ orig.sh  abc
# Example 1 :  
#    $ orig.sh  abc
# Result: abc_orig is created 
# Example 2 :  
#    $ orig.sh  abc  def
# Result: abc_orig and def_orig are created 
# Example 3 :  
#    $ orig.sh  *java
# Result: all java files are saved with suffix "_orig"
#
###################################################

files=`ls $@`

for file in $files 
do 
  fileOut=${file}_orig 
  cp $file $fileOut
done
