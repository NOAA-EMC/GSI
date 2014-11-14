#!/bin/bash 
# Date: 7/30/2014
# Deyong Xu / RTi @ NOAA/JCSDA
# Purpose: change 1 to 01 at end of filename
#  eg: change file "example1" to "example01"
#      so it can be sorted correctly.
# How to run:
#   ./addZero.sh  example
# 

cur_dir=`pwd`
dirs=`ls` 

# $1= example
# Files are: example[1-9]
filename=$1

for dir in $dirs
do
   # Test if it's directory
   if [ -d $dir ]
   then 
      cd $dir
      # Find all the files, eg: example[1-0]
      files=`ls $filename[1-9]`
      for file in $files 
      do
         # 0-based 
         last_digit=${file:7}
         echo mv $file example0$last_digit
      done
   fi

   # Go back to parent dir
   cd $cur_dir
done
