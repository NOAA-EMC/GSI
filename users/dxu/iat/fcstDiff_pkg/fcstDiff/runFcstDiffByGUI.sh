#!/bin/bash
#====================================================
# Purpose:
#    Used by IAT GUI to run fcstDiff
# Author: Deyong Xu / RTi@JCSDA
# History:
#    2/3/2015, D. Xu / RTi@JCSDA , initial code.
#
#====================================================

source fcstDiff_gui.config

# Remove old log file
if [ -e out.log ] 
then
    rm -rf out.log 
fi

# Run FcstDiff package
./run_template.sh  > out.log 2>>out.log
