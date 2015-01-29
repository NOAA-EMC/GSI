#!/bin/bash 
##################################################
# Purpose:  
#    Make all letters upper case
# Date: July 7, 2014
# Author:  Deyong Xu, RTI@JCSDA
# Usage:  
#     $ upper.sh   file_1
##################################################

cat $1 | tr '[:lower:]' '[:upper:]' > ~/tmp_$$ 
mv ~/tmp_$$  $1
