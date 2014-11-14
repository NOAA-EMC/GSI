#!/bin/bash 
##################################################
# Purpose:  
#    Make all letters lower case
# Date: July 7, 2014
# Author:  Deyong Xu, RTI@JCSDA
# Usage:  
#     $ lower.sh   file_1
##################################################

cat $1 | tr '[:upper:]' '[:lower:]' > ~/tmp_$$ 
mv ~/tmp_$$  $1
