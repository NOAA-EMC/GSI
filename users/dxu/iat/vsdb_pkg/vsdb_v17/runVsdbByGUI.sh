#!/bin/bash
#====================================================
# Purpose:
#    Used by GUI to run Vsdb
# Author: Deyong Xu / RTi@JCSDA
# History:
#    2/24/2015, D. Xu / RTi@JCSDA , initial code.
#
#====================================================

# Remove old log file
if [ -e out.log ]
then
    rm -rf out.log
fi

./vsdbjob_submit_template.sh   > out.log 2>> out.log


