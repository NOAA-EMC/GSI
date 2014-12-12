#!/bin/bash 
########################################################
# Date: 12/12/2014
# Author: Deyong Xu / RTi @ NOAA/NESDIS/STAR/JCSDA
# Purpose : Create run dir with step name to easily
#      identify run dir for different steps in vsdb.
########################################################
set -ax

stepName=$1
export tmpdir=$STMP/$LOGNAME/nwpvrfy$$_${stepName}
mkdir -p $tmpdir ||exit
cd $tmpdir ||exit
rm *.out

