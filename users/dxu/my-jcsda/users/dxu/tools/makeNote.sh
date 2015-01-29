#!/bin/bash 
####################################################
# Author  : Deyong Xu @ RTI ( deyong.xu@noaa.gov )
# Date    : May 19, 2014
# Purpose : Quickly create a note with a name 
#           made of command line parameters and date
# Usage   : $ ./makeNote.sh  info1 info2 etc.
# Example :
#    $ ./makeNote.sh  fwd running status
#    It creates file "fwd_running_status_2014_05_19"
#
###################################################

today=`date +%Y_%m_%d`
newName=""
for name in $@
do 
   newName=${newName}${name}_
   echo $newName
done

newName=${newName}$today

vi ${newName}
