#!/bin/sh

#
#  Check to determine if this maching is currently
#  the production machine.  
#
#     Return values:
#          1 = prod 
#          0 = dev 
#
   iamprod=0
   machine=`hostname | cut -c1`
   if [[ -e /etc/prod ]]; then
      prod=`cat /etc/prod | cut -c1`
   
      if [[ $machine = $prod ]]; then
         iamprod=1
      fi
   fi 

   echo $iamprod
   exit
