#!/bin/sh

#
#  Check to determine if this maching is currently
#  the production machine.  
#
#     Return values:
#          1 = prod 
#          0 = dev 
#
   machine=`hostname | cut -c1`
   prod=`cat /etc/prod | cut -c1`
   iamprod=0
   
   if [[ $machine = $prod ]]; then
      iamprod=1
   fi

   echo $iamprod
   exit
