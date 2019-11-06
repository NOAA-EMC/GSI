#!/bin/bash

echo "start transfer.sh"

set -ax

#  Check to determine if this maching is currently
#  the production machine.  
#
#     Return values:
#          1 = prod 
#          0 = dev 
#
#   iamprod=0
#   machine=`hostname | cut -c1`
#   if [[ -e /etc/prod ]]; then
#      prod=`cat /etc/prod | cut -c1`
#   
#      if [[ $machine = $prod ]]; then
#         iamprod=1
#      fi
#   fi 

#   echo $iamprod

   echo "OZN_IMGN_TANKDIR = ${OZN_IMGN_TANKDIR}"
   echo "OZNMON_SUFFIX = ${OZNMON_SUFFIX}  "
   echo "WEB_SVR       = $WEB_SVR"
   echo "WEB_USER      = $WEB_USER"
   echo "WEB_DIR       = $WEB_DIR"
   echo "RSYNCH        = $RSYNC"

   if [[ ${OZN_IMGN_TANKDIR} != "/" ]]; then			# sanity check 

      if [[ $MY_MACHINE = "wcoss" || $MY_MACHINE = "wcoss_d" || \
            $MY_MACHINE = "cray" ]]; then

         #----------------------------------------------------------------
         #  expand WEB_DIR to include the suffix and conditionally the RUN 
         #
         if [[ $OZN_USE_RUN -eq 1 ]]; then
            export WEB_DIR=${WEB_DIR}/${OZNMON_SUFFIX}/${RUN}
         else
            export WEB_DIR=${WEB_DIR}/${OZNMON_SUFFIX}
         fi

	 ssh ${WEB_USER}@${WEB_SVR}.ncep.noaa.gov "mkdir -p ${WEB_DIR}"

         #----------------------------------------------------------------
	 #  use rsync to perform the file transfer
         #
         $RSYNC -ave ssh ${OZN_IMGN_TANKDIR}/ \
            ${WEB_USER}@${WEB_SVR}.ncep.noaa.gov:${WEB_DIR}/
      fi

   fi

   echo "end transfer.sh"
   exit
