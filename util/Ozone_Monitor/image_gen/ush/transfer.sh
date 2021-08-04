#!/bin/bash

echo "start transfer.sh"

set -ax

echo "OZN_IMGN_TANKDIR = ${OZN_IMGN_TANKDIR}"
echo "OZNMON_SUFFIX = ${OZNMON_SUFFIX}  "
echo "WEB_SVR       = $WEB_SVR"
echo "WEB_USER      = $WEB_USER"
echo "WEB_DIR       = $WEB_DIR"
echo "RSYNCH        = $RSYNC"

if [[ ${OZN_IMGN_TANKDIR} != "/" ]]; then			# sanity check 

   if [[ $MY_MACHINE = "wcoss_c" || $MY_MACHINE = "wcoss_d" ]]; then

      WEB_DIR=${WEB_DIR}/${OZNMON_SUFFIX}/${RUN}
      ssh ${WEB_USER}@${WEB_SVR}.ncep.noaa.gov "mkdir -p ${WEB_DIR}"

      #----------------------------------------------------------------
      #  use rsync to perform the file transfer
      #
      $RSYNC -ave ssh --delete-during ${OZN_IMGN_TANKDIR}/ \
         ${WEB_USER}@${WEB_SVR}.ncep.noaa.gov:${WEB_DIR}/
   fi

fi

echo "end transfer.sh"
exit
