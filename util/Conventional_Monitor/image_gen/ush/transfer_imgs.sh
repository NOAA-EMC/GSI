#!/bin/sh

echo begin transfer_imgs.sh

if [[ ${C_IMGNDIR} != "/" ]]; then
   echo "C_IMGNDIR   = $C_IMGNDIR"
   echo "MY_MACHINE  = $MY_MACHINE"
   echo "WEBUSER     = $WEBUSER"
   echo "WEBSVR      = $WEBSVR"
   echo "WEBDIR      = $WEBDIR"
   
   if [[ $MY_MACHINE = "wcoss_d" || $MY_MACHINE = "cray" ]]; then
      /usr/bin/rsync -ave ssh --exclude *.ctl.${Z} --delete-during ${C_IMGNDIR}/ \
         ${WEBUSER}@${WEBSVR}.ncep.noaa.gov:${WEBDIR}/
   fi
fi

echo end transfer_imgs.sh
exit
