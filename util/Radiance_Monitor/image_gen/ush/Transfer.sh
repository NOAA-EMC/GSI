#!/bin/sh

function usage {
  echo "Usage:  Transfer.sh suffix"
  echo "            File name for Transfer.sh may be full or relative path"
  echo "            Suffix is data source identifier that matches data in "
  echo "              the $TANKDIR/stats directory."
}

set -ax
echo start Transfer.sh

nargs=$#
if [[ $nargs -ne 1 ]]; then
   usage
   exit 1
fi

SUFFIX=$1
this_file=`basename $0`
this_dir=`dirname $0`

top_parm=${this_dir}/../../parm

if [[ -s ${top_parm}/RadMon_config ]]; then
   . ${top_parm}/RadMon_config
else
   echo "ERROR:  Unable to source ${top_parm}/RadMon_config"
   exit
fi

if [[ -s ${top_parm}/RadMon_user_settings ]]; then
   . ${top_parm}/RadMon_user_settings
else
   echo "ERROR:  Unable to source ${top_parm}/RadMon_user_settings"
   exit
fi

. ${RADMON_IMAGE_GEN}/parm/plot_rad_conf

#--------------------------------------------------------------------
# Get the area (glb/rgn) for this suffix
#--------------------------------------------------------------------
area=$RAD_AREA

if [[ $area == "glb" ]]; then
  . ${RADMON_IMAGE_GEN}/parm/glbl_conf
elif [[ $area == "rgn" ]]; then
  . ${RADMON_IMAGE_GEN}/parm/rgnl_conf
else
  echo "ERROR:  Unable to determine area for ${SUFFIX}"
  exit
fi

log_file=${LOGSverf_rad}/Transfer_${SUFFIX}.log
err_file=${LOGSverf_rad}/Transfer_${SUFFIX}.err

if [[ ${TOP_IMGNDIR} != "/" ]]; then
   if [[ $MY_MACHINE = "ccs" ]]; then
      /usrx/local/bin/rsync -ave ssh --exclude *.ctl*  ${TOP_IMGNDIR}/ \
         ${WEB_USER}@${WEB_SVR}.ncep.noaa.gov:${WEBDIR}/
   elif [[ $MY_MACHINE = "wcoss" ]]; then
      /usr/bin/rsync -ave ssh --exclude *.ctl*  ${TOP_IMGNDIR}/ \
         ${WEB_USER}@${WEB_SVR}.ncep.noaa.gov:${WEBDIR}/
   fi
fi

echo end Transfer.sh
exit
