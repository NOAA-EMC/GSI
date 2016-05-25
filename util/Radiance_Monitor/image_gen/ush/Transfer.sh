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
export RADMON_CONFIG=${RADMON_CONFIG:-${top_parm}/RadMon_config}

if [[ -s ${RADMON_CONFIG} ]]; then
   . ${RADMON_CONFIG}
else
   echo "ERROR:  Unable to source ${RADMON_CONFIG}"
   exit
fi

if [[ -s ${RADMON_USER_SETTINGS} ]]; then
   . ${RADMON_USER_SETTINGS}
else
   echo "ERROR:  Unable to source ${RADMON_USER_SETTINGS}"
   exit
fi

. ${IG_PARM}/plot_rad_conf


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

#--------------------------------------------------------------------
#  Check for my monitoring use.  Abort if running on prod machine.
#--------------------------------------------------------------------

if [[ RUN_ONLY_ON_DEV -eq 1 ]]; then
   is_prod=`${IG_SCRIPTS}/onprod.sh`
   if [[ $is_prod = 1 ]]; then
      exit 10
   fi
fi

#--------------------------------------------------------------------

log_file=${LOGdir}/Transfer_${SUFFIX}.log
err_file=${LOGdir}/Transfer_${SUFFIX}.err

if [[ ${TOP_IMGNDIR} != "/" ]]; then
   if [[ $MY_MACHINE = "wcoss" || $MY_MACHINE = "cray" ]]; then
      /usr/bin/rsync -ave ssh --exclude *.ctl.${Z} --exclude 'horiz' ${TOP_IMGNDIR}/ \
         ${WEB_USER}@${WEB_SVR}.ncep.noaa.gov:${WEBDIR}/
   fi
fi

echo end Transfer.sh
exit
