#!/bin/sh

function usage {
  echo "Usage:  Transfer.sh suffix"
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

CMON_SUFFIX=$1
this_file=`basename $0`
this_dir=`dirname $0`

top_parm=${this_dir}/../../parm
export CMON_CONFIG=${CMON_CONFIG:-${top_parm}/CMon_config}
export CMON_USER_SETTINGS=${CMON_USER_SETTINGS:-${top_parm}/CMon_user_settings}

if [[ -s ${CMON_CONFIG} ]]; then
   . ${CMON_CONFIG}
else
   echo "ERROR:  Unable to source ${CMON_CONFIG}"
   exit
fi

if [[ -s ${CMON_USER_SETTINGS} ]]; then
   . ${CMON_USER_SETTINGS}
else
   echo "ERROR:  Unable to source ${CMON_USER_SETTINGS}"
   exit
fi


#--------------------------------------------------------------------
# Get the area (glb/rgn) for this suffix
#--------------------------------------------------------------------
#area=$RAD_AREA

#if [[ $area == "glb" ]]; then
#  . ${CMON_IMAGE_GEN}/parm/glbl_conf
#elif [[ $area == "rgn" ]]; then
#  . ${RADMON_IMAGE_GEN}/parm/rgnl_conf
#else
#  echo "ERROR:  Unable to determine area for ${CMON_SUFFIX}"
#  exit
#fi

#--------------------------------------------------------------------
#  Check for my monitoring use.  Abort if running on prod machine.
#--------------------------------------------------------------------

if [[ RUN_ONLY_ON_DEV -eq 1 ]]; then
   is_prod=`${C_IG_SCRIPTS}/onprod.sh`
   if [[ $is_prod = 1 ]]; then
      exit 10
   fi
fi

#--------------------------------------------------------------------

log_file=${LOGdir}/Transfer_${CMON_SUFFIX}.log
err_file=${LOGdir}/Transfer_${CMON_SUFFIX}.err

WEBDIR=${WEBDIR}/${CMON_SUFFIX}

if [[ ${C_IMGNDIR} != "/" ]]; then
   echo "C_IMGNDIR   = $C_IMGNDIR"
   echo "MY_MACHINE  = $MY_MACHINE"
   echo "WEBUSER     = $WEBUSER"
   echo "WEBSVR      = $WEBSVR"
   echo "WEBDIR      = $WEBDIR"
   
   if [[ $MY_MACHINE = "wcoss" || $MY_MACHINE = "cray" ]]; then
      /usr/bin/rsync -ave ssh --exclude *.ctl.${Z} ${C_IMGNDIR}/ \
         ${WEBUSER}@${WEBSVR}.ncep.noaa.gov:${WEBDIR}/
   fi
fi

echo end Transfer.sh
exit
