#!/bin/sh

function usage {
  echo "Usage:  Transfer.sh [-n|--nosrc] suffix"
  echo "            Suffix is data source identifier that matches data in "
  echo "              the $TANKDIR/stats directory."
  echo "	-n|--nosrc   Do not source the radmon_package/parm/* files."
  echo "		This should be used if the parm files have already"
  echo "		been sourced by the calling script."
  echo "        -a|--area  Specifies the geographic area.  Valid entries are"
  echo "		'glb' or 'rgn'."
  echo "        -r|--run   Specifies the RUN value (typically 'gdas' or 'gfs')"
}

echo start Transfer.sh

nargs=$#
if [[ $nargs -le 1 || $nargs -gt 6 ]]; then
   usage
   exit 1
fi

#---------------------------------------
#  set default values, parse arguments
#---------------------------------------
SOURCE_PARMS=1
AREA=glb
RUN=""

while [[ $# -ge 1 ]]
do
   key="$1"
   echo $key

   case $key in
      -n|--nosrc)
         SOURCE_PARMS=0
      ;;
      -a|--area)
         AREA=$2
         shift # past argument
      ;;
      -r|--run)
         RUN=$2
         TANK_USE_RUN=1
         shift # past argument
      ;;
      *)
         #any unspecified key is RADMON_SUFFIX
         export RADMON_SUFFIX=$key
      ;;
   esac

   shift
done

echo "SOURCE_PARMS  = $SOURCE_PARMS"
echo "AREA          = $AREA"
echo "RADMON_SUFFIX = $RADMON_SUFFIX"
echo "RUN           = $RUN"
echo "TANK_USE_RUN  = $TANK_USE_RUN"

set -ax


if [[ $SOURCE_PARMS -eq 1 ]]; then
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

   if [[ $AREA == "glb" ]]; then
     . ${RADMON_IMAGE_GEN}/parm/glbl_conf
   elif [[ $AREA == "rgn" ]]; then
     . ${RADMON_IMAGE_GEN}/parm/rgnl_conf
   else
     echo "ERROR:  Unable to determine AREA for ${RADMON_SUFFIX}"
     exit
   fi

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

log_file=${LOGdir}/Transfer_${RADMON_SUFFIX}.log
err_file=${LOGdir}/Transfer_${RADMON_SUFFIX}.err

if [[ ${IMGNDIR} != "/" ]]; then
   if [[ $MY_MACHINE = "wcoss" || $MY_MACHINE = "wcoss_d" || \
	 $MY_MACHINE = "cray" ]]; then
      /usr/bin/rsync -ave ssh --exclude *.ctl.${Z} \
         --exclude 'horiz' --exclude *.png ${IMGNDIR}/ \
         ${WEB_USER}@${WEB_SVR}.ncep.noaa.gov:${WEBDIR}/
   fi
fi

echo end Transfer.sh
exit
