#!/bin/sh

function usage {
  echo "Usage:  Transfer.sh [-n|--nosrc] suffix"
  echo "            Suffix is data source identifier that matches data in "
  echo "              the $MY_TANKDIR/stats directory."
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

set -ax


if [[ $SOURCE_PARMS -eq 1 ]]; then
   this_dir=`dirname $0`
   top_parm=${this_dir}/../../parm

   radmon_config=${radmon_config:-${top_parm}/RadMon_config}
   if [[ ! -e ${radmon_config} ]]; then
      echo "Unable to locate ${radmon_config} file"
      exit 3
   fi

   . ${radmon_config}
   if [[ $? -ne 0 ]]; then
      echo "Error detected while sourcing ${radmon_config} file"
      exit $?
   fi

   radmon_user_settings=${radmon_user_settings:-${top_parm}/RadMon_user_settings}
   if [[ ! -e ${radmon_user_settings} ]]; then
      echo "Unable to locate ${radmon_user_settings} file"
      exit 4
   fi

   . ${radmon_user_settings}
   if [[ $? -ne 0 ]]; then
      echo "Error detected while sourcing ${radmon_user_settings} file"
      exit $?
   fi

fi


#--------------------------------------------------------------------

log_file=${LOGdir}/Transfer_${RADMON_SUFFIX}.log
err_file=${LOGdir}/Transfer_${RADMON_SUFFIX}.err

echo "IMGNDIR = ${IMGNDIR}"
echo "WEBDIR  = ${WEBDIR}"

if [[ ${IMGNDIR} != "/" ]]; then
   if [[ $MY_MACHINE = "wcoss_d" || $MY_MACHINE = "wcoss_c" || $MY_MACHINE = "wcoss2" ]]; then
      /usr/bin/rsync -ave ssh --exclude *.ctl.${Z} \
         --exclude 'horiz' --exclude *.png --delete-during ${IMGNDIR}/ \
         ${WEB_USER}@${WEB_SVR}.ncep.noaa.gov:${WEBDIR}/
   fi
fi

echo end Transfer.sh
exit
