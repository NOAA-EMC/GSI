#!/bin/sh

#--------------------------------------------------------------------
#  usage
#--------------------------------------------------------------------
function usage {
  echo "Usage:  ConMon_IG.sh suffix [-r|--run gdas|gfs]"
  echo "            Suffix is the indentifier for this data source."
  echo "            -r | --run   the gdas|gfs run to be processed"
  echo "              use only if data in TANKdir stores both runs, otherwise"
  echo "              gdas is assumed."
  echo " "
}


set -ax
echo start Transfer.sh

nargs=$#
if [[ $nargs -lt 1 || $nargs -gt 3 ]]; then
   usage
   exit 1
fi

#-----------------------------------------------
#  Process command line arguments
#
export RUN=gdas

while [[ $# -ge 1 ]]
do
   key="$1"
   echo $key

   case $key in
      -r|--run)
         export RUN="$2"
         shift # past argument
      ;;
      *)
         #any unspecified key is CONMON_SUFFIX
         export CONMON_SUFFIX=$key
      ;;
   esac

   shift
done


CMON_SUFFIX=$CONMON_SUFFIX
this_file=`basename $0`
this_dir=`dirname $0`

top_parm=${this_dir}/../../parm
export CMON_CONFIG=${CMON_CONFIG:-${top_parm}/ConMon_config}
export CMON_USER_SETTINGS=${CMON_USER_SETTINGS:-${top_parm}/ConMon_user_settings}

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
#
log_file=${LOGdir}/Transfer_${CMON_SUFFIX}.log
err_file=${LOGdir}/Transfer_${CMON_SUFFIX}.err

WEBDIR=${WEBDIR}/${CMON_SUFFIX}/${RUN}

if [[ ${C_IMGNDIR} != "/" ]]; then
   echo "C_IMGNDIR   = $C_IMGNDIR"
   echo "MY_MACHINE  = $MY_MACHINE"
   echo "WEBUSER     = $WEBUSER"
   echo "WEBSVR      = $WEBSVR"
   echo "WEBDIR      = $WEBDIR"
   
   if [[ $MY_MACHINE = "wcoss" || $MY_MACHINE = "wcoss_d" || $MY_MACHINE = "cray" ]]; then
      /usr/bin/rsync -ave ssh --exclude *.ctl.${Z} ${C_IMGNDIR}/ \
         ${WEBUSER}@${WEBSVR}.ncep.noaa.gov:${WEBDIR}/
   fi
fi

echo end Transfer.sh
exit
