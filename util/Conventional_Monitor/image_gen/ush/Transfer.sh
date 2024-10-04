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
mybin=`ls ~/bin`
echo "test: $mybin"

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


logfile=${C_LOGDIR}/transfer_${CMON_SUFFIX}.log

export JOB_QUEUE=dev_transfer
WEBDIR=${WEBDIR}/${CMON_SUFFIX}/${RUN}

export jobname=transfer_${CMON_SUFFIX}_conmon

#--------------------------------------------------------
#  Note that transfers from hera are not straightforward,
#  and must go through a system that is allowed to access
#  emcrzdm.  This script will just report that situation
#  and leave it to the user to manually transfer files to
#  the server.
#
if [[ $MY_MACHINE == "wcoss_d" || $MY_MACHINE == "wcoss_c" ]]; then
   $SUB -P $PROJECT -q $JOB_QUEUE -o ${logfile} -M 80 -W 1:30 \
        -R affinity[core] -J ${jobname} -cwd ${PWD} \
        ${C_IG_SCRIPTS}/transfer_imgs.sh
else
   echo "Unable to transfer files from $MY_MACHINE to $WEBSVR."
   echo "Manual intervention is required."
fi


echo end Transfer.sh
exit
