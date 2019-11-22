#!/bin/sh
#------------------------------------------------------------------------
#  OznMon_Transfer.sh
#
#     Move all files for a given source to the web server.
#------------------------------------------------------------------------

function usage {
  echo " "
  echo "Usage:  OznMon_Transfer.sh OZNMON_SUFFIX -r|run [run value]"
  echo "            OZNMON_SUFFIX is data source identifier that matches data"
  echo "                 in the $TANKverf/stats directory."
  echo "            -r|--run [gdas|gfs] option to include the run value in file"
  echo "                 paths"
  echo " "
}

echo start OznMon_Transfer.sh
set -ax

nargs=$#

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
         #any unspecified key is OZNMON_SUFFIX
         export OZNMON_SUFFIX=$key
      ;;
   esac

   shift
done


if [[ $nargs -lt 1 ]]; then
   usage
   exit 1
fi

echo "OZNMON_SUFFIX, RUN = $OZNMON_SUFFIX, $RUN"


#--------------------------------------------------------------------
#  Check for my monitoring use.  Abort if running on prod machine.
#--------------------------------------------------------------------

if [[ $RUN_ONLY_ON_DEV -eq 1 ]]; then
   is_prod=`${OZN_IG_SCRIPTS}/onprod.sh`
   if [[ $is_prod = 1 ]]; then
      exit 10
   fi
fi


#--------------------------------------------------
# source verison, config, and user_settings files
#--------------------------------------------------
this_file=`basename $0`
this_dir=`dirname $0`

top_parm=${this_dir}/../../parm

oznmon_version_file=${oznmon_version:-${top_parm}/OznMon.ver}
if [[ -s ${oznmon_version_file} ]]; then
   . ${oznmon_version_file}
   echo "able to source ${oznmon_version_file}"
else
   echo "Unable to source ${oznmon_version_file} file"
   exit 2
fi

oznmon_user_settings=${oznmon_user_settings:-${top_parm}/OznMon_user_settings}
if [[ -s ${oznmon_user_settings} ]]; then
   . ${oznmon_user_settings}
   echo "able to source ${oznmon_user_settings}"
else
   echo "Unable to source ${oznmon_user_settings} file"
   exit 4
fi

oznmon_config=${oznmon_config:-${top_parm}/OznMon_config}
if [[ -s ${oznmon_config} ]]; then
   . ${oznmon_config}
   echo "able to source ${oznmon_config}"
else
   echo "Unable to source ${oznmon_config} file"
   exit 3
fi



job=${OZNMON_SUFFIX}_ozn_transfer

logf=${OZN_LOGdir}/TF.log
if [[ -e $logf ]]; then
   rm -f $logf
fi

errf=${OZN_LOGdir}/TF.err
if [[ -e $errf ]]; then
   rm -f $errf
fi

transfer_script=${OZN_IG_SCRIPTS}/transfer.sh
job=${OZNMON_SUFFIX}_ozn_transfer

if [[ $MY_MACHINE = "wcoss" || $MY_MACHINE = "wcoss_d" ]]; then

   job_queue="transfer"
   if [[ $MY_MACHINE = "wcoss_d" ]]; then
      job_queue="dev_transfer"
   fi

   echo "PROJECT = $PROJECT"
   echo "logf    = $logf"
   echo "errf    = $errf"
   echo "transfer_script = $transfer_script"

   $SUB -P $PROJECT -q $job_queue -o ${logf} -e ${errf} -M 50 -W 0:20 \
        -R affinity[core] -J ${job} -cwd ${OZN_IG_SCRIPTS} \
        ${transfer_script} 
   
elif [[ $MY_MACHINE = "cray" ]]; then

   echo ""
#   ${OZN_IG_SCRIPTS}/transfer.sh ${OZNMON_SUFFIX} \
#     1>${logf} 2> ${errf}

   ${OZN_IG_SCRIPTS}/transfer.sh 1>${logf} 2> ${errf}

#    $SUB -q ${JOB_QUEUE} -P ${PROJECT} -o ${logf} -e ${errf} \
#            -R "select[mem>100] rusage[mem=100]" \
#            -M 100 -W 0:05 -J ${job} -cwd ${OZN_IG_SCRIPTS} ${transfer_script}

fi

exit
