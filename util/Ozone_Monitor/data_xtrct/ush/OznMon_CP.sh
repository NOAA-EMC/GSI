#!/bin/bash

#--------------------------------------------------------------------
#  OznMon_CP.sh
#
#    This script searches for new oznmon output from the global GDAS
#    and copies those filess to the user's $TANKDIR directory under 
#    the specified suffix argument. 
#
#    The bad_penalty, low count, and missing diag reports are 
#    reevaluated using local copies of the base file and satype
#    files in the $TANKdir/$suffix/info directory. 
#    
#    Note that processing occurs within TANKdir, not in stmp space.
#
#    The unified error report is journaled to warning.${PDY}${CYC}.
#
#--------------------------------------------------------------------

function usage {
  echo "Usage:  OznMon_CP_glb.sh suffix [-r|--run gdas|gfs -p|--pdate yyyymmddhh"
  echo ""
  echo "            Suffix (NET) is the indentifier for this data source."
  echo ""
  echo "            -r|--run is the run value, typically gdas or gfs.  Default value is gdas." 
  echo ""
  echo "            -p|--pdate is 10 digit yyyymmddhh string of cycle to be copied."
  echo "                       If not specified the pdate will be calculated by finding the latest"
  echo "                       cycle time in $OZN_STATS_TANKDIR and incrementing it by 6 hours."
  echo ""
  echo "            --oznf parent directory to file location.  This will be extended by "
  echo "                       $RUN.$PDY/$CYC/atmos/oznmon and the files there copied to OZN_STATS_TANKDIR."
  echo ""
  echo "            --ostat directory of oznstat file."
}


echo start OznMon_CP.sh
exit_value=0

nargs=$#
if [[ $nargs -le 0 || $nargs -gt 7 ]]; then
   usage
   exit 1
fi

set -ax

#-----------------------------------------------------------
#  Set default values and process command line arguments.
#
run=gdas

while [[ $# -ge 1 ]]
do
   key="$1"
   echo $key

   case $key in
      -p|--pdate)
         pdate="$2"
         shift # past argument
      ;;
      -r|--run)
         run="$2"
         shift # past argument
      ;;
      --oznf)
         oznmon_file_loc="$2"
         shift # past argument
      ;;
      --ostat)
         oznmon_stat_loc="$2"
         shift # past argument
      ;;
      *)
         #any unspecified key is OZNMON_SUFFIX
         export OZNMON_SUFFIX=$key
      ;;
   esac

   shift
done

echo "OZNMON_SUFFIX    = $OZNMON_SUFFIX"
echo "run              = $run"
echo "pdate            = $pdate"
echo "oznmon_file_loc  = ${oznmon_file_loc}"
echo "oznmon_stat_loc  = ${oznmon_stat_loc}"

export RUN=${RUN:-${run}}


#--------------------------------------------------------------------
# Set environment variables
#--------------------------------------------------------------------
this_dir=`dirname $0`

top_parm=${this_dir}/../../parm

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

if [[ ${oznmon_stat_loc} = "" ]]; then
   oznmon_stat_loc=${OZNSTAT_LOCATION}
fi


#---------------------------------------------------------------
# Create any missing directories.
#---------------------------------------------------------------
if [[ ! -d ${OZN_STATS_TANKDIR} ]]; then
   mkdir -p ${OZN_STATS_TANKDIR}
fi
if [[ ! -d ${OZN_LOGdir} ]]; then
   mkdir -p ${OZN_LOGdir}
fi

#---------------------------------------------------------------
# If the pdate (processing date) was not specified at the 
# command line then set it by finding the latest cycle in
# $TANKverf and increment 6 hours.
#---------------------------------------------------------------
if [[ $pdate = "" ]]; then
   ldate=`${OZN_DE_SCRIPTS}/find_cycle.pl --run $RUN --cyc 1 --dir ${OZN_STATS_TANKDIR}`
   pdate=`${NDATE} +06 ${ldate}`
fi
export PDATE=${pdate}

export PDY=`echo $PDATE|cut -c1-8`
export CYC=`echo $PDATE|cut -c9-10`

#---------------------------------------------------------------
#  Verify the data files are available
#---------------------------------------------------------------
export OZNSTAT_LOCATION=${oznmon_stat_loc}/${RUN}.${PDY}/${CYC}/atmos


#---------------------------------------------------------------
#  Location of the oznmon files is an adventure.  The if case
#  is the default gfs output location.  Para runs are handled
#  in the else condition
#
if [[ ${oznmon_file_loc} = "" ]]; then
   data_location=${OZNSTAT_LOCATION}/oznmon
else
   data_location=${oznmon_file_loc}
   if [[ -d ${data_location}/${RUN}.${PDY} ]]; then
      data_location=${data_location}/${RUN}.${PDY}
   fi
fi

export DATA_LOCATION=${data_location}
echo "DATA_LOCATION = ${DATA_LOCATION}"
nfile_src=`ls -l ${DATA_LOCATION}/time/*${PDATE}*ieee_d* | egrep -c '^-'`

export OZNSTAT=${OZNSTAT_LOCATION}/${RUN}.t${CYC}z.oznstat


if [[  ${nfile_src} -gt 0 ]]; then
   job=${OZN_DE_SCRIPTS}/oznmon_copy.sh
   jobname=OznMon_CP_${OZNMON_SUFFIX}
   logfile=${OZN_LOGdir}/CP.${PDY}.${CYC}.log
   if [[ -e ${logfile} ]]; then
     rm -f ${logfile}
   fi


   if [[ $MY_MACHINE = "wcoss_d" ]]; then
      $SUB -q $JOB_QUEUE -P $PROJECT -o ${logfile} \
           -M 80 -R affinity[core] -W 0:10 -J ${jobname} -cwd ${PWD} ${job}

   elif [[ $MY_MACHINE = "wcoss_c" ]]; then
      $SUB -q $JOB_QUEUE -P $PROJECT -o ${logfile} \
           -M 100 -W 0:20 -J ${jobname} -cwd ${PWD} ${job}

   elif [[ $MY_MACHINE = "hera" ]]; then
      $SUB --account=${ACCOUNT} --time=10 -J ${jobname} -D . \
        -o ${logfile} --ntasks=1 --mem=5g ${job}
   fi
else
   echo "Unable to locate DATA_LOCATION: ${DATA_LOCATION}"
   exit_value=4
fi


echo end OznMon_CP.sh
exit ${exit_value}

