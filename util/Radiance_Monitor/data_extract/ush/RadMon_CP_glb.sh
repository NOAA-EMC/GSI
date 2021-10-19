#!/bin/bash

#--------------------------------------------------------------------
#  RadMon_CP_glb.sh
#
#    This script searches for new radmon output from the global GDAS
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
  echo "Usage:  RadMon_CP_glb.sh suffix [-r|--run gdas|gfs -p|--pdate yyyymmddhh"
  echo ""
  echo "            Suffix (NET) is the indentifier for this data source."
  echo ""
  echo "            -r|--run is the run value, typically gdas or gfs.  Default value is gdas." 
  echo ""
  echo "            -p|--pdate is 10 digit yyyymmddhh string of cycle to be copied."
  echo "                       If not specified the pdate will be calculated by finding the latest"
  echo "                       cycle time in $TANKverf and incrementing it by 6 hours."
  echo ""
  echo "            -f|--radf parent directory to radstat file location.  This will be extended by "
  echo "                       $RUN.$PDY/$CYC/atmos/radmon and the files there copied to TANKverf."
  echo ""
  echo "            -d|--dataf parent directory to extracted radstat data file location.  This will be extended by "
  echo "                       $RUN.$PDY/$CYC and the files there copied to TANKverf."

}


echo start RadMon_CP_glb.sh
exit_value=0

nargs=$#
if [[ $nargs -le 0 || $nargs -gt 9 ]]; then
   usage
   exit 1
fi


export RAD_AREA=glb

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
      -f|--radf)
         radstat_loc="$2"
         shift # past argument
      ;;
      -d|--dataf)
         data_file_loc="$2"
         shift # past argument
      ;;
      *)
         #any unspecified key is RADMON_SUFFIX
         export RADMON_SUFFIX=$key
      ;;
   esac

   shift
done

echo "RADMON_SUFFIX    = $RADMON_SUFFIX"
echo "run              = $run"
echo "pdate            = $pdate"
echo "radstat_loc      = ${radstat_loc}"
echo "data_file_loc    = ${data_file_loc}"

export RUN=${RUN:-${run}}

set -ax

#--------------------------------------------------------------------
# Set environment variables
#--------------------------------------------------------------------
this_dir=`dirname $0`


top_parm=${this_dir}/../../parm

export RADMON_CONFIG=${RADMON_CONFIG:-${top_parm}/RadMon_config}
if [[ -s ${RADMON_CONFIG} ]]; then
   . ${RADMON_CONFIG}
else
   echo "Unable to source ${RADMON_CONFIG} (radmon config) file"
   exit 2
fi

if [[ -s ${RADMON_USER_SETTINGS} ]]; then
   . ${RADMON_USER_SETTINGS}
else
   echo "Unable to source ${RADMON_USER_SETTINGS} (radmon user settings) file"
   exit 3
fi

export USHradmon=${USHradmon:-$HOMEradmon/ush}


#---------------------------------------------------------------
# Create any missing directories.
#---------------------------------------------------------------
if [[ ! -d ${TANKverf} ]]; then
   mkdir -p $TANKverf
fi
if [[ ! -d ${LOGdir} ]]; then
   mkdir -p $LOGdir
fi

#---------------------------------------------------------------
# If the pdate (processing date) was not specified at the 
# command line then set it by finding the latest cycle in
# $TANKverf and increment 6 hours.
#---------------------------------------------------------------
if [[ $pdate = "" ]]; then
   ldate=`${DE_SCRIPTS}/nu_find_cycle.pl --run $RUN --cyc 1 --dir ${TANKverf}`
   pdate=`${NDATE} +06 ${ldate}`
fi
echo "pdate = $pdate"
export PDATE=${pdate}

export PDY=`echo $PDATE|cut -c1-8`
export CYC=`echo $PDATE|cut -c9-10`

#---------------------------------------------------------------
#  Set data and radstat locations     
#---------------------------------------------------------------
if [[ -n ${radstat_loc} ]]; then 
   export RADSTAT_LOCATION=${radstat_loc}
fi
export RADSTAT_LOCATION=${RADSTAT_LOCATION}/${RUN}.${PDY}/${CYC}/atmos


if [[ -n ${data_file_loc} ]]; then
   export DATA_LOCATION=${data_file_loc}/${RUN}.${PDY}
else  
   export DATA_LOCATION=${RADSTAT_LOCATION}/radmon
fi


if [[  -d ${DATA_LOCATION} ]]; then
   job=${DE_SCRIPTS}/radmon_copy.sh
   jobname=RadMon_CP_${RADMON_SUFFIX}
   logfile=${LOGdir}/CP.${PDY}.${CYC}.log
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


echo end RadMon_CP_glb.sh
exit ${exit_value}

