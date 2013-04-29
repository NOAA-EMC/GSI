#!/bin/sh

#--------------------------------------------------------------------
#  RunCopy.sh
#
#  Run the copy script.
#
#  This script will run the data copy for a given source in a
#  loop.  The loop can be:
#     1) from the last proceessed date until the available radstat 
#        data is exhausted,
#     2) from the input start date until available data is exhausted,
#     3) from the start to the stop date.
# 
#--------------------------------------------------------------------

function usage {
  echo "Usage:  RunVrfy.sh suffix start_date [end_date]"
  echo "            File name for RunCopy.sh can be full or relative path"
  echo "            Suffix is the indentifier for this data source."
  echo "            Start_date is the optional starting cycle to process (YYYYMMDDHH format)."
  echo "            End_date   is the optional ending cycle to process (YYYYMMDDHH format)."
}

#
#  with no data_map.xml we'll need to add glb/rgn as a parm 
#

set -ax
echo start RunCopy.sh

#
#  Temporary change, abort if running on prod.  This is an attempt to
#  eliminate prossible crons running on the prod machine for me only.
#
#   machine=`hostname | cut -c1`
#   prod=`cat /etc/prod | cut -c1`
#
#   if [[ $machine = $prod ]]; then
#      exit 10 
#   fi
#
#  End temporary change
#


nargs=$#
if [[ $nargs -lt 1 ]]; then
   usage
   exit 1
fi

this_file=`basename $0`
this_dir=`dirname $0`

SUFFIX=$1
START_DATE=$2
END_DATE=$3

RUN_ENVIR=${RUN_ENVIR:-dev}

echo SUFFIX     = $SUFFIX
echo START_DATE = $START_DATE
echo END_DATE   = $END_DATE

#--------------------------------------------------------------------
# Set environment variables
#--------------------------------------------------------------------
top_parm=${this_dir}/../../parm

if [[ -s ${top_parm}/RadMon_config ]]; then
   . ${top_parm}/RadMon_config
else
   echo "Unable to source RadMon_config file in ${top_parm}"
   exit 2 
fi

if [[ -s ${top_parm}/RadMon_user_settings ]]; then
   . ${top_parm}/RadMon_user_settings
else
   echo "Unable to source RadMon_user_settings file in ${top_parm}"
   exit 6 
fi


. ${RADMON_DATA_EXTRACT}/parm/data_extract_config

if [[ $RAD_AREA = glb ]]; then
   copy_script=Copy_glbl.sh
   . ${RADMON_DATA_EXTRACT}/parm/glbl_conf
elif [[ $RAD_AREA = rgn ]]; then
   copy_script=Copy_rgnl.sh
   . ${RADMON_DATA_EXTRACT}/parm/rgnl_conf
else
   exit 3
fi


#--------------------------------------------------------------------
# If end date was specified, confirm the start is before end date.
#--------------------------------------------------------------------
end_len=`echo ${#END_DATE}`
if [[ ${end_len} -gt 0 ]]; then
   if [[ $START_DATE -gt $END_DATE ]]; then
      echo ERROR -- start date is greater then end date  : $START_DATE $END_DATE
      exit 1
   fi
fi


#--------------------------------------------------------------------
# If we have a START_DATE then use it, otherwise use the 
#   find_last_cycle.pl script to determine the last copied cycle.
#--------------------------------------------------------------------
start_len=`echo ${#START_DATE}`
if [[ ${start_len} -le 0 ]]; then
   pdate=`${USHverf_rad}/find_last_cycle.pl ${TANKDIR}`
   pdate_len=`echo ${#pdate}`
   if [[ ${pdate_len} -ne 10 ]]; then
      exit 4
   fi 
   START_DATE=`${NDATE} +06 $pdate`
fi

cdate=$START_DATE


#--------------------------------------------------------------------
# Run in a loop until END_DATE is processed, or an error occurs, or 
#   we run out of data.
#--------------------------------------------------------------------
done=0
ctr=0
while [[ $done -eq 0 ]]; do

   #--------------------------------------------------------------------
   # Check for running jobs   
   #--------------------------------------------------------------------
   if [[ $MY_MACHINE = "ccs" ]]; then
      running=`llq -u ${LOGNAME} -f %jn | grep data_extract_${SUFFIX} | wc -l`
   elif [[ $MY_MACHINE = "wcoss" ]]; then
      running=`bjobs -l | grep data_extract_${SUFFIX} | wc -l`
   elif [[ $MY_MACHINE = "zeus" ]]; then
      running=`qstat -u $LOGNAME | grep data_extract_${SUFFIX} | wc -l`
   fi

   if [[ $running -ne 0 ]]; then
      #----------------------------------------------------
      #  sleep or time-out after 30 tries.
      #----------------------------------------------------
      ctr=$(( $ctr + 1 ))
      if [[ $ctr -le 30 ]]; then
         echo sleeping.....
         sleep 60
      else
         done=1
      fi
   else

      #-----------------------------------------------------------------
      # Run the copy script
      #-----------------------------------------------------------------
      log_file=${LOGSverf_rad}/CopyRad_${SUFFIX}_${cdate}.log
      err_file=${LOGSverf_rad}/CopyRad_${SUFFIX}_${cdate}.err

      echo Processing ${cdate}
      ${USHverf_rad}/${copy_script} ${SUFFIX} ${cdate} 1>${log_file} 2>${err_file}

      #-----------------------------------------------------------------
      # done is true (1) if the copy_script produced an error code, or
      # we're at END_DATE  
      #-----------------------------------------------------------------
      rc=`echo $?`
      if [[ $rc -ne 0 ]]; then
         done=1
      elif [[ $cdate -eq $END_DATE ]]; then
         done=1
      else
         #--------------------------------------------------------------
         # If not done advance the cdate to the next cycle
         #--------------------------------------------------------------
         cdate=`${NDATE} +06 $cdate`
         ctr=0
      fi
   fi

done 


echo "end RunCopy.sh"
exit 
