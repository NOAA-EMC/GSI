#!/bin/sh

#--------------------------------------------------------------------
#  RunVrfy.sh
#
#  Run the verification and data extract.
#
#  This script will run the data extraction for a given source in a
#  loop.  The loop can be from the last cycle processed (as 
#  determined by the contents of $TANKDIR) until the available
#  radstat data is exhausted, from the input start date until 
#  available data is exhausted, or from the start to the end date.
# 
#  Calling sequence is Suffix, Area, [start_date], [end_date}
#    suffix     = identifier for this data source
#    area       = "glb" or "rgn"
#    start_date = optional starting cycle to process
#    end_date   = optional ending cycle to process
#--------------------------------------------------------------------

function usage {
  echo "Usage:  RunVrfy.sh suffix rad_area start_date [end_date]"
  echo "            Suffix is the indentifier for this data source."
  echo "            Start_date is the optional starting cycle to process (YYYYMMDDHH format)."
  echo "            End_date   is the optional ending cycle to process (YYYYMMDDHH format)."
}

set -ax
echo start RunVrfy.sh

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
   exit 2 
fi

. ${RADMON_DATA_EXTRACT}/parm/data_extract_config

log_file=${LOGSverf_rad}/VrfyRad_${SUFFIX}.log
err_file=${LOGSverf_rad}/VrfyRad_${SUFFIX}.err

if [[ $RAD_AREA = glb ]]; then
   vrfy_script=VrfyRad_glbl.sh
   . ${RADMON_DATA_EXTRACT}/parm/glbl_conf
elif [[ $RAD_AREA = rgn ]]; then
   vrfy_script=VrfyRad_rgnl.sh
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
      echo "ERROR:  start date is greater then end date  : $START_DATE $END_DATE"
      exit 1
   fi
fi


#--------------------------------------------------------------------
# If we don't have a START_DATE the find the last processed cycle, 
#   and add 6 hrs to it.
#--------------------------------------------------------------------
start_len=`echo ${#START_DATE}`
if [[ ${start_len} -gt 0 ]]; then
   pdate=`${NDATE} -06 $START_DATE`
   ${USHverf_rad}/update_data_map.pl ${DATA_MAP} ${SUFFIX} prodate ${pdate}
else
   pdate=`${USHverf_rad}/find_last_cycle.pl ${TANKDIR}`
   pdate_len=`echo ${#pdate}`
   if [[ ${pdate_len} -eq 10 ]]; then
      ${USHverf_rad}/update_data_map.pl ${DATA_MAP} ${SUFFIX} prodate ${pdate}
   fi 
   START_DATE=`${NDATE} +06 $pdate`
fi



#--------------------------------------------------------------------
# Run in a loop until END_DATE is processed, or an error occurs, or 
# we run out of data.
#--------------------------------------------------------------------
cdate=$START_DATE
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
      # Run the verification/extraction script
      #-----------------------------------------------------------------
      echo Processing ${cdate}
      ${USHverf_rad}/${vrfy_script} ${SUFFIX} ${RUN_ENVIR} ${cdate} 1>${log_file} 2>${err_file}

      #-----------------------------------------------------------------
      # done is true (1) if the vrfy_script produced an error code, or
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


echo "end RunVrfy.sh"
exit 
