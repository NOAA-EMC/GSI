#!/bin/sh

#--------------------------------------------------------------------
#  RunVrfy.sh
#
#  Run the verification and data extract.
#  This only runs as RUN_ENVIR "dev", not as "para" or "prod"
#--------------------------------------------------------------------

function usage {
  echo "Usage:  RunVrfy.sh suffix start_date [end_date]"
  echo "            File name for RunVrfy.sh can be full or relative path"
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

#
# assume for the moment that all runs will be dev
#
RUN_ENVIR=dev

echo SUFFIX     = $SUFFIX
echo START_DATE = $START_DATE
echo END_DATE   = $END_DATE

#echo RUN_ENVIR = $RUN_ENVIR
#echo VRFYRAD_DIR = $VRFYRAD_DIR

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

. ${RADMON_DATA_EXTRACT}/parm/data_extract_config

echo SCRIPTS = ${SCRIPTS}

#--------------------------------------------------------------------
# Get the area (glb/rgn) for this suffix
#--------------------------------------------------------------------
area=`${SCRIPTS}/get_area.sh ${SUFFIX} ${DATA_MAP}`
echo $area

log_file=${LOGSverf_rad}/VrfyRad_${SUFFIX}.log
err_file=${LOGSverf_rad}/VrfyRad_${SUFFIX}.err

if [[ $area = glb ]]; then
   vrfy_script=VrfyRad_glbl.sh
else
   vrfy_script=VrfyRad_rgnl.sh
fi


#--------------------------------------------------------------------
# If end date is present, confirm the start is before end date.
#--------------------------------------------------------------------
end_len=`echo ${#END_DATE}`
if [[ ${end_len} -gt 0 ]]; then
   if [[ $START_DATE -gt $END_DATE ]]; then
      echo ERROR -- start date is greater then end date  : $START_DATE $END_DATE
      exit 1
   fi
fi


#--------------------------------------------------------------------
# If we have a START_DATE then set the prodate to the previous cycle 
#   in the data_map file.  Otherwise, run using the current value in 
#   the data_map as the START_DATE.
#--------------------------------------------------------------------
start_len=`echo ${#START_DATE}`
if [[ ${start_len} -gt 0 ]]; then
   pdate=`${NDATE} -06 $START_DATE`
   ${SCRIPTS}/set_prodate.sh ${SUFFIX} ${DATA_MAP} ${pdate}
else
   pdate=`${SCRIPTS}/get_prodate.sh ${SUFFIX} ${DATA_MAP}`
   START_DATE=`${NDATE} +06 $pdate`
fi

cdate=$START_DATE


#--------------------------------------------------------------------
# Run in a loop until END_DATE is processed, or an error occurs, or 
# we run out of data.
#--------------------------------------------------------------------
done=0
ctr=0
while [[ $done -eq 0 ]]; do

   #--------------------------------------------------------------------
   # Check for running jobs   
   #--------------------------------------------------------------------
   count=`ls ${LOADLQ}/verf*_$SUFFIX* | wc -l`
   complete=`grep "COMPLETED" ${LOADLQ}/verf*_$SUFFIX* | wc -l`
   running=`expr $count - $complete`

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
      ${SCRIPTS}/${vrfy_script} ${SUFFIX} ${RUN_ENVIR} 1>${log_file} 2>${err_file}

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


echo end RunVrfy.sh
exit 
