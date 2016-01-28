#!/bin/sh

#--------------------------------------------------------------------
#  RunCMonDE.sh
#
#  Run the CMon data extract in a loop.
#
#  This script will run the data extraction for a given source in a
#  loop.  The loop can be from the last cycle processed (as 
#  determined by the contents of $TANKverf) until the available
#  cnvstat data is exhausted, from the input start date until 
#  available data is exhausted, or from the start to the end date.
#
#  Calling sequence is Suffix, Area, [start_date], [end_date}
#    suffix     = identifier for this data source
#    start_date = optional starting cycle to process
#    end_date   = optional ending cycle to process
#--------------------------------------------------------------------

function usage {
  echo "Usage:  RunCMonDE.sh suffix [start_date] [end_date]"
  echo "            Suffix is the indentifier for this data source."
  echo "            Start_date is the optional starting cycle to process (YYYYMMDDHH format)."
  echo "            End_date   is the optional ending cycle to process (YYYYMMDDHH format)."
}

set -ax
echo start RunCMonDE.sh

nargs=$#
if [[ $nargs -lt 1 ]]; then
   usage
   exit 1
fi

#
#  Check for my monitoring use.  Abort if running on prod machine.
#
   machine=`hostname | cut -c1`
   prod=`cat /etc/prod | cut -c1`

   if [[ $machine = $prod ]]; then
      exit 10
   fi
#
#  End check.

this_file=`basename $0`
this_dir=`dirname $0`

CMON_SUFFIX=$1
START_DATE=$2
END_DATE=$3

#RUN_ENVIR=${RUN_ENVIR:-dev}
#RAD_AREA=${RAD_AREA:-glb}

echo CMON_SUFFIX = $CMON_SUFFIX
echo START_DATE  = $START_DATE
echo END_DATE    = $END_DATE

#--------------------------------------------------------------------
# Set environment variables
#--------------------------------------------------------------------
top_parm=${this_dir}/../../parm

cmon_version_file=${cmon_version:-${top_parm}/CMon.ver}
if [[ -s ${cmon_version_file} ]]; then
   . ${cmon_version_file}
   echo "able to source ${cmon_version_file}"
else
   echo "Unable to source ${cmon_version_file} file"
   exit 2
fi

cmon_config=${cmon_config:-${top_parm}/CMon_config}
if [[ -s ${cmon_config} ]]; then
   . ${cmon_config}
   echo "able to source ${cmon_config}"
else
   echo "Unable to source ${cmon_config} file"
   exit 3
fi


#--------------------------------------------------------------------
#  Check setting of RUN_ONLY_ON_DEV and possible abort if on prod and
#  not permitted to run there.
#--------------------------------------------------------------------

if [[ RUN_ONLY_ON_DEV -eq 1 ]]; then
   is_prod=`${C_DE_SCRIPTS}/onprod.sh`
   if [[ $is_prod = 1 ]]; then
      exit 10
   fi
fi


if [[ ! -d ${LOGSverf_cmon} ]]; then
   mkdir -p ${LOGSverf_cmon}
fi

log_file=${LOGSverf_cmon}/RunCMonDE_${CMON_SUFFIX}.log
err_file=${LOGSverf_cmon}/RunCMonDE_${CMON_SUFFIX}.err

vrfy_script=CMon_DE.sh


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
else
   pdate=`${C_DE_SCRIPTS}/find_cycle.pl 1 ${C_TANKDIR}`
   pdate_len=`echo ${#pdate}`
   START_DATE=`${NDATE} +06 $pdate`
fi


#--------------------------------------------------------------------
# Run in a loop until END_DATE is processed, or an error occurs, or 
# we run out of data.
#--------------------------------------------------------------------
cdate=$START_DATE
done=0
ctr=0
jobname=$DATA_EXTRACT_JOBNAME

while [[ $done -eq 0 ]]; do

   #--------------------------------------------------------------------
   # Check for running jobs   
   #--------------------------------------------------------------------
   if [[ $MY_MACHINE = "wcoss" ]]; then
      running=`bjobs -l | grep CMon_de_${CMON_SUFFIX} | wc -l`
   elif [[ $MY_MACHINE = "theia" ]]; then
      running=1 
      line=`qstat -u ${LOGNAME} | grep ${jobname}`
      test=`echo $line | gawk '{print $10}'`

      total=`echo $line | grep ${jobname} | wc -l`
      if [[ $test = "C" || $total -le 0 ]]; then
         running=0
      fi

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
      ${C_DE_SCRIPTS}/${vrfy_script} ${CMON_SUFFIX} ${cdate} 1>${log_file} 2>${err_file}

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
         # If not done advance the cdate to the next cycle.
         # sleep is for zeus's job queue to catch up.
         #--------------------------------------------------------------
         cdate=`${NDATE} +06 $cdate`
         ctr=0
         if [[ $MY_MACHINE = "theia" ]]; then
            sleep 30 
         fi
      fi
   fi

done 


echo "end RunCMonDE.sh"
exit 
