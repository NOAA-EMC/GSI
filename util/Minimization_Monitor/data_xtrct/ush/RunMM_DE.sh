#!/bin/sh

#--------------------------------------------------------------------
#  RunMM_DE.sh
#
#  Run the MinMon_DE script in a loop from start to end date or 
#   from start to end of available data.
#
#  This script will run the MinMOn data extraction for a given source 
#  in a loop.  The loop can be:
#     1) from the last proceessed date until the available gsistat 
#        data is exhausted,
#     2) from the input start date until available data is exhausted,
#     3) from the start to the stop date.
# 
#
#  NOTE:  for the moment the script only runs as 3) above.  Start and
#    end dates must both be specified.
#--------------------------------------------------------------------

function usage {
  echo "Usage:  RunMM_DE.sh suffix [start_date] [end_date]"
  echo "            File name for RunCopy.sh can be full or relative path"
  echo "            Suffix is the indentifier for this data source."
  echo "            Start_date is the optional starting cycle to process (YYYYMMDDHH format)."
  echo "            End_date   is the optional ending cycle to process (YYYYMMDDHH format)."
}

set -ax
echo start RunMM_DE.sh


nargs=$#
if [[ $nargs -lt 3 ]]; then
   usage
   exit 1
fi

this_file=`basename $0`
this_dir=`dirname $0`

SUFFIX=$1
START_DATE=$2
END_DATE=$3

echo SUFFIX     = $SUFFIX
echo START_DATE = $START_DATE
echo END_DATE   = $END_DATE

#--------------------------------------------------------------------
# Set environment variables
#--------------------------------------------------------------------
top_parm=${this_dir}/../../parm
export MINMON_VERSION=${MINMON_VERSION:-${top_parm}/MinMon.ver}
if [[ -s ${MINMON_VERSION} ]]; then
   echo "sourcing $MINMON_VERSION"
   . ${MINMON_VERSION}
else
   echo "Unable to source ${MINMON_VERSION} file"
   exit 2 
fi

export MINMON_CONFIG=${MINMON_CONFIG:-${top_parm}/MinMon_config}
if [[ -s ${MINMON_CONFIG} ]]; then
   echo "sourcing $MINMON_CONFIG"
   . ${MINMON_CONFIG}
else
   echo "Unable to source ${MINMON_CONFIG} file"
   exit 3 
fi

export MINMON_USER_SETTINGS=${MINMON_USER_SETTINGS:-${top_parm}/MinMon_user_settings}
if [[ -s ${MINMON_USER_SETTINGS} ]]; then
   echo "sourcing $MINMON_USER_SETTINGS"
   . ${MINMON_USER_SETTINGS}
else
   echo "Unable to source ${MINMON_USER_SETTINGS} file"
   exit 4 
fi

#--------------------------------------------------------------------
#  Check setting of RUN_ONLY_ON_DEV and possible abort if on prod and
#  not permitted to run there.
#--------------------------------------------------------------------

if [[ RUN_ONLY_ON_DEV -eq 1 ]]; then
   is_prod=`${M_DE_SCRIPTS}/onprod.sh`
   if [[ $is_prod = 1 ]]; then
      exit 10
   fi
fi


#-----------------------------------------------------------------------------
# If start and end dates were specified, confirm the start is before end date.
#  NOTE:  Start date = end date will result in one cycle (start date) running.
#-----------------------------------------------------------------------------
start_len=`echo ${#START_DATE}`
end_len=`echo ${#END_DATE}`
echo "start, end len = $start_len, $end_len"

if [[ ${start_len} -gt 0  &&  ${end_len} -gt 0 ]]; then
   if [[ $START_DATE -gt $END_DATE ]]; then
      echo ERROR -- start date is greater then end date  : $START_DATE $END_DATE
      exit 11
   fi
fi


#--------------------------------------------------------------------
# If we have a START_DATE then use it, otherwise use the last entry 
#   in a ${SUFFIX}_minmon.YYYYMMDD/${SUFFIX}.gnorm_data.txt file to
#   determine the last date processed.
#--------------------------------------------------------------------
start_len=`echo ${#START_DATE}`
verf_dir="${TANKverf}/stats/${SUFFIX}/gsistat"

if [[ ${start_len} -le 0 ]]; then
   pdate=`${M_DE_SCRIPTS}/find_cycle.pl ${SUFFIX} 1 ${verf_dir}`
   pdate_len=`echo ${#pdate}`
   if [[ ${pdate_len} -ne 10 ]]; then
      exit 12 
   fi 
   START_DATE=`${NDATE} +06 $pdate`
fi

cdate=$START_DATE
echo "start date = $START_DATE"

mkdir -p $LOGdir
##--------------------------------------------------------------------
## Run in a loop until END_DATE is processed, or an error occurs, or 
##   we run out of data.
##--------------------------------------------------------------------
done=0
ctr=0
while [[ $done -eq 0 ]]; do

   #--------------------------------------------------------------------
   # Check for running jobs   
   #--------------------------------------------------------------------
   if [[ $MY_MACHINE = "wcoss" ]]; then
      running=`bjobs -l | grep minmon_de_${SUFFIX} | wc -l`
   elif [[ $MY_MACHINE = "zeus" ]]; then
      running=`qstat -u $LOGNAME | grep minmon_de_${SUFFIX} | wc -l`
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
      # Run the MinMon_DE.sh script
      #-----------------------------------------------------------------
      log_file=${LOGdir}/MinMon_DE_${SUFFIX}_${cdate}.log
      err_file=${LOGdir}/MinMon_DE_${SUFFIX}_${cdate}.err

      echo Processing ${cdate}
      # need a switch for glb and rgn
      ${M_DE_SCRIPTS}/MinMon_DE.sh ${SUFFIX} dev ${cdate} 1>${log_file} 2>${err_file}

      #-----------------------------------------------------------------
      # done is true (1) if the copy_script produced an error code, or
      # we're at END_DATE  
      #
      #  NOTE:  gonna have to do some work to get the correct $rc; this 
      #  job gets submitted to the queue so I'm going to have to dig 
      #  through the log file to find the RC.  Alternately, if no END
      #  is specified, I could calculate that up front.
      #-----------------------------------------------------------------
#      sleep 60
#      PDY=`echo $cdate|cut -c1-8`
#      cyc=`echo $cdate|cut -c9-10`
#      logfile="${m_jlogfile}${SUFFIX}.${PDY}.${cyc}.log"
#      rc=`grep "exit value" ${logfile} | tail -1 | gawk '{split($0,a," "); print a[6]}'`
#      rc_len=`echo ${#rc}`

#      echo "rc = $rc"

#      if [[ $rc -ne 0 ]]; then
#         done=1
      if [[ $cdate -eq $END_DATE ]]; then
#      elif [[ $cdate -eq $END_DATE ]]; then
         done=1
#      elif [[ $rc_len -eq 0 ]]; then
#         done=1
      else
         #--------------------------------------------------------------
         # If not done advance the cdate to the next cycle
         #--------------------------------------------------------------
         cdate=`${NDATE} +06 $cdate`
         ctr=0
      fi
   fi

done 


echo "end RunMM_DE.sh"
exit 
