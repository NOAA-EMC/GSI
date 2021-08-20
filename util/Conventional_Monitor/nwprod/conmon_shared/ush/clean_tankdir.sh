#!/bin/bash

#------------------------------------------------------------------
#
#  clean_tankdir.sh
#
#------------------------------------------------------------------
   set -ax

   echo "--> clean_tankdir.sh"
   rc=0

   #---------------------------------------------------------------
   #  Algorithm
   #
   #  1. Build a list of $run.${PDY} directories.
   #  2. All $CYC/conmon directories older than 40 days get
   #        deleted.
   #  3. All $cyc/conmon/horz_hist directories older than 7 
   #        days get deleted.
   #---------------------------------------------------------------


   #----------------------------------------
   #  Get list of directories in $C_TANKDIR
   #
   dirs=`ls ${C_TANKDIR}`
  
   #----------------------------------------------- 
   #  Determine number of days from $PDATE for all 
   #  directories that start with "gdas".  Note 
   #  that this expects the directories to be in
   #  the format of ${RUN}.yyyymmdd. 
   #
   for dir in $dirs; do
      file_name="${dir##*/}"
      file="${file_name%.*}"
      file_extension="${file_name##*.}"

      if [ $file = $RUN ]; then

         #----------------------------------------------
         # Use date utility to determine number of days 
         # from PDATE.  Note that time difference is
         # calculated in seconds, hence the division 
         # by the number of seconds/day.
         #
         days=$(( ($(date --date=${PDY} +%s) - $(date --date=${file_extension} +%s) )/(60*60*24) ))

         #--------------------------------------
         #  rm anything older than 40 days and
         #  horz_hist subdirectories older than
         #  7 days
         #
         if [ $days -gt 40 ]; then
            echo "RM ${C_TANKDIR}/$dir/00/conmon"
            rm -rf ${C_TANKDIR}/$dir/00/conmon 
            echo "RM ${C_TANKDIR}/$dir/06/conmon"
            rm -rf ${C_TANKDIR}/$dir/06/conmon 
            echo "RM ${C_TANKDIR}/$dir/12/conmon"
            rm -rf ${C_TANKDIR}/$dir/12/conmon 
            echo "RM ${C_TANKDIR}/$dir/18/conmon"
            rm -rf ${C_TANKDIR}/$dir/18/conmon 

         elif [ $days -gt 7 ]; then
            echo "RM ${C_TANKDIR}/${dir}/00/conmon/horz_hist"
            rm -rf ${C_TANKDIR}/$dir/00/conmon/horz_hist
            echo "RM ${C_TANKDIR}/${dir}/06/conmon/horz_hist"
            rm -rf ${C_TANKDIR}/$dir/06/conmon/horz_hist
            echo "RM ${C_TANKDIR}/${dir}/12/conmon/horz_hist"
            rm -rf ${C_TANKDIR}/$dir/12/conmon/horz_hist
            echo "RM ${C_TANKDIR}/${dir}/18/conmon/horz_hist"
            rm -rf ${C_TANKDIR}/$dir/18/conmon/horz_hist
         fi

      fi

   done

   echo "<-- clean_tankdir.sh"

exit ${rc}

