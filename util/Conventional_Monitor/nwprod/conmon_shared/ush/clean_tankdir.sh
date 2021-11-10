#!/bin/bash

#------------------------------------------------------------------
#
#  clean_tankdir.sh
#
#------------------------------------------------------------------
   set -ax

   rc=0
   hrs=(00 06 12 18)

#   for hr in "${hrs[@]}"; do
#      echo "$hr"
#   done
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

         #--------------------------------------------
         #  Remove anything older than 40 days. 
         #  Remove horz_hist files older than
         #  7 days, keeping only the nobs.ges* files.
         #
         if [ $days -gt 40 ]; then
            for hr in "${hrs[@]}"; do
               echo "RM ${C_TANKDIR}/${dir}/${hr}/conmon"
               rm -rf ${C_TANKDIR}/${dir}/${hr}/conmon 
            done

         elif [ $days -gt 7 ]; then
            for hr in "${hrs[@]}"; do
               echo "RM ${C_TANKDIR}/${dir}/${hr}/conmon/horz_hist"
               mv ${C_TANKDIR}/${dir}/${hr}/conmon/horz_hist/ges/nobs.ges* ${C_STMP_USER}/.
               rm -rf ${C_TANKDIR}/$dir/${hr}/conmon/horz_hist
               mkdir -p ${C_TANKDIR}/$dir/${hr}/conmon/horz_hist/ges
               mv ${C_STMP_USER}/nobs.ges* ${C_TANKDIR}/$dir/${hr}/conmon/horz_hist/ges/.
            done
         fi

      fi

   done


exit ${rc}

