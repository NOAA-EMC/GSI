#!/bin/bash

#------------------------------------------------------------------
#
#  clean_tankdir.sh
#
#------------------------------------------------------------------

   echo "--> clean_tankdir.sh"
   rc=0

   area=$1
   days_to_keep=$2
   echo " area, days_to_keep = ${area}, ${days_to_keep}"

   #---------------------------------------------------------------
   #  Algorithm
   #
   #  1. Build list of $run.${PDY} || radmon.${PDY} sudirectories.
   #  2. All subdirectories older than $days_to_keep get deleted.
   #---------------------------------------------------------------
   if [[ ${area} = 'rgn' ]]; then
      subdir="radmon"
   else
      subdir=${RUN}
   fi

   echo "TANKverf = ${TANKverf}"

   dirs=`ls ${TANKverf}`
   echo "dirs = ${dirs}"


   if [[ ${area} = 'rgn' ]]; then
      echo " in rgn"
      #----------------------------------------------- 
      #  Determine number of days from $PDATE for all 
      #  directories that start with "radmon".  Note 
      #  that this expects the directories to be in
      #  the format of ${RUN}.yyyymmdd. 
      #
      for dir in $dirs; do
         file_name="${dir##*/}"
         file="${file_name%.*}"
         file_extension="${file_name##*.}"
         echo "file, file_extension = ${file}, ${file_extension}"

         if [ ${file} = ${subdir} ]; then

            #----------------------------------------------
            # Use date utility to determine number of days 
            # from PDATE.  Note that time difference is
            # calculated in seconds, hence the division 
            # by the number of seconds/day.
            #
            days=$(( ($(date --date=${PDY} +%s) - $(date --date=${file_extension} +%s) )/(60*60*24) ))
            echo "days = ${days}"

            #----------------------------------------
            #  rm anything older than $days_to_keep
            #
            if [ $days -gt ${days_to_keep} ]; then
                
               cycle_dir="${TANKverf}/${dir}"
               echo "cycle_dir = ${cycle_dir}"

               if [[ -d ${cycle_dir} ]]; then
                  echo "RM ${cycle_dir}"
                  rm -rf ${cycle_dir}
               fi
            fi

         fi
      done

   else
  
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

         if [ ${file} = ${subdir} ]; then

            #----------------------------------------------
            # Use date utility to determine number of days 
            # from PDATE.  Note that time difference is
            # calculated in seconds, hence the division 
            # by the number of seconds/day.
            #
            days=$(( ($(date --date=${PDY} +%s) - $(date --date=${file_extension} +%s) )/(60*60*24) ))

            #----------------------------------------
            #  rm anything older than $days_to_keep
            #
            if [ $days -gt ${days_to_keep} ]; then
               cycles="00 06 12 18"
 
               for cyc in $cycles; do
                  cycle_dir="${TANKverf}/${dir}/${cyc}/radmon"
                  if [[ -d ${cycle_dir} ]]; then
                     echo "RM ${cycle_dir}"
                     rm -rf ${cycle_dir}
                  fi
               done

            fi

         fi

      done
   fi

   echo "<-- clean_tankdir.sh"

exit ${rc}

