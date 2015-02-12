#!/bin/bash
#====================================================
# Purpose:
#    Used by GUI to run Radmon
# Author: Deyong Xu / RTi@JCSDA
# History:
#    1/22/2015, D. Xu / RTi@JCSDA , initial code.
#
#====================================================

# include config file created by GUI
source  ./radmon_gui.config 

if [ ${ENV_RUN_STEP} -eq 1 ]
then
   cd ../data_extract/ush 

   startCycle=""
   endCycle=""

   # Create start cycle and end cycle
   index=0
   for cycle in $ENV_CYCLES
   do
      let index=$index+1
      if [ $index -eq 1 ]
      then
	 startCycle=${cycle}00
      fi

      if [ $index -eq 2 ]
      then
	 endCycle=${cycle}18
      fi
   done


   # Loop thru. all the cycles till end of date range.  
   while [ ${startCycle} -le ${endCycle} ]
   do
      # Remove old log file
      if [ -e out${startCycle}.log ]
      then
	  rm -rf out${startCycle}.log
      fi
 
     ./VrfyRad_glbl_template.sh   $ENV_ID  ${startCycle}   > out${startCycle}.log 2>>out${startCycle}.log 
     startCycle=`/usr/local/jcsda/nwprod_gdas_2014/util/exec/ndate +6 ${startCycle} `
   done
else 
   cd ../image_gen/ush
   logFile=${ENV_PTMP}/${LOGNAME}/logs/CkPlt_glbl_template.log
   ./CkPlt_glbl_template.sh   $ENV_ID   > ${logFile} 2>> ${logFile}
fi


