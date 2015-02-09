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
source  radmon_gui.config 

if [ ${ENV_RUN_STEP} -eq 1 ]
then
   cd ../data_extract/ush 
   ls *sh 
   for cycle in $ENV_CYCLES
   do 
      ./VrfyRad_glbl_template.sh   $ENV_ID  $cycle 
   done
else 
   cd ../image_gen/ush
   logFile=${ENV_PTMP}/${LOGNAME}/logs/CkPlt_glbl_template.log
   ./CkPlt_glbl_template.sh   $ENV_ID   > ${logFile} 2>> ${logFile}
fi


