#!/bin/ksh
#-----------------------------------------------------------------------
# Script gen_be_stage0_wrf.ksh
#
# Purpose: To calculate ensemble perturbations in "standard fields".
#
# Note: START_DATE and END_DATE are defined as the times of the first and 
# last perturbation. We derive START_DATE_STAGE0 and END_DATE_STAGE0
# from these using FCST_RANGE.  
#-----------------------------------------------------------------------

#-----------------------------------------------------------------------------------
# Don't change anything below this line.
#-----------------------------------------------------------------------------------

export REL_DIR=${REL_DIR:-$HOME/trunk}
export GEN_BE_DIR=${GEN_BE_DIR:-$REL_DIR/gen_be}
export SCRIPTS_DIR=${SCRIPTS_DIR:-$GEN_BE_DIR/scripts}

. ${SCRIPTS_DIR}/gen_be_set_defaults.ksh

if [[ ! -d $RUN_DIR ]]; then mkdir $RUN_DIR; fi
if [[ ! -d $STAGE0_DIR ]]; then mkdir $STAGE0_DIR; fi

#Derive times of initial/final FCST_RANGE forecasts:
export START_DATE_STAGE0=$(${BUILD_DIR}/da_advance_time.exe $START_DATE -$FCST_RANGE1)
export END_DATE_STAGE0=$(${BUILD_DIR}/da_advance_time.exe $END_DATE   -$FCST_RANGE1)
#export START_DATE_STAGE0=$START_DATE  #GD DOWELL
#export END_DATE_STAGE0=$END_DATE      # GD DOWELL
export DATE=$START_DATE_STAGE0

echo "${BUILD_DIR}/da_advance_time.exe $START_DATE -$FCST_RANGE1"
echo "stage 0 $DATE  $START_DATE"

while [[ $DATE -le $END_DATE_STAGE0 ]]; do

   export TMP_DIR=${WORK_DIR}/${DATE}
   rm -rf ${TMP_DIR} 2>/dev/null
   mkdir ${TMP_DIR}  2>/dev/null
   cd ${TMP_DIR}
   if [[ ! -d ${TMP_DIR}/mask ]]; then mkdir ${TMP_DIR}/mask; fi

   #  Create file dates:
   export FCST_TIME=$(${BUILD_DIR}/da_advance_time.exe $DATE $FCST_RANGE1)
   # export FCST_TIME=$DATE GD DOWELL
   export YYYY=$(echo $FCST_TIME | cut -c1-4)
   export MM=$(echo $FCST_TIME | cut -c5-6)
   export DD=$(echo $FCST_TIME | cut -c7-8)
   export HH=$(echo $FCST_TIME | cut -c9-10)
   export FILE_DATE=${YYYY}-${MM}-${DD}_${HH}:00:00
   export DATE0=${YYYY}${MM}${DD}${HH}
   export FILE=${FC_DIR}/${DATE}/wrfout_d${DOMAIN}_${FILE_DATE}
   export FILE1=${DATE0}
   export FILE2=${DATE0}.e001
   export FILE3=${DATE0}.e002
   export NEXT_DATE=$(${BUILD_DIR}/da_advance_time.exe $DATE $INTERVAL)
   echo "gen_be_stage0: Calculating standard perturbation fields valid at time " $FCST_TIME

   if [[ $BE_METHOD == NMC ]]; then
     ln -sf $FILE ${TMP_DIR}/$FILE2
     ln -sf $FILE ${TMP_DIR}/$FILE
     ln -sf ${FC_DIR}/${NEXT_DATE}/wrfout_d${DOMAIN}_${FILE_DATE} ${TMP_DIR}/$FILE3
     pwd
     echo ""
     cp  $FILE $FILE2
     cp  ${FC_DIR}/${NEXT_DATE}/wrfout_d${DOMAIN}_${FILE_DATE} $FILE3 
      echo "ln -sf $FILE $FILE1"
      echo "ln -sf $FILE $FILE2"
      echo "ln -sf ${FC_DIR}/${NEXT_DATE}/wrfout_d${DOMAIN}_${FILE_DATE} $FILE3"
   fi

   if [[ $BE_METHOD == ENS ]]; then
      typeset -Z3 count
      count=1 
      while [[ $count -le ${NE} ]];do
         echo "ln -sf ${FC_DIR}/${DATE}/${DATE}.e${count}/wrfout_d${DOMAIN}_${FILE_DATE} ${DATE}.e${count}"
         #ln -sf ${FC_DIR}/${DATE}/${DATE}.e${count}/wrfout_d${DOMAIN}_${FILE_DATE} ${DATE}.e${count}
         ln -sf ${FC_DIR}/${DATE}.e${count}/wrfout_d${DOMAIN}_${FILE_DATE} ${DATE}.e${count}
         (( count += 1 ))
      done
   fi

   # copy or create namelist file
   if [[ -e ${WORK_DIR}/namelist.template ]]; then
      echo "using namelist.template ..." 
      rm -f namelist.input
      sed -e s/_START_DATE_/$DATE0/g ${WORK_DIR}/namelist.template > namelist.tmp
      sed -e s/_END_DATE_/$DATE0/g namelist.tmp > namelist.input
      rm -f namelist.tmp
#   else
#      cp -v ${WORK_DIR}/namelist.input .
#      echo "no template file namelist.template available ${WORK_DIR}/namelist.template"
#      echo "by default, the START_DATE and END_DATE comes from the namelist.input"
   fi

   ln -fs ${BUILD_DIR}/gen_be_stage0.exe .
   pwd
   echo "./gen_be_stage0.exe ${BE_METHOD} ${FCST_TIME} $NE $FILE1"
   ./gen_be_stage0.exe ${BE_METHOD} ${FCST_TIME} $NE $FILE1 # > gen_be_stage0.${FCST_TIME}.log 2>&1
   pwd
   filename='mesh_grid.nc'
   if [[ ! -e ${WORK_DIR}/${filename}  ]] ; then cp ${filename} ${WORK_DIR}/${filename}; fi
   filename='mesh_grid_fulldomain.nc'
   if [[ ! -e ${WORK_DIR}/${filename}  ]] ; then cp ${filename} ${WORK_DIR}/${filename}; fi
   filename='standard_variables.txt'
   if [[ ! -e ${WORK_DIR}/${filename}  ]] ; then cp ${filename} ${WORK_DIR}/${filename}; fi
   filename='control_variables.txt'
   if [[ ! -e ${WORK_DIR}/${filename}  ]] ; then cp ${filename} ${WORK_DIR}/${filename}; fi
   filename='bin.nc'
   if [[ ! -e ${WORK_DIR}/${filename}  ]] ; then cp ${filename} ${WORK_DIR}/${filename}; fi
   if [[ ! -d ${WORK_DIR}/mask ]]; then mkdir ${WORK_DIR}/mask; fi
   mv mask/* ${WORK_DIR}/mask   

   #  Tidy:
   #mv pert.${FCST_TIME}* ${STAGE0_DIR}
   mv pert.${FCST_TIME}* ${WORK_DIR}
#   mv raincl/raincl.${FCST_TIME}* ${WORK_DIR}/raincl/
   mv gen_be_stage0.${FCST_TIME}.log ${STAGE0_DIR}
   # rm -rf $TMP_DIR 2> /dev/null

   echo $DATE $FILE ${FC_DIR}/${NEXT_DATE}/wrfout_d${DOMAIN}_${FILE_DATE}
   export DATE=$(${BUILD_DIR}/da_advance_time.exe $DATE $INTERVAL)
   export FCST_TIME=$(${BUILD_DIR}/da_advance_time.exe $DATE $FCST_RANGE1)

done     # End loop over dates.

exit 0

