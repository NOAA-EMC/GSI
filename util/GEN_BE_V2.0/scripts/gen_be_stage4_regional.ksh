#!/bin/ksh
#-----------------------------------------------------------------------
# Script gen_be_stage4_regional.ksh
#
# Purpose: To calculate correlation lengthscales for 2D control variable fields. 
#
#-----------------------------------------------------------------------

#-----------------------------------------------------------------------------------
# Don't change anything below this line.
#-----------------------------------------------------------------------------------

export REL_DIR=${REL_DIR:-$HOME/trunk}
export GEN_BE_DIR=${GEN_BE_DIR:-$REL_DIR/gen_be}
export SCRIPTS_DIR=${SCRIPTS_DIR:-$GEN_BE_DIR/scripts}

. ${SCRIPTS_DIR}/gen_be_set_defaults.ksh

echo "---------------------------------------------------------------"
echo "Run Stage 4: Calculate horizontal covariances (regional lengthscales)." 
echo "---------------------------------------------------------------"

export BEGIN_CPU=$(date)
echo "Beginning CPU time: ${BEGIN_CPU}"

export TMP_DIR=${WORK_DIR}/gen_be_stage4_regional
if [[ ! -d $TMP_DIR ]]; then mkdir $TMP_DIR 2> /dev/null; fi

#===============================================================
for VARIABLE in $CONTROL_VARIABLES; do
#===============================================================
   # Check data exists:
   if [[ ! -d ${WORK_DIR}/$VARIABLE ]]; then
      echo "Input data directory ${WORK_DIR}/$VARIABLE is missing. Exiting"
      exit 1
   fi

   if [[ $VARIABLE == "ps" || $VARIABLE == "ps_u" || $VARIABLE == "ps_b" ]]; then
      let MAX_VINDEX=1
   else
      let MAX_VINDEX=$NUM_LEVELS
   fi

   let VINDEX=1
   let JOB=1

   #==============================================================
   while [[ $VINDEX -le $MAX_VINDEX ]]; do
   #==============================================================
      export TMP_DIR1=${TMP_DIR}/dir.${VARIABLE}${VINDEX}
      mkdir ${TMP_DIR1} 2> /dev/null
      cd ${TMP_DIR1}

      ln -sf ${WORK_DIR}/namelist.input .
      ln -sf ${WORK_DIR}/mesh_grid.nc .
      ln -sf ${WORK_DIR}/bins.nc .
      ln -sf ${WORK_DIR}/mask .
      #ln -sf ${BUILD_DIR}/gen_be_stage4.exe .
      cp ${BUILD_DIR}/gen_be_stage4.exe .
  pwd

  cat > gen_be_stage4_nl.nl << EOF
&gen_be_stage4_nl
    variable = '${VARIABLE}',
    k = ${VINDEX} /
EOF
pwd

   
#  echo "./gen_be_stage4.exe $VARIABLE $VINDEX" 
#  ./gen_be_stage4.exe $VARIABLE $VINDEX
      if $LOCAL; then
         echo "Submitting job for variable $VARIABLE and vertical index $VINDEX on local machine"
         # (./gen_be_stage4_regional.exe > gen_be_stage4_regional_${VARIABLE}_${VINDEX}.out 2>&1) &
         (./gen_be_stage4.exe $VARIABLE $VINDEX  gen_be_stage4_regional_${VARIABLE}_${VINDEX}.out 2>&1 &) 
         echo "./gen_be_stage4.exe $VARIABLE $VINDEX"
         pwd
      else
         export MACHINE=${MACHINES[$JOB]}
         logfile=${TMP_DIR1}/file.log
         echo "Submitting job for variable $VARIABLE and vertical index $VINDEX on $MACHINE"
cat > gen_be_stage4_regional.csh << EOF
#!/bin/csh
cd $TMP_DIR1
./gen_be_stage4.exe
EOF
chmod 755 gen_be_stage4_regional.csh 

#qsub -l nodes=1:ppn=4,walltime=$WALLTIME -N stage4 -j oe -o ${logfile}  ./gen_be_stage4_regional.csh > gen_be_stage4_regional_${VARIABLE}_${VINDEX}.out 2>&1 &
#  (rsh -n $MACHINE "cd $TMP_DIR1; ./gen_be_stage4_regional.exe > gen_be_stage4_regional_${VARIABLE}_${VINDEX}.out 2>&1") &

 ./gen_be_stage4_regional.csh ##  > gen_be_stage4_regional_${VARIABLE}_${VINDEX}.out 2>&1 

      sleep 2 # Create small gap between submissions to avoid overwriting output.
      fi

      if [[ $SL_METHOD -le 1 ]] ; then
         let VINDEX=$VINDEX+1
         let JOB=$JOB+1
      else
         let VINDEX=$VINDEX+$MAX_VINDEX
      fi

      if [[ $JOB -gt $NUM_JOBS || $VINDEX -gt $MAX_VINDEX ]]; then
         wait # Wait for current jobs to finish.
         let JOB=1
      fi
 
   #==============================================================
   done  # End loop over VINDEX.
   #==============================================================

#========================================================
done     # End loop over VARIABLE.
#========================================================


for VARIABLE in $CONTROL_VARIABLES; do

   if [[ $VARIABLE == "ps" || $VARIABLE == "ps_u" || $VARIABLE == "ps_b" ]]; then
      let MAX_VINDEX=1
   else
      let MAX_VINDEX=$NUM_LEVELS
   fi

   if [[ $SL_METHOD -le 1 ]] ; then
    if [[ $VARIABLE == "ps" || $VARIABLE == "ps_u" ]]; then
    SL_FILE=${TMP_DIR}/dir.${VARIABLE}${MAX_VINDEX}/sl_print.${VARIABLE}.0${MAX_VINDEX}
    else
    SL_FILE=${TMP_DIR}/dir.${VARIABLE}${MAX_VINDEX}/sl_print.${VARIABLE}.${MAX_VINDEX}
    fi  
   else
   SL_FILE=${TMP_DIR}/dir.${VARIABLE}1/sl_print.b001.${VARIABLE}
   fi
   
   while [[ ! -s ${SL_FILE} ]] ; do
   echo "Waiting for sl file: " ${SL_FILE}
   sleep 60

   done  # End loop over Waiting for lat sl
   
   let VINDEX=1
   if [[ $SL_METHOD -le 1 ]] ; then
      cp ${TMP_DIR}/dir.${VARIABLE}${VINDEX}/sl_* ${WORK_DIR}/${VARIABLE}/sl_print.${VARIABLE}
   else
      cp ${TMP_DIR}/dir.${VARIABLE}${VINDEX}/sl_print.b*${VARIABLE} ${WORK_DIR}/${VARIABLE}
   fi

   if [[ $SL_METHOD -le 1 ]] ; then
   # Collect files together:
   if [[ $MAX_VINDEX -gt 1 ]]; then
      let VINDEX=2
      while [[ $VINDEX -le $MAX_VINDEX ]]; do
         cat ${TMP_DIR}/dir.${VARIABLE}${VINDEX}/sl_* >> ${WORK_DIR}/${VARIABLE}/sl_print.${VARIABLE}
         let VINDEX=$VINDEX+1
      done
   fi
   fi

done     # End loop over VARIABLE.

export END_CPU=$(date)
echo "Ending CPU time: ${END_CPU}"

exit 0

