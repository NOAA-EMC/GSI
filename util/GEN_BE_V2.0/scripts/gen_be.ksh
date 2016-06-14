#!/bin/ksh
#-----------------------------------------------------------------------
# Purpose : Create BE statistics from input perturbation files.
# Run Stage 0: Calculate ensemble perturbations from model forecasts.
# Run Stage 1: Read "standard fields", and remove time/ensemble/area mean.
# Run Stage 1: Read "standard fields", and remove time/ensemble/area mean.
# Run Stage 2: Calculate regression coefficients.
# Run Stage 2a: Calculate control variable fields.
# Run Stage 3: Read 3D control variable fields, and calculate vertical covariances.
# Run Stage 4: Calculate horizontal covariances.
# Finally, gather data together into a single BE file.
#-----------------------------------------------------------------------

#-----------------------------------------------------------------------------------
# Don't change anything below this line.
#-----------------------------------------------------------------------------------

#-----------------------------------------------------------------------
# [1] Set defaults for required environment variables:
#-----------------------------------------------------------------------

export REL_DIR=${REL_DIR:-$HOME/trunk}
export GEN_BE_DIR=${GEN_BE_DIR:-$REL_DIR/gen_be}
export SCRIPTS_DIR=${SCRIPTS_DIR:-$GEN_BE_DIR/scripts}

. ${SCRIPTS_DIR}/gen_be_set_defaults.ksh
echo "${SCRIPTS_DIR}/gen_be_set_defaults.ksh"

if [[ ! -d $RUN_DIR ]]; then mkdir -p $RUN_DIR; fi
if [[ ! -d $STAGE0_DIR ]]; then mkdir -p $STAGE0_DIR; fi

mkdir -p $WORK_DIR
cd $WORK_DIR

#-----------------------------------------------------------------------
# Prerequies, create namelist 
#-----------------------------------------------------------------------

if [[ -e $WORK_DIR/namelist.input ]]; then

  echo "$WORK_DIR/namelist.input exists, will not create the namelist"

else

  echo "create the namelist "

  if [[ -e ${SCRIPTS_DIR}/namelist.template ]]; then
      echo "using namelist.template ..." 
      rm -f namelist.input
      sed -e s/_INTERVAL_/$INTERVAL/g ${SCRIPTS_DIR}/namelist.template > namelist.input
      sed -e s/_NE_/$NE/g namelist.input > namelist.temp
      sed -e s/_BE_METHOD_/$BE_METHOD/g namelist.temp > namelist.input
      cp namelist.input namelist.template
      rm -f namelist.temp

      #sed -e s/_END_DATE_/_START_DATE_/g namelist.input > namelist.temp
      #sed -e s/_START_DATE_/$START_DATE/g namelist.temp > namelist.input

      sed -e s/_START_DATE_/$START_DATE/g namelist.input > namelist.temp
      sed -e s/_END_DATE_/$END_DATE/g namelist.temp > namelist.input
      rm -f namelist.temp

  else
      echo "no template file namelist.template available ${SCRIPTS_DIR}/namelist.template"
      exit -1
  fi

fi

#------------------------------------------------------------------------
# Run Stage 0: Calculate ensemble perturbations from model forecasts.
#------------------------------------------------------------------------

echo 
echo $(date) "Start"
echo "GEN_BE_DIR is" $GEN_BE_DIR $(svnversion $GEN_BE_DIR)

if $RUN_GEN_BE_STAGE0; then

   echo "---------------------------------------------------------------"
   echo "Run Stage 0: Calculate ensemble perturbations from model forecasts."
   echo "---------------------------------------------------------------"

   export BEGIN_CPU=$(date)
   echo "Beginning CPU time: ${BEGIN_CPU}"

   filename='standard_variables.txt'
   if [[  -e ${WORK_DIR}/${filename}  ]] ; then rm -f ${WORK_DIR}/${filename}; fi
   filename='control_variables.txt'
   if [[  -e ${WORK_DIR}/${filename}  ]] ; then rm -f ${WORK_DIR}/${filename}; fi


#   $SCRIPTS_DIR/gen_be_stage0_wrf.ksh #> gen_be_stage0.log 2>&1
   $SCRIPTS_DIR/gen_be_stage0.ksh #> gen_be_stage0.log 2>&1

   RC=$?
   if [[ $RC != 0 ]]; then
      echo "Stage 0 for $MODEL failed with error" $RC
      echo "stage 0" > $RUN_DIR/FAIL
      exit 1
   fi

   export END_CPU=$(date)
   echo "Ending CPU time: ${END_CPU}"

fi

#------------------------------------------------------
# step needed after stage0
#------------------------------------------------------
nCV=0
STANDARD_VARIABLES=""
CONTROL_VARIABLES=""

# initilize standard variables
if [[ -e  $WORK_DIR/standard_variables.txt ]] ; then
   nb_var=`sed '1q' standard_variables.txt`
   nb_var=$(($nb_var+1))
   var=`sed ${nb_var}'q' standard_variables.txt`
   nvar=-1
   for CV in $var; do
      if [[ $nvar != -1 ]]; then
         STANDARD_VARIABLES="$STANDARD_VARIABLES $CV"
      fi
      nvar=$(($nvar+1))
   done
   echo "$nvar STANDARD_VARIABLES $STANDARD_VARIABLES"
else
      echo "$WORK_DIR/standard_variables.txt is missing"
      exit
fi

# initilize control variables
if [[ -e  $WORK_DIR/standard_variables.txt ]] ; then
   nb_var=`sed '1q' control_variables.txt`
   nb_var=$(($nb_var+1))
   var=`sed ${nb_var}'q' control_variables.txt`
   nvar=-1
   for CV in $var; do
      if [[ $nvar = -1 ]]; then
         nCV=$CV
      else
         CONTROL_VARIABLES="$CONTROL_VARIABLES $CV"
      fi
      nvar=$(($nvar+1))
   done
   echo "$nvar CONTROL_VARIABLES $CONTROL_VARIABLES"
else
      echo "$WORK_DIR/control_variables.txt is missing"
      exit
fi

for SV in $STANDARD_VARIABLES; do mkdir -p $SV; done
for SV in $CONTROL_VARIABLES; do mkdir -p ${SV}; done

#------------------------------------------------------------------------
#  Run Stage 1: Read "standard fields", and remove time/ensemble/area mean.
#------------------------------------------------------------------------

if $RUN_GEN_BE_STAGE1; then
   echo "---------------------------------------------------------------"
   echo "Run Stage 1: Read "standard fields", and remove time/ensemble/area mean."
   echo "---------------------------------------------------------------"

   export BEGIN_CPU=$(date)
   echo "Beginning CPU time: ${BEGIN_CPU}"

   
   ln -sf ${BUILD_DIR}/gen_be_stage1.exe .
   ./gen_be_stage1.exe > gen_be_stage1.log 2>&1
   RC=$?
   if [[ $RC != 0 ]]; then
      echo "Stage 1 failed with error" $RC
      echo "stage 1" > $RUN_DIR/FAIL
      exit 1
   fi

   export END_CPU=$(date)
   echo "Ending CPU time: ${END_CPU}"
fi

#------------------------------------------------------------------------
#  Run Stage 2: Calculate regression coefficients.
#------------------------------------------------------------------------

if $RUN_GEN_BE_STAGE2; then
   echo "---------------------------------------------------------------"
   echo "Run Stage 2: Calculate regression coefficients."
   echo "---------------------------------------------------------------"

   export BEGIN_CPU=$(date)
   echo "Beginning CPU time: ${BEGIN_CPU}"

   ln -sf ${BUILD_DIR}/gen_be_stage2.exe .
   ./gen_be_stage2.exe > gen_be_stage2.log 2>&1
   RC=$?
   if [[ $RC != 0 ]]; then
      echo "Stage 2 failed with error" $RC
      echo "stage 2" > $RUN_DIR/FAIL
      #exit 1
   fi

   filename="plot_regcoeff.ncl"
   cp ${GRAPHICS_DIR}/$filename . 
   ncl $filename > /dev/null
   dirout="./plots/regcoeff"
   mkdir -p $dirout
   mv regcoeff*${GRAPHIC_WORKS} $dirout

   export END_CPU=$(date)
   echo "Ending CPU time: ${END_CPU}"

fi


#------------------------------------------------------------------------
#  Run Stage 3: Read 3D control variable fields, and calculate vertical covariances.
#------------------------------------------------------------------------

if $RUN_GEN_BE_STAGE3; then

   echo "---------------------------------------------------------------"
   echo "Run Stage 3: Read 3D control variable fields, and calculate vertical covariances."
   echo "---------------------------------------------------------------"

   export BEGIN_CPU=$(date)
   echo "Beginning CPU time: ${BEGIN_CPU}"

   ln -sf ${BUILD_DIR}/gen_be_stage3.exe .
   pwd
   echo "CV in $CONTROL_VARIABLES"

   for CV in $CONTROL_VARIABLES; do

      echo "gen_be_stage3.exe $CV"
      pwd
      ./gen_be_stage3.exe $CV  > gen_be_stage3.${CV}.log 2>&1

      RC=$?
      if [[ $RC != 0 ]]; then
         echo "Stage 3 for $CV failed with error" $RC
         echo "stage 3" > $RUN_DIR/FAIL
      #   exit 1
      fi
      
      #if [[ $CV = "ps_u" || $CV = "ps" ]];then
      #   print "no auto_covar computed for variable 2d $CV"
      #else
      #   filename="plot_stage3_auto_covar.ncl"
      #   m4 -D_VARIABLE_=$CV ${GRAPHICS_DIR}/$filename > $filename
      #   ncl $filename > /dev/null
      #   dirout="./plots/autocovar/$CV"
      #   mkdir -p $dirout
      #   mv stage3_auto_covar*${GRAPHIC_WORKS} $dirout             
      #fi

   done

   export END_CPU=$(date)
   echo "Ending CPU time: ${END_CPU}"

fi

#------------------------------------------------------------------------
#  Run Stage 4: Calculate horizontal covariances.
#------------------------------------------------------------------------

if $RUN_GEN_BE_STAGE4; then
   export BEGIN_CPU=$(date)
   echo "Beginning CPU time: ${BEGIN_CPU}"

   if $GLOBAL; then    
      echo "---------------------------------------------------------------"
      echo "Run Stage 4: Calculate horizontal covariances (global power spectra)."
      echo "---------------------------------------------------------------"

      ${SCRIPTS_DIR}/gen_be_stage4_global.ksh > gen_be_stage4_global.log 2>&1

   else
      echo "---------------------------------------------------------------"
      echo "Run Stage 4: Calculate horizontal covariances (regional lengthscales)."
      echo "---------------------------------------------------------------"

      ${SCRIPTS_DIR}/gen_be_stage4_regional.ksh #> gen_be_stage4_regional.log 2>&1
      RC=$?
      if [[ $RC != 0 ]]; then
         echo "Stage 4 failed with error" $RC
         echo "stage 4" > $RUN_DIR/FAIL
         exit 1
      fi
   fi 

   export END_CPU=$(date)
   echo "Ending CPU time: ${END_CPU}"
fi

#------------------------------------------------------------------------
#  Finally, gather data together into a single BE file:
#------------------------------------------------------------------------

if $RUN_GEN_BE_DIAGS; then
   ln -sf ${BUILD_DIR}/gen_be_diags.exe .

   ./gen_be_diags.exe > gen_be_diags.log 2>&1
   RC=$?
   if [[ $RC != 0 ]]; then
      echo "Stage gen_be_diags failed with error" $RC
      echo "gen_be_diags" > $RUN_DIR/FAIL
      exit 1
   fi

   export END_CPU=$(date)
   echo "Ending CPU time: ${END_CPU}"
fi

#------------------------------------------------------------------------
#  Read BE file to check data packed correctly, and write plot diagnostics.
#------------------------------------------------------------------------

if $RUN_GEN_BE_DIAGS_READ; then

   ln -sf ${BUILD_DIR}/gen_be_diags_read.exe .
   ./gen_be_diags_read.exe > gen_be_diags_read.log 2>&1

   ifort_ls=300
   ifort_ev=200
   for CV in $CONTROL_VARIABLES; do

      cp -v fort.${ifort_ls} length_scale_${CV}.dat
      (( ifort_ls = ifort_ls + 1 ))
      cp -v fort.${ifort_ev} global_eigenvector_${CV}.dat
      (( ifort_ev = ifort_ev + 1 ))
      cp -v fort.${ifort_ev} global_eigenvalues_${CV}.dat
      (( ifort_ev = ifort_ev + 1 ))
      cp -v fort.${ifort_ev} local_eigenvector_${CV}.dat
      (( ifort_ev = ifort_ev + 1 ))
      cp -v fort.${ifort_ev} local_eigenvalues_${CV}.dat
      (( ifort_ev = ifort_ev + 1 ))

      filename="gen_be_lengthscale_byvariable.ncl"
      m4 -D_VARIABLE_=$CV ${GRAPHICS_DIR}/$filename > $filename
      ncl $filename > /dev/null
      dirout="./plots/lengthscale/"
      mkdir -p $dirout
      mv gen_be_lengthscale*${GRAPHIC_WORKS} $dirout

   done

   filename="gen_be_global_evals.ncl"
   m4 -D_VARIABLE_=$CV ${GRAPHICS_DIR}/$filename > $filename
   ncl $filename > /dev/null
   dirout="./plots/globalevals"
   mkdir -p $dirout
   filename="gen_be_global_evalsbyvariable.ncl"
   m4 -D_VARIABLE_=$CV ${GRAPHICS_DIR}/$filename > $filename
   ncl $filename > /dev/null
   mv gen_be_globalevals*${GRAPHIC_WORKS} $dirout


   filename="gen_be_global_evecs.ncl"
   m4 -D_VARIABLE_=$CV ${GRAPHICS_DIR}/$filename > $filename
   ncl $filename > /dev/null
   dirout="./plots/globalevecs"
   mkdir -p $dirout
   filename="gen_be_global_evecsbyvariable.ncl"
   m4 -D_VARIABLE_=$CV ${GRAPHICS_DIR}/$filename > $filename
   ncl $filename > /dev/null
   mv gen_be_globalevecs*${GRAPHIC_WORKS} $dirout

   

   filename="gen_be_lengthscales.ncl"
   m4 -D_VARIABLE_=$CV ${GRAPHICS_DIR}/$filename > $filename
   ncl $filename > /dev/null
   dirout="./plots/lengthscale"
   mkdir -p $dirout
   mv gen_be_lengthscale*${GRAPHIC_WORKS} $dirout

   RC=$?
   if [[ $RC != 0 ]]; then
      echo "Stage gen_be_diags_read failed with error" $RC
      echo "gen_be_diags_read" > $RUN_DIR/FAIL
      exit 1
   fi

fi

#------------------------------------------------------------------------
#  Calculate multivariate regression diagnostics:
#------------------------------------------------------------------------

if $RUN_GEN_BE_MULTICOV; then

   for CV in $STANDARD_VARIABLES ; do

     if [[ $CV == "fullflds" || $CV == "ps_u" || $CV == "ps" || $CV == "psi" || $CV == "vor" ]]; then
         echo "variable $CV can not be process by GEN_BE_MULTICOV"
     else
         echo "RUN_GEN_BE_MULTICOV $CV"
         VARIABLE1=${CV}_u
         VARIABLE2=${CV} 
         ln -sf ${BUILD_DIR}/gen_be_cov3d.exe .
         
         echo "./gen_be_cov3d.exe ${VARIABLE1} ${VARIABLE2}"
         
         ./gen_be_cov3d.exe ${VARIABLE1} ${VARIABLE2} # > gen_be_cov3d.$VARIABLE1.$VARIABLE1.log 2>&1
         m4 -D_VAR1_=$VARIABLE1 -D_VAR2_=$VARIABLE2 ${GRAPHICS_DIR}/gen_be_vert_3d_corr.ncl > gen_be_vert_3d_corr.ncl
         ncl gen_be_vert_3d_corr.ncl > /dev/null
         mkdir -p ./plots/vert3dcorr
         mv gen_be_vertcorr*${GRAPHIC_WORKS} ./plots/vert3dcorr 

         RC=$?
         if [[ $RC != 0 ]]; then
           echo "gen_be_cov3d failed with error" $RC
           echo "gen_be_cov3d " > $RUN_DIR/FAIL
         exit 1
         fi
     fi

   done

fi


#------------------------------------------------------------------------
#  Calculate histogram diagnostics:
#------------------------------------------------------------------------
#echo "RUN_GEN_BE_HISTOG $RUN_GEN_BE_HISTOG"
if $RUN_GEN_BE_HISTOG; then
   echo "---------------------------------------------------------------"
   echo "Run Hist: Diagnose pdf for perturbations"
   echo "---------------------------------------------------------------"

   export BEGIN_CPU=$(date)
   echo "Beginning CPU time: ${BEGIN_CPU}"

   ln -sf ${BUILD_DIR}/gen_be_hist.exe .

   for CV in $CONTROL_VARIABLES; do

        if [[ $CV = "ps_u" ]];then
           print "no histogram for variable 2d $CV"
        else
      	./gen_be_hist.exe $CV #> gen_be_hist.${CV}.log 2>&1

      	RC=$?
      	if [[ $RC != 0 ]]; then
         	echo "Hist for $CV failed with error" $RC
         	echo "Hist" > $RUN_DIR/FAIL
         	exit 1
      	fi

        filename="gen_be_hist_bylev.ncl"
        m4 -D_VARIABLE_=$CV ${GRAPHICS_DIR}/$filename > $filename
        ncl $filename > /dev/null
        mkdir -p ./plots/hist/${CV}
        mv hist_${CV}*${GRAPHIC_WORKS} ./plots/hist/${CV}

        fi

   done

   export END_CPU=$(date)
   echo "Ending CPU time: ${END_CPU}"
fi


#-----------------------------------------------------------------------

if $RUN_GEN_BE_VARIANCE_CONTRIB; then

   echo "---------------------------------------------------------------"
   echo " RUN_GEN_BE_VARIANCE_CONTRIB "
   echo "---------------------------------------------------------------"

   export BEGIN_CPU=$(date)
   echo "Beginning CPU time: ${BEGIN_CPU}"

   ln -sf ${BUILD_DIR}/gen_be_variance_contrib.exe .
   ./gen_be_variance_contrib.exe > variance_contrib.log 2>&1

   RC=$?
   if [[ $RC != 0 ]]; then
      echo "gen_be_graphics failed with error" $RC
      echo "gen_be_graphics" > $RUN_DIR/FAIL
      exit 1
   fi

   export END_CPU=$(date)
   echo "Ending CPU time: ${END_CPU}"

   for CV in $STANDARD_VARIABLES; do

      if [[ $CV == "fullflds" || $CV == "ps" ]]; then
         echo "no plot cross covariance for ps and fullflds"
      else
         filename=${CV}_variance_contrib.txt
         if [[ -e $filename ]]; then

            # plot cross covar 3d (bin,k,k)
            filename="plot_vertcovar.ncl"
            m4 -D_VARIABLE_=$CV ${GRAPHICS_DIR}/$filename > $filename
            ncl $filename > /dev/null
            mkdir -p ./plots/crosscovar
            mv cross_covar_${CV}*${GRAPHIC_WORKS} ./plots/crosscovar

            # plot contrib of different variance for each CV
            filename="plot_explained_ratio.ncl"
            m4 -D_VARIABLE_=$CV ${GRAPHICS_DIR}/$filename > $filename
            ncl $filename > /dev/null
            mkdir -p ./plots/variance_contrib            
            mv explained_*${GRAPHIC_WORKS} ./plots/variance_contrib            

         fi
      fi

   done

fi

exit

# Preserve the interesting log files
mv $WORK_DIR/*log $RUN_DIR
mv $STAGE0_DIR/*log $RUN_DIR
mv $WORK_DIR/be.dat $RUN_DIR
mv $WORK_DIR/*.pdf  $RUN_DIR
mv $WORK_DIR/*.dat  $RUN_DIR
mv $WORK_DIR/fort.* $RUN_DIR

if $CLEAN; then rm -rf $WORK_DIR; fi

echo
echo $(date) "Finished"

touch $RUN_DIR/SUCCESS

exit 0
