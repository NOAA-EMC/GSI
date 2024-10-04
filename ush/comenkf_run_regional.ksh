#!/bin/ksh
#####################################################
# machine set up (users should change this part)
#####################################################
#

set -x

#
# GSIPROC = processor number used for GSI analysis
#------------------------------------------------
  GSIPROC=32
  ARCH='LINUX_LSF'

# Supported configurations:
            # IBM_LSF,
            # LINUX, LINUX_LSF, LINUX_PBS,
            # DARWIN_PGI
#
#####################################################
# case set up (users should change this part)
#####################################################
#
# ANAL_TIME= analysis time  (YYYYMMDDHH)
# WORK_ROOT= working directory, where GSI runs
# PREPBURF = path of PreBUFR conventional obs
# OBS_ROOT = path of observations files
# FIX_ROOT = path of fix files
# ENKF_EXE  = path and name of the EnKF executable 
  ANAL_TIME=2014021300  #used by comenkf_namelist.sh
  JOB_DIR=the_job_directory
     #normally you put run scripts here and submit jobs form here, require a copy of enkf_wrf.x at this directory
  RUN_NAME=a_descriptive_run_name_such_as_case05_3denvar_etc
  OBS_ROOT=the_directory_where_observation_files_are_located
  BK_ROOT=the_directory_where_background_files_are_located
  GSI_ROOT=the_comgsi_main directory where src/ ush/ fix/ etc are located
  CRTM_ROOT=the_CRTM_directory
  diag_ROOT=the_observer_directory_where_diag_files_exist
  ENKF_EXE=${JOB_DIR}/enkf_wrf.x
  WORK_ROOT=${JOB_DIR}/${RUN_NAME}
  FIX_ROOT=${GSI_ROOT}/fix
  ENKF_NAMELIST=${GSI_ROOT}/ush/comenkf_namelist.sh

# ensemble parameters
#
  NMEM_ENKF=20
  BK_FILE_mem=${BK_ROOT}/wrfarw
  NLONS=129
  NLATS=70 
  NLEVS=50
  IF_ARW=.true.
  IF_NMM=.false.
  list="conv"
#  list="conv amsua_n18 mhs_n18 hirs4_n19"
#
#####################################################
# Users should NOT change script after this point
#####################################################
#

case $ARCH in
   'IBM_LSF')
      ###### IBM LSF (Load Sharing Facility)
      RUN_COMMAND="mpirun.lsf " ;;

   'LINUX')
      if [ $GSIPROC = 1 ]; then
         #### Linux workstation - single processor
         RUN_COMMAND=""
      else
         ###### Linux workstation -  mpi run
        RUN_COMMAND="mpirun -np ${GSIPROC} "
      fi ;;

   'LINUX_LSF')
      ###### LINUX LSF (Load Sharing Facility)
      RUN_COMMAND="mpirun.lsf " ;;

   'LINUX_PBS')
      #### Linux cluster PBS (Portable Batch System)
      RUN_COMMAND="mpirun -np ${GSIPROC} " ;;

   'DARWIN_PGI')
      ### Mac - mpi run
      if [ $GSIPROC = 1 ]; then
         #### Mac workstation - single processor
         RUN_COMMAND=""
      else
         ###### Mac workstation -  mpi run
         RUN_COMMAND="mpirun -np ${GSIPROC} -machinefile ~/mach "
      fi ;;

   * )
     print "error: $ARCH is not a supported platform configuration."
     exit 1 ;;
esac

# Given the analysis date, compute the date from which the
# first guess comes.  Extract cycle and set prefix and suffix
# for guess and observation data files
# gdate=`$ndate -06 $adate`
#gdate=$ANAL_TIME
#YYYYMMDD=`echo $adate | cut -c1-8`
#HH=`echo $adate | cut -c9-10`

# Fixed files
# CONVINFO=${FIX_ROOT}/global_convinfo.txt
# SATINFO=${FIX_ROOT}/global_satinfo.txt
# SCANINFO=${FIX_ROOT}/global_scaninfo.txt
# OZINFO=${FIX_ROOT}/global_ozinfo.txt
ANAVINFO=${diag_ROOT}/anavinfo
CONVINFO=${diag_ROOT}/convinfo
SATINFO=${diag_ROOT}/satinfo
SCANINFO=${diag_ROOT}/scaninfo
OZINFO=${diag_ROOT}/ozinfo
# LOCINFO=${FIX_ROOT}/global_hybens_locinfo.l64.txt

# Set up workdir
rm -rf $WORK_ROOT
mkdir -p $WORK_ROOT
cd $WORK_ROOT

cp $ENKF_EXE enkf.x

cp $ANAVINFO        ./anavinfo
cp $CONVINFO        ./convinfo
cp $SATINFO         ./satinfo
cp $SCANINFO        ./scaninfo
cp $OZINFO          ./ozinfo
# cp $LOCINFO         ./hybens_locinfo

cp $diag_ROOT/satbias_in ./satbias_in
cp $diag_ROOT/satbias_pc ./satbias_pc

# get mean
ln -s ${BK_FILE_mem}.ensmean ./firstguess.ensmean
for type in $list; do
   ln -s $diag_ROOT/diag_${type}_ges.ensmean .
done

# get each member
imem=1
while [[ $imem -le $NMEM_ENKF ]]; do
   member="mem"`printf %03i $imem`
   ln -s ${BK_FILE_mem}.${member} ./firstguess.${member}
   for type in $list; do
      ln -s $diag_ROOT/diag_${type}_ges.${member} .
   done
   (( imem = $imem + 1 ))
done

# Build the GSI namelist on-the-fly
. $ENKF_NAMELIST

# make analysis files
cp firstguess.ensmean analysis.ensmean
# get each member
imem=1
while [[ $imem -le $NMEM_ENKF ]]; do
   member="mem"`printf %03i $imem`
   cp firstguess.${member} analysis.${member}
   (( imem = $imem + 1 ))
done

#
###################################################
#  run  EnKF
###################################################
echo ' Run EnKF'

${RUN_COMMAND} ./enkf.x < enkf.nml > stdout 2>&1

##################################################################
#  run time error check
##################################################################
error=$?

if [ ${error} -ne 0 ]; then
  echo "ERROR: ${ENKF_EXE} crashed  Exit status=${error}"
  exit ${error}
fi

exit
