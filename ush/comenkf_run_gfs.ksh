#!/bin/ksh
#####################################################
# machine set up (users should change this part)
#####################################################

set -x

#--------------------------------------------------
  GSIPROC=32
  ARCH='LINUX_LSF'

#####################################################
##case set up (users should change this part)
#####################################################
#
# GFSCASE = cases used for DTC test
#           T574, T254, T126, T62, enkf_glb_t254 
# ANAL_TIME= analysis time  (YYYYMMDDHH)
# WORK_ROOT= working directory, where GSI runs
# PREPBURF = path of PreBUFR conventional obs
# BK_ROOT  = path of background files
# OBS_ROOT = path of observations files
# FIX_ROOT = path of fix files
# ENKF_EXE  = path and name of the EnKF executable 
  ANAL_TIME=2014092918
  GFSCASE=T62
  JOB_DIR=the_job_directory
     #normally you put run scripts here and submit jobs form here, require a copy of enkf_gfs.x at this directory
  RUN_NAME=a_descriptive_run_name_such_as_case05_3denvar_etc
  OBS_ROOT=the_directory_where_observation_files_are_located
  BK_ROOT=the_directory_where_background_files_are_located
  GSI_ROOT=the_comgsi_main directory where src/ ush/ fix/ etc are located
  CRTM_ROOT=the_CRTM_directory
  diag_ROOT=the_observer_directory_where_diag_files_exist
  WORK_ROOT=${JOB_DIR}/${RUN_NAME}
  FIX_ROOT=${GSI_ROOT}/fix
  ENKF_EXE=${JOB_DIR}/enkf_gfs.x
  ENKF_NAMELIST=${GSI_ROOT}/ush/comenkf_namelist_gfs.sh

# Note:  number of pe >= NMEM_ENKF
NMEM_ENKF=10
LEVS=64
NVARS=5

# Set the JCAP resolution which you want.
# All resolutions use LEVS=64
if [[ "$GFSCASE" = "T62" ]]; then
  JCAP=62
  JCAP_B=62
elif [[ "$GFSCASE" = "T126" ]]; then
  JCAP=126
  JCAP_B=126
elif [[ "$GFSCASE" = "enkf_glb_t254" ]]; then
  JCAP=254
  JCAP_B=254
elif [[ "$GFSCASE" = "T254" ]]; then
  JCAP=254
  JCAP_B=574
elif [[ "$GFSCASE" = "T574" ]]; then
  JCAP=574
  JCAP_B=1534
else
   echo "INVALID case = $GFSCASE"
   exit
fi

# Given the requested resolution, set dependent resolution parameters
if [[ "$JCAP" = "382" ]]; then
   LONA=768
   LATA=384
   DELTIM=180
   resol=1
elif [[ "$JCAP" = "574" ]]; then
   LONA=1152
   LATA=576
   DELTIM=1200
   resol=2
elif [[ "$JCAP" = "254" ]]; then
   LONA=512
   LATA=256
   DELTIM=1200
   resol=2
elif [[ "$JCAP" = "126" ]]; then
   LONA=256
   LATA=128
   DELTIM=1200
   resol=2
elif [[ "$JCAP" = "62" ]]; then
   LONA=192
   LATA=94
   DELTIM=1200
   resol=2
else
   echo "INVALID JCAP = $JCAP"
   exit
fi
NLAT=` expr $LATA + 2 `

ncp=/bin/cp
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
PDYa=`echo $ANAL_TIME | cut -c1-8`
cyca=`echo $ANAL_TIME | cut -c9-10`
gdate=`date -u -d "$PDYa $cyca -6 hour" +%Y%m%d%H` #6hr ago

PDYg=`echo $gdate | cut -c1-8`
cycg=`echo $gdate | cut -c9-10`
prefix_tbc=gdas1.t${cycg}z

# Directories for test case
dirtbc=$BK_ROOT

# Fixed files
ANAVINFO=${diag_ROOT}/anavinfo
CONVINFO=${diag_ROOT}/convinfo
SATINFO=${diag_ROOT}/satinfo
SCANINFO=${diag_ROOT}/scaninfo
OZINFO=${diag_ROOT}/ozinfo
LOCINFO=${diag_ROOT}/hybens_info

# Set up workdir
rm -rf $WORK_ROOT
mkdir -p $WORK_ROOT
cd $WORK_ROOT

#Build EnKF namelist on-the-fly
. $ENKF_NAMELIST

#$ncp $ENKF_EXE        ./enkf.x
cp $ENKF_EXE enkf.x

cp $ANAVINFO        ./anavinfo
cp $CONVINFO        ./convinfo
cp $SATINFO         ./satinfo
cp $SCANINFO        ./scaninfo
cp $OZINFO          ./ozinfo
cp $LOCINFO         ./hybens_info

cp $diag_ROOT/satbias_in ./satbias_in
cp $diag_ROOT/satbias_pc ./satbias_pc
cp $diag_ROOT/satbias_angle ./satbias_angle

# get mean
#ln -s $BK_ROOT/sfg_${gdate}_fhr06_ensmean ./sfg_${gdate}_fhr06_ensmean
ln -s $BK_ROOT/sfg_${gdate}_fhr06_ensmean ./sfg_${ANAL_TIME}_fhr06_ensmean #enkf_gfs requires sfg_ValidTime.fhr06_ensmean
list="conv amsua_metop-a amsua_n18 amsua_n15"
for type in $list; do
   ln -s $diag_ROOT/diag_${type}_ges.ensmean .
done

# get each member
imem=1
while [[ $imem -le $NMEM_ENKF ]]; do
   member="mem"`printf %03i $imem`
   #ln -s $BK_ROOT/sfg_${gdate}_fhr06_${member} ./sfg_${gdate}_fhr06_${member}
   ln -s $BK_ROOT/sfg_${gdate}_fhr06_${member} ./sfg_${ANAL_TIME}_fhr06_${member} ##enkf_gfs requires sfg_ValidTime.fhr06_MEMBER
   list="conv amsua_metop-a amsua_n18 amsua_n15"
   for type in $list; do
      ln -s $diag_ROOT/diag_${type}_ges.${member} .
   done
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
