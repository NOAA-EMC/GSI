#!/bin/ksh
#####################################################
# machine set up (users should change this part)
#####################################################


set -x
# GSIPROC = processor number used for GSI analysis
#------------------------------------------------
  GSIPROC=1
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
# GFSCASE = cases used for DTC test
#           T574, T254, T126, T62, enkf_glb_t62
# ANAL_TIME= analysis time  (YYYYMMDDHH)
# WORK_ROOT= working directory, where GSI runs
# PREPBURF = path of PreBUFR conventional obs
# BK_ROOT  = path of background files
# OBS_ROOT = path of observations files
# FIX_ROOT = path of fix files
# GSI_EXE  = path and name of the gsi executable 
  ANAL_TIME=2014080400
  GFSCASE=T62
  JOB_DIR=the_job_directory
     #normally you put run scripts here and submit jobs form here, require a copy of gsi.x at this directory
  RUN_NAME=a_descriptive_run_name_such_as_case05_3denvar_etc
  OBS_ROOT=the_directory_where_observation_files_are_located
  BK_ROOT=the_directory_where_background_files_are_located
  GSI_ROOT=the_comgsi_main directory where src/ ush/ fix/ etc are located
  CRTM_ROOT=the_CRTM_directory
  GSI_EXE=${JOB_DIR}/gsi.x  #assume you have a copy of gsi.x here
  WORK_ROOT=${JOB_DIR}/${RUN_NAME}
  FIX_ROOT=${GSI_ROOT}/fix
  GSI_NAMELIST=${GSI_ROOT}/ush/comgsi_namelist_gfs.sh
  PREPBUFR=${OBS_ROOT}/prepbufr
  FIX_ROOT=${GSI_ROOT}/fix
#
#  ENS_ROOT=the_directory_where_ensemble_backgrounds_are_located
#------------------------------------------------
# if_clean = clean  : delete temperal files in working directory (default)
#            no     : leave running directory as is (this is for debug only)
  if_clean=clean

# if_observer = Yes  : only used as observation operater for enkf
# no_member     number of ensemble members
# BK_FILE_mem   path and base for ensemble members
  if_observer=No  # Yes ,or, No -- case sensitive!!!
  no_member=10
  PDYa=`echo $ANAL_TIME | cut -c1-8`
  cyca=`echo $ANAL_TIME | cut -c9-10`
  gdate=`date -u -d "$PDYa $cyca -6 hour" +%Y%m%d%H` #guess date is 6hr ago
  BK_FILE_mem=${BK_ROOT}/sfg_${gdate}

#
# Set the JCAP resolution which you want.
# All resolutions use LEVS=64
if [[ "$GFSCASE" = "T62" ]]; then
  JCAP=62
  JCAP_B=62
elif [[ "$GFSCASE" = "T126" ]]; then
  JCAP=126
  JCAP_B=126
elif [[ "$GFSCASE" = "enkf_glb_t62" ]]; then
  JCAP=62
  JCAP_B=62
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
  LEVS=64
#
#
  BYTE_ORDER=Big_Endian
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


##################################################################################
# Check GSI needed environment variables are defined and exist
#
 
# Make sure ANAL_TIME is defined and in the correct format
if [ ! "${ANAL_TIME}" ]; then
  echo "ERROR: \$ANAL_TIME is not defined!"
  exit 1
fi

# Make sure WORK_ROOT is defined and exists
if [ ! "${WORK_ROOT}" ]; then
  echo "ERROR: \$WORK_ROOT is not defined!"
  exit 1
fi

# Make sure the background file exists
if [ ! -r "${BK_ROOT}" ]; then
  echo "ERROR: ${BK_ROOT} does not exist!"
  exit 1
fi

# Make sure OBS_ROOT is defined and exists
if [ ! "${OBS_ROOT}" ]; then
  echo "ERROR: \$OBS_ROOT is not defined!"
  exit 1
fi
if [ ! -d "${OBS_ROOT}" ]; then
  echo "ERROR: OBS_ROOT directory '${OBS_ROOT}' does not exist!"
  exit 1
fi

# Set the path to the GSI static files
if [ ! "${FIX_ROOT}" ]; then
  echo "ERROR: \$FIX_ROOT is not defined!"
  exit 1
fi
if [ ! -d "${FIX_ROOT}" ]; then
  echo "ERROR: fix directory '${FIX_ROOT}' does not exist!"
  exit 1
fi

# Set the path to the CRTM coefficients 
if [ ! "${CRTM_ROOT}" ]; then
  echo "ERROR: \$CRTM_ROOT is not defined!"
  exit 1
fi
if [ ! -d "${CRTM_ROOT}" ]; then
  echo "ERROR: fix directory '${CRTM_ROOT}' does not exist!"
  exit 1
fi


# Make sure the GSI executable exists
if [ ! -x "${GSI_EXE}" ]; then
  echo "ERROR: ${GSI_EXE} does not exist!"
  exit 1
fi

# Check to make sure the number of processors for running GSI was specified
if [ -z "${GSIPROC}" ]; then
  echo "ERROR: The variable $GSIPROC must be set to contain the number of processors to run GSI"
  exit 1
fi

################################################################################
## Given the analysis date, compute the date from which the
# first guess comes.  Extract cycle and set prefix
# for guess and observation data files
#hha=`echo $ANAL_TIME | cut -c9-10`
#hhg=`echo $GUESS_TIME | cut -c9-10`

#
##################################################################################
# Create the ram work directory and cd into it

workdir=${WORK_ROOT}
echo " Create working directory:" ${workdir}

if [ -d "${workdir}" ]; then
  rm -rf ${workdir}
fi
mkdir -p ${workdir}
cd ${workdir}

#
##################################################################################
# Set some parameters for use by the GSI executable and to build the namelist
echo " Build the namelist "

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

# CO2 namelist and file decisions
ICO2=${ICO2:-0}
if [ $ICO2 -gt 0 ] ; then
        # Copy co2 files to $workdir
        co2dir=${FIX_ROOT}
        yyyy=`echo $ANAL_TIME | cut -c1-4`
        rm ./global_co2_data.txt
        co2=$co2dir/global_co2.gcmscl_$yyyy.txt
        while [ ! -s $co2 ] ; do
                ((yyyy-=1))
                co2=$co2dir/global_co2.gcmscl_$yyyy.txt
        done
        if [ -s $co2 ] ; then
                cp $co2 ./global_co2_data.txt
        fi
        if [ ! -s ./global_co2_data.txt ] ; then
                echo "\./global_co2_data.txt" not created
                exit 1
        fi
fi
#CH4 file decision
ICH4=${ICH4:-0}
if [ $ICH4 -gt 0 ] ; then
#        # Copy ch4 files to $workdir
        ch4dir=${FIX_ROOT}
        yyyy=`echo $ANAL_TIME | cut -c1-4`
        rm ./ch4globaldata.txt
        ch4=$ch4dir/global_ch4_esrlctm_$yyyy.txt
        while [ ! -s $ch4 ] ; do
                ((yyyy-=1))
                ch4=$ch4dir/global_ch4_esrlctm_$yyyy.txt
        done
        if [ -s $ch4 ] ; then
                cp $ch4 ./ch4globaldata.txt
        fi
        if [ ! -s ./ch4globaldata.txt ] ; then
                echo "\./ch4globaldata.txt" not created
                exit 1
        fi
fi
IN2O=${IN2O:-0}
if [ $IN2O -gt 0 ] ; then
#        # Copy ch4 files to $workdir
        n2odir=${FIX_ROOT}
        yyyy=`echo $ANAL_TIME | cut -c1-4`
        rm ./n2oglobaldata.txt
        n2o=$n2odir/global_n2o_esrlctm_$yyyy.txt
        while [ ! -s $n2o ] ; do
                ((yyyy-=1))
                n2o=$n2odir/global_n2o_esrlctm_$yyyy.txt
        done
        if [ -s $n2o ] ; then
                cp $n2o ./n2oglobaldata.txt
        fi
        if [ ! -s ./n2oglobaldata.txt ] ; then
                echo "\./n2oglobaldata.txt" not created
                exit 1
        fi
fi
ICO=${ICO:-0}
if [ $ICO -gt 0 ] ; then
#        # Copy CO files to $workdir
        codir=${FIX_ROOT}
        yyyy=`echo $ANAL_TIME | cut -c1-4`
        rm ./coglobaldata.txt
        co=$codir/global_co_esrlctm_$yyyy.txt
        while [ ! -s $co ] ; do
                ((yyyy-=1))
                co=$codir/global_co_esrlctm_$yyyy.txt
        done
        if [ -s $co ] ; then
                cp $co ./coglobaldata.txt
        fi
        if [ ! -s ./coglobaldata.txt ] ; then
                echo "\./coglobaldata.txt" not created
                exit 1
        fi
fi

##################################################################################
# Set some parameters for use by the GSI executable and to build the namelist
echo " Build the namelist "

vs_op='0.7,'
hzscl_op='1.7,0.8,0.5,'

if [ ${if_observer} = Yes ] ; then
  nummiter=0
  if_read_obs_save='.true.'
  if_read_obs_skip='.false.'
else
  nummiter=2
  if_read_obs_save='.false.'
  if_read_obs_skip='.false.'
fi

# Build the GSI namelist on-the-fly
. $GSI_NAMELIST

##################################################################################

echo " Copy GSI executable, background file, and link observation bufr to working directory"

# Save a copy of the GSI executable in the workdir
cp ${GSI_EXE} gsi.x

# Bring over background field (it's modified by GSI so we can't link to it)
# Copy bias correction, atmospheric and surface files
if [[ "$GFSCASE" = "enkf_glb_t62" ]]; then
  cp $BK_ROOT/bfg_${gdate}_fhr03_ensmean  ./sfcf03
  cp $BK_ROOT/bfg_${gdate}_fhr06_ensmean  ./sfcf06
  cp $BK_ROOT/bfg_${gdate}_fhr09_ensmean  ./sfcf09

  cp $BK_ROOT/sfg_${gdate}_fhr03_mem001    ./sigf03
  cp $BK_ROOT/sfg_${gdate}_fhr06_mem001    ./sigf06
  cp $BK_ROOT/sfg_${gdate}_fhr09_mem001    ./sigf09
else

  cp $BK_ROOT/sfcf03  ./sfcf03
  cp $BK_ROOT/sfcf06  ./sfcf06
  cp $BK_ROOT/sfcf09  ./sfcf09

  cp $BK_ROOT/sigf03  ./sigf03
  cp $BK_ROOT/sigf06  ./sigf06
  cp $BK_ROOT/sigf09  ./sigf09
fi

cp ${GSI_ROOT}/fix/comgsi_satbias_in ./satbias_in
cp ${GSI_ROOT}/fix/comgsi_satbias_pc_in ./satbias_pc_in 

# link GFS ensemble files
# ln -s $ENS_ROOT/sigf06_ens_mem* .
# link the localization file
# ln -s ${ENS_ROOT}/hybens_locinfo .
# Link to the prepbufr data
ln -s ${PREPBUFR} ./prepbufr

# Link to the other observation data
if [ -r "${OBS_ROOT}/satwnd" ]; then
   ln -s ${OBS_ROOT}/satwnd .
fi
if [ -r "${OBS_ROOT}/gpsrobufr" ]; then
   ln -s ${OBS_ROOT}/gpsrobufr .
fi
if [ -r "${OBS_ROOT}/ssmirrbufr" ]; then
   ln -s ${OBS_ROOT}/ssmirrbufr .
fi
if [ -r "${OBS_ROOT}/tmirrbufr" ]; then
   ln -s ${OBS_ROOT}/tmirrbufr .
fi
if [ -r "${OBS_ROOT}/sbuvbufr" ]; then
   ln -s ${OBS_ROOT}/sbuvbufr .
fi
if [ -r "${OBS_ROOT}/gsnd1bufr" ]; then
   ln -s ${OBS_ROOT}/gsnd1bufr .
fi
if [ -r "${OBS_ROOT}/amsuabufr" ]; then
   ln -s ${OBS_ROOT}/amsuabufr amsuabufr 
fi
if [ -r "${OBS_ROOT}/amsubbufr" ]; then
   ln -s ${OBS_ROOT}/amsubbufr amsubbufr 
fi
if [ -r "${OBS_ROOT}/hirs2bufr" ]; then
   ln -s ${OBS_ROOT}/hirs2bufr .
fi
if [ -r "${OBS_ROOT}/hirs3bufr" ]; then
   ln -s ${OBS_ROOT}/hirs3bufr .
fi
if [ -r "${OBS_ROOT}/hirs4bufr" ]; then
   ln -s ${OBS_ROOT}/hirs4bufr .
fi
if [ -r "${OBS_ROOT}/mhsbufr" ]; then
   ln -s ${OBS_ROOT}/mhsbufr .
fi
if [ -r "${OBS_ROOT}//msubufr" ]; then
   ln -s ${OBS_ROOT}/msubufr .
fi
if [ -r "${OBS_ROOT}//airsbufr" ]; then
   ln -s ${OBS_ROOT}/airsbufr .
fi
if [ -r "${OBS_ROOT}//atmsbufr" ]; then
   ln -s ${OBS_ROOT}/atmsbufr .
fi
if [ -r "${OBS_ROOT}//crisbufr" ]; then
   ln -s ${OBS_ROOT}/crisbufr .
fi
if [ -r "${OBS_ROOT}//seviribufr" ]; then
   ln -s ${OBS_ROOT}/seviribufr .
fi
if [ -r "${OBS_ROOT}//iasibufr" ]; then
   ln -s ${OBS_ROOT}/iasibufr .
fi
if [ -r "${OBS_ROOT}//ssmitbufr" ]; then
   ln -s ${OBS_ROOT}/ssmitbufr .
fi
if [ -r "${OBS_ROOT}//amsrebufr" ]; then
   ln -s ${OBS_ROOT}/amsrebufr .
fi
if [ -r "${OBS_ROOT}//ssmisbufr" ]; then
   ln -s ${OBS_ROOT}/ssmisbufr .
fi
if [ -r "${OBS_ROOT}//gomebufr" ]; then
   ln -s ${OBS_ROOT}/gomebufr .
fi
if [ -r "${OBS_ROOT}//omibufr" ]; then
   ln -s ${OBS_ROOT}/omibufr .
fi
if [ -r "${OBS_ROOT}/mlsbufr" ]; then
   ln -s ${OBS_ROOT}/mlsbufr .
fi
if [ -r "${OBS_ROOT}/hirs3bufrears" ]; then
   ln -s ${OBS_ROOT}/hirs3bufrears .
fi
if [ -r "${OBS_ROOT}/amsuabufrears" ]; then
   ln -s ${OBS_ROOT}/amsuabufrears .
fi
if [ -r "${OBS_ROOT}/amsubbufrears" ]; then
   ln -s ${OBS_ROOT}/amsubbufrears .
fi
if [ -r "${OBS_ROOT}/tcvitl" ]; then
   ln -s ${OBS_ROOT}/tcvitl .
fi
if [ -r "${OBS_ROOT}/satwndbufr" ]; then
   ln -s ${OBS_ROOT}/satwndbufr .
fi


#
##################################################################################

echo " Copy fixed files and link CRTM coefficient files to working directory"

# Set fixed files
#   berror   = forecast model background error statistics
#   specoef  = CRTM spectral coefficients
#   trncoef  = CRTM transmittance coefficients
#   emiscoef = CRTM coefficients for IR sea surface emissivity model
#   aerocoef = CRTM coefficients for aerosol effects
#   cldcoef  = CRTM coefficients for cloud effects
#   satinfo  = text file with information about assimilation of brightness temperatures
#   satangl  = angle dependent bias correction file (fixed in time)
#   pcpinfo  = text file with information about assimilation of prepcipitation rates
#   ozinfo   = text file with information about assimilation of ozone data
#   errtable = text file with obs error for conventional data (regional only)
#   convinfo = text file with information about assimilation of conventional data
#   bufrtable= text file ONLY needed for single obs test (oneobstest=.true.)
#   bftab_sst= bufr table for sst ONLY needed for sst retrieval (retrieval=.true.)

ANAVINFO=${FIX_ROOT}/global_anavinfo.l64.txt
BERROR=${FIX_ROOT}/${BYTE_ORDER}/global_berror.l${LEVS}y${NLAT}.f77
SATINFO=${FIX_ROOT}/global_satinfo.txt
scaninfo=${FIX_ROOT}/global_scaninfo.txt
SATANGL=${FIX_ROOT}/global_satangbias.txt
atmsbeamdat=${FIX_ROOT}/atms_beamwidth.txt
CONVINFO=${FIX_ROOT}/global_convinfo_reg_test.txt
OZINFO=${FIX_ROOT}/global_ozinfo.txt
PCPINFO=${FIX_ROOT}/global_pcpinfo.txt
OBERROR=${FIX_ROOT}/prepobs_errtable.global
CLOUDYRADINFO=${FIX_ROOT}/cloudy_radiance_info.txt
HYBENSINFO=${FIX_ROOT}/global_hybens_info.l64.txt 

# Only need this file for single obs test
bufrtable=${FIX_ROOT}/prepobs_prep.bufrtable

# Only need this file for sst retrieval
bftab_sst=${FIX_ROOT}/bufrtab.012

#  copy Fixed fields to working directory
 cp $ANAVINFO anavinfo
 cp $BERROR   berror_stats
 cp $SATANGL  satbias_angle
 cp $atmsbeamdat  atms_beamwidth.txt
 cp $SATINFO  satinfo
 cp $scaninfo scaninfo
 cp $CONVINFO convinfo
 cp $OZINFO   ozinfo
 cp $PCPINFO  pcpinfo
 cp $OBERROR  errtable
 cp $CLOUDYRADINFO cloudy_radiance_info.txt
 cp $HYBENSINFO hybens_info

 cp $bufrtable ./prepobs_prep.bufrtable
 cp $bftab_sst ./bftab_sstphr

#
# CRTM Spectral and Transmittance coefficients
RTMFIX=${CRTM_ROOT}/${BYTE_ORDER}
emiscoef_IRwater=${RTMFIX}/Nalli.IRwater.EmisCoeff.bin
emiscoef_IRice=${RTMFIX}/NPOESS.IRice.EmisCoeff.bin
emiscoef_IRland=${RTMFIX}/NPOESS.IRland.EmisCoeff.bin
emiscoef_IRsnow=${RTMFIX}/NPOESS.IRsnow.EmisCoeff.bin
emiscoef_VISice=${RTMFIX}/NPOESS.VISice.EmisCoeff.bin
emiscoef_VISland=${RTMFIX}/NPOESS.VISland.EmisCoeff.bin
emiscoef_VISsnow=${RTMFIX}/NPOESS.VISsnow.EmisCoeff.bin
emiscoef_VISwater=${RTMFIX}/NPOESS.VISwater.EmisCoeff.bin
emiscoef_MWwater=${RTMFIX}/FASTEM6.MWwater.EmisCoeff.bin
aercoef=${RTMFIX}/AerosolCoeff.bin
cldcoef=${RTMFIX}/CloudCoeff.bin

ln -s $emiscoef_IRwater ./Nalli.IRwater.EmisCoeff.bin
ln -s $emiscoef_IRice ./NPOESS.IRice.EmisCoeff.bin
ln -s $emiscoef_IRsnow ./NPOESS.IRsnow.EmisCoeff.bin
ln -s $emiscoef_IRland ./NPOESS.IRland.EmisCoeff.bin
ln -s $emiscoef_VISice ./NPOESS.VISice.EmisCoeff.bin
ln -s $emiscoef_VISland ./NPOESS.VISland.EmisCoeff.bin
ln -s $emiscoef_VISsnow ./NPOESS.VISsnow.EmisCoeff.bin
ln -s $emiscoef_VISwater ./NPOESS.VISwater.EmisCoeff.bin
ln -s $emiscoef_MWwater ./FASTEM6.MWwater.EmisCoeff.bin
ln -s $aercoef  ./AerosolCoeff.bin
ln -s $cldcoef  ./CloudCoeff.bin
# Copy CRTM coefficient files based on entries in satinfo file
for file in `awk '{if($1!~"!"){print $1}}' ./satinfo | sort | uniq` ;do
   ln -s ${RTMFIX}/${file}.SpcCoeff.bin ./
   ln -s ${RTMFIX}/${file}.TauCoeff.bin ./
done

#
###################################################
#  run  GSI
###################################################
echo ' Run GSI with' ${bk_core} 'background'

case $ARCH in
   'IBM_LSF')
      ${RUN_COMMAND} ./gsi.x < gsiparm.anl > stdout 2>&1  ;;

   * )
      ${RUN_COMMAND} ./gsi.x > stdout 2>&1  ;;
esac

##################################################################
#  run time error check
##################################################################
error=$?

if [ ${error} -ne 0 ]; then
  echo "ERROR: ${GSI} crashed  Exit status=${error}"
  exit ${error}
fi
#
##################################################################
#
# Copy the output to more understandable names
ln -s stdout      stdout.anl.${ANAL_TIME}
ln -s fort.201    fit_p1.${ANAL_TIME}
ln -s fort.202    fit_w1.${ANAL_TIME}
ln -s fort.203    fit_t1.${ANAL_TIME}
ln -s fort.204    fit_q1.${ANAL_TIME}
ln -s fort.207    fit_rad1.${ANAL_TIME}

# Loop over first and last outer loops to generate innovation
# diagnostic files for indicated observation types (groups)
#
# NOTE:  Since we set miter=2 in GSI namelist SETUP, outer
#        loop 03 will contain innovations with respect to
#        the analysis.  Creation of o-a innovation files
#        is triggered by write_diag(3)=.true.  The setting
#        write_diag(1)=.true. turns on creation of o-g
#        innovation files.
#

echo "Time before diagnostic loop is `date` "
loops="01 03"
for loop in $loops; do

case $loop in
  01) string=ges;;
  03) string=anl;;
   *) string=$loop;;
esac

#  Collect diagnostic files for obs types (groups) below
 listall=`ls pe* | cut -f2 -d"." | awk '{print substr($0, 0, length($0)-3)}' | sort | uniq `

   for type in $listall; do
      count=`ls pe*${type}_${loop}* | wc -l`
      if [[ $count -gt 0 ]]; then
         cat pe*${type}_${loop}* > diag_${type}_${string}.${ANAL_TIME}
      fi
   done
done

#  Clean working directory to save only important files 
ls -l * > list_run_directory
if [[ ${if_clean} = clean  &&  ${if_observer} != Yes ]]; then
  echo ' Clean working directory after GSI run'
  rm -f *Coeff.bin     # all CRTM coefficient files
  rm -fr pe0*          # diag files on each processor
  rm -f obs_input.*    # observation middle files
  rm -f sigf* sfcf*    # background  files
  rm -f fsize_*        # delete temperal file for bufr size
fi

#################################################
# start to calculate diag files for each member
#################################################
#
if [ ${if_observer} = Yes ] ; then
  string=ges
  for type in $listall; do
    count=0
    if [[ -f diag_${type}_${string}.${ANAL_TIME} ]]; then
       mv diag_${type}_${string}.${ANAL_TIME} diag_${type}_${string}.ensmean
    fi
  done

# Build the GSI namelist on-the-fly for each member
  nummiter=0
  if_read_obs_save='.false.'
  if_read_obs_skip='.true.'
. $GSI_NAMELIST

# Loop through each member
  loop="01"
  ensmem=1
  while [[ $ensmem -le $no_member ]];do

     rm pe0*

     print "\$ensmem is $ensmem"
     ensmemid=`printf %3.3i $ensmem`

# get new background for each member
     if [[ -f sigf03 ]]; then
       rm sigf03
     fi
     if [[ -f sigf06 ]]; then
       rm sigf06
     fi
     if [[ -f sigf09 ]]; then
       rm sigf09
     fi

     BK_FILE03=${BK_FILE_mem}_fhr03_mem${ensmemid}
     BK_FILE06=${BK_FILE_mem}_fhr06_mem${ensmemid}
     BK_FILE09=${BK_FILE_mem}_fhr09_mem${ensmemid}
     echo $BK_FILE06
     ln -s $BK_FILE03 ./sigf03
     ln -s $BK_FILE06 ./sigf06
     ln -s $BK_FILE09 ./sigf09

#  run  GSI
     echo ' Run GSI with' ${bk_core} 'for member ', ${ensmemid}

     case $ARCH in
        'IBM_LSF')
           ${RUN_COMMAND} ./gsi.x < gsiparm.anl > stdout_mem${ensmemid} 2>&1  ;;

        * )
           ${RUN_COMMAND} ./gsi.x > stdout_mem${ensmemid} 2>&1 ;;
     esac

#  run time error check and save run time file status
     error=$?

     if [ ${error} -ne 0 ]; then
       echo "ERROR: ${GSI} crashed for member ${ensmemid} Exit status=${error}"
       exit ${error}
     fi

     ls -l * > list_run_directory_mem${ensmemid}

# generate diag files

     for type in $listall; do
           count=`ls pe*${type}_${loop}* | wc -l`
        if [[ $count -gt 0 ]]; then
           cat pe*${type}_${loop}* > diag_${type}_${string}.mem${ensmemid}
        fi
     done

# next member
     (( ensmem += 1 ))

  done

fi

exit 0

exit 0
