
set -x

# Set variables used in script
#   CLEAN up $tmpdir when finished (YES=remove, NO=leave alone)
#   ncp is cp replacement, currently keep as /bin/cp

UNCOMPRESS=gunzip
CLEAN=NO
ncp=/bin/cp

#
# Set experiment name
#
exp=$jobname

#-----------------------------------------------------------------------
#
# Extract from ADATE the starting year, month, day, and hour of the
# forecast.  These are needed below for various operations.
#
#-----------------------------------------------------------------------
#
adate=${rrfs_3denvar_glbens_adate}
YYYYMMDDHH=$(date +%Y%m%d%H -d "${adate:0:8} ${adate:8:2}")
JJJ=$(date +%j -d "${adate:0:8} ${adate:8:2}")

YYYY=${YYYYMMDDHH:0:4}
MM=${YYYYMMDDHH:4:2}
DD=${YYYYMMDDHH:6:2}
HH=${YYYYMMDDHH:8:2}
YYYYMMDD=${YYYYMMDDHH:0:8}
#
#MESO_USELIST_FN=$(date +%Y-%m-%d -d "${START_DATE} -1 day")_meso_uselist.txt
#AIR_REJECT_FN=$(date +%Y%m%d -d "${START_DATE} -1 day")_rejects.txt

#
#-----------------------------------------------------------------------
#
# go to working directory and save directory.
# define fix and background path
#
#-----------------------------------------------------------------------
# Set runtime and save directories
tmpdir=$tmpdir/tmpreg_rrfs_3denvar_glbens/${exp}
savdir=$savdir/outreg_rrfs_3denvar_glbens/${exp}

# Set up $tmpdir
rm -rf $tmpdir
mkdir -p $tmpdir
chgrp rstprod $tmpdir
chmod 750 $tmpdir
cd $tmpdir

bkpath=${rrfs_3denvar_glbens_ges}
# decide background type
if [ -r "${bkpath}/fv3_coupler.res" ]; then
  BKTYPE=0              # warm start
else
  BKTYPE=1              # cold start
fi

fixcrtm=${fixcrtm:-$CRTM_FIX}

#---------------------------------------------------------------------
#
# decide regional_ensemble_option: global ensemble (1) or FV3LAM ensemble (5)
#
#---------------------------------------------------------------------
#
echo "regional_ensemble_option is ",${regional_ensemble_option:-1}

echo "$VERBOSE" "fixgsi is $fixgsi"
echo "$VERBOSE" "fixgriddir is $fixgriddir"
echo "$VERBOSE" "default bkpath is $bkpath"
echo "$VERBOSE" "background type is is $BKTYPE"

ifhyb=.false.
if  [[ ${regional_ensemble_option:-1} -eq 1 ]]; then #using GDAS
  #-----------------------------------------------------------------------
  # Make a list of the latest GFS EnKF ensemble
  #-----------------------------------------------------------------------
  ls ${rrfs_3denvar_glbens_ens}/*gdas.t??z.atmf009.mem0??.nc >> filelist03

  nummem=$(more filelist03 | wc -l)
  nummem=$((nummem - 3 ))
  if [[ ${nummem} -ge 10 ]]; then
    echo "$VERBOSE" "Do hybrid with ${memname}"
    ifhyb=.true.
    echo "$VERBOSE" " Cycle ${YYYYMMDDHH}: GSI hybrid uses ${memname} with n_ens=${nummem}"
  else
    echo "$VERBOSE" " Cycle ${YYYYMMDDHH}: GSI does pure 3DVAR."
    echo "$VERBOSE" " Hybrid needs at least ${HYBENSMEM_NMIN} ${memname} ensembles, only ${nummem} available"
  fi

fi

#-----------------------------------------------------------------------
#
# link or copy background and grib configuration files
#
#  Using ncks to add phis (terrain) into cold start input background.
#           it is better to change GSI to use the terrain from fix file.
#  Adding radar_tten array to fv3_tracer. Should remove this after add this array in
#           radar_tten converting code.
#-----------------------------------------------------------------------

ln -snf ${bkpath}/fv3_akbk                     fv3_akbk
ln -snf ${bkpath}/fv3_grid_spec                fv3_grid_spec

if [ ${BKTYPE} -eq 1 ]; then  # cold start uses background from INPUT
  ln -snf ${bkpath}/phis.nc               phis.nc
  ncks -A -v  phis               phis.nc           ${bkpath}/gfs_data.tile7.halo0.nc

  ln_vrfy -snf ${bkpath}/sfc_data.tile7.halo0.nc   fv3_sfcdata
  ln_vrfy -snf ${bkpath}/gfs_data.tile7.halo0.nc   fv3_dynvars
  ln_vrfy -s fv3_dynvars                           fv3_tracer

  fv3lam_bg_type=1
else                          # cycle uses background from restart
  timestring=${YYYYMMDD}.${HH}0000
  cp ${bkpath}/${timestring}.fv_core.res.tile1.nc             fv3_dynvars
  cp ${bkpath}/${timestring}.fv_tracer.res.tile1.nc           fv3_tracer
  cp ${bkpath}/${timestring}.sfc_data.nc                      fv3_sfcdata
  cp ${bkpath}/${timestring}.phy_data.nc                      fv3_phyvars
  fv3lam_bg_type=0
fi

# update times in coupler.res to current cycle time
cp ${bkpath}/fv3_coupler.res          coupler.res
sed -i "s/yyyy/${YYYY}/" coupler.res
sed -i "s/mm/${MM}/"     coupler.res
sed -i "s/dd/${DD}/"     coupler.res
sed -i "s/hh/${HH}/"     coupler.res


#
#-----------------------------------------------------------------------
#
# link observation files
# copy observation files to working directory 
#
#-----------------------------------------------------------------------
  obs_source=rap
  obsfileprefix=${YYYYMMDDHH}.${obs_source}
  obspath_tmp=${rrfs_3denvar_glbens_obs}

  obs_files_source[0]=${obspath_tmp}/${obsfileprefix}.t${HH}${SUBH}z.prepbufr.tm00
  obs_files_target[0]=prepbufr

  obs_number=${#obs_files_source[@]}
  obs_files_source[${obs_number}]=${obspath_tmp}/${obsfileprefix}.t${HH}${SUBH}z.satwnd.tm00.bufr_d
  obs_files_target[${obs_number}]=satwndbufr

  obs_number=${#obs_files_source[@]}
  obs_files_source[${obs_number}]=${obspath_tmp}/${obsfileprefix}.t${HH}${SUBH}z.nexrad.tm00.bufr_d
  obs_files_target[${obs_number}]=l2rwbufr


obs_number=${#obs_files_source[@]}
for (( i=0; i<${obs_number}; i++ ));
do
  obs_file=${obs_files_source[$i]}
  obs_file_t=${obs_files_target[$i]}
  if [ -r "${obs_file}" ]; then
    ln -s "${obs_file}" "${obs_file_t}"
  else
    echo "$VERBOSE" "Warning: ${obs_file} does not exist!"
  fi
done

#
#-----------------------------------------------------------------------
#
# Create links to fix files in the FIXgsi directory.
#
#-----------------------------------------------------------------------

ANAVINFO=${fixgsi}/anavinfo.rrfs
CONVINFO=${fixgsi}/convinfo.rrfs
HYBENSINFO=${fixgsi}/hybens_info.rrfs
OBERROR=${fixgsi}/errtable.rrfs
BERROR=${fixgsi}/rrfs_glb_berror.l127y770.f77

SATINFO=${fixgsi}/global_satinfo.txt
OZINFO=${fixgsi}/global_ozinfo.txt
PCPINFO=${fixgsi}/global_pcpinfo.txt
ATMS_BEAMWIDTH=${fixgsi}/atms_beamwidth.txt

# Fixed fields
cp ${ANAVINFO} anavinfo
cp ${BERROR}   berror_stats
cp $SATINFO    satinfo
cp $CONVINFO   convinfo
cp $OZINFO     ozinfo
cp $PCPINFO    pcpinfo
cp $OBERROR    errtable
cp $ATMS_BEAMWIDTH atms_beamwidth.txt
cp ${HYBENSINFO} hybens_info

cp ${bkpath}/gsd_sfcobs_provider.txt gsd_sfcobs_provider.txt
cp ${bkpath}/current_bad_aircraft current_bad_aircraft
cp ${bpath}/gsd_sfcobs_uselist.txt  gsd_sfcobs_uselist.txt

#-----------------------------------------------------------------------
#
# CRTM Spectral and Transmittance coefficients
#
#-----------------------------------------------------------------------
CRTMFIX=${fixcrtm}
emiscoef_IRwater=${CRTMFIX}/Nalli.IRwater.EmisCoeff.bin
emiscoef_IRice=${CRTMFIX}/NPOESS.IRice.EmisCoeff.bin
emiscoef_IRland=${CRTMFIX}/NPOESS.IRland.EmisCoeff.bin
emiscoef_IRsnow=${CRTMFIX}/NPOESS.IRsnow.EmisCoeff.bin
emiscoef_VISice=${CRTMFIX}/NPOESS.VISice.EmisCoeff.bin
emiscoef_VISland=${CRTMFIX}/NPOESS.VISland.EmisCoeff.bin
emiscoef_VISsnow=${CRTMFIX}/NPOESS.VISsnow.EmisCoeff.bin
emiscoef_VISwater=${CRTMFIX}/NPOESS.VISwater.EmisCoeff.bin
emiscoef_MWwater=${CRTMFIX}/FASTEM6.MWwater.EmisCoeff.bin
aercoef=${CRTMFIX}/AerosolCoeff.bin
cldcoef=${CRTMFIX}/CloudCoeff.bin

ln -s ${emiscoef_IRwater} Nalli.IRwater.EmisCoeff.bin
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
for file in $(awk '{if($1!~"!"){print $1}}' ./satinfo | sort | uniq) ;do
   ln -s ${CRTMFIX}/${file}.SpcCoeff.bin ./
   ln -s ${CRTMFIX}/${file}.TauCoeff.bin ./
done

#-----------------------------------------------------------------------
#
# Build the GSI namelist on-the-fly
#
#-----------------------------------------------------------------------
# 

. $scripts/regression_nl_update.sh

SETUP="$SETUP_update"
GRIDOPTS="$GRIDOPTS_update"
BKGVERR="$BKGVERR_update"
ANBKGERR="$ANBKERR_update"
JCOPTS="$JCOPTS_update"
STRONGOPTS="$STRONGOPTS_update"
OBSQC="$OBSQC_update"
OBSINPUT="$OBSINPUT_update"
SUPERRAD="$SUPERRAD_update"
HYBRID_ENSEMBLE='ensemble_path="",'
SINGLEOB="$SINGLEOB_update"

if [ "$debug" = ".false." ]; then
   . $scripts/regression_namelists.sh rrfs_3denvar_glbens
else
   . $scripts/regression_namelists_db.sh rrfs_3denvar_glbens
fi

cat << EOF > gsiparm.anl

$gsi_namelist

EOF

# Copy executable and fixed files to $tmpdir
if [[ $exp == *"updat"* ]]; then
   $ncp $gsiexec_updat  ./gsi.x
elif [[ $exp == *"contrl"* ]]; then
   $ncp $gsiexec_contrl ./gsi.x
fi

#cp $fv3_netcdf_ges/nam.t06z.satbias_pc.tm04 ./satbias_pc
#cp $fv3_netcdf_ges/nam.t06z.satbias.tm04 ./satbias_in
#cp $fv3_netcdf_ges/nam.t06z.radstat.tm04    ./radstat.gdas

# Run GSI
cd $tmpdir
echo "run gsi now"
eval "$APRUN $tmpdir/gsi.x > stdout 2>&1"
rc=$?
exit $rc
