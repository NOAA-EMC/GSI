
set -x

# Set analysis date
#adate=2015061000

# Set experiment name
exp=$jobname

#TM=00
#TM2=03
#tmmark=tm${TM}


# Set path/file for gsi executable
#gsiexec=/meso/save/Wanshu.Wu/Code/trunk/trunk_40320/src/global_gsi_org
#gsiexec=/da/save/Michael.Lueken/trunk/src/global_gsi.x

# Set runtime and save directories
tmpdir=$tmpdir/tmpreg_netcdf_fv3_regional/${exp}
savdir=$savdir/outreg_netcdf_fv3_regional/${exp}

# Set variables used in script
#   CLEAN up $tmpdir when finished (YES=remove, NO=leave alone)
#   ncp is cp replacement, currently keep as /bin/cp

UNCOMPRESS=gunzip
CLEAN=NO
ncp=/bin/cp


# Set up $tmpdir
rm -rf $tmpdir
mkdir -p $tmpdir
chgrp rstprod $tmpdir
chmod 750 $tmpdir
cd $tmpdir

#FIXnam=/da/save/Michael.Lueken/trunk/fix
fixcrtm=${fixcrtm:-$CRTM_FIX}

berror=$fixgsi/nam_nmm_berror.f77.gcv
anavinfo=$fixgsi/anavinfo_fv3


# Make gsi namelist

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
   . $scripts/regression_namelists.sh netcdf_fv3_regional
else
   . $scripts/regression_namelists_db.sh netcdf_fv3_regional
fi

#   dmesh(1)=120.0,time_window_max=1.5,ext_sonde=.true.,

cat << EOF > gsiparm.anl

$gsi_namelist

EOF

emiscoef_IRwater=$fixcrtm/Nalli.IRwater.EmisCoeff.bin
emiscoef_IRice=$fixcrtm/NPOESS.IRice.EmisCoeff.bin
emiscoef_IRland=$fixcrtm/NPOESS.IRland.EmisCoeff.bin
emiscoef_IRsnow=$fixcrtm/NPOESS.IRsnow.EmisCoeff.bin
emiscoef_VISice=$fixcrtm/NPOESS.VISice.EmisCoeff.bin
emiscoef_VISland=$fixcrtm/NPOESS.VISland.EmisCoeff.bin
emiscoef_VISsnow=$fixcrtm/NPOESS.VISsnow.EmisCoeff.bin
emiscoef_VISwater=$fixcrtm/NPOESS.VISwater.EmisCoeff.bin
emiscoef_MWwater=$fixcrtm/FASTEM6.MWwater.EmisCoeff.bin
aercoef=$fixcrtm/AerosolCoeff.bin
cldcoef=$fixcrtm/CloudCoeff.bin
satinfo=$fixgsi/nam_regional_satinfo.txt
cloudyinfo=$fixgsi/cloudy_radiance_info.txt
scaninfo=$fixgsi/global_scaninfo.txt
pcpinfo=$fixgsi/nam_global_pcpinfo.txt
ozinfo=$fixgsi/nam_global_ozinfo.txt
errtable=$fixgsi/nam_errtable.r3dv
convinfo=$fixgsi/nam_regional_convinfo.txt
mesonetuselist=$fixgsi/nam_mesonet_uselist.txt
stnuselist=$fixgsi/nam_mesonet_stnuselist.txt
qdaylist=$fixgsi/rtma_q_day_rejectlist
qnightlist=$fixgsi/rtma_q_night_rejectlist
tdaylist=$fixgsi/rtma_t_day_rejectlist
tnightlist=$fixgsi/rtma_t_night_rejectlist
wbinuselist=$fixgsi/rtma_wbinuselist
locinfo=$fixgsi/nam_hybens_d01_locinfo
### add 9 tables
errtable_pw=$fixgsi/prepobs_errtable_pw.global
errtable_ps=$fixgsi/prepobs_errtable_ps.global_nqcf
errtable_t=$fixgsi/prepobs_errtable_t.global_nqcf
errtable_q=$fixgsi/prepobs_errtable_q.global_nqcf
errtable_uv=$fixgsi/prepobs_errtable_uv.global_nqcf
btable_ps=$fixgsi/nqc_b_ps.global_nqcf
btable_t=$fixgsi/nqc_b_t.global_nqcf
btable_q=$fixgsi/nqc_b_q.global_nqcf
btable_uv=$fixgsi/nqc_b_uv.global_nqcf

# add vertical profile of localization and beta_s,beta_e weights for hybrid ensemble runs
hybens_info=$fixgsi/nam_hybens_d01_info


# Copy executable and fixed files to $tmpdir
if [[ $exp == *"updat"* ]]; then
   $ncp $gsiexec_updat  ./gsi.x
elif [[ $exp == *"contrl"* ]]; then
   $ncp $gsiexec_contrl ./gsi.x
fi

cp $anavinfo ./anavinfo
cp $berror   ./berror_stats
cp $errtable ./errtable
cp $emiscoef_IRwater ./Nalli.IRwater.EmisCoeff.bin
cp $emiscoef_IRice ./NPOESS.IRice.EmisCoeff.bin
cp $emiscoef_IRsnow ./NPOESS.IRsnow.EmisCoeff.bin
cp $emiscoef_IRland ./NPOESS.IRland.EmisCoeff.bin
cp $emiscoef_VISice ./NPOESS.VISice.EmisCoeff.bin
cp $emiscoef_VISland ./NPOESS.VISland.EmisCoeff.bin
cp $emiscoef_VISsnow ./NPOESS.VISsnow.EmisCoeff.bin
cp $emiscoef_VISwater ./NPOESS.VISwater.EmisCoeff.bin
cp $emiscoef_MWwater ./FASTEM6.MWwater.EmisCoeff.bin
cp $aercoef  ./AerosolCoeff.bin
cp $cldcoef  ./CloudCoeff.bin
cp $satinfo  ./satinfo
cp $cloudyinfo  ./cloudy_radiance_info.txt
cp $scaninfo ./scaninfo
cp $pcpinfo  ./pcpinfo
cp $ozinfo   ./ozinfo
cp $convinfo ./convinfo
cp $mesonetuselist ./mesonetuselist
cp $stnuselist ./mesonet_stnuselist
cp $qdaylist ./q_day_rejectlist
cp $qnightlist ./q_night_rejectlist
cp $tdaylist ./t_day_rejectlist
cp $tnightlist ./t_night_rejectlist
cp $wbinuselist ./wbinuselist
#cp $locinfo ./hybens_info
#add 9 tables for new varqc
$ncp $errtable_pw           ./errtable_pw
$ncp $errtable_ps           ./errtable_ps
$ncp $errtable_t           ./errtable_t
$ncp $errtable_q           ./errtable_q
$ncp $errtable_uv           ./errtable_uv
$ncp $btable_ps           ./btable_ps
$ncp $btable_t           ./btable_t
$ncp $btable_q           ./btable_q
$ncp $btable_uv           ./btable_uv

$ncp $hybens_info ./hybens_info


###### crtm coeff's #######################
set +x
for file in `awk '{if($1!~"!"){print $1}}' satinfo | sort | uniq` ;do
   cp $fixcrtm/${file}.SpcCoeff.bin ./
   cp $fixcrtm/${file}.TauCoeff.bin ./
done
set -x

PDY=`echo $adate | cut -c1-8`
CYC=`echo $adate | cut -c9-10`

#datdir=/meso/noscrub/Wanshu.Wu/CASE/$adate

cp $fv3_netcdf_obs/ndas.t06z.radwnd.tm06.bufr_d   ./radarbufr
cp $fv3_netcdf_obs/ndas.t06z.prepbufr.tm06        ./prepbufr
cp $fv3_netcdf_obs/ndas.t06z.1bamua.tm06.bufr_d   ./amsuabufr
cp $fv3_netcdf_obs/ndas.t06z.1bmhs.tm06.bufr_d    ./mhsbufr
cp $fv3_netcdf_obs/ndas.t06z.1bhrs4.tm06.bufr_d   ./hirs4bufr
cp $fv3_netcdf_obs/ndas.t06z.goesfv.tm06.bufr_d   ./gsnd1bufr
cp $fv3_netcdf_obs/ndas.t06z.airsev.tm06.bufr_d   ./airsbufr
cp $fv3_netcdf_obs/ndas.t06z.satwnd.tm06.bufr_d   ./satwndbufr

cp $fv3_netcdf_ges/coupler.res coupler.res
cp $fv3_netcdf_ges/fv_core.res.nest02.nc fv3_akbk
cp $fv3_netcdf_ges/grid_spec.nest02.nc fv3_grid_spec
#the current GSI parallel IO for fv3-lam require the netcdf 4 format for nc files containing 3d fields
nccopy -4  $fv3_netcdf_ges/fv_core.res.nest02.tile7.nc fv3_dynvars
nccopy -4  $fv3_netcdf_ges/fv_tracer.res.nest02.tile7.nc fv3_tracer
cp $fv3_netcdf_ges/sfc_data.nest02.tile7.nc fv3_sfcdata


cp $fv3_netcdf_ges/nam.t06z.satbias_pc.tm04 ./satbias_pc
cp $fv3_netcdf_ges/nam.t06z.satbias.tm04 ./satbias_in
cp $fv3_netcdf_ges/nam.t06z.radstat.tm04    ./radstat.gdas

listdiag=`tar xvf radstat.gdas | cut -d' ' -f2 | grep _ges`
for type in $listdiag; do
   diag_file=`echo $type | cut -d',' -f1`
   fname=`echo $diag_file | cut -d'.' -f1`
   date=`echo $diag_file | cut -d'.' -f2`
   $UNCOMPRESS $diag_file
   fnameanl=$(echo $fname|sed 's/_ges//g')
   mv $fname.$date $fnameanl
done


# Run GSI
cd $tmpdir
echo "run gsi now"
eval "$APRUN $tmpdir/gsi.x > stdout 2>&1"
rc=$?
exit $rc
