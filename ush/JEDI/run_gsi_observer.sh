#!/bin/bash
# run_gsi_observer.sh
# run GSI observer for a specified
# analysis cycle and subset of observations
# cory.r.martin@noaa.gov
set -x
## variable definitions
MYDIR=`dirname "$0"`
YAMLFILE=$1
## source helper functions
source $MYDIR/helpers/parse_yaml.sh

## read YAML config file
if [[ -e $YAMLFILE ]]; then
   eval $(parse_yaml $YAMLFILE "GSI_")
else
  echo "ERROR: YAML FILE $YAMLFILE DOES NOT EXIST, ABORT!"
  exit 1
fi

## resolution things for GSI
export JCAP=$GSI_background_jcap
export JCAP_B=$GSI_background_jcap_b
export LEVS=$GSI_background_levs
source $MYDIR/helpers/get_lonlat_jcap.sh

## load modules for GSI
export GSIDIR=$GSI_observer_gsidir
set +x
source $MYDIR/helpers/load_modules_gsi.sh
set -x

## load env vars as needed
export APRUN_GSI=$GSI_env_launcher

## variables for executables
gsiexec=$GSIDIR/exec/global_gsi.x
nccat=$GSIDIR/exec/ncdiag_cat.x
NDATE=${NDATE:-`which ndate`}
ncpc=/bin/cp
ncpl="ln -fs"

## get analysis/guess date
adate=$GSI_time_year$GSI_time_month$GSI_time_day$GSI_time_cycle
PDYa=`echo $adate | cut -c1-8`
cyca=`echo $adate | cut -c9-10`
gdate=`$NDATE -06 $adate`
PDYg=`echo $gdate | cut -c1-8`
cycg=`echo $gdate | cut -c9-10`

## GFSv16 adds /atmos/ to the guess dir
if [[ "$GSI_background_gfsv16" = "true" ]]; then
  atmos=atmos
fi

## variables for other useful paths
fixgsi=$GSIDIR/fix
ushgsi=$GSIDIR/ush
crtm_coeffs=./crtm_coeffs/
datobs=$GSI_observations_obsdir/${GSI_observations_dump}.$PDYa/$cyca
datges=$GSI_background_guessdir/${GSI_observations_dump}.$PDYg/$cycg/$atmos
prefix_obs=${GSI_observations_dump}.t${cyca}z
prefix_ges=${GSI_observations_dump}.t${cycg}z
suffix=tm00.bufr_d

## netCDF or NEMSIO?
if [[ "$GSI_background_format" = "netcdf" ]]; then
  SUFFIX=".nc"
  use_gfs_nemsio=".false."
  use_gfs_ncio=".true."
else
  SUFFIX=".nemsio"
  use_gfs_nemsio=".true."
  use_gfs_ncio=".false."
fi

## paths of fix files
anavinfo=$fixgsi/global_anavinfo.l${LEVS}.txt
berror=$fixgsi/Big_Endian/global_berror.l${LEVS}y${NLAT}.f77
locinfo=$fixgsi/global_hybens_info.l${LEVS}.txt
satinfo=$fixgsi/global_satinfo.txt
scaninfo=$fixgsi/global_scaninfo.txt
satangl=$fixgsi/global_satangbias.txt
pcpinfo=$fixgsi/global_pcpinfo.txt
ozinfo=$fixgsi/global_ozinfo.txt
convinfo=$fixgsi/global_convinfo.txt
insituinfo=$fixgsi/global_insituinfo.txt
errtable=$fixgsi/prepobs_errtable.global
aeroinfo=$fixgsi/global_aeroinfo.txt
atmsbeaminfo=$fixgsi/atms_beamwidth.txt
cloudyinfo=$fixgsi/cloudy_radiance_info.txt

emiscoef_IRwater=$CRTM_FIX/Nalli.IRwater.EmisCoeff.bin
emiscoef_IRice=$CRTM_FIX/NPOESS.IRice.EmisCoeff.bin
emiscoef_IRland=$CRTM_FIX/NPOESS.IRland.EmisCoeff.bin
emiscoef_IRsnow=$CRTM_FIX/NPOESS.IRsnow.EmisCoeff.bin
emiscoef_VISice=$CRTM_FIX/NPOESS.VISice.EmisCoeff.bin
emiscoef_VISland=$CRTM_FIX/NPOESS.VISland.EmisCoeff.bin
emiscoef_VISsnow=$CRTM_FIX/NPOESS.VISsnow.EmisCoeff.bin
emiscoef_VISwater=$CRTM_FIX/NPOESS.VISwater.EmisCoeff.bin
emiscoef_MWwater=$CRTM_FIX/FASTEM6.MWwater.EmisCoeff.bin
aercoef=$CRTM_FIX/AerosolCoeff.bin
cldcoef=$CRTM_FIX/CloudCoeff.bin

## rm, make, and cd to working directory
rm -rf $GSI_observer_workdir
mkdir -p $GSI_observer_workdir
cd $GSI_observer_workdir

## copy executable and fix files
$ncpc $gsiexec ./gsi.x

$ncpc $anavinfo ./anavinfo
$ncpc $berror   ./berror_stats
$ncpc $locinfo  ./hybens_info
$ncpc $satinfo  ./satinfo
$ncpc $scaninfo ./scaninfo
$ncpc $pcpinfo  ./pcpinfo
$ncpc $ozinfo   ./ozinfo
$ncpc $convinfo ./convinfo
$ncpc $insituinfo ./insituinfo
$ncpc $errtable ./errtable
$ncpc $aeroinfo ./aeroinfo
$ncpc $atmsbeaminfo ./atms_beamwidth.txt
$ncpc $cloudyinfo   ./cloudy_radiance_info.txt

## copy CRTM coefficient files based on entries in satinfo file
mkdir -p ${crtm_coeffs}
for file in `awk '{if($1!~"!"){print $1}}' satinfo | sort | uniq` ;do
   $ncpc $CRTM_FIX/${file}.SpcCoeff.bin ${crtm_coeffs}
   $ncpc $CRTM_FIX/${file}.TauCoeff.bin ${crtm_coeffs}
done
$ncpc $emiscoef_IRwater  ${crtm_coeffs}Nalli.IRwater.EmisCoeff.bin
$ncpc $emiscoef_IRice    ${crtm_coeffs}NPOESS.IRice.EmisCoeff.bin
$ncpc $emiscoef_IRsnow   ${crtm_coeffs}NPOESS.IRsnow.EmisCoeff.bin
$ncpc $emiscoef_IRland   ${crtm_coeffs}NPOESS.IRland.EmisCoeff.bin
$ncpc $emiscoef_VISice   ${crtm_coeffs}NPOESS.VISice.EmisCoeff.bin
$ncpc $emiscoef_VISland  ${crtm_coeffs}NPOESS.VISland.EmisCoeff.bin
$ncpc $emiscoef_VISsnow  ${crtm_coeffs}NPOESS.VISsnow.EmisCoeff.bin
$ncpc $emiscoef_VISwater ${crtm_coeffs}NPOESS.VISwater.EmisCoeff.bin
$ncpc $emiscoef_MWwater  ${crtm_coeffs}FASTEM6.MWwater.EmisCoeff.bin
$ncpc $aercoef           ${crtm_coeffs}AerosolCoeff.bin
$ncpc $cldcoef           ${crtm_coeffs}CloudCoeff.bin

## copy observations
if [[ "$GSI_observations_restricted" = "true" ]]; then
  $ncpl $datobs/${prefix_obs}.prepbufr                ./prepbufr
else
  $ncpl $datobs/${prefix_obs}.prepbufr.nr                ./prepbufr
fi
$ncpl $datobs/${prefix_obs}.prepbufr.acft_profiles  ./prepbufr_profl
$ncpl $datobs/${prefix_obs}.nsstbufr                ./nsstbufr
$ncpl $datobs/${prefix_obs}.gpsro.${suffix}         ./gpsrobufr
$ncpl $datobs/${prefix_obs}.satwnd.${suffix}        ./satwndbufr
$ncpl $datobs/${prefix_obs}.spssmi.${suffix}        ./ssmirrbufr
$ncpl $datobs/${prefix_obs}.sptrmm.${suffix}        ./tmirrbufr
$ncpl $datobs/${prefix_obs}.osbuv8.${suffix}        ./sbuvbufr
$ncpl $datobs/${prefix_obs}.goesfv.${suffix}        ./gsnd1bufr
$ncpl $datobs/${prefix_obs}.1bamua.${suffix}        ./amsuabufr
$ncpl $datobs/${prefix_obs}.1bamub.${suffix}        ./amsubbufr
$ncpl $datobs/${prefix_obs}.1bhrs2.${suffix}        ./hirs2bufr
$ncpl $datobs/${prefix_obs}.1bhrs3.${suffix}        ./hirs3bufr
$ncpl $datobs/${prefix_obs}.1bhrs4.${suffix}        ./hirs4bufr
$ncpl $datobs/${prefix_obs}.1bmhs.${suffix}         ./mhsbufr
$ncpl $datobs/${prefix_obs}.1bmsu.${suffix}         ./msubufr
$ncpl $datobs/${prefix_obs}.airsev.${suffix}        ./airsbufr
$ncpl $datobs/${prefix_obs}.sevcsr.${suffix}        ./seviribufr
$ncpl $datobs/${prefix_obs}.mtiasi.${suffix}        ./iasibufr
$ncpl $datobs/${prefix_obs}.ssmit.${suffix}         ./ssmitbufr
$ncpl $datobs/${prefix_obs}.ssmisu.${suffix}        ./ssmisbufr
$ncpl $datobs/${prefix_obs}.gome.${suffix}          ./gomebufr
$ncpl $datobs/${prefix_obs}.omi.${suffix}           ./omibufr
$ncpl $datobs/${prefix_obs}.mls.${suffix}           ./mlsbufr
$ncpl $datobs/${prefix_obs}.ompsn8.${suffix}        ./ompsnpbufr
$ncpl $datobs/${prefix_obs}.ompst8.${suffix}        ./ompstcbufr
$ncpl $datobs/${prefix_obs}.eshrs3.${suffix}        ./hirs3bufrears
$ncpl $datobs/${prefix_obs}.esamua.${suffix}        ./amsuabufrears
$ncpl $datobs/${prefix_obs}.esamub.${suffix}        ./amsubbufrears
$ncpl $datobs/${prefix_obs}.atms.${suffix}          ./atmsbufr
$ncpl $datobs/${prefix_obs}.cris.${suffix}          ./crisbufr
$ncpl $datobs/${prefix_obs}.crisf4.${suffix}        ./crisfsbufr
$ncpl $datobs/${prefix_obs}.syndata.tcvitals.tm00   ./tcvitl
$ncpl $datobs/${prefix_obs}.avcsam.${suffix}        ./avhambufr
$ncpl $datobs/${prefix_obs}.avcspm.${suffix}        ./avhpmbufr
$ncpl $datobs/${prefix_obs}.saphir.${suffix}.nr       ./saphirbufr
$ncpl $datobs/${prefix_obs}.gmi1cr.${suffix}        ./gmibufr
$ncpl $datobs/${prefix_obs}.esiasi.${suffix}        ./iasibufrears
$ncpl $datobs/${prefix_obs}.hrs3db.${suffix}        ./hirs3bufr_db
$ncpl $datobs/${prefix_obs}.amuadb.${suffix}        ./amsuabufr_db
$ncpl $datobs/${prefix_obs}.amubdb.${suffix}        ./amsubbufr_db
$ncpl $datobs/${prefix_obs}.iasidb.${suffix}        ./iasibufr_db
$ncpl $datobs/${prefix_obs}.crisdb.${suffix}        ./crisbufr_db
$ncpl $datobs/${prefix_obs}.atmsdb.${suffix}        ./atmsbufr_db
$ncpl $datobs/${prefix_obs}.escris.${suffix}        ./crisbufrears
$ncpl $datobs/${prefix_obs}.esatms.${suffix}        ./atmsbufrears

## copy gsistats
$ncpl $datges/${prefix_obs}.gsistat        ./gsistat

## copy bias correction, atmospheric and surface files
$ncpl $datges/${prefix_ges}.abias                   ./satbias_in
$ncpl $datges/${prefix_ges}.abias_pc                ./satbias_pc
$ncpl $datges/${prefix_ges}.abias_air               ./aircftbias_in

$ncpl $datges/${prefix_ges}.sfcf003$SUFFIX          ./sfcf03
$ncpl $datges/${prefix_ges}.sfcf004$SUFFIX          ./sfcf04
$ncpl $datges/${prefix_ges}.sfcf005$SUFFIX          ./sfcf05
$ncpl $datges/${prefix_ges}.sfcf006$SUFFIX          ./sfcf06
$ncpl $datges/${prefix_ges}.sfcf007$SUFFIX          ./sfcf07
$ncpl $datges/${prefix_ges}.sfcf008$SUFFIX          ./sfcf08
$ncpl $datges/${prefix_ges}.sfcf009$SUFFIX          ./sfcf09

$ncpl $datges/${prefix_ges}.atmf003$SUFFIX          ./sigf03
$ncpl $datges/${prefix_ges}.atmf004$SUFFIX          ./sigf04
$ncpl $datges/${prefix_ges}.atmf005$SUFFIX          ./sigf05
$ncpl $datges/${prefix_ges}.atmf006$SUFFIX          ./sigf06
$ncpl $datges/${prefix_ges}.atmf007$SUFFIX          ./sigf07
$ncpl $datges/${prefix_ges}.atmf008$SUFFIX          ./sigf08
$ncpl $datges/${prefix_ges}.atmf009$SUFFIX          ./sigf09

if [ -f $datges/${prefix_ges}.atmf004$SUFFIX ]; then
   nhr_obsbin=1
else
   nhr_obsbin=3
fi

## create GSI namelist
cat > gsiparm.anl << EOF
&SETUP
  miter=0,
  niter(1)=1,niter(2)=1,
  niter_no_qc(1)=50,niter_no_qc(2)=0,
  write_diag(1)=.true.,write_diag(2)=.false.,
  qoption=2,
  gencode=0,deltim=$DELTIM,
  factqmin=0.5,factqmax=0.0002,
  iguess=-1,
  tzr_qc=1,
  oneobtest=.false.,retrieval=.false.,l_foto=.false.,
  use_pbl=.false.,use_compress=.true.,nsig_ext=12,gpstop=50.,
  use_gfs_nemsio=${use_gfs_nemsio},use_gfs_ncio=${use_gfs_ncio},sfcnst_comb=.true.,
  use_readin_anl_sfcmask=.false.,
  lrun_subdirs=.true.,
  crtm_coeffs_path='./crtm_coeffs/',
  newpc4pred=.true.,adp_anglebc=.true.,angord=4,passive_bc=.true.,use_edges=.false.,
  diag_precon=.true.,step_start=1.e-3,emiss_bc=.true.,nhr_obsbin=${nhr_obsbin:-3},
  cwoption=3,imp_physics=11,lupp=.true.,cnvw_option=.false.,
  netcdf_diag=.true.,binary_diag=.false.,
  lobsdiag_forenkf=.false.,
  write_fv3_incr=.true.,
  $SETUP
/
&GRIDOPTS
  JCAP_B=$JCAP_B,JCAP=$JCAP,NLAT=$NLAT,NLON=$NLON,nsig=$LEVS,
  regional=.false.,nlayers(63)=3,nlayers(64)=6,
  $GRIDOPTS
/
&BKGERR
  vs=0.7,
  hzscl=1.7,0.8,0.5,
  hswgt=0.45,0.3,0.25,
  bw=0.0,norsp=4,
  bkgv_flowdep=.true.,bkgv_rewgtfct=1.5,
  bkgv_write=.false.,
  cwcoveqqcov=.false.,
  $BKGVERR
/
&ANBKGERR
  anisotropic=.false.,
  $ANBKGERR
/
&JCOPTS
  ljcdfi=.false.,alphajc=0.0,ljcpdry=.true.,bamp_jcpdry=5.0e7,
  $JCOPTS
/
&STRONGOPTS
  tlnmc_option=2,nstrong=1,nvmodes_keep=8,period_max=6.,period_width=1.5,
  $STRONGOPTS
/
&OBSQC
  dfact=0.75,dfact1=3.0,noiqc=.true.,oberrflg=.false.,c_varqc=0.02,
  use_poq7=.true.,qc_noirjaco3_pole=.true.,vqc=.true.,nvqc=.false.,
  aircraft_t_bc=.true.,biaspredt=1.0e5,upd_aircraft=.true.,cleanup_tail=.true.,
  tcp_width=70.0,tcp_ermax=7.35,
  $OBSQC
/
&OBS_INPUT
  dmesh(1)=145.0,dmesh(2)=150.0,dmesh(3)=100.0,time_window_max=3.0,
  $OBSINPUT
/
OBS_INPUT::
!  dfile          dtype       dplat       dsis                dval    dthin dsfcalc
   prepbufr       ps          null        ps                  0.0     0     0
   prepbufr       t           null        t                   0.0     0     0
   prepbufr_profl t           null        t                   0.0     0     0
   prepbufr       q           null        q                   0.0     0     0
   prepbufr_profl q           null        q                   0.0     0     0
   prepbufr       pw          null        pw                  0.0     0     0
   prepbufr       uv          null        uv                  0.0     0     0
   prepbufr_profl uv          null        uv                  0.0     0     0
   satwndbufr     uv          null        uv                  0.0     0     0
   prepbufr       spd         null        spd                 0.0     0     0
   prepbufr       dw          null        dw                  0.0     0     0
   radarbufr      rw          null        rw                  0.0     0     0
   nsstbufr       sst         nsst        sst                 0.0     0     0
   gpsrobufr      gps_bnd     null        gps                 0.0     0     0
   ssmirrbufr     pcp_ssmi    dmsp        pcp_ssmi            0.0    -1     0
   tmirrbufr      pcp_tmi     trmm        pcp_tmi             0.0    -1     0
   sbuvbufr       sbuv2       n16         sbuv8_n16           0.0     0     0
   sbuvbufr       sbuv2       n17         sbuv8_n17           0.0     0     0
   sbuvbufr       sbuv2       n18         sbuv8_n18           0.0     0     0
   hirs3bufr      hirs3       n17         hirs3_n17           0.0     1     0
   hirs4bufr      hirs4       metop-a     hirs4_metop-a       0.0     1     1
   gimgrbufr      goes_img    g11         imgr_g11            0.0     1     0
   gimgrbufr      goes_img    g12         imgr_g12            0.0     1     0
   airsbufr       airs        aqua        airs_aqua           0.0     1     1
   amsuabufr      amsua       n15         amsua_n15           0.0     1     1
   amsuabufr      amsua       n18         amsua_n18           0.0     1     1
   amsuabufr      amsua       metop-a     amsua_metop-a       0.0     1     1
   airsbufr       amsua       aqua        amsua_aqua          0.0     1     1
   amsubbufr      amsub       n17         amsub_n17           0.0     1     1
   mhsbufr        mhs         n18         mhs_n18             0.0     1     1
   mhsbufr        mhs         metop-a     mhs_metop-a         0.0     1     1
   ssmitbufr      ssmi        f15         ssmi_f15            0.0     1     0
   amsrebufr      amsre_low   aqua        amsre_aqua          0.0     1     0
   amsrebufr      amsre_mid   aqua        amsre_aqua          0.0     1     0
   amsrebufr      amsre_hig   aqua        amsre_aqua          0.0     1     0
   ssmisbufr      ssmis       f16         ssmis_f16           0.0     1     0
   ssmisbufr      ssmis       f17         ssmis_f17           0.0     1     0
   ssmisbufr      ssmis       f18         ssmis_f18           0.0     1     0
   gsnd1bufr      sndrd1      g12         sndrD1_g12          0.0     1     0
   gsnd1bufr      sndrd2      g12         sndrD2_g12          0.0     1     0
   gsnd1bufr      sndrd3      g12         sndrD3_g12          0.0     1     0
   gsnd1bufr      sndrd4      g12         sndrD4_g12          0.0     1     0
   gsnd1bufr      sndrd1      g11         sndrD1_g11          0.0     1     0
   gsnd1bufr      sndrd2      g11         sndrD2_g11          0.0     1     0
   gsnd1bufr      sndrd3      g11         sndrD3_g11          0.0     1     0
   gsnd1bufr      sndrd4      g11         sndrD4_g11          0.0     1     0
   gsnd1bufr      sndrd1      g13         sndrD1_g13          0.0     1     0
   gsnd1bufr      sndrd2      g13         sndrD2_g13          0.0     1     0
   gsnd1bufr      sndrd3      g13         sndrD3_g13          0.0     1     0
   gsnd1bufr      sndrd4      g13         sndrD4_g13          0.0     1     0
   iasibufr       iasi        metop-a     iasi_metop-a        0.0     1     1
   gomebufr       gome        metop-a     gome_metop-a        0.0     2     0
   omibufr        omi         aura        omi_aura            0.0     2     0
   sbuvbufr       sbuv2       n19         sbuv8_n19           0.0     0     0
   hirs4bufr      hirs4       n19         hirs4_n19           0.0     1     1
   amsuabufr      amsua       n19         amsua_n19           0.0     1     1
   mhsbufr        mhs         n19         mhs_n19             0.0     1     1
   tcvitl         tcp         null        tcp                 0.0     0     0
   seviribufr     seviri      m08         seviri_m08          0.0     1     0
   seviribufr     seviri      m09         seviri_m09          0.0     1     0
   seviribufr     seviri      m10         seviri_m10          0.0     1     0
   seviribufr     seviri      m11         seviri_m11          0.0     1     0
   hirs4bufr      hirs4       metop-b     hirs4_metop-b       0.0     1     1
   amsuabufr      amsua       metop-b     amsua_metop-b       0.0     1     1
   mhsbufr        mhs         metop-b     mhs_metop-b         0.0     1     1
   iasibufr       iasi        metop-b     iasi_metop-b        0.0     1     1
   gomebufr       gome        metop-b     gome_metop-b        0.0     2     0
   atmsbufr       atms        npp         atms_npp            0.0     1     1
   atmsbufr       atms        n20         atms_n20            0.0     1     1
   crisbufr       cris        npp         cris_npp            0.0     1     0
   crisfsbufr     cris-fsr    npp         cris-fsr_npp        0.0     1     0
   crisfsbufr     cris-fsr    n20         cris-fsr_n20        0.0     1     0
   gsnd1bufr      sndrd1      g14         sndrD1_g14          0.0     1     0
   gsnd1bufr      sndrd2      g14         sndrD2_g14          0.0     1     0
   gsnd1bufr      sndrd3      g14         sndrD3_g14          0.0     1     0
   gsnd1bufr      sndrd4      g14         sndrD4_g14          0.0     1     0
   gsnd1bufr      sndrd1      g15         sndrD1_g15          0.0     1     0
   gsnd1bufr      sndrd2      g15         sndrD2_g15          0.0     1     0
   gsnd1bufr      sndrd3      g15         sndrD3_g15          0.0     1     0
   gsnd1bufr      sndrd4      g15         sndrD4_g15          0.0     1     0
   oscatbufr      uv          null        uv                  0.0     0     0
   mlsbufr        mls30       aura        mls30_aura          0.0     0     0
   avhambufr      avhrr       metop-a     avhrr3_metop-a      0.0     1     0
   avhpmbufr      avhrr       n18         avhrr3_n18          0.0     1     0
   avhambufr      avhrr       metop-b     avhrr3_metop-b      0.0     1     0
   avhpmbufr      avhrr       n19         avhrr3_n19          0.0     1     0
   amsr2bufr      amsr2       gcom-w1     amsr2_gcom-w1       0.0     3     0
   gmibufr        gmi         gpm         gmi_gpm             0.0     3     0
   saphirbufr     saphir      meghat      saphir_meghat       0.0     3     0
   ahibufr        ahi         himawari8   ahi_himawari8       0.0     1     0
   abibufr        abi         g16         abi_g16             0.0     1     0
   abibufr        abi         g17         abi_g17             0.0     1     0
   rapidscatbufr  uv          null        uv                  0.0     0     0
   ompsnpbufr     ompsnp      npp         ompsnp_npp          0.0     0     0
   ompstcbufr     ompstc8     npp         ompstc8_npp         0.0     2     0
   amsuabufr      amsua       metop-c     amsua_metop-c       0.0     1     1
   mhsbufr        mhs         metop-c     mhs_metop-c         0.0     1     1
   iasibufr       iasi        metop-c     iasi_metop-c        0.0     1     1
::
&SUPEROB_RADAR
  $SUPERRAD
/
&LAG_DATA
  $LAGDATA
/
&HYBRID_ENSEMBLE
  l_hyb_ens=.false.,
  generate_ens=.false.,
  beta_s0=0.125,readin_beta=.false.,
  s_ens_h=800.,s_ens_v=-0.8,readin_localization=.true.,
  aniso_a_en=.false.,oz_univ_static=.false.,uv_hyb_ens=.true.,
  ensemble_path='./ensemble_data/',
  ens_fast_read=.true.,
  $HYBRID_ENSEMBLE
/
&RAPIDREFRESH_CLDSURF
  dfi_radar_latent_heat_time_period=30.0,
  $RAPIDREFRESH_CLDSURF
/
&CHEM
  $CHEM
/
&SINGLEOB_TEST
  maginnov=0.1,magoberr=0.1,oneob_type='t',
  oblat=45.,oblon=180.,obpres=1000.,obdattim=${adate},
  obhourset=0.,
  $SINGLEOB
/
&NST
  nst_gsi=3,
  nstinfo=4,fac_dtl=1,fac_tsl=1,zsea1=0,zsea2=0,
  $NSST
/
EOF
cat gsiparm.anl

## run GSI observer
export OMP_NUM_THREADS=$GSI_env_nthreads
$APRUN_GSI ./gsi.x > gsi.stdout || exit 1

## cat diags
ntype=3
numfile[0]=0
numfile[1]=0
numfile[2]=0
numfile[3]=0
diagtype[0]="conv conv_gps conv_ps conv_q conv_sst conv_t conv_uv"
diagtype[1]="pcp_ssmi_dmsp pcp_tmi_trmm"
diagtype[2]="sbuv2_n16 sbuv2_n17 sbuv2_n18 sbuv2_n19 gome_metop-a gome_metop-b omi_aura mls30_aura ompsnp_npp
ompstc8_npp"
diagtype[3]="hirs2_n14 msu_n14 sndr_g08 sndr_g11 sndr_g12 sndr_g13 sndr_g08_prep sndr_g11_prep sndr_g12_prep
sndr_g13_prep sndrd1_g11 sndrd2_g11 sndrd3_g11 sndrd4_g11 sndrd1_g12 sndrd2_g12 sndrd3_g12 sndrd4_g12 sndrd1_g13
sndrd2_g13 sndrd3_g13 sndrd4_g13 sndrd1_g14 sndrd2_g14 sndrd3_g14 sndrd4_g14 sndrd1_g15 sndrd2_g15 sndrd3_g15 sndrd4_g15
hirs3_n15 hirs3_n16 hirs3_n17 amsua_n15 amsua_n16 amsua_n17 amsub_n15 amsub_n16 amsub_n17 hsb_aqua airs_aqua amsua_aqua
imgr_g08 imgr_g11 imgr_g12 imgr_g14 imgr_g15 ssmi_f13 ssmi_f15 hirs4_n18 hirs4_metop-a amsua_n18 amsua_metop-a mhs_n18
mhs_metop-a amsre_low_aqua amsre_mid_aqua amsre_hig_aqua ssmis_f16 ssmis_f17 ssmis_f18 ssmis_f19 ssmis_f20 iasi_metop-a
hirs4_n19 amsua_n19 mhs_n19 seviri_m08 seviri_m09 seviri_m10 seviri_m11 cris_npp cris-fsr_npp cris-fsr_n20 atms_npp
atms_n20 hirs4_metop-b amsua_metop-b mhs_metop-b iasi_metop-b amsua_metop-c mhs_metop-c avhrr_n18 avhrr_metop-a amsr2_gcom-w1 gmi_gpm
saphir_meghat ahi_himawari8"

prefix=" dir.*/"
loops="01"
for loop in $loops; do
   case $loop in
      01) string=ges;;
      03) string=anl;;
       *) string=$loop;;
   esac
   echo $(date) START loop $string >&2
   n=-1
   while [ $((n+=1)) -le $ntype ] ;do
      for type in $(echo ${diagtype[n]}); do
         count=$(ls ${prefix}${type}_${loop}* | wc -l)
         if [ $count -gt 0 ]; then
	          file=diag_${type}_${string}.${adate}.nc4
	          # note if the GSI utility is not working correctly, use the python version
	          # same syntax is used to call it, just change what $nccat is
            $nccat -o $file ${prefix}${type}_${loop}.nc4 &
            sleep 5
            echo "diag_${type}_${string}.${adate}*" >> ${diaglist[n]}
            numfile[n]=$(expr ${numfile[n]} + 1)
         fi
      done
   done
   echo $(date) END loop $string >&2
done
wait

cat `echo fort.*` > gsistat.out

## move files to output directory
if [[ "$GSI_observer_cleanup" = "true" ]]; then
  rm -rf $GSI_observer_outputdir
fi
mkdir -p $GSI_observer_outputdir
mv diag_* $GSI_observer_outputdir
cp satbias_in ${GSI_observer_outputdir}/satbias.$gdate
cp satbias_pc ${GSI_observer_outputdir}/satbias_pc.$gdate
cp aircftbias_in ${GSI_observer_outputdir}/aircftbias.$gdate
cp gsistat ${GSI_observer_outputdir}/gsistat.ops.$adate
cp gsistat.out ${GSI_observer_outputdir}/gsistat.$adate
cp gsi.stdout ${GSI_observer_outputdir}/stdout.$adate

if [[ "$GSI_observer_cleanup" = "true" ]]; then
  cd $GSI_observer_outputdir
  rm -rf $GSI_observer_workdir
fi
date
echo "GSI observer script completed"
