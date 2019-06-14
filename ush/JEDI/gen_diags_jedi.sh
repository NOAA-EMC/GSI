#!/bin/bash
#SBATCH -J gen_diags_jedi 
#SBATCH -A da-cpu
#SBATCH --qos=debug
#SBATCH --partition=bigmem
#SBATCH --nodes=2
#SBATCH --ntasks-per-node=24
#SBATCH -t 30:00
#SBATCH -o SLURM_%x.o%j
#SBATCH -e SLURM_%x.e%j
#SBATCH --mail-user=$LOGNAME@noaa.gov

set -x 

### user defined arguments
adate=2018041500
ObsDir=/scratch4/NCEPDEV/global/noscrub/dump/
GuessDir=/scratch4/NCEPDEV/da/noscrub/Andrew.Collard/ICs_for_JEDI
WorkDir=/scratch3/NCEPDEV/stmp1/$LOGNAME/JEDI/GSI_work/$adate
OutDir=/scratch3/NCEPDEV/stmp1/$LOGNAME/JEDI/output/$adate

GSIDir=/scratch4/NCEPDEV/da/save/Cory.R.Martin/GSI/
GSIBuildDir=$GSIDir/build_jedi
gsiexec=$GSIBuildDir/bin/gsi.x
nccat=$GSIBuildDir/bin/nc_diag_cat_serial.x
fixgsi=/scratch4/NCEPDEV/da/save/Cory.R.Martin/GSI/ProdGSI_jedi/fix
fixcrtm=/scratch4/NCEPDEV/da/save/Cory.R.Martin/CRTM/fix
USHDir=$GSIDir/ProdGSI_jedi/ush/

dumpobs=gdas
dumpobs_nr=gdasnr

# Set the JCAP resolution which you want.
# All resolutions use LEVS=64
export JCAP=382
export JCAP_B=382
export LEVS=64

# load modules here used to compile GSI
source /apps/lmod/7.7.18/init/sh

module list
module purge
### load modules
# system installed
module load intel
module load impi
module load netcdf
module load grads
module load rocoto/1.3.0
# /contrib modules
module use -a /contrib/modulefiles
module load anaconda/anaconda2
# /contrib/da modules
module use -a /contrib/da/modulefiles
module load boost
module load eigen

#   NCEPLIBS
module use -a /scratch3/NCEPDEV/nwprod/lib/modulefiles
module load nemsio
module load bacio
module load w3nco
#module load crtm/v2.2.3

module list
# CRTM things for 2.3.0
export CRTM_SRC=/scratch4/NCEPDEV/da/save/Cory.R.Martin/CRTM/REL-2.3.0/
export CRTM_INC=/scratch4/NCEPDEV/da/save/Cory.R.Martin/CRTM/REL-2.3.0/build/crtm_v2.3.0/include
export CRTM_LIB=/scratch4/NCEPDEV/da/save/Cory.R.Martin/CRTM/REL-2.3.0/build/crtm_v2.3.0/lib/libcrtm.a
export CRTM_FIX=/scratch4/NCEPDEV/da/save/Cory.R.Martin/CRTM/fix
export CRTM_VER=2.3.0

#####----- normal users need not change anything below this line -----##### 
export crtm_coeffs=./crtm_coeffs/
NDATE=${NDATE:-/scratch4/NCEPDEV/global/save/glopara/nwpara/util/exec/ndate}
ncpc=/bin/cp
ncpl="ln -fs"

# get analysis/guess date
PDYa=`echo $adate | cut -c1-8`
cyca=`echo $adate | cut -c9-10`
gdate=`$NDATE -06 $adate`
PDYg=`echo $gdate | cut -c1-8`
cycg=`echo $gdate | cut -c9-10`

datobs=$ObsDir/$PDYa$cyca/$obsdump/$dumpobs/
datobsnr=$ObsDir/$PDYa$cyca/$obsdump/${dumpobs_nr}/
datges=$GuessDir
prefix_obs=${dumpobs}.t${cyca}z
prefix_ges=gdas.t${cycg}z
suffix=tm00.bufr_d

# Given the requested resolution, set dependent resolution parameters
if [[ "$JCAP" = "1534" ]]; then
   export LONA=3072
   export LATA=1536
   export DELTIM=120
elif [[ "$JCAP" = "1148" ]]; then
   export LONA=2304
   export LATA=1152
   export DELTIM=120
elif [[ "$JCAP" = "878" ]]; then
   export LONA=1760
   export LATA=880
   export DELTIM=120
elif [[ "$JCAP" = "766" ]]; then
   export LONA=1536
   export LATA=768
   export DELTIM=${DELTIM:-$((3600/($JCAP/20)))}
elif [[ "$JCAP" = "574" ]]; then
   export LONA=1152
   export LATA=576
   export DELTIM=450
elif [[ "$JCAP" = "382" ]]; then
   export LONA=768
   export LATA=384
   export DELTIM=180
elif [[ "$JCAP" = "254" ]]; then
   export LONA=512
   export LATA=256
   export DELTIM=450
elif [[ "$JCAP" = "126" ]]; then
   export LONA=384
   export LATA=190
   export DELTIM=600
elif [[ "$JCAP" = "62" ]]; then
   export LONA=192
   export LATA=94
   export DELTIM=1200
else
   echo "INVALID JCAP = $JCAP"
   exit
fi
export NLON=$LONA
export NLAT=$((${LATA}+2))

# delete and recreate the working directory
rm -rf $WorkDir
mkdir -p $WorkDir
cd $WorkDir

# link and copy files
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

# Copy executable and fixed files to $DATA
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

# Copy CRTM coefficient files based on entries in satinfo file
mkdir -p ${crtm_coeffs}
for file in `awk '{if($1!~"!"){print $1}}' satinfo | sort | uniq` ;do
   $ncpc $fixcrtm/${file}.SpcCoeff.bin ${crtm_coeffs}
   $ncpc $fixcrtm/${file}.TauCoeff.bin ${crtm_coeffs}
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

# Copy observational data to $DATA
$ncpl $datobsnr/${prefix_obs}.prepbufr                ./prepbufr
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
## $ncpl $datobs/${prefix_obs}.amsre.${suffix}      ./amsrebufr
$ncpl $datobs/${prefix_obs}.ssmisu.${suffix}        ./ssmisbufr
$ncpl $datobs/${prefix_obs}.gome.${suffix}          ./gomebufr
$ncpl $datobs/${prefix_obs}.omi.${suffix}           ./omibufr
$ncpl $datobs/${prefix_obs}.mls.${suffix}           ./mlsbufr
$ncpl $datobs/${prefix_obs}oznnp8.${suffix}         ./ompsnpbufr
$ncpl $datobs/${prefix_obs}ozntc8.${suffix}         ./ompstcbufr
$ncpl $datobs/${prefix_obs}.eshrs3.${suffix}        ./hirs3bufrears
$ncpl $datobs/${prefix_obs}.esamua.${suffix}        ./amsuabufrears
$ncpl $datobs/${prefix_obs}.esamub.${suffix}        ./amsubbufrears
$ncpl $datobs/${prefix_obs}.atms.${suffix}          ./atmsbufr
$ncpl $datobs/${prefix_obs}.cris.${suffix}          ./crisbufr
$ncpl $datobs/${prefix_obs}.crisf4.${suffix}        ./crisfsbufr
$ncpl $datobs/${prefix_obs}.syndata.tcvitals.tm00   ./tcvitl
$ncpl $datobs/${prefix_obs}.avcsam.${suffix}        ./avhambufr
$ncpl $datobs/${prefix_obs}.avcspm.${suffix}        ./avhpmbufr
$ncpl $datobsnr/${prefix_obs}.saphir.${suffix}        ./saphirbufr
$ncpl $datobs/${prefix_obs}.gmi1cr.${suffix}        ./gmibufr
## $ncpl $datobs/${prefix_obs}.amsr2.tm00.bufr_d    ./amsr2bufr
$ncpl $datobs/${prefix_obs}.esiasi.${suffix}        ./iasibufrears
$ncpl $datobs/${prefix_obs}.hrs3db.${suffix}        ./hirs3bufr_db
$ncpl $datobs/${prefix_obs}.amuadb.${suffix}        ./amsuabufr_db
$ncpl $datobs/${prefix_obs}.amubdb.${suffix}        ./amsubbufr_db
$ncpl $datobs/${prefix_obs}.iasidb.${suffix}        ./iasibufr_db
$ncpl $datobs/${prefix_obs}.crisdb.${suffix}        ./crisbufr_db
$ncpl $datobs/${prefix_obs}.atmsdb.${suffix}        ./atmsbufr_db
$ncpl $datobs/${prefix_obs}.escris.${suffix}        ./crisbufrears
$ncpl $datobs/${prefix_obs}.esatms.${suffix}        ./atmsbufrears

# Copy bias correction, atmospheric and surface files
$ncpl $datges/${prefix_ges}.abias                   ./satbias_in
$ncpl $datges/${prefix_ges}.abias_pc                ./satbias_pc
$ncpl $datges/${prefix_ges}.abias_air               ./aircftbias_in

$ncpl $datges/${prefix_ges}.sfcf003.nemsio          ./sfcf03
$ncpl $datges/${prefix_ges}.sfcf006.nemsio          ./sfcf06
$ncpl $datges/${prefix_ges}.sfcf009.nemsio          ./sfcf09

$ncpl $datges/${prefix_ges}.atmf003.nemsio          ./sigf03
$ncpl $datges/${prefix_ges}.atmf006.nemsio          ./sigf06
$ncpl $datges/${prefix_ges}.atmf009.nemsio          ./sigf09



# create GSI namelist
cat <<EOF > gsiparm.anl
 &SETUP
   miter=0,
   niter_no_qc(1)=25,niter_no_qc(2)=0,
   write_diag(1)=.true.,write_diag(2)=.false.,write_diag(3)=.true.,
   qoption=2,
   gencode=0,factqmin=0.5,factqmax=0.005,deltim=$DELTIM,
   iguess=-1,tzr_qc=1,
   oneobtest=.false.,retrieval=.false.,l_foto=.false.,
   sfcnst_comb=.true.,
   use_pbl=.false.,use_compress=.true.,nsig_ext=12,gpstop=50.,
   use_gfs_nemsio=.true.,lrun_subdirs=.true.,use_readin_anl_sfcmask=.false.,
   crtm_coeffs_path='${crtm_coeffs}',
   newpc4pred=.true.,adp_anglebc=.true.,angord=4,passive_bc=.true.,use_edges=.false.,
   diag_precon=.true.,step_start=1.e-3,emiss_bc=.true.,cwoption=3,nhr_obsbin=3,
   verbose=.false.,imp_physics=11,lupp=.true.,
   binary_diag=.false.,netcdf_diag=.true.,clip_supersaturation=.false.,
   lobsdiag_forenkf=.false.,sfcmodel=.true.,
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
   $BKGERR   
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
   use_poq7=.true.,qc_noirjaco3_pole=.true.,vqc=.true.,
   aircraft_t_bc=.true.,biaspredt=1000.0,upd_aircraft=.true.,cleanup_tail=.true.,
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
   ssmisbufr      ssmis       f19         ssmis_f19           0.0     1     0
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
   amsr2bufr      amsr2       gcom-w1     amsr2_gcom-w1       0.0     3     0
   gmibufr        gmi         gpm         gmi_gpm             0.0     3     0
   saphirbufr     saphir      meghat      saphir_meghat       0.0     3     0
   ahibufr        ahi         himawari8   ahi_himawari8       0.0     3     0
   rapidscatbufr  uv          null        uv                  0.0     0     0
   ompsnpbufr     ompsnp      npp         ompsnp_npp          0.0     0     0
   ompstcbufr     ompstc8     npp         ompstc8_npp         0.0     2     0
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
   $HYBRIDENSEMBLE
 /

  &RAPIDREFRESH_CLDSURF
   dfi_radar_latent_heat_time_period=30.0,
   $RR_CLDSURF
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

# run GSI
cd $WorkDir
ulimit -s unlimited
export OMP_NUM_THREADS=1
env
srun ./gsi.x > stdout

# cat diag files
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
atms_n20 hirs4_metop-b amsua_metop-b mhs_metop-b iasi_metop-b avhrr_n18 avhrr_metop-a amsr2_gcom-w1 gmi_gpm
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
	    file=diag_${type}_${string}.${adate}_ensmean.nc4 # _ensmean is to work with python script
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

# move GSI diags
mkdir -p $OutDir/GSI_diags
mv diag_* $OutDir/GSI_diags/. 

# now use ioda-converters to convert to JEDI compatible files
# the performance of these converters is currently not great
# will submit a 1-node slurm task to just do the conversion after GSI runs

sbatch $USHDir/JEDI/convert_gsi_diags.sh $OutDir 

