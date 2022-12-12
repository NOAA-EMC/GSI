
set -x

# Set experiment name and analysis date

exp=$jobname

# Set the JCAP resolution which you want.
export JCAP=48
export LEVS=127
export JCAP_B=$JCAP

# Set runtime and save directories
tmpdir=$tmpdir/$tmpregdir/${exp}
savdir=$savdir/out${JCAP}/${exp}

# Specify GSI fixed field and data directories.
fixcrtm=${fixcrtm:-$CRTM_FIX}

# Set variables used in script
#   CLEAN up $tmpdir when finished (YES=remove, NO=leave alone)
#   ncp is cp replacement, currently keep as /bin/cp

UNCOMPRESS=gunzip
CLEAN=NO
ncp=/bin/cp
nln="/bin/ln -fs"


# Given the requested resolution, set dependent resolution parameters
if [[ "$JCAP" = "96" ]]; then
   export LONA=384
   export LATA=192
   export DELTIM=1200
elif [[ "$JCAP" = "48" ]]; then
   export LONA=192
   export LATA=96
   export DELTIM=1200
else
   echo "INVALID JCAP = $JCAP"
   exit
fi
export NLON=$LONA
export NLAT=$((${LATA}+2))


# Given the analysis date, compute the date from which the
# first guess comes.  Extract cycle and set prefix and suffix
# for guess and observation data files
echo "global_adate is $global_adate" 
gdate=`date +%Y%m%d%H -d "${global_adate:0:8} ${global_adate:8:2} - 6 hours"`
PDYa=`echo $global_adate | cut -c1-8`
cyca=`echo $global_adate | cut -c9-10`
PDYg=`echo $gdate | cut -c1-8`
cycg=`echo $gdate | cut -c9-10`

dumpobs=gdas
prefix_obs=${dumpobs}.t${cyca}z
prefix_ges=gdas.t${cycg}z
prefix_ens=gdas.t${cycg}z
suffix=tm00.bufr_d

dumpges=gdas
COMROOTgfs=/scratch1/NCEPDEV/da/Russ.Treadon/CASES/regtest/gfs/prod
datobs=$COMROOTgfs/$dumpobs.$PDYa/${cyca}/atmos
datges=$COMROOTgfs/$dumpges.$PDYg/${cycg}/atmos
datens=$COMROOTgfs/enkfgdas.$PDYg/${cycg}/atmos

# Set up $tmpdir
rm -rf $tmpdir
mkdir -p $tmpdir
cd $tmpdir
rm -rf core*


# CO2 namelist and file decisions
ICO2=${ICO2:-0}
if [ $ICO2 -gt 0 ] ; then
        # Copy co2 files to $tmpdir
        co2dir=${CO2DIR:-$fixgsi}
        yyyy=$(echo ${CDATE:-$global_adate}|cut -c1-4)
        rm ./global_co2_data.txt
        co2=$co2dir/global_co2.gcmscl_$yyyy.txt
        while [ ! -s $co2 ] ; do
                ((yyyy-=1))
                co2=$co2dir/global_co2.gcmscl_$yyyy.txt
        done
        if [ -s $co2 ] ; then
                $ncp $co2 ./global_co2_data.txt
        fi
        if [ ! -s ./global_co2_data.txt ] ; then
                echo "\./global_co2_data.txt" not created
                exit 1
   fi
fi
#CH4 file decision
ICH4=${ICH4:-0}
if [ $ICH4 -gt 0 ] ; then
#        # Copy ch4 files to $tmpdir
        ch4dir=${CH4DIR:-$fixgsi}
        yyyy=$(echo ${CDATE:-$global_adate}|cut -c1-4)
        rm ./ch4globaldata.txt
        ch4=$ch4dir/global_ch4_esrlctm_$yyyy.txt
        while [ ! -s $ch4 ] ; do
                ((yyyy-=1))
                ch4=$ch4dir/global_ch4_esrlctm_$yyyy.txt
        done
        if [ -s $ch4 ] ; then
                $ncp $ch4 ./ch4globaldata.txt
        fi
        if [ ! -s ./ch4globaldata.txt ] ; then
                echo "\./ch4globaldata.txt" not created
                exit 1
   fi
fi
IN2O=${IN2O:-0}
if [ $IN2O -gt 0 ] ; then
#        # Copy ch4 files to $tmpdir
        n2odir=${N2ODIR:-$fixgsi}
        yyyy=$(echo ${CDATE:-$global_adate}|cut -c1-4)
        rm ./n2oglobaldata.txt
        n2o=$n2odir/global_n2o_esrlctm_$yyyy.txt
        while [ ! -s $n2o ] ; do
                ((yyyy-=1))
                n2o=$n2odir/global_n2o_esrlctm_$yyyy.txt
        done
        if [ -s $n2o ] ; then
                $ncp $n2o ./n2oglobaldata.txt
        fi
        if [ ! -s ./n2oglobaldata.txt ] ; then
                echo "\./n2oglobaldata.txt" not created
                exit 1
   fi
fi
ICO=${ICO:-0}
if [ $ICO -gt 0 ] ; then
#        # Copy CO files to $tmpdir
        codir=${CODIR:-$fixgsi}
        yyyy=$(echo ${CDATE:-$global_adate}|cut -c1-4)
        rm ./coglobaldata.txt
        co=$codir/global_co_esrlctm_$yyyy.txt
        while [ ! -s $co ] ; do
                ((yyyy-=1))
                co=$codir/global_co_esrlctm_$yyyy.txt
        done
        if [ -s $co ] ; then
                $ncp $co ./coglobaldata.txt
        fi
        if [ ! -s ./coglobaldata.txt ] ; then
                echo "\./coglobaldata.txt" not created
                exit 1
   fi
fi

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
SINGLEOB="$SINGLEOB_update"

if [ "$debug" = ".false." ]; then
   . $scripts/regression_namelists.sh global_3dvar
else
   . $scripts/regression_namelists_db.sh global_3dvar
fi

##!   l4dvar=.false.,nhr_assimilation=6,nhr_obsbin=6,
##!   lsqrtb=.true.,lcongrad=.false.,ltlint=.true.,
##!   idmodel=.true.,lwrtinc=.false.,

cat << EOF > gsiparm.anl

$gsi_namelist

EOF

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
#   errtable = text file with obs error for conventional data (optional)
#   convinfo = text file with information about assimilation of conventional data
#   bufrtable= text file ONLY needed for single obs test (oneobstest=.true.)
#   bftab_sst= bufr table for sst ONLY needed for sst retrieval (retrieval=.true.)
#   aeroinfo = text file with information about assimilation of aerosol data

anavinfo=$fixgsi/global_anavinfo.l${LEVS}.txt
berror=$fixgsi/Big_Endian/global_berror.l${LEVS}y${NLAT}.f77
locinfo=$fixgsi/global_hybens_info.l${LEVS}.txt
satinfo=$fixgsi/global_satinfo.txt
scaninfo=$fixgsi/global_scaninfo.txt
satangl=$fixgsi/global_satangbias.txt
pcpinfo=$fixgsi/global_pcpinfo.txt
ozinfo=$fixgsi/global_ozinfo.txt
convinfo=$fixgsi/global_convinfo.txt
vqcdat=$fixgsi/vqctp001.dat
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
#cldcoef=$fixcrtm/CloudCoeff.GFDLFV3.-109z-1.bin   # use with crtm/2.4.0

# Only need this file for single obs test
bufrtable=$fixgsi/prepobs_prep.bufrtable

# Only need this file for sst retrieval
bftab_sst=$fixgsi/bufrtab.012

# Copy executable and fixed files to $tmpdir
if [[ $exp == *"updat"* ]]; then
   $ncp $gsiexec_updat  ./gsi.x
elif [[ $exp == *"contrl"* ]]; then
   $ncp $gsiexec_contrl ./gsi.x
fi

$ncp $anavinfo ./anavinfo
$ncp $berror   ./berror_stats
$ncp $locinfo  ./hybens_info
$ncp $satinfo  ./satinfo
$ncp $scaninfo ./scaninfo
$ncp $pcpinfo  ./pcpinfo
$ncp $ozinfo   ./ozinfo
$ncp $convinfo ./convinfo
$ncp $vqcdat   ./vqctp001.dat
$ncp $insituinfo ./insituinfo
$ncp $errtable ./errtable
$ncp $aeroinfo ./aeroinfo
$ncp $atmsbeaminfo ./atms_beamwidth.txt
$ncp $cloudyinfo   ./cloudy_radiance_info.txt

$ncp $bufrtable ./prepobs_prep.bufrtable
$ncp $bftab_sst ./bftab_sstphr

#If using correlated error, get the covariance files
if grep -q "Rcov" $anavinfo ;
then
  if ls ${fixgsi}/Rcov* 1> /dev/null 2>&1;
  then
    $ncp ${fixgsi}/Rcov* $DATA

#   Correlated error utlizes mkl lapack.  Found it necesary to fix the
#   number of mkl threads to ensure reproducible results independent
#   of the job configuration.
    export MKL_NUM_THREADS=1

  else
    echo "Warning: Satellite error covariance files are missing."
    echo "Check for the required Rcov files in " $ANAVINFO
    exit 1
  fi
fi

# Copy CRTM coefficient files based on entries in satinfo file
crtm_coeffs="./crtm_coeffs/"
mkdir -p ${crtm_coeffs}
for file in `awk '{if($1!~"!"){print $1}}' satinfo | sort | uniq` ;do
   $nln $fixcrtm/${file}.SpcCoeff.bin ${crtm_coeffs}/${file}.SpcCoeff.bin
   $nln $fixcrtm/${file}.TauCoeff.bin ${crtm_coeffs}/${file}.TauCoeff.bin
done
$nln $fixcrtm/amsua_metop-a_v2.SpcCoeff.bin ${crtm_coeffs}/amsua_metop-a_v2.SpcCoeff.bin

$nln $emiscoef_IRwater  ${crtm_coeffs}Nalli.IRwater.EmisCoeff.bin
$nln $emiscoef_IRice    ${crtm_coeffs}NPOESS.IRice.EmisCoeff.bin
$nln $emiscoef_IRsnow   ${crtm_coeffs}NPOESS.IRsnow.EmisCoeff.bin
$nln $emiscoef_IRland   ${crtm_coeffs}NPOESS.IRland.EmisCoeff.bin
$nln $emiscoef_VISice   ${crtm_coeffs}NPOESS.VISice.EmisCoeff.bin
$nln $emiscoef_VISland  ${crtm_coeffs}NPOESS.VISland.EmisCoeff.bin
$nln $emiscoef_VISsnow  ${crtm_coeffs}NPOESS.VISsnow.EmisCoeff.bin
$nln $emiscoef_VISwater ${crtm_coeffs}NPOESS.VISwater.EmisCoeff.bin
$nln $emiscoef_MWwater  ${crtm_coeffs}FASTEM6.MWwater.EmisCoeff.bin
$nln $aercoef           ${crtm_coeffs}AerosolCoeff.bin
$nln $cldcoef           ${crtm_coeffs}CloudCoeff.bin

# Copy observational data to $DATA
$nln $datobs/${prefix_obs}.prepbufr                ./prepbufr
$nln $datobs/${prefix_obs}.prepbufr.acft_profiles  ./prepbufr_profl
$nln $datobs/${prefix_obs}.nsstbufr                ./nsstbufr
$nln $datobs/${prefix_obs}.syndata.tcvitals.tm00   ./tcvitl
$nln $datobs/${prefix_obs}.gpsro.${suffix}         ./gpsrobufr
$nln $datobs/${prefix_obs}.satwnd.${suffix}        ./satwndbufr
$nln $datobs/${prefix_obs}.hdob.${suffix}          ./hdobbufr

$nln $datobs/${prefix_obs}.osbuv8.${suffix}        ./sbuvbufr
$nln $datobs/${prefix_obs}.gome.${suffix}          ./gomebufr
$nln $datobs/${prefix_obs}.omi.${suffix}           ./omibufr
$nln $datobs/${prefix_obs}.mls.${suffix}           ./mlsbufr
$nln $datobs/${prefix_obs}.ompsn8.${suffix}        ./ompsnpbufr
$nln $datobs/${prefix_obs}.ompst8.${suffix}        ./ompstcbufr
$nln $datobs/${prefix_obs}.ompslp.${suffix}        ./ompslpbufr

$nln $datobs/${prefix_obs}.goesfv.${suffix}        ./gsnd1bufr
$nln $datobs/${prefix_obs}.hrs3db.${suffix}        ./hirs3bufr_db
$nln $datobs/${prefix_obs}.airsev.${suffix}        ./airsbufr
$nln $datobs/${prefix_obs}.sevcsr.${suffix}        ./seviribufr
$nln $datobs/${prefix_obs}.saphir.${suffix}        ./saphirbufr
$nln $datobs/${prefix_obs}.avcsam.${suffix}        ./avhambufr
$nln $datobs/${prefix_obs}.avcspm.${suffix}        ./avhpmbufr
$nln $datobs/${prefix_obs}.1bhrs4.${suffix}        ./hirs4bufr
$nln $datobs/${prefix_obs}.1bhrs2.${suffix}        ./hirs2bufr
$nln $datobs/${prefix_obs}.1bhrs3.${suffix}        ./hirs3bufr
$nln $datobs/${prefix_obs}.eshrs3.${suffix}        ./hirs3bufrears
$nln $datobs/${prefix_obs}.hrs3db.${suffix}        ./hirs3bufr_db
$nln $datobs/${prefix_obs}.mtiasi.${suffix}        ./iasibufr
$nln $datobs/${prefix_obs}.esiasi.${suffix}        ./iasibufrears
$nln $datobs/${prefix_obs}.iasidb.${suffix}        ./iasibufr_db
$nln $datobs/${prefix_obs}.crisf4.${suffix}        ./crisfsbufr
$nln $datobs/${prefix_obs}.escrsf.${suffix}        ./crisfsbufrears
$nln $datobs/${prefix_obs}.crsfdb.${suffix}        ./crisfsbufr_db
$nln $datobs/${prefix_obs}.ahicsr.${suffix}        ./ahibufr
$nln $datobs/${prefix_obs}.gsrcsr.${suffix}        ./abibufr
$nln $datobs/${prefix_obs}.sstvcw.${suffix}        ./sstviirs

$nln $datobs/${prefix_obs}.1bmhs.${suffix}         ./mhsbufr
$nln $datobs/${prefix_obs}.1bmsu.${suffix}         ./msubufr
$nln $datobs/${prefix_obs}.gmi1cr.${suffix}        ./gmibufr
$nln $datobs/${prefix_obs}.ssmit.${suffix}         ./ssmitbufr
$nln $datobs/${prefix_obs}.ssmisu.${suffix}        ./ssmisbufr
$nln $datobs/${prefix_obs}.1bamua.${suffix}        ./amsuabufr
$nln $datobs/${prefix_obs}.esamua.${suffix}        ./amsuabufrears
$nln $datobs/${prefix_obs}.amuadb.${suffix}        ./amsuabufr_db
$nln $datobs/${prefix_obs}.1bamub.${suffix}        ./amsubbufr
$nln $datobs/${prefix_obs}.esamub.${suffix}        ./amsubbufrears
$nln $datobs/${prefix_obs}.amubdb.${suffix}        ./amsubbufr_db
$nln $datobs/${prefix_obs}.atms.${suffix}          ./atmsbufr
$nln $datobs/${prefix_obs}.atmsdb.${suffix}        ./atmsbufr_db
$nln $datobs/${prefix_obs}.esatms.${suffix}        ./atmsbufrears

# Do not process
## $nln $datobs/${prefix_obs}.amsre.${suffix}      ./amsrebufr
## $nln $datobs/${prefix_obs}.amsr2.tm00.bufr_d    ./amsr2bufr

# Copy bias correction, atmospheric and surface files
$nln $datges/${prefix_ges}.abias                      ./satbias_in
$nln $datges/${prefix_ges}.abias_pc                   ./satbias_pc
$nln $datges/${prefix_ges}.abias_air                  ./aircftbias_in

member=mem001
$nln $datens/$member/${prefix_ges}.sfcf003.nc         ./sfcf03
$nln $datens/$member/${prefix_ges}.sfcf006.nc         ./sfcf06
$nln $datens/$member/${prefix_ges}.sfcf009.nc         ./sfcf09

$nln $datens/$member/${prefix_ges}.atmf003.nc         ./sigf03
$nln $datens/$member/${prefix_ges}.atmf006.nc         ./sigf06
$nln $datens/$member/${prefix_ges}.atmf009.nc         ./sigf09

$nln $datens/${prefix_ens}.sfcf006.ensmean.nc         ./sfcf06_anlgrid


 
# Copy CRTM coefficient files based on entries in satinfo file
for file in `awk '{if($1!~"!"){print $1}}' ./satinfo | sort | uniq` ;do
    $ncp $fixcrtm/${file}.SpcCoeff.bin ./
    $ncp $fixcrtm/${file}.TauCoeff.bin ./
done


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
cd $tmpdir
loops="01 03"
for loop in $loops; do

case $loop in
  01) string=ges;;
  03) string=anl;;
   *) string=$loop;;
esac

#  Collect diagnostic files for obs types (groups) below
   listall="hirs2_n14 msu_n14 sndr_g08 sndr_g11 sndr_g11 sndr_g12 sndr_g13 sndr_g08_prep sndr_g11_prep sndr_g12_prep sndr_g13_prep sndrd1_g11 sndrd2_g11 sndrd3_g11 sndrd4_g11 sndrd1_g12 sndrd2_g12 sndrd3_g12 sndrd4_g12 sndrd1_g13 sndrd2_g13 sndrd3_g13 sndrd4_g13 hirs3_n15 hirs3_n16 hirs3_n17 amsua_n15 amsua_n16 amsua_n17 amsub_n15 amsub_n16 amsub_n17 hsb_aqua airs_aqua amsua_aqua imgr_g08 imgr_g11 imgr_g12 pcp_ssmi_dmsp pcp_tmi_trmm conv sbuv2_n16 sbuv2_n17 sbuv2_n18 gome_metop-a omi_aura ssmi_f13 ssmi_f15 hirs4_n18 hirs4_metop-a amsua_n18 amsua_metop-a mhs_n18 mhs_metop-a amsre_low_aqua amsre_mid_aqua amsre_hig_aqua ssmis_las_f16 ssmis_uas_f16 ssmis_img_f16 ssmis_env_f16 iasi_metop-a"
   for type in $listall; do
      count=`ls dir.*/${type}_${loop}* | wc -l`
      if [[ $count -gt 0 ]]; then
         cat dir.*/${type}_${loop}* > diag_${type}_${string}.${global_adate}
         compress diag_${type}_${string}.${global_adate}
         $ncp diag_${type}_${string}.${global_adate}.Z $savdir/
      fi
   done
done
echo "Time after diagnostic loop is `date` "



# If requested, clean up $tmpdir
if [[ "$CLEAN" = "YES" ]];then
   if [[ $rc -eq 0 ]];then
      rm -rf $tmpdir
      cd $tmpdir
      cd ../
      rmdir $tmpdir
   fi
fi


# End of script
exit
