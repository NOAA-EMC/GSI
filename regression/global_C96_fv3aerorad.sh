set -x
# Set experiment name and analysis date

exp=$jobname

# Set the JCAP resolution which you want.
# All resolutions use LEVS=64
export LEVS=64

# Set runtime and save directories
tmpdir=$tmpdir/$tmpregdir/${exp}
savdir=$savdir/outC96_fv3aerorad/${exp}

# Specify GSI fixed field and data directories.
fixcrtm=${fixcrtm:-$CRTM_FIX}

# Set variables used in script
#   CLEAN up $tmpdir when finished (YES=remove, NO=leave alone)
#   ncp is cp replacement, currently keep as /bin/cp

UNCOMPRESS=gunzip
CLEAN=NO
ncp=/bin/cp
ncpl="ln -fs"

# Given the analysis date, compute the date from which the
# first guess comes.  Extract cycle and set prefix and suffix
# for guess and observation data files
PDY=`echo $global_C96_fv3aerorad_adate | cut -c1-8`
cyc=`echo $global_C96_fv3aerorad_adate | cut -c9-10`
gdate=`$ndate -06 $global_C96_fv3aerorad_adate`
gPDY=`echo $gdate | cut -c1-8`
gcyc=`echo $gdate | cut -c9-10`
hha=`echo $global_C96_fv3aerorad_adate | cut -c9-10`
hhg=`echo $gdate | cut -c9-10`
prefix_obs=gfs.t${hha}z
prefix_prep=$prefix_obs
prefix_tbc=gdas1.t${hhg}z
prefix_sfc=gfsC96.t${hhg}z
prefix_atm=gfsC96.t${hhg}z
prefix_aer=gfsC96.t${hhg}z
suffix_obs=tm00.bufr_d

datobs=$global_C96_fv3aerorad_obs/gfs.$PDY/$cyc
datanl=$global_C96_fv3aerorad_obs/gfs.$PDY/$cyc
datges=$global_C96_fv3aerorad_ges/gfs.$gPDY/$gcyc

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
        yyyy=$(echo ${CDATE:-$global_C96_fv3aerorad_adate}|cut -c1-4)
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
        yyyy=$(echo ${CDATE:-$global_C96_fv3aerorad_adate}|cut -c1-4)
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
        yyyy=$(echo ${CDATE:-$global_C96_fv3aerorad_adate}|cut -c1-4)
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
        yyyy=$(echo ${CDATE:-$global_C96_fv3aerorad_adate}|cut -c1-4)
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

. $scripts/regression_namelists.sh global_C96_fv3aerorad

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
#   cloudyinfo  = text file with information about assimilation of cloudy radiance
#   satangl  = angle dependent bias correction file (fixed in time)
#   pcpinfo  = text file with information about assimilation of prepcipitation rates
#   ozinfo   = text file with information about assimilation of ozone data
#   errtable = text file with obs error for conventional data (optional)
#   convinfo = text file with information about assimilation of conventional data
#   bufrtable= text file ONLY needed for single obs test (oneobstest=.true.)
#   bftab_sst= bufr table for sst ONLY needed for sst retrieval (retrieval=.true.)

berror=$fixgsi/Big_Endian/global_berror.l64y194.f77

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
satangl=$fixgsi/global_satangbias.txt
scaninfo=$fixgsi/global_scaninfo.txt
satinfo=$fixgsi/fv3aerorad_satinfo.txt
cloudyinfo=$fixgsi/cloudy_radiance_info.txt
convinfo=$fixgsi/global_convinfo_reg_test.txt
anavinfo=$fixgsi/anavinfo_fv3aerorad
ozinfo=$fixgsi/global_ozinfo.txt
pcpinfo=$fixgsi/global_pcpinfo.txt
hybens_info=$fixgsi/global_hybens_info.l64.txt
errtable=$fixgsi/prepobs_errtable.global
atmsbeaminfo=$fixgsi/atms_beamwidth.txt

# Copy executable and fixed files to $tmpdir
if [[ $exp == *"updat"* ]]; then
   $ncp $gsiexec_updat  ./gsi.x
elif [[ $exp == *"contrl"* ]]; then
   $ncp $gsiexec_contrl ./gsi.x
fi

mkdir ./crtm_coeffs
$ncp $berror   ./berror_stats
$ncp $emiscoef_IRwater ./crtm_coeffs/Nalli.IRwater.EmisCoeff.bin
$ncp $emiscoef_IRice ./crtm_coeffs/NPOESS.IRice.EmisCoeff.bin
$ncp $emiscoef_IRsnow ./crtm_coeffs/NPOESS.IRsnow.EmisCoeff.bin
$ncp $emiscoef_IRland ./crtm_coeffs/NPOESS.IRland.EmisCoeff.bin
$ncp $emiscoef_VISice ./crtm_coeffs/NPOESS.VISice.EmisCoeff.bin
$ncp $emiscoef_VISland ./crtm_coeffs/NPOESS.VISland.EmisCoeff.bin
$ncp $emiscoef_VISsnow ./crtm_coeffs/NPOESS.VISsnow.EmisCoeff.bin
$ncp $emiscoef_VISwater ./crtm_coeffs/NPOESS.VISwater.EmisCoeff.bin
$ncp $emiscoef_MWwater ./crtm_coeffs/FASTEM6.MWwater.EmisCoeff.bin
$ncp $aercoef  ./crtm_coeffs/AerosolCoeff.bin
$ncp $cldcoef  ./crtm_coeffs/CloudCoeff.bin
$ncp $satangl  ./satbias_angle
$ncp $scaninfo ./scaninfo
$ncp $satinfo  ./satinfo
$ncp $cloudyinfo  ./cloudy_radiance_info.txt
$ncp $pcpinfo  ./pcpinfo
$ncp $ozinfo   ./ozinfo
$ncp $convinfo ./convinfo
$ncp $errtable ./errtable
$ncp $anavinfo ./anavinfo
$ncp $aeroinfo ./aeroinfo
$ncp $hybens_info ./hybens_info
$ncp $atmsbeaminfo ./atms_beamwidth.txt

# Copy CRTM coefficient files
for file in `awk '{if($1!~"!"){print $1}}' ./satinfo | sort | uniq` ;do
    $ncp $fixcrtm/${file}.SpcCoeff.bin ./crtm_coeffs
    $ncp $fixcrtm/${file}.TauCoeff.bin ./crtm_coeffs
done

# Copy observational data to $tmpdir
$ncpl $datobs/${prefix_obs}.prepbufr                ./prepbufr
$ncpl $datobs/${prefix_obs}.prepbufr.acft_profiles  ./prepbufr_profl
$ncpl $datobs/${prefix_obs}.nsstbufr                ./nsstbufr
$ncpl $datobs/${prefix_obs}.airsev.${suffix_obs}        ./airsbufr
$ncpl $datobs/${prefix_obs}.mtiasi.${suffix_obs}        ./iasibufr
$ncpl $datobs/${prefix_obs}.cris.${suffix_obs}          ./crisbufr

# Copy bias correction, atmospheric and surface files
$ncpl $datges/gfs.t18z.abias           ./satbias_in
$ncpl $datges/gfs.t18z.abias_pc        ./satbias_pc
$ncpl $datges/gfs.t18z.abias_air       ./aircftbias_in
#$ncpl $datges/gfs.t18z.radstat         ./radstat.gdas

if [[ "$endianness" = "Big_Endian" ]]; then
   ln -s -f $datges/${prefix_sfc}.sfcf03            ./sfcf03
   ln -s -f $datges/${prefix_sfc}.sfcf06            ./sfcf06
   ln -s -f $datges/${prefix_sfc}.sfcf09            ./sfcf09
elif [[ "$endianness" = "Little_Endian" ]]; then
   ln -s -f $datges/${prefix_sfc}.sfcf03.le         ./sfcf03
   ln -s -f $datges/${prefix_sfc}.sfcf06.le         ./sfcf06
   ln -s -f $datges/${prefix_sfc}.sfcf09.le         ./sfcf09
fi

if [[ "$endianness" = "Big_Endian" ]]; then
   ln -s -f $datges/${prefix_atm}.sigf03        ./sigf03
   ln -s -f $datges/${prefix_atm}.sigf06        ./sigf06
   ln -s -f $datges/${prefix_atm}.sigf09        ./sigf09
elif [[ "$endianness" = "Little_Endian" ]]; then
   ln -s -f $datges/${prefix_atm}.sigf03.le     ./sigf03
   ln -s -f $datges/${prefix_atm}.sigf06.le     ./sigf06
   ln -s -f $datges/${prefix_atm}.sigf09.le     ./sigf09
fi

if [[ "$endianness" = "Big_Endian" ]]; then
   $ncpl $datges/${prefix_aer}.sigf03            ./aerf03
   $ncpl $datges/${prefix_aer}.sigf06            ./aerf06
   $ncpl $datges/${prefix_aer}.sigf09            ./aerf09
elif [[ "$endianness" = "Little_Endian" ]]; then
   $ncpl $datges/${prefix_aer}.sigf03.le         ./aerf03
   $ncpl $datges/${prefix_aer}.sigf06.le         ./aerf06
   $ncpl $datges/${prefix_aer}.sigf09.le         ./aerf09
fi

# Run GSI
cd $tmpdir
echo "run gsi now"
eval "$APRUN $tmpdir/gsi.x > stdout 2>&1"
rc=$?
exit $rc

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
