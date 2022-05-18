#!/bin/sh
# It is now possible to run all regression tests (except RTMA) using the hybrid ensemble option with
# internally generated random ensemble perturbations.  No script changes are required.
#  To run with hybrid ensemble option on, change HYBENS_GLOBAL and/or HYBENS_REGIONAL from "false" to "true".
#  These are located at the end of this script.

if [ "$#" = 7 ] ; then
  export basedir=$1
  export builddir=$2
  export gsisrc=$3
  export gsiexec_updat=$4
  export enkfexec_updat=$5
  export gsiexec_contrl=$6
  export enkfexec_contrl=$7
  export fixgsi="$gsisrc/fix"
  export scripts="$gsisrc/regression"
  export ush="$gsisrc/ush"
  export cmaketest="true"
  export clean="false"
  export ptmpName=`echo $builddir | sed -e "s/\//_/g"`
else
  # Name of the branch being tested
  updat="XXXXXXXX"
  contrl="XXXXXXXX"
  export cmaketest="false"
  export clean="false"
  export ptmpName=""
fi

# Determine the machine
if [[ -d /dcom && -d /hwrf ]]; then # WCOSS
  export machine="WCOSS"
elif [[ -d /glade ]]; then # Cheyenne
  export machine="Cheyenne"
elif [[ -d /scratch1 ]]; then # Hera
  export machine="Hera"
elif [[ -d /jetmon ]]; then # Jet
  export machine="Jet"
elif [[ -d /cm ]]; then # LUNA or SURGE
  export machine="WCOSS_C"
elif [[ -d /ioddev_dell ]]; then # venus or mars
  export machine="WCOSS_D"
elif [[ -d /discover ]]; then # NCCS Discover
  export machine="Discover"
elif [[ -d /sw/gaea ]]; then # Gaea
  export machine="Gaea"
elif [[ -d /data/prod ]]; then # S4
  export machine="S4"
elif [[ -d /work ]]; then # Orion
  export machine="Orion"
fi
echo "Running Regression Tests on '$machine'";

case $machine in
  WCOSS_D)
    export noscrub=/gpfs/dell2/emc/modeling/noscrub/$LOGNAME
    export group="dev"
    export queue="dev"

    export ptmp="/gpfs/dell2/ptmp/$LOGNAME/$ptmpName"

    export fixcrtm="/gpfs/dell2/emc/modeling/noscrub/Michael.Lueken/fix_update"
    export casesdir="/gpfs/dell2/emc/modeling/noscrub/Michael.Lueken/CASES"

    export check_resource="yes"

    export accnt=""
  ;;
  WCOSS)
    if [ -d /da/noscrub/$LOGNAME ]; then
      export noscrub=/da/noscrub/$LOGNAME
    elif [ -d /global/noscrub/$LOGNAME ]; then
      export noscrub=/global/noscrub/$LOGNAME
    fi
    if [[ "$cmaketest" = "false" ]]; then
      export basedir="/global/save/$LOGNAME/gsi"
    fi
    export group="dev"
    export queue="dev"

    export ptmp="/ptmpp1/$LOGNAME/$ptmpName"

    export fixcrtm="/da/save/Michael.Lueken/CRTM_REL-2.2.3/crtm_v2.2.3/fix_update"
    export casesdir="/da/noscrub/Michael.Lueken/CASES"

    export check_resource="yes"

    export accnt=""
  ;;
  Cheyenne)
    export queue="economy"
    export noscrub="/glade/scratch/$LOGNAME"
    export group="global"
    if [[ "$cmaketest" = "false" ]]; then
      export basedir="/glade/scratch/$LOGNAME/gsi"
    fi
    export ptmp="/glade/scratch/$LOGNAME/$ptmpName"

    export fixcrtm="/glade/p/ral/jntp/tools/crtm/2.2.3/fix_update"
    export casesdir="/glade/p/ral/jntp/tools/CASES"

    export check_resource="no"
    export accnt="p48503002"
  ;;
  Hera)
    if [ -d /scratch1/NCEPDEV/da/$LOGNAME ]; then
      export noscrub="/scratch1/NCEPDEV/da/$LOGNAME/noscrub"
    elif [ -d /scratch1/NCEPDEV/global/$LOGNAME ]; then
      export noscrub="/scratch1/NCEPDEV/global/$LOGNAME/noscrub"
     elif [ -d /scratch2/BMC/gsienkf/$LOGNAME ]; then
      export noscrub="/scratch2/BMC/gsienkf/$LOGNAME"
    fi

    export group="global"
    export queue="batch"
    if [[ "$cmaketest" = "false" ]]; then
      export basedir="/scratch1/NCEPDEV/da/$LOGNAME/git/gsi"
    fi

    export ptmp="/scratch1/NCEPDEV/stmp2/$LOGNAME/$ptmpName"

    export fixcrtm="/scratch1/NCEPDEV/da/Michael.Lueken/CRTM_REL-2.2.3/crtm_v2.2.3/fix_update"
    export casesdir="/scratch1/NCEPDEV/da/Michael.Lueken/noscrub/CASES"

    export check_resource="no"

    export accnt="da-cpu"

    #  On Hera, there are no scrubbers to remove old contents from stmp* directories.
    #  After completion of regression tests, will remove the regression test subdirecories
    export clean=".true."
  ;;
  Jet)

    export noscrub=/lfs1/NESDIS/nesdis-rdo2/$LOGNAME/noscrub
    export ptmp=/lfs1/NESDIS/nesdis-rdo2/$LOGNAME/ptmp
    export fixcrtm="/lfs1/NESDIS/nesdis-rdo2/David.Huber/save/CRTM_REL-2.2.3/crtm_v2.2.3/fix_update"
    export casesdir="/lfs1/NESDIS/nesdis-rdo2/David.Huber/save/CASES"
    export check_resource="no"
    export accnt="nesdis-rdo2"

    export group="global"
    export queue="batch"
    if [[ "$cmaketest" = "false" ]]; then
      export basedir="/lfs1/NESDIS/nesdis-rdo2/$LOGNAME/gsi"
    fi

    export ptmp="/lfs1/NESDIS/nesdis-rdo2/$LOGNAME/ptmp/$ptmpName"

    #  On Jet, there are no scrubbers to remove old contents from stmp* directories.
    #  After completion of regression tests, will remove the regression test subdirecories
    export clean=".true."
  ;;
  WCOSS_C)
    if [ -d /gpfs/hps3/emc/global/noscrub/$LOGNAME ]; then
       export noscrub="/gpfs/hps3/emc/global/noscrub/$LOGNAME"
    elif [ -d /gpfs/hps3/emc/da/noscrub/$LOGNAME ]; then
       export noscrub="/gpfs/hps3/emc/da/noscrub/$LOGNAME"
    elif [ -d /gpfs/hps3/emc/hwrf/noscrub/$LOGNAME ]; then
        export noscrub="/gpfs/hps3/emc/hwrf/noscrub/$LOGNAME"
    fi
    if [[ "$cmaketest" = "false" ]]; then
      export basedir="/gpfs/hps3/emc/global/noscrub/$LOGNAME/svn/gsi"
    fi
    export group="dev"
    export queue="dev"

    export ptmp="/gpfs/hps/ptmp/$LOGNAME/$ptmpName"

    export fixcrtm="/gpfs/hps3/emc/da/noscrub/Michael.Lueken/CRTM_REL-2.2.3/fix_update"
    export casesdir="/gpfs/hps3/emc/da/noscrub/Michael.Lueken/CASES"

    export check_resource="no"

    export accnt=""
  ;;
  Discover)
    if [[ "$cmaketest" = "false" ]]; then
        echo "Regression tests on Discover need to be run via ctest"
        exit 1
    fi
    export ptmp=$basedir
    export ptmp=$basedir
    export noscrub=$basedir
    export fixcrtm="/discover/nobackup/projects/gmao/share/gmao_ops/fvInput_4dvar/gsi/etc/fix_ncep20170329/REL-2.2.3-r60152_local-rev_1/CRTM_Coeffs/$endianness"
    export casesdir="/discover/nobackup/projects/gmao/obsdev/wrmccart/NCEP_regression/CASES"
    export check_resource="no"
    export accnt="g0613"
    export queue="compute"
    export clean=".false."
  ;;
  *)
    echo "Regression tests are not setup on '$machine', ABORT!"
    exit 1
  ;;
esac

if [[ "$cmaketest" = "false" ]]; then
  export builddir=$noscrub/build
  export gsisrc="$basedir/$updat/src"
  export gsiexec_updat="$gsisrc/global_gsi.x"
  export gsiexec_contrl="$basedir/$contrl/src/global_gsi.x"
  export enkfexec_updat="$gsisrc/enkf/global_enkf.x"
  export enkfexec_contrl="$basedir/$contrl/src/enkf/global_enkf.x"
  export fixgsi="$basedir/$updat/fix"
  export scripts="$basedir/$updat/regression"
  export ush="$basedir/$updat/ush"
fi

# We are dealing with *which* endian files
export endianness="Big_Endian"

# Paths to tmpdir and savedir base on ptmp
export tmpdir="$ptmp"
export savdir="$ptmp"

# Variables with the same values are defined below.

# Default resolution
export JCAP="62"

# Case Study analysis dates
export global_T62_adate="2016120300"
export global_4dvar_T62_adate="2014080400"
export global_hybrid_T126_adate="2014092912"
export global_4denvar_T126_adate="2019041500"
export global_fv3_4denvar_T126_adate="2018110500"
export global_fv3_4denvar_C192_adate="2019061006"
export global_enkf_T62_adate="2014092912"
export global_lanczos_T62_adate="2014080400"
export global_nemsio_T62_adate="2013011400"
export nmmb_nems_adate="2015061000"
export arw_binary_adate="2010072412"
export arw_netcdf_adate="2008051112"
export nmm_binary_adate="2010021600"
export nmm_netcdf_adate="2007122000"
export rtma_adate="2020022420"
export hwrf_nmm_adate="2012102812"
export fv3_netcdf_adate="2017030100"
export global_C96_fv3aero_adate="2019062200"
export global_C96_fv3aerorad_adate="2019062200"

# Paths for canned case data.
export global_T62_obs="$casesdir/global/sigmap/$global_T62_adate"
export global_T62_ges="$casesdir/global/sigmap/$global_T62_adate"
export global_4dvar_T62_obs="$casesdir/global/sigmap/$global_4dvar_T62_adate"
export global_4dvar_T62_ges="$casesdir/global/sigmap/$global_4dvar_T62_adate"
export global_hybrid_T126_datobs="$casesdir/global/sigmap/$global_hybrid_T126_adate/obs"
export global_4denvar_T126_datges="$casesdir/global/sigmap/$global_4denvar_T126_adate"
export global_4denvar_T126_datobs="$casesdir/global/sigmap/$global_4denvar_T126_adate"
export global_fv3_4denvar_T126_datges="$casesdir/global/fv3/$global_fv3_4denvar_T126_adate"
export global_fv3_4denvar_T126_datobs=$global_fv3_4denvar_T126_datges
export global_fv3_4denvar_C192_datges="$casesdir/global/fv3/$global_fv3_4denvar_C192_adate"
export global_fv3_4denvar_C192_datobs=$global_fv3_4denvar_C192_datges
export global_hybrid_T126_datges="$casesdir/global/sigmap/$global_hybrid_T126_adate/ges"
export global_enkf_T62_datobs="$casesdir/global/sigmap/$global_enkf_T62_adate/new_obs"
export global_enkf_T62_datges="$casesdir/global/sigmap/$global_enkf_T62_adate/ges"
export global_lanczos_T62_obs="$casesdir/global/sigmap/$global_lanczos_T62_adate"
export global_lanczos_T62_ges="$casesdir/global/sigmap/$global_lanczos_T62_adate"
export global_nemsio_T62_obs="$casesdir/global/sigmap/$global_nemsio_T62_adate"
export global_nemsio_T62_ges="$casesdir/global/sigmap_nemsio/$global_nemsio_T62_adate"
export nmmb_nems_4denvar_obs="$casesdir/regional/nmmb_nems/$nmmb_nems_adate"
export nmmb_nems_4denvar_ges="$casesdir/regional/nmmb_nems/$nmmb_nems_adate"
export arw_binary_obs="$casesdir/regional/arw_binary/$arw_binary_adate"
export arw_binary_ges="$casesdir/regional/arw_binary/$arw_binary_adate"
export arw_netcdf_obs="$casesdir/regional/arw_netcdf/$arw_netcdf_adate"
export arw_netcdf_ges="$casesdir/regional/arw_netcdf/$arw_netcdf_adate"
export nmm_binary_obs="$casesdir/regional/ndas_binary/$nmm_binary_adate"
export nmm_binary_ges="$casesdir/regional/ndas_binary/$nmm_binary_adate"
export nmm_netcdf_obs="$casesdir/regional/ndas_binary/$nmm_netcdf_adate"
export nmm_netcdf_ges="$casesdir/regional/nmm_netcdf/$nmm_netcdf_adate"
export rtma_obs="$casesdir/regional/rtma_binary/$rtma_adate"
export rtma_ges="$casesdir/regional/rtma_binary/$rtma_adate"
export hwrf_nmm_obs="$casesdir/regional/hwrf_nmm/$hwrf_nmm_adate"
export hwrf_nmm_ges="$casesdir/regional/hwrf_nmm/$hwrf_nmm_adate"
export fv3_netcdf_obs="$casesdir/regional/fv3_netcdf/$fv3_netcdf_adate"
export fv3_netcdf_ges="$casesdir/regional/fv3_netcdf/$fv3_netcdf_adate"
export global_C96_fv3aero_obs="$casesdir/global/fv3/$global_C96_fv3aero_adate"
export global_C96_fv3aero_ges="$casesdir/global/fv3/$global_C96_fv3aero_adate"
export global_C96_fv3aerorad_obs="$casesdir/global/fv3/$global_C96_fv3aerorad_adate"
export global_C96_fv3aerorad_ges="$casesdir/global/fv3/$global_C96_fv3aerorad_adate"

# Define type of GPSRO data to be assimilated (refractivity or bending angle)
export gps_dtype="gps_bnd"

# Regression vfydir
export regression_vfydir="$noscrub/regression"

# Define debug variable - If you want to run the debug tests, set this variable to .true.  Default is .false.
export debug=".false."

# Define parameters for global_T62_3d4dvar and global_T62_4dvar
export minimization="lanczos"  # If "lanczos", use sqrtb lanczos minimization algorithm.  Otherwise use "pcgsoi".
export nhr_obsbin="6"          # Time window for observation binning.  Use "6" for 3d4dvar test.  Otherwise use "1"

# Define parameters for hybrid ensemble option test.
# (default is set to false, so no hybrid ensemble option test.)

export HYBENS_GLOBAL=".false."
export ENSEMBLE_SIZE_GLOBAL="10"
export HYBENS_UV_GLOBAL=".true."
export BETA_S0_GLOBAL="0.5"
export HYBENS_HOR_SCALE_GLOBAL="1500"
export HYBENS_VER_SCALE_GLOBAL="20"
export GENERATE_ENS_GLOBAL=".true."
export HYBENS_ANISO_GLOBAL=".false."

export HYBENS_REGIONAL=".false."
export ENSEMBLE_SIZE_REGIONAL="10"
export HYBENS_UV_REGIONAL=".true."
export BETA_S0_REGIONAL="0.5"
export HYBENS_HOR_SCALE_REGIONAL="1500"
export HYBENS_VER_SCALE_REGIONAL="20"
export GENERATE_ENS_REGIONAL=".true."
export HYBENS_ANISO_REGIONAL=".false."
export NLON_ENS_REGIONAL="0"
export NLAT_ENS_REGIONAL="0"
export JCAP_ENS_REGIONAL="0"
export JCAP_ENS_TEST_REGIONAL="0"

# Toggle EnKF update code bias correction flag: lupd_satbiasc
# TRUE  =        compute and update radiance bias correction
# FALSE = do NOT compute or  update radiance bias correction
# default is FALSE (as done in NCEP operations)
export lupd_satbiasc=".false."
