#!/bin/sh

# This script removes or restores directories and/or files found in 
# the rlist below from the current working directory.   This script
# is intended to be executed when NCO implements GFS DA updates.
# NCO does not want package installations for operations to include
# non-operational code.   This script removes directories and files 
# not used by operations from the installation directory.
#
# Two modes are supported:  prune, restore
#   prune:  use git rm -r to remove directories and files.
#           The directories and files to remove are found
#           in rlist below
#   restore:  use git RESET head and git checkout to restore
#             removed directories and files
#    

set -ex

mode=$1

# Check mode and set string
if [[ "$mode" = "prune" ]]; then
    string="rm -r"
elif [[ "$mode" = "restore" ]]; then
    string="reset HEAD"
else
    echo " "
    echo "***ERROR*** invalid mode= $mode"
    echo " valid modes are prune or restore"
    echo " "
    exit
fi


# Set root directory
readonly topdir=$(cd "$(dirname "$(readlink -f -n "${BASH_SOURCE[0]}" )" )/.." && pwd -P)
cd $topdir

echo " "
echo "Execute git $string in $topdir"
echo " "


# Process top level directories
cd $topdir
rlist="regression src/GSD unit-tests"
for type in $rlist; do
    git $string ${type}*
    rc=$?
    if [[ $rc -ne 0 ]]; then
        echo "***ERROR* git $string ${type}"
        exit
    fi
    if [[ "$mode" = "restore" ]]; then
	git checkout ${type}*
	rc=$?
	if [[ $rc -ne 0 ]]; then
            echo "***ERROR* git checkout ${type}"
            exit
	fi
    fi
done


# Process doc directories and files
cd $topdir/doc
rlist="EnKF_user_guide GSI_user_guide README.discover Release_Notes.fv3gfs_da.v15.0.0.txt Release_Notes.gfsda.v16.0.0.txt"
for type in $rlist; do
    git $string ${type}*
    rc=$?
    if [[ $rc -ne 0 ]]; then
        echo "***ERROR* git $string ${type}"
        exit
    fi
    if [[ "$mode" = "restore" ]]; then
        git checkout ${type}*
        rc=$?
        if [[ $rc -ne 0 ]]; then
            echo "***ERROR* git checkout ${type}"
            exit
        fi
    fi
done


# Process jobs directories and files
cd $topdir/jobs
rlist="JGDAS_EFSOI"
for type in $rlist; do
    git $string ${type}*
    rc=$?
    if [[ $rc -ne 0 ]]; then
        echo "***ERROR* git $string ${type}"
        exit
    fi
    if [[ "$mode" = "restore" ]]; then
        git checkout ${type}*
        rc=$?
        if [[ $rc -ne 0 ]]; then
            echo "***ERROR* git checkout ${type}"
            exit
        fi
    fi
done


# Process scripts directories and files
cd $topdir/scripts
rlist="exurma2p5_gsianl.sh exgdas_efsoi"
for type in $rlist; do
    git $string ${type}*
    rc=$?
    if [[ $rc -ne 0 ]]; then
        echo "***ERROR* git $string ${type}"
        exit
    fi
    if [[ "$mode" = "restore" ]]; then
        git checkout ${type}*
        rc=$?
        if [[ $rc -ne 0 ]]; then
            echo "***ERROR* git checkout ${type}"
            exit
        fi
    fi
done


# Process ush directories and files
cd $topdir/ush
rlist="Get_Initial_Files comenkf comgsi gfs_truncate_enkf llsub para refactor_4nco_global run_arw rungsi sub"
for type in $rlist; do
    git $string ${type}*
    rc=$?
    if [[ $rc -ne 0 ]]; then
        echo "***ERROR* git $string ${type}"
        exit
    fi
    if [[ "$mode" = "restore" ]]; then
        git checkout ${type}*
        rc=$?
        if [[ $rc -ne 0 ]]; then
            echo "***ERROR* git checkout ${type}"
            exit
        fi
    fi
done


# Process util directories and files
cd $topdir/util
rlist="Aero Analysis_Utilities Baseline Config Conventional_Monitor/data_extract Correlated_Obs DTC EFSOI Fit2Obs_Scorecard FOV GEN_BE_V2.0 GMI_BUFR MODIS_AOD Minimization_Monitor/data_xtrct Minimization_Monitor/image_gen Minimization_Monitor/nwprod/nam_minmon Misc NCEP NMC_Bkerror Ozone_Monitor/image_gen README Radar_Monitor Radiance_bias_correction_Utilities Radiance_Monitor/nwprod/nam_radmon Radiance_Utilities Single_Observation bufr_tools global_angupdate gsienvreport.sh python_utilities radar_process zero_biascoeff"
for type in $rlist; do
    git $string ${type}*
    rc=$?
    if [[ $rc -ne 0 ]]; then
        echo "***ERROR* git $string ${type}"
        exit
    fi
    if [[ "$mode" = "restore" ]]; then
        git checkout ${type}*
        rc=$?
        if [[ $rc -ne 0 ]]; then
            echo "***ERROR* git checkout ${type}"
            exit
        fi
    fi
done


# Process util/EnKF directories and files
cd $topdir/util/EnKF
rlist="arw python_utilities"
for type in $rlist; do
    git $string ${type}*
    rc=$?
    if [[ $rc -ne 0 ]]; then
        echo "***ERROR* git $string ${type}"
        exit
    fi
    if [[ "$mode" = "restore" ]]; then
        git checkout ${type}*
        rc=$?
        if [[ $rc -ne 0 ]]; then
            echo "***ERROR* git checkout ${type}"
            exit
        fi
    fi
done


# Process util/EnKF/gfs/src directories and files
cd $topdir/util/EnKF/gfs/src
rlist="adjustps misc preproc gribmean recenterncio_hybgain recenternemsiop_hybgain getnstensmeanp adderrspec getsfcnstensupdp"
for type in $rlist; do
    git $string ${type}*
    rc=$?
    if [[ $rc -ne 0 ]]; then
        echo "***ERROR* git $string ${type}"
        exit
    fi
    if [[ "$mode" = "restore" ]]; then
        git checkout ${type}*
        rc=$?
        if [[ $rc -ne 0 ]]; then
            echo "***ERROR* git checkout ${type}"
            exit
        fi
    fi
done
