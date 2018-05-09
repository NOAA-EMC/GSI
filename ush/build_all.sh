#!/bin/sh

set -ex

cd ..
pwd=$(pwd)

target=$1
dir_root=${2:-$pwd}

BUILD_GSI=${BUILD_GSI:-"YES"}
BUILD_ENKF=${BUILD_ENKF:-"YES"}
BUILD_UTILS=${BUILD_UTILS:-"YES"}
BUILD_RADMON=${BUILD_RADMON:-"YES"}
BUILD_OZNMON=${BUILD_OZNMON:-"YES"}

BUILD_RADMON_VER=2.0.4
BUILD_OZNMON_VER=2.0.0

if [ $target = wcoss ]; then
    . /usrx/local/Modules/3.2.10/init/sh
    conf_target=nco
elif [ $target = cray -o $target = wcoss_c ]; then
    . $MODULESHOME/init/sh
    conf_target=nco
elif [ $target = dell -o $target = wcoss_d ]; then
    . $MODULESHOME/init/sh
    conf_target=nco
elif [ $target = theia ]; then
    . /apps/lmod/lmod/init/sh
    conf_target=theia
else
    echo "unknown target = $target"
    exit 9
fi

dir_modules=$dir_root/modulefiles
if [ ! -d $dir_modules ]; then
    echo "modulefiles does not exist in $dir_modules"
    exit 10
fi
[ -d $dir_root/exec ] || mkdir -p $dir_root/exec

# First build GSI
if [ $BUILD_GSI = "YES" -o $BUILD_ENKF = "YES" ]; then

    clean=YES
    [[ $BUILD_ENKF = "YES" ]] && clean=NO
    $dir_root/ush/build_gsi.sh $target $pwd $clean

fi

# Next build EnKF
if [ $BUILD_ENKF = "YES" ]; then

    clean=YES
    $dir_root/ush/build_enkf.sh $target $pwd $clean

fi

# Next build EnKF utilities
if [ $BUILD_UTILS = "YES" ]; then

    clean=YES
    $dir_root/ush/build_enkf_utils.sh $target $pwd $clean

fi

# Next build Radiance Monitor
if [ $BUILD_RADMON = "YES" ]; then

    clean=YES
    $dir_root/ush/build_radmon.sh $target $pwd $clean $BUILD_RADMON_VER

fi

# Next build Ozone Monitor
if [ $BUILD_OZNMON = "YES" ]; then
    
    clean=YES
    $dir_root/ush/build_oznmon.sh $target $pwd $clean $BUILD_OZNMON_VER

fi


exit 0
