#!/bin/bash

set -eu

if [[ $(uname -s) == Darwin ]]; then
  readonly DIR_ROOT=$(cd "$(dirname "$(greadlink -f -n "${BASH_SOURCE[0]}" )" )/.." && pwd -P)
else
  readonly DIR_ROOT=$(cd "$(dirname "$(readlink -f -n "${BASH_SOURCE[0]}" )" )/.." && pwd -P)
fi

BUILD_TYPE=${1:-${BUILD_TYPE:-"Release"}}
CMAKE_OPTS="${2:-${CMAKE_OPTS:-}}"
COMPILER=${3:-${COMPILER:-"intel"}}

# Detect machine
source $DIR_ROOT/detect_machine.sh

# Load modules
source $DIR_ROOT/ush/module-setup.sh
module use $DIR_ROOT/modulefiles
module load modulefile.ProdGSI.$MACHINE_ID
module list

# Set CONTROLPATH variables for Regression testing on supported MACHINE_ID
if [[ $MACHINE_ID = wcoss ]] ; then
    CONTROLPATH="/da/save/Michael.Lueken/svn1/build"
elif [[ $MACHINE_ID = wcoss_dell_p3 ]] ; then
    CONTROLPATH="/gpfs/dell2/emc/modeling/noscrub/Michael.Lueken/svn1/build"
elif [[ $MACHINE_ID = hera.intel ]] ; then
    CONTROLPATH="/scratch1/NCEPDEV/da/Michael.Lueken/svn1/build"
fi

# Collect BUILD Options
CMAKE_OPTS+=" -DCMAKE_BUILD_TYPE=$BUILD_TYPE"

# Install destination for built executables, libraries, CMake Package config
CMAKE_OPTS+=" -DCMAKE_INSTALL_PREFIX=$DIR_ROOT/install"

# By default; build the Regional GSI and Global EnKF applications
CMAKE_OPTS+=" -DGSI_MODE=Regional -DENKF_MODE=GFS"

# Build utilities
CMAKE_OPTS+=" -DBUILD_UTIL_COV_CALC=ON -DBUILD_UTIL_ENKF_GFS=ON -DBUILD_UTIL_EFSOIL=ON -DBUILD_UTIL_MON=ON"

# Build regression test
[[ -n $CONTROLPATH ]] || CMAKE_OPTS+=" -DBUILD_REG_TESTING=ON -DCONTROLPATH=$CONTROLPATH"

rm -rf $DIR_ROOT/build
mkdir -p $DIR_ROOT/build
cd $DIR_ROOT/build

cmake $CMAKE_OPTS $DIR_ROOT

make -j ${BUILD_JOBS:-8} VERBOSE=${BUILD_VERBOSE:-}

make install

exit
