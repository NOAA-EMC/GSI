#!/bin/bash

set -eu

# Get the root of the cloned GSI directory
readonly DIR_ROOT=$(cd "$(dirname "$(readlink -f -n "${BASH_SOURCE[0]}" )" )/.." && pwd -P)

# User Options
BUILD_TYPE=${BUILD_TYPE:-"Release"}
CMAKE_OPTS=${CMAKE_OPTS:-}
COMPILER=${COMPILER:-"intel"}
BUILD_DIR=${BUILD_DIR:-"${DIR_ROOT}/build"}
INSTALL_PREFIX=${INSTALL_PREFIX:-"${DIR_ROOT}/install"}
GSI_MODE=${GSI_MODE:-"Regional"}  # By default build Regional GSI (for regression testing)
ENKF_MODE=${ENKF_MODE:-"GFS"}     # By default build Global EnKF  (for regression testing)
UTIL_OPTS=${UTIL_OPTS:-"-DBUILD_UTIL_ALL=ON"} # By default build all GFS utilities
REGRESSION_TESTS=${REGRESSION_TESTS:-"YES"} # Build regression test suite

#==============================================================================#

# Detect machine (sets MACHINE_ID)
source $DIR_ROOT/ush/detect_machine.sh

# Load modules
source $DIR_ROOT/ush/module-setup.sh
module use $DIR_ROOT/modulefiles
module load gsi_$MACHINE_ID
module list

# Set CONTROLPATH variables for Regression testing on supported MACHINE_ID
if [[ $MACHINE_ID = wcoss ]] ; then
    CONTROLPATH="/da/save/Michael.Lueken/svn1/build"
elif [[ $MACHINE_ID = wcoss_dell_p3 ]] ; then
    CONTROLPATH="/gpfs/dell2/emc/modeling/noscrub/Michael.Lueken/svn1/install/bin"
elif [[ $MACHINE_ID = hera.intel ]] ; then
    CONTROLPATH="/scratch1/NCEPDEV/da/Michael.Lueken/svn1/install/bin"
fi

# Collect BUILD Options
CMAKE_OPTS+=" -DCMAKE_BUILD_TYPE=$BUILD_TYPE"

# Install destination for built executables, libraries, CMake Package config
CMAKE_OPTS+=" -DCMAKE_INSTALL_PREFIX=$INSTALL_PREFIX"

# Configure for GSI and EnKF
CMAKE_OPTS+=" -DGSI_MODE=$GSI_MODE -DENKF_MODE=${ENKF_MODE}"

# Build utilities
[[ -n ${UTIL_OPTS:-""} ]] && CMAKE_OPTS+=" $UTIL_OPTS"

# Build regression test suite (on supported MACHINE_ID where CONTROLPATH exists)
[[ ${REGRESSION_TESTS} =~ [yYtT] ]] && CMAKE_OPTS+=" -DBUILD_REG_TESTING=ON -DCONTROLPATH=${CONTROLPATH:-}"

# Re-use or create a new BUILD_DIR (Default: create new BUILD_DIR)
[[ ${BUILD_CLEAN:-"YES"} =~ [yYtT] ]] && rm -rf $BUILD_DIR
mkdir -p $BUILD_DIR && cd $BUILD_DIR

# Configure, build, install
set -x
cmake $CMAKE_OPTS $DIR_ROOT
make -j ${BUILD_JOBS:-8} VERBOSE=${BUILD_VERBOSE:-}
make install
set +x

exit
