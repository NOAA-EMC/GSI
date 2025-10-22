#!/bin/bash

set -eux

# checking if the first positional argument is specified to build a special system
if [[ $# -ge 1 ]] ; then
    DASYS_NAME="$1"
    if [[ ${DASYS_NAME,,} == "3drtma" ||  ${DASYS_NAME,,} == "rtma3d" ]] ; then
        BUILD_GSI4RTMA3D="Yes"
        echo " ****** Building GSI for 3D-RTMA ****** "
    else
        unset BUILD_GSI4RTMA3D
    fi
fi
unset DASYS_NAME

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
REGRESSION_TESTS=${REGRESSION_TESTS:-"YES"} # Build regression test suite

#==============================================================================#

# Detect machine (sets MACHINE_ID)
source $DIR_ROOT/ush/detect_machine.sh

# Load modules
set +x
source $DIR_ROOT/ush/module-setup.sh
module use $DIR_ROOT/modulefiles
module load "gsi_${MACHINE_ID}.${COMPILER}"
module list
set -x

# Set CONTROLPATH variable to user develop installation
CONTROLPATH="$DIR_ROOT/../develop/install/bin"
# Collect BUILD Options
CMAKE_OPTS+=" -DCMAKE_BUILD_TYPE=$BUILD_TYPE"

# Install destination for built executables, libraries, CMake Package config
CMAKE_OPTS+=" -DCMAKE_INSTALL_PREFIX=$INSTALL_PREFIX"

# Configure for GSI and EnKF
CMAKE_OPTS+=" -DGSI_MODE=$GSI_MODE -DENKF_MODE=${ENKF_MODE}"

# Build regression test suite (on supported MACHINE_ID where CONTROLPATH exists)
[[ ${REGRESSION_TESTS} =~ [yYtT] ]] && CMAKE_OPTS+=" -DBUILD_REG_TESTING=ON -DCONTROLPATH=${CONTROLPATH:-}"

# Re-use or create a new BUILD_DIR (Default: create new BUILD_DIR)
[[ ${BUILD_CLEAN:-"YES"} =~ [yYtT] ]] && rm -rf $BUILD_DIR
mkdir -p $BUILD_DIR && cd $BUILD_DIR

# Configure, build, install
#     specifit options for 3DRTMA
if [[ -v BUILD_GSI4RTMA3D && ${BUILD_GSI4RTMA3D} =~ [yYtT] ]] ; then
    echo " ****** Building GSI with GSD Cloud Analysis for 3D-RTMA ****** "
    BUILD_GSDCLOUD=${BUILD_GSDCLOUD:-"ON"}      # Build GSD Cloud Analysis library
    USE_GSDCLOUD=${USE_GSDCLOUD:-"ON"}          # Build with GSD Cloud Analysis library
    CMAKE_OPTS+=" -DBUILD_GSDCLOUD=${BUILD_GSDCLOUD} -DUSE_GSDCLOUD=${USE_GSDCLOUD}"
    cmake $CMAKE_OPTS $DIR_ROOT 2>&1 | tee log.cmake
    make -j ${BUILD_JOBS:-8} VERBOSE=${BUILD_VERBOSE:-1} 2>&1 | tee log.make
    make install 2>&1 | tee log.install
else
    cmake $CMAKE_OPTS $DIR_ROOT
    make -j ${BUILD_JOBS:-8} VERBOSE=${BUILD_VERBOSE:-}
    make install
fi

exit
