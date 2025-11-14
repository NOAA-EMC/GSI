#!/bin/bash

set -eu
set +x

#=================================================================================================#
usage() {
#   This usage function is based on the usage function in the build.sh in NOAA-EMC RDASApp 
#   for JEDI-based DA pacakge. see
#   https://github.com/NOAA-EMC/RDASApp/blob/e9ea4c1c02ece82931bf66d2f36b6009a921f6d9/build.sh
  set +x
  echo
  echo "$0: GSI building script"
  echo "Usage: "
  echo "  -c additional CMake options. Multiple options are separated with comma, e.g., -c c1,c2"
  echo "     DEFAULT: <none>"
  echo "     Example: $0               # building GSI with default config (for most DA systems, e.g. GDAS, RRFS-DA, etc.)"
  echo "              $0 -c GSDCLOUD   # building GSI with GSD Cloud Analysis support (only for RAP/HRRR, 3DRTMA, not default)"
  echo "  -v build with verbose output and enable script-debugging by set -x"
  echo "     DEFAULT: <none> (this option requires no argument)"
  echo "  -l save the building log files under build directory"
  echo "     DEFAULT: <none> (this option requires no argument)"
  echo "  -h display this usage/help information and quit"
  echo 
  exit 1
}
#=================================================================================================#

# First, checking if any specific option(s) is passed through the option arguments
unset OPTS4CMAKE
unset SAVELOG
while getopts "c:hlv" opt; do
  case ${opt} in
    c)
      OPTS4CMAKE="${OPTARG}"
      ;;
    v)
      set -x
      BUILD_VERBOSE=1
      ;;
    l)
      SAVELOG="Yes"
      ;;
    h|\?|\:)
      usage
      ;;
  esac
done

shift $((OPTIND - 1)) # Shift positional parameters to remove parsed options

# Get the root of the cloned GSI directory
readonly DIR_ROOT=$(cd "$(dirname "$(readlink -f -n "${BASH_SOURCE[0]}" )" )/.." && pwd -P)

# User Options -- Defaults
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

# Configure for building GSI with User-specified options which are passed through "-c" option
shopt -s nocasematch		# enable case-insensitive matching for patterns
if [[ -v OPTS4CMAKE && -n "${OPTS4CMAKE}" ]] ; then
  IFS=',' read -ra ITEMS <<< "${OPTS4CMAKE}"
  for item in "${ITEMS[@]}"; do
    case "${item}" in
      gsdcloud|gsdcldanl|gsdcld|gsdcloudanalysis|cloudanalysis|cldanl)
        echo " ****** Building GSI with GSD Cloud Analysis Support ****** "
        BUILD_GSDCLOUD="ON"				# Build GSD Cloud Analysis library
        USE_GSDCLOUD="ON"				# Build with GSD Cloud Analysis library
        CMAKE_OPTS+=" -DBUILD_GSDCLOUD=${BUILD_GSDCLOUD} -DUSE_GSDCLOUD=${USE_GSDCLOUD}"
        ;;
      *)
        ;;
    esac
  done
fi
shopt -u nocasematch		# disable case-insensitive matching for patterns
unset OPTS4CMAKE

# Build regression test suite (on supported MACHINE_ID where CONTROLPATH exists)
[[ ${REGRESSION_TESTS} =~ [yYtT] ]] && CMAKE_OPTS+=" -DBUILD_REG_TESTING=ON -DCONTROLPATH=${CONTROLPATH:-}"

# Re-use or create a new BUILD_DIR (Default: create new BUILD_DIR)
[[ ${BUILD_CLEAN:-"YES"} =~ [yYtT] ]] && rm -rf $BUILD_DIR
mkdir -p $BUILD_DIR && cd $BUILD_DIR

# Configure, build, install
if [[ -v SAVELOG && ${SAVELOG} =~ [yYtT] ]] ; then
    cmake $CMAKE_OPTS $DIR_ROOT 2>&1 | tee log.cmake
    make -j ${BUILD_JOBS:-8} VERBOSE=${BUILD_VERBOSE:-} 2>&1 | tee log.make
    make install 2>&1 | tee log.install
else
    cmake $CMAKE_OPTS $DIR_ROOT
    make -j ${BUILD_JOBS:-8} VERBOSE=${BUILD_VERBOSE:-}
    make install
fi

exit
