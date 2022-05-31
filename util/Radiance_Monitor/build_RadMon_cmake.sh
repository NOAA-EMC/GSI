#! /bin/bash

#------------------------------------------------------------------
#  build_RadMon_cmake.sh
#
#  This script builds all of the RadMon executables and moves them
#  to nwprod/radmon_shared/exec, data_extract/exec, and 
#  image_gen/exec locations. 
#------------------------------------------------------------------
RAD_ROOT=${PWD}
echo "RAD_ROOT = ${RAD_ROOT}"

# Get the root of the cloned GSI directory
readonly GSI_ROOT=$(cd "$(dirname "$(readlink -f -n "${BASH_SOURCE[0]}" )" )/../.." && pwd -P)

echo "GSI_ROOT = ${GSI_ROOT}"

#-------------------------------------------------------
# Modify CoMakeLists.txt to turn off GSI and ENKF builds
#-------------------------------------------------------
cml=${GSI_ROOT}/CMakeLists.txt
cp ${cml} ${cml}.orig

sed -i 's/option(BUILD_GSI "Build GSI" ON)/option(BUILD_GSI "Build GSI" OFF)/' ${cml}
sed -i 's/option(BUILD_ENKF "Build EnKF" ON)/option(BUILD_ENKF "Build EnKF" OFF)/' ${cml}

#--------------------------------------------------------------------
# Modify top level build script to build only the RadMon executables
#--------------------------------------------------------------------
cd ${GSI_ROOT}/ush
bld=build.sh
cp ${bld} ${bld}.orig

sed -i 's/-DBUILD_UTIL_ALL=ON/-DBUILD_UTIL_ALL=OFF -DBUILD_UTIL_RAD=ON/' ${bld}

./${bld}

mv ${bld}.orig ${bld}
mv ${cml}.orig ${cml}


#-------------------------------------------------------
#  move the executables to the correct exec directories
#-------------------------------------------------------
BIN_DIR=${GSI_ROOT}/install/bin

SHRD_EXEC=${RAD_ROOT}/nwprod/radmon_shared/exec
if [[ ! -d ${SHRD_EXEC} ]]; then
   mkdir -p ${SHRD_EXEC}
fi

file_list1="radmon_angle.x radmon_bcoef.x radmon_bcor.x radmon_time.x"
for file in $file_list1; do
   cp ${BIN_DIR}/$file ${SHRD_EXEC}/.
done

DE_EXEC=${RAD_ROOT}/data_extract/exec
if [[ ! -d ${DE_EXEC} ]]; then
   mkdir -p ${DE_EXEC}
fi

file_list_de="radmon_mk_base.x radmon_validate_tm.x"
for file in $file_list_de; do
   cp ${BIN_DIR}/$file ${DE_EXEC}/.
done

IG_EXEC=${RAD_ROOT}/image_gen/exec
if [[ ! -d ${IG_EXEC} ]]; then
   mkdir -p ${IG_EXEC}
fi

file_list_ig="radmon_ig_angle.x radmon_ig_bcoef.x radmon_ig_horiz.x radmon_ig_summary.x radmon_ig_time.x"
for file in $file_list_ig; do
   cp ${BIN_DIR}/$file $IG_EXEC/.
done

exit
