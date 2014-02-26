#!/bin/bash

# Usg:  mspps_orbit_1b.bash  NSS.AMAX.M2.D09263.S1208.E1348.B1515657.SV

if [[ $# -ne 1 ]] ; then
  echo "Error: orbit file missing."
  echo "Usg: mspps_orbit_1b.bash  NSS.AMAX.M2.D09263.S1208.E1348.B1515657.SV"
  exit 1
else
  file=$1
fi


################################################################
# MSPPS Root Directory
################################################################
MSPPS_ROOT=/home/pub/wchen/mspps_operational


################################################################
# External Data Path of 1B and GFS
################################################################
MSPPS_1B=/net/orbit227l/home/pub/external_data/1b/backup
GFS_HIST=/net/orbit095l/disk2/pub/mspps/avn/backup_temp


################################################################
# Directories derived from MSPPS_ROOT
################################################################
MSPPS_BIN=${MSPPS_ROOT}/bin
MSPPS_SCRIPTS=${MSPPS_ROOT}/scripts
GFS_DIR=${MSPPS_ROOT}/data/GFS/current


################################################################
# Some functions and LIBS
################################################################
cd ${MSPPS_SCRIPTS}
. ${MSPPS_SCRIPTS}/mspps_functions.bash


SZIP_LIB=`grep SZIP_LIB ../include/Makefile_include | cut -f2 -d=`
FORTRAN_LIB=`grep 'FORTRAN_LIB=' ../include/Makefile_include | cut -f2 -d=`
export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}:${SZIP_LIB}:${FORTRAN_LIB}



################################################################
# Get file information
################################################################
satId=`echo $file | cut -f3 -d .`
orbitInfo=`echo $file | cut -f3-8 -d .`
orbitInfo2=`echo $file | cut -f3-7 -d .`

yyjjj=`echo $file | cut -f4 -d . | cut -c2-6`
Y=`echo $yyjjj | cut -c1`

if [[ $Y -eq 0 ]] ; then
  yyyyjjj=20${yyjjj}
else
  yyyyjjj=19${yyjjj}
fi

yyyy=`echo ${yyyyjjj} | cut -c1-4`
jjj=`echo ${yyyyjjj} | cut -c5-7`
yyyymmdd=`yyyyjjj2yyyymmdd ${yyyy} ${jjj}`


file_amsua=NSS.AMAX.${orbitInfo}
file_mhs=NSS.MHSX.${orbitInfo}


################################################################
# Check AMSUA/MHS both available
################################################################

if [[ ! -e ${MSPPS_1B}/${file_amsua} ]] ; then
  echo "${file_amsua} not exist"
  exit 1
fi

if [[ ! -e ${MSPPS_1B}/${file_mhs} ]] ; then
  echo "${file_mhs} not exist"
  exit 1
fi

satName=""
satSuffix=""
if   [[ ${satId} == "M2" ]] ; then
  satName="Metop"
  satSuffix="m2"
elif [[ ${satId} == "NN" ]] ; then 
  satName="NOAA-N"
  satSuffix="nn"
elif [[ ${satId} == "NP" ]] ; then 
  satName="NOAA-P"
  satSuffix="np"
else
  echo "Wrong satellite. Only N18(NN)/N19(NP)/Metop(M2) use 1B."
  exit 1
fi

################################################################
# Directories and executables derived from satellite
################################################################

MSPPS_IN=${MSPPS_ROOT}/data/${satName}/input
MSPPS_OUT=${MSPPS_ROOT}/data/${satName}/output
MSPPS_MISC=${MSPPS_ROOT}/data/${satName}/misc
MSPPS_TMP=${MSPPS_ROOT}/data/${satName}/tmp
MSPPS_LOG=${MSPPS_ROOT}/logs/${satName}

exe_amsua=${MSPPS_BIN}/runamsua_${satSuffix}
exe_mhs_pre=${MSPPS_BIN}/runmhs_pre_${satSuffix}
exe_mhs_fnl=${MSPPS_BIN}/runmhs_fnl_${satSuffix}


################################################################
# Check GFS data availability; if not, then copy from corresponding
# history directory. You may need to defined your own get_avn
# function to reflect your own system.
################################################################
num=`ls ${GFS_DIR} | wc -l | awk '{print $1}'`
if [[ $num -lt 40 ]] ; then 
  
  echo "Copy GFS data to ${GFS_DIR}"

  # STAR version is get_avn
  get_avn  ${GFS_HIST}  ${GFS_DIR}  ${yyyyjjj}

  # OSDPD version is get_avn2
  #get_avn2  ${GFS_HIST}  ${GFS_DIR}  ${yyyymmdd}

fi




################################################################
# Clean input directory first and then copy new input 1b file
################################################################
find ${MSPPS_ROOT}/data/${satName} -name "*${orbitInfo2}*" -exec rm -f {} \;


cp ${MSPPS_1B}/${file_amsua} ${MSPPS_IN} 
cp ${MSPPS_1B}/${file_mhs}   ${MSPPS_IN}


list_amsua=${MSPPS_MISC}/amsuafile
list_mhs=${MSPPS_MISC}/mhsfile

echo ${file_amsua} > ${list_amsua}
echo ${file_mhs}   > ${list_mhs}


################################################################
# To test whether little or big endian of machine to decide 
# if endian swap needed.
# 1 - little endian machine(linux) need endian swapping
# 0 - big endian machine(IBM AIX)  no need endian swapping
################################################################

declare -i machine_endian
machine_endian=`get_machine_endian`

if [[ ${machine_endian} -eq 1 ]] ; then
  
  echo "This is a little endian machine and need endian swapping of 1b files..."
  cd ${MSPPS_SCRIPTS}
  ${MSPPS_SCRIPTS}/swap_endian_1b.pl -s Structure_AMSUA_1b -f ${list_amsua} -d ${MSPPS_IN}
  ${MSPPS_SCRIPTS}/swap_endian_1b.pl -s Structure_MHS_1b   -f ${list_mhs}   -d ${MSPPS_IN}

fi



################################################################
# Start processing AMSUA
################################################################

ls -1 ${MSPPS_IN}/${file_amsua} > ${list_amsua}
ls -1 ${MSPPS_IN}/${file_mhs}   > ${list_mhs}

${exe_amsua} ${satName} > ${MSPPS_LOG}/log.amsua.${orbitInfo} 2>&1


################################################################
# Start processing MHS pre
# MHS pre is located in data/${satName}/tmp/
################################################################

${exe_mhs_pre} ${satName} > ${MSPPS_LOG}/log.mhs.pre.${orbitInfo} 2>&1


################################################################
# Start processing MHS fnl
################################################################
swath_list_amsua=${MSPPS_MISC}/swath_A.list
swath_list_mhs=${MSPPS_MISC}/swath_M.list

echo "NPR.AAOP.${orbitInfo2}.NS" > $swath_list_amsua
echo "NPR.MHOP.${orbitInfo2}.NS" > $swath_list_mhs

${exe_mhs_fnl} ${satName} > ${MSPPS_LOG}/log.mhs.fnl.${orbitInfo} 2>&1


################################################################
# Move fnl MHS to output/swath directory
################################################################
mv -f ${MSPPS_TMP}/NPR.MHOP.${orbitInfo2}.NS  ${MSPPS_OUT}/swath/


################################################################
# Clean intermediate files
################################################################
if [[ ${machine_endian} -eq 1 ]] ; then
  rm -f ${MSPPS_IN}/${file_amsua}
  rm -f ${MSPPS_IN}/${file_mhs}
fi

