#!/bin/bash

################################################################
#							       #
# Usg: 	mspps_daily_1b.bash  SATID yyyy-mm-dd                  #
#                                                              #
# Description:						       #
#	It calles orbit mode to process files one by one.      #
#	Only for N18/N19/Metop. 			       #
#							       #
# Author: Wanchun Chen		09/16/2009		       #
#							       #
################################################################

if [[ $# -ne 2 ]] ; then
  echo "Error: Incorrect number of arguments"
  echo "mspps_daily_1b.bash SATID(M2/NN/NP) yyyy-mm-dd"
  exit 1
fi

MSPPS_ROOT=/home/pub/wchen/mspps_operational

SATID=$1

if [[ $SATID != "M2" && $SATID != "NN" && $SATID != "NP" ]] ; then
  echo "Error: Incorrect SATID"
  echo "Correct ones: NN, NP, M2"
  exit 1
fi


yyyymmdd=$2

ymd=`echo $yyyymmdd | cut -c1-4,6-7,9-10`

# check non-digit in ymd 
if [[ $ymd =~ [^0-9] ]] ; then
  echo "Non-digit found: $BASH_REMATCH"
  exit 1
fi


yyyyjjj=`date -d "$yyyymmdd" "+%Y%j"`
yyjjj=`date -d "$yyyymmdd" "+%y%j"`


DIR_1B=/net/orbit227l/home/pub/external_data/1b/backup


list_amsua=(`ls -1 ${DIR_1B}/NSS.AMAX.${SATID}.D${yyjjj}*`)
for file_a in ${list_amsua[*]} ; do
  
  file_m=${file_a/AMAX/MHSX}
  
  if [[ -e ${file_m} ]] ; then
    echo $file_a
    echo $file_m
    file=`basename ${file_m}`
    #echo $file
    ${MSPPS_ROOT}/scripts/mspps_orbit_1b.bash $file
    wait
  fi
  
  echo 
  
done




##################################################
# Plot images section
##################################################

if [[ ${SATID} == "M2" ]] ; then
  satid="m2"
  satDir="Metop"
elif [[ ${SATID} == "NN" ]] ; then
  satid="n18"
  satDir="NOAA-N"
elif [[ ${SATID} == "NP" ]] ; then
  satid="n19"
  satDir="NOAA-P"
fi

figsDir=${MSPPS_ROOT}/data/img
fileList=${MSPPS_ROOT}/logs/${satDir}/filelist_${satid}_${ymd}
ls -1 ${MSPPS_ROOT}/data/${satDir}/output/swath/NPR.MHOP.${SATID}.D${yyjjj}*.NS > ${fileList}

lines=`wc -l ${fileList} | awk '{print $1}'`
if [[ $lines -eq 0 ]] ; then
  echo "NO Swath files under ${MSPPS_ROOT}/data/${satDir}/output/swath/"
  exit 1
fi

idlExe=/usr/local/bin/idl
idlSrc=${MSPPS_ROOT}/src/IDL
namelist=${MSPPS_ROOT}/logs/${satDir}/namelist_$satid_$ymd

. ${MSPPS_ROOT}/scripts/mspps_functions.bash
figsGen ${satid} ${ymd} ${fileList} ${figsDir} ${idlExe} ${idlSrc} ${namelist}


