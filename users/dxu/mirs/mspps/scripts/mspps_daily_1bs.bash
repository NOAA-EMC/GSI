#!/bin/bash

################################################################
#							       #
# Usg: 	mspps_daily_1bs.bash  SATID yyyy-mm-dd                 #
#                                                              #
# Description:						       #
#	It calles orbit mode to process files one by one.      #
#	Only for N15/N16/N17.				       #
#							       #
# Author: Wanchun Chen		09/16/2009		       #
#							       #
################################################################

if [[ $# -ne 2 ]] ; then
  echo "Error: Incorrect number of arguments"
  echo "mspps_daily_1bs.bash SATID(NK/NL/NM) yyyy-mm-dd"
  exit 1
fi

MSPPS_ROOT=/home/pub/wchen/mspps_operational

SATID=$1

if [[ $SATID != "NK" && $SATID != "NL" && $SATID != "NM" ]] ; then
  echo "Error: Incorrect SATID"
  echo "Correct ones: NK,NL,NM"
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


list_amsua=(`ls -1 ${DIR_1B}/N1S.AMAX.${SATID}.D${yyjjj}*`)
for file_a in ${list_amsua[*]} ; do
  
  file_b=${file_a/AMAX/AMBX}
  
  if [[ -e ${file_b} ]] ; then
    echo $file_a
    echo $file_b
    file=`basename ${file_b}`
    #echo $file
    ${MSPPS_ROOT}/scripts/mspps_orbit_1bs.bash  $file
    wait
  fi
  
  echo
  
done



##################################################
# Plot images section
##################################################

if [[ $SATID == "NK" ]] ; then
  satid="n15"
  satDir="NOAA-K"
elif [[ $SATID == "NL" ]] ; then
  satid="n16"
  satDir="NOAA-L"
elif [[ $SATID == "NM" ]] ; then
  satid="n17"
  satDir="NOAA-M"
fi

figsDir=${MSPPS_ROOT}/data/img/
fileList=${MSPPS_ROOT}/logs/${satDir}/filelist_${satid}_${ymd}

ls -1 ${MSPPS_ROOT}/data/${satDir}/output/swath/NPR.ABOP.${SATID}.D${yyjjj}*.NS > ${fileList}
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

