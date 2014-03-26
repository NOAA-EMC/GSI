#!/bin/bash
#---------------------------------------------------------------------------------
# Name: makePath.sh
#
# Description:
#   To point to idl code of local MIRS.
#
# Author: Deyong Xu (RTI) @ JCSDA,
#         Deyong.Xu@noaa.gov
# Version: Mar 26, 2014, DXu, Initial coding
#
#---------------------------------------------------------------------------------

mirsIDL_Dir=${1}"/src/lib_idl"

files="stats_sub.pro utilities.pro meteorFcts_sub.pro io_scene.pro io_coloc.pro io_measur.pro io_covBkg.pro io_dropsondes.pro io_misc.pro io_monitor.pro io_regressAlgors.pro io_dep.pro io_Mapping.pro misc.pro algors.pro Export_IMG.pro " 

grep -ve "\.pro" paths_idl.pro  > tmpFile

for file in $files
do 
   echo "@${mirsIDL_Dir}/${file} " >> tmpFile
done

mv tmpFile  paths_idl.pro

