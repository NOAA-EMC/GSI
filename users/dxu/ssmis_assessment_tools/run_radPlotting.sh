#!/bin/bash
#---------------------------------------------------------------------------------
# Name: run_radPlotting.sh
#
# Description:
#   To run IDL code to generate radiance plots that contain both observed radiances 
#   and simulated radiances. 
#
# Author: Deyong Xu (RTI) @ JCSDA,
#         Deyong.Xu@noaa.gov
# Version: Mar 10, 2014, DXu, Initial coding
#
#---------------------------------------------------------------------------------

# Define working directory
wkDir="/data/home001/dxu/ssmis_assessment_tools"
archiveDir=${wkDir}/archiveData
mainCode=main_AT.pro

cd ${wkDir}

# 1. Save old .ps files 
if [ -d ${archiveDir} ]
then
   mv *.ps ${archiveDir}
   mv *.jpg ${archiveDir}
else 
   mkdir ${archiveDir}
   mv *.ps ${archiveDir}
   mv *.jpg ${archiveDir}
fi

# 2. Run IDL code to generate radiance plots
idl <<EOF 
   .run  ${mainCode}
   7 
   1
EOF

# 3. Convert .ps files into .jpg files 
psFiles=`ls *.ps `
for file in ${psFiles}
do 
   # get filename without path and suffix
   fn=${file%.*}
   convert ${file}  ${fn}.jpg 
done


