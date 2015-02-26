#!/bin/bash
#====================================================
# Purpose:
#    Used by IAT GUI to run HIT
# Author: Deyong Xu / RTi@JCSDA
# History:
#    2/3/2015, D. Xu / RTi@JCSDA , initial code.
#
#====================================================

source hit_gui.config

year_reg_info=${ENV_REGION}${ENV_YEAR}_${ENV_REGION}
trackFile=track${year_reg_info}_template.sh

# Remove old log file
if [ -e out.log ]
then
    rm -rf out.log
fi

if [ -e out2.log ]
then
    rm -rf out.log
fi

# Run HIT package

# Remove output location where to extrac figures to generate PAR
if [ -e ${ENV_SCRDIR}/hit_data_loc ]
then
   rm -rf ${ENV_SCRDIR}/hit_data_loc
fi

if [ ${ENV_MEAN} = "yes" ]
then 
  ./track_mean_template.sh  > out2.log 2>>out2.log
fi 


./${trackFile}  > out.log 2>>out.log
