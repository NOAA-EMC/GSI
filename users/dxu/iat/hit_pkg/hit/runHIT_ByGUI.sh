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

if [ ${ENV_MEAN} = "yes" ]
then 
  echo track_mean_template.sh
else 
  echo not run  track_mean_template.sh
fi 


echo ${trackFile}
