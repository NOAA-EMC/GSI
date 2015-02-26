#!/bin/bash
# Krishna Kumar NOAA/NESDIS/JCSDA 10-17-2013.
# Modified this script for WCOSS system
# Modified this script for S4 system
# 7/1/2014, D. Xu / RTi@JCSDA, facilitated script to work on badger so : 
#   - Isolate all changes needed into a configuration file for better organization.
#   - Pass new ENV var to sub-script so wind.setup works for both wind and z.  
#   - Remove sleep-statement, which could potentially run into conflict.
#   - Reorgzanize code structure.
#                           
set -xa
#=======================================
# 1. Run config file to set up all 
#    the configuration parameters
#=======================================
source gribExtreme_template.config 

#================================== 
# 2. Check cycle time to make sure 
#    it's in right format.
#================================== 
len=${#cycleTime}
if [ ${len} -ne 10 ]
then
  echo "Cycle date is incorrect, it should be in YYYYMMDDHH format."
  echo "For example, 2013071906  "; exit
fi 

#=============================
# 3. Create working_dir
#=============================
# Create working_dir if not exist, otherwise, do nothing.
ls -d ${working_dir}
returnCode=$?   
if [ ${returnCode} -ne 0 ]
then
  mkdir -p ${working_dir}
fi

# Save output location where to extrac figures to generate PAR
echo  ${working_dir}  > ${ENV_GRIBEXTR_DIR}/scripts/ge_data_loc

# Go to working directory 
cd ${working_dir}

#=====================================================
# 4. ENV var used in GRIB Extrm f90 code.
#======================================================
GFS_GES_F90_ENV=${gfs_ges_input_file}
CRNT_ANL_F90_ENV=${crnt_anl_input_file}
REF_ANL_F90_ENV=${ref_anl_input_file}

# Files needed for GRIB Extrm f90 code.
gradsout_file=gradsout.${cycleTime}
gemout_file=gemout.${cycleTime}
log_file=log.${cycleTime}
err_file=err.${cycleTime}

#===============================================
# 5. Run f90 code to generate GRIB Extreme file
#===============================================
script_dir=${gribExtr_dir}/scripts
exec_dir=${gribExtr_dir}/exec
exec_file=gribExtreme.x
# Copy input grib files into working directory 
cp ${gfs_ges_input_dir}/${gfs_ges_input_file}   ${working_dir}/.
cp ${crnt_anl_input_dir}/${crnt_anl_input_file} ${working_dir}/.
cp ${ref_anl_input_dir}/${ref_anl_input_file}   ${working_dir}/.

# Link two empty files to fort.55 and fort.66, to which f90 code writes data later.
ln -sf  "${working_dir}/${gemout_file}"      "fort.55"
ln -sf  "${working_dir}/${gradsout_file}"    "fort.60"
${exec_dir}/${exec_file} > ${working_dir}/${log_file} 2> ${working_dir}/${err_file}

#===============================================
# 6. Call all the plotting Grads script
#===============================================
# Read in the file (fort.60) and process each line in the file.
# 
while read one_line
do
   fieldname=` echo ${one_line} | cut -d" " -f1` # fieldname 
   orderNo=` echo ${one_line} | cut -d" " -f2`   # order number
   level=` echo ${one_line} | cut -d" " -f3`     # pres level
   lat=` echo ${one_line} | cut -d" " -f4`       # lat
   lon=` echo ${one_line} | cut -d" " -f5`       # lon

   # Set up command paramemters
   command_param=" $cycleTime $level b 06 grdprs $lat $lon $orderNo " 

   # Create filename for output png files created in wind.setup/temp.setup scripts.
   aString=_${cycleTime}_${lat}_${lon}.png
   
   if [ ${fieldname} == 'Z' ]; then
      # vars needed in sub-script
      png_file_1=z${level}_${title1}${aString}
      png_file_2=z${level}_${title2}${aString}
      png_file_3=z${level}_${title3}${aString}
      png_file_4=z${level}_diff_${title3}_MINUS_${title2}${aString}
      png_file_5=z${level}_diff_${title2}_MINUS_${title1}${aString}
      uniq_ID=z${level}_${cycleTime}_${lat}_${lon}
      # Generate grads png file
      $script_dir/wind.setup ${command_param}
   fi

   if [ $fieldname == 'W' ]; then
      # vars needed in sub-script
      png_file_1=wind${level}_${title1}${aString}
      png_file_2=wind${level}_${title2}${aString}
      png_file_3=wind${level}_${title3}${aString}
      png_file_4=wind${level}_diff_${title3}_MINUS_${title2}${aString}
      png_file_5=wind${level}_diff_${title2}_MINUS_${title1}${aString}
      uniq_ID=wind${level}_${cycleTime}_${lat}_${lon}
      # Generate grads png file
      $script_dir/wind.setup ${command_param}
   fi

   if [ $fieldname == 'T' ]; then
      # vars needed in sub-script
      png_file_1=t${level}_${title1}${aString}
      png_file_2=t${level}_${title2}${aString}
      png_file_3=t${level}_${title3}${aString}
      png_file_4=t${level}_diff_${title3}_MINUS_${title2}${aString}
      png_file_5=t${level}_diff_${title2}_MINUS_${title1}${aString}
      uniq_ID=t${level}_${cycleTime}_${lat}_${lon}
      # Generate grads png file
      $script_dir/temp.setup ${command_param}
   fi
done < $working_dir/${gradsout_file}

exit
