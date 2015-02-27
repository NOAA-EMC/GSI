#!/bin/bash 
#===============================================================
# Purpose:
#    Used by GUI to generate Performance Assessment Report (PAR)
# Author: Deyong Xu / RTi@JCSDA
# History:
#    2/26/2015, D. Xu / RTi@JCSDA , initial code.
#
#===============================================================
# Functions defined to get output for IAT packages
function get_fd_data
{
   local file_list=""
   locs=$@
   for loc in ${locs}
   do
       cd ${loc}
       files=`find . -type f -name "*500*.gif"  `
       for file in $files
       do 
          file_list="${file_list} ${loc}/${file}"
       done
   done
   
   # Return file list 
   echo $file_list
}

function get_ge_data
{
   local file_list=""
   locs=$@
   for loc in ${locs}
   do
       cd ${loc}
       files=`find . -type f -name "*diff*png"  `
       for file in $files
       do 
          file_list="${file_list} ${loc}/${file}"
       done
   done
   
   # Return file list 
   echo $file_list
}

function get_hit_data
{
   local file_list=""
   locs=$@
   for loc in ${locs}
   do
       cd ${loc}
       files=`find . -type f -name "*.gif"  `
       for file in $files
       do 
          file_list="${file_list} ${loc}/${file}"
       done
   done
   
   # Return file list 
   echo $file_list
}

function get_radmon_data
{
   local file_list=""
   locs=$@
   for loc in ${locs}
   do
       cd ${loc}
       files=`find . -type f -name "*.png"  `
       for file in $files
       do 
          file_list="${file_list} ${loc}/${file}"
       done
   done
   
   # Return file list 
   echo $file_list
}

function get_vsdb_data
{
   local file_list=""
   locs=$@
   for loc in ${locs}
   do
       cd ${loc}/G2/anom/PMSL
       files=`ls cor*day*G2.png  cordieoff_*PMSL*G2.png  cordiff_*PMSL*G2.png |sort `
       for file in $files
       do 
          file_list="${file_list} ${loc}/G2/anom/PMSL/${file}"
       done

       cd ${loc}/G2/anom/T
       files=`ls cor*day*500*G2.png  cordieoff_*T*500*G2.png  cordiff_*T*500*G2.png |sort `
       for file in $files
       do 
          file_list="${file_list} ${loc}/G2/anom/T/${file}"
       done

       cd ${loc}/G2/anom/HGT
       files=`ls cor*day*500*G2.png  cordieoff_*HGT*500*G2.png  cordiff_*HGT*500*G2.png |sort `
       for file in $files
       do 
          file_list="${file_list} ${loc}/G2/anom/HGT/${file}"
       done

       cd ${loc}/G2/pres/T
       files=`ls bias*day**500*G2.png   biasdieoff_T_*500*G2.png |sort `
       for file in $files
       do
          file_list="${file_list} ${loc}/G2/pres/T/${file}"
       done

       cd ${loc}/G2/pres/HGT
       files=`ls bias*day**500*G2.png   biasdieoff_HGT_*500*G2.png |sort `
       for file in $files
       do
          file_list="${file_list} ${loc}/G2/pres/HGT/${file}"
       done


   done
   
   # Return file list 
   echo $file_list
}

function get_score_data
{
   local file_list=""
   locs=$@
   for loc in ${locs}
   do

       cd ${loc}
       files=`ls *html *.css `
       for file in $files
       do
          file_list="${file_list} ${loc}/${file}"
       done

   done
   
   # Return file list 
   echo $file_list
}


# Get GUI src directory
gui_dir=`pwd`
# IAt root directory
iat_dir=${gui_dir}/../../
# IAT package directories
fd_dir=${iat_dir}/fcstDiff_pkg/fcstDiff
ge_dir=${iat_dir}/ge_pkg/ge/scripts
hit_dir=${iat_dir}/hit_pkg/hit
radmon_dir=${iat_dir}/radmon_pkg/radmon/parm
vsdb_dir=${iat_dir}/vsdb_pkg/vsdb_v17

# Remove old data location file
loc_file=${gui_dir}/all_data_loc
if [ -e ${loc_file} ]
then
    rm -rf ${loc_file}
fi


# Pakcage names expected are: fd, ge, hit, radmon and vsdb
pkgs=$@

all_file_list="${gui_dir}/../data/iat_title.png "
score_file_list="${gui_dir}/../data/vsdb_title.png "

for pkg in $pkgs 
do
   # collect data 
   case ${pkg} in 
   "fd") 
       echo "fd"
       if [ -e ${fd_dir}/fcstDiff_data_loc ]
       then
	  cat ${fd_dir}/fcstDiff_data_loc 
	  locs=`cat ${fd_dir}/fcstDiff_data_loc  `
	  all_file_list="${all_file_list} ${gui_dir}/../data/fd_title.png $(get_fd_data ${locs}) "
       fi
       ;;
   "ge") 
       echo "ge"
       if [ -e ${ge_dir}/ge_data_loc ]
       then
          cat ${ge_dir}/ge_data_loc 
          locs=`cat ${ge_dir}/ge_data_loc   `
          all_file_list="${all_file_list} ${gui_dir}/../data/ge_title.png $(get_ge_data ${locs}) "
       fi
       ;;
   "hit") 
       echo "hit"
       if [ -e ${hit_dir}/hit_data_loc ]
       then
	  cat ${hit_dir}/hit_data_loc 
	  locs=`cat ${hit_dir}/hit_data_loc  `
	  all_file_list="${all_file_list} ${gui_dir}/../data/hit_title.png $(get_hit_data ${locs}) "
       fi
       ;;
   "radmon") 
       echo "radmon"
       if [ -e ${radmon_dir}/radmon_data_loc ]
       then
	  cat ${radmon_dir}/radmon_data_loc 
	  locs=`cat ${radmon_dir}/radmon_data_loc  `
	  all_file_list="${all_file_list} ${gui_dir}/../data/radmon_title.png $(get_radmon_data ${locs}) "
       fi
       ;;
   "vsdb") 
       echo "vsdb"
       if [ -e ${vsdb_dir}/vsdb_data_loc ]
       then
	  cat ${vsdb_dir}/vsdb_data_loc  
	  locs=`cat ${vsdb_dir}/vsdb_data_loc   `
	  all_file_list="${all_file_list} ${gui_dir}/../data/vsdb_title.png $(get_vsdb_data ${locs}) "
       fi
       if [ -e ${vsdb_dir}/vsdb_score_loc ]
       then
	  cat ${vsdb_dir}/vsdb_score_loc
	  locs=`cat ${vsdb_dir}/vsdb_score_loc `
	  score_file_list="$(get_score_data ${locs}) "
       fi
       ;;
   esac
done

echo "------------------------------"
echo "${all_file_list} "
echo "------------------------------"

convert ${all_file_list}  ${gui_dir}/par.pdf
cp ${score_file_list} ${gui_dir}/score
exit


