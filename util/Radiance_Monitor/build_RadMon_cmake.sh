#! /bin/bash

#------------------------------------------------------------------
#  build_RadMon_cmake.sh
#
#  This script builds all of the executables in the 
#  nwprod/radmon_shared/exec, data_extract/exec, and image_gen/exec
#  subdirectories.  
#
#  The operational RadMon executables (in nwprod/radmon_shared/exec) 
#  may also be built as part of the whole GSI package.  To do this 
#  ensure BUILD_UTIL=ON when running cmake or use the 
#  ProdGSI/ush/build_all_cmake.sh script.
#------------------------------------------------------------------
set -ax

mode=${1:-}
top_level=${PWD}
echo "top_level = ${top_level}"

if [[ -d /dcom && -d /hwrf ]] ; then
    . /usrx/local/Modules/3.2.10/init/sh
    target=wcoss
    . $MODULESHOME/init/sh
elif [[ -d /cm ]] ; then
    . $MODULESHOME/init/sh
    target=wcoss_c
elif [[ -d /ioddev_dell ]]; then
    . $MODULESHOME/init/sh
    target=wcoss_d
elif [[ -d /scratch1 ]] ; then
    . /apps/lmod/lmod/init/sh
    target=hera
else
    echo "unknown target = $target"
    exit 9
fi

GSI_Pkg=${top_level}/../..
echo "GSI_Pkg = ${GSI_Pkg}"

echo "target = $target"

dir_modules=${GSI_Pkg}/modulefiles
if [ ! -d $dir_modules ]; then
    echo "modulefiles does not exist in $dir_modules"
    exit 10
fi


#------------------------------
#  source RadMon_config
#------------------------------
. ${top_level}/parm/RadMon_config


#---------------------------------------------------           
#  Verify this is a supported machine
#---------------------------------------------------           

if [[ ${target} = "hera"     || ${target} = "wcoss" \
   || ${target} = "wcoss_c"  || ${target} = "wcoss_d" ]]; then
   echo Building nwprod executables on ${target}
   echo


   #-------------------------------------
   #  load modules 
   #-------------------------------------
   if [ $target = wcoss_d ]; then
      module purge
      module use -a $dir_modules
      module load modulefile.ProdGSI.$target
   elif [ $target = wcoss -o $target = gaea ]; then
      module purge
      module load $dir_modules/modulefile.ProdGSI.$target
   elif [ $target = hera -o $target = cheyenne ]; then
      module purge
      source $dir_modules/modulefile.ProdGSI.$target
   elif [ $target = wcoss_c ]; then
      module purge
      module load $dir_modules/modulefile.ProdGSI.$target
   fi


   #-------------------------------------
   #  use cmake to build the executables
   #-------------------------------------
   if [[ -d ./build ]]; then
      rm -rf ./build
   fi 
   mkdir build
   cd ./build
  
   cmake ..
   make -j8

   cd bin

   #-------------------------------------------------------
   #  move the executables to the correct exec directories
   #-------------------------------------------------------
   file_list1="radmon_angle.x radmon_bcoef.x radmon_bcor.x radmon_time.x"
   for file in $file_list1; do
      cp $file $HOMEradmon/exec/.
   done

   file_list_de="radmon_mk_base.x radmon_validate_tm.x"
   for file in $file_list_de; do
      cp $file $DE_EXEC/.
   done

   file_list_ig="radmon_ig_angle.x radmon_ig_bcoef.x radmon_ig_horiz.x radmon_ig_summary.x radmon_ig_time.x"
   for file in $file_list_ig; do
      cp $file $IG_EXEC/.
   done

else
   echo ${machine} is not supported 
fi


set +x

exit
