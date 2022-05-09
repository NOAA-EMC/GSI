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
MY_RADMON=${2:-}

top_level=${PWD}
echo "top_level = ${top_level}"

export MY_RADMON=${MY_RADMON:-$top_level}
echo "MY_RADMON = ${MY_RADMON}"

target=`./get_machine.sh`
echo "target = $target"

if [[ $target = "wcoss_c" || $target = "wcoss_d" ||
      $target = "orion"   || $target = "wcoss2"  || 
      $target = "s4" ]] ; then
    . $MODULESHOME/init/sh
elif [[ $target = "hera" ]] ; then
    . /apps/lmod/lmod/init/sh
elif [[ $target = "jet" ]] ; then
    . /apps/lmod/lmod/init/sh
elif [[ -d /lfs && -d /dfs ]]; then
    . $MODULESHOME/init/bash
    target=wcoss2
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
   || ${target} = "wcoss_c"  || ${target} = "wcoss_d" \
   || ${target} = "orion"    || ${target} = "jet" \
   || ${target} = "s4"       || ${target} = "wcoss2" ]]; then
   echo Building nwprod executables on ${target}
   echo


   #-------------------------------------
   #  load modules 
   #-------------------------------------
   if [ $target = wcoss_d -o $target = "wcoss2" ]; then
      module purge
      module use -a $dir_modules
      module load modulefile.ProdGSI.$target
   elif [ $target = wcoss -o $target = gaea ]; then
      module purge
      module load $dir_modules/modulefile.ProdGSI.$target
   elif [ $target = hera -o $target = orion -o $target = jet -o $target = s4 ]; then
    module purge
    module use $dir_modules
    module load modulefile.ProdGSI.$target
   elif [ $target = cheyenne ]; then
      module purge
      source $dir_modules/modulefile.ProdGSI.$target
   elif [ $target = wcoss_c ]; then
      module purge
      module load $dir_modules/modulefile.ProdGSI.$target
   elif [ $target = wcoss2 ]; then
      module purge
      module use -a $dir_modules
      module load modulefile.ProdGSI.$target.lua 
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
