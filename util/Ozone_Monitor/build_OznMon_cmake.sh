#! /bin/bash

#------------------------------------------------------------------
#  build_OznMon_cmake.sh
#
#  This script builds all of the executables in the 
#  nwprod/oznmon_shared/exec and data_xtrct/exec subdirectories.
#
#  The operational OznMon executables (in nwprod/oznmon_shared/exec) 
#  may also be built as part of the whole GSI package.  To do this 
#  ensure BUILD_UTIL=ON when running cmake or use the 
#  ProdGSI/ush/build_all_cmake.sh script.
#------------------------------------------------------------------
set -ax

mode=${1:-}
MY_OZNMON=${2:-}

top_level=${PWD}
echo "top_level = ${top_level}"

export MY_OZNMON=${MY_OZNMON:-$top_level}
echo "MY_OZNMON = ${MY_OZNMON}"

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
else
   echo "unknown target = $target"
   exit 9
fi


GSI_Pkg=${top_level}/../..
echo "GSI_Pkg = ${GSI_Pkg}"

#machine=`./get_hostname.pl`
echo "target = $target"

dir_modules=${GSI_Pkg}/modulefiles
if [ ! -d $dir_modules ]; then
    echo "modulefiles does not exist in $dir_modules"
    exit 10
fi


#---------------------------------------------------           
#  Verify this is a supported machine
#---------------------------------------------------           

if [[ ${target} = "hera"    || ${target} = "wcoss_c"  \
   || ${target} = "wcoss_d" || ${target} = "orion" \
   || ${target} = "jet"     || ${target} = "s4" \
   || ${target} = "wcoss2" ]]; then
   echo Building nwprod executables on ${target}
   echo


   #-------------------------------------
   #  load modules 
   #-------------------------------------
   if [ $target = wcoss_d ]; then
      module purge
      module use -a $dir_modules
      module load modulefile.ProdGSI.$target
   elif [ $target = gaea ]; then
      module purge
      module load $dir_modules/modulefile.ProdGSI.$target
   elif [ $target = hera -o $target = orion -o $target = s4 -o $target = jet ]; then
      module purge
      module use $dir_modules
      module load modulefile.ProdGSI.$target
   elif [ $target = cheyenne ]; then
      module purge
      source $dir_modules/modulefile.ProdGSI.$target
   elif [ $target = wcoss_c ]; then
      module purge
      module use -a $dir_modules
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

   #------------------------------
   #  source OznMon_config
   #------------------------------
   . ${top_level}/parm/OznMon_config

   #-------------------------------------------------------
   #  move the executables to the correct exec directories
   #-------------------------------------------------------

   file_list1="oznmon_horiz.x oznmon_time.x"
   for file in $file_list1; do
      cp $file $HOMEoznmon/exec/.
   done

   file_list_de="oznmon_make_base.x"
   for file in $file_list_de; do
      cp $file $OZN_DE_EXEC/.
   done

else
   echo ${machine} is not supported 
fi


set +x

exit
