#! /bin/bash

#------------------------------------------------------------------
#  makeall
#
#  This scripts makes all of the executables in the 
#  nwprod/oznmon_shared/sorc subdirectories and places the 
#  executables into the proper exec directories.
#
#  An optional arguments to this script include "clean", "debug",
#  and "check_prereqs".  Do not use "install" as an optional 
#  argument -- this script will do that automatically.  
#  If no argument is included "all" and "install" are assumed.
#------------------------------------------------------------------
set -ax

mode=${1:-}
top_level=${PWD}
echo "top_level = ${top_level}"

machine=`./get_hostname.pl`
echo "machine = $machine"

#------------------------------
#  source OznMon_config
#------------------------------
. ${top_level}/parm/OznMon_config
. ${top_level}/parm/OznMon.ver

HOMEoznmon=${MY_OZNMON}/nwprod/oznmon_shared.${shared_oznmon_ver}
echo HOMEoznmon = $HOMEoznmon

if [[ ${machine} = "theia" || ${machine} = "wcoss" || ${machine} = "cray" || ${machine} = "dell" ]]; then
   echo Building executables on ${machine}
   echo

   #------------------------------------------------------------------
   #  make data extract executables in ${HOMEoznmon}/sorc directories
   #------------------------------------------------------------------

   module use -a ${HOMEoznmon}/modulefiles/${machine}
   module load OznMonBuild

   executables="oznmon_horiz oznmon_time"
   echo "Making executables in nwprod/oznmon_shared.${shared_oznmon_ver}/sorc:"
   export dir_root=${HOMEoznmon}
   for var in ${executables}; do
      cd ${HOMEoznmon}/sorc/${var}.fd

      make ${mode}
      if [[ $mode = "" ]]; then 
         make install
      fi

      echo

   done

   executables="make_base"
   echo "Making executables in data_xtrc/sorc:"
   export dir_root=${MY_OZNMON}/data_xtrct/
   for var in ${executables}; do
      cd ${OZN_DE_SORC}/${var}.fd

      make ${mode}
      if [[ $mode = "" ]]; then
         make install
      fi

      echo

   done

#   module unload OznMonBuild
   set +x

else
   echo ${machine} is not supported 
fi

exit

