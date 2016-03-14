#! /bin/bash

#------------------------------------------------------------------
#  makeall
#
#  This scripts makes all of the executables in the nwprod, 
#  data_extract and image_gen subdirectories and places the 
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
#  source RadMon_config
#------------------------------
. ${top_level}/parm/RadMon_config
. ${top_level}/parm/radmon.ver


if [[ ${machine} = "theia" || ${machine} = "wcoss" || ${machine} = "cray" ]]; then
   echo Building executables on ${machine}
   echo

   #------------------------------------------------------------------
   #  make data extract executables
   #------------------------------------------------------------------

   module use -a ${HOMEradmon}/modulefiles/${machine}
   module load RadMonBuild

   executables="angle bcoef bcor time"
   echo "Making executables in nwprod/radmon_shared.v${radmon_shared_ver}/sorc:"
   for var in ${executables}; do
      if [[ $var = "angle" ]]; then
         cd ${top_level}/nwprod/radmon_shared.v${radmon_shared_ver}/sorc/verf_radang.fd
      else
         cd ${top_level}/nwprod/radmon_shared.v${radmon_shared_ver}/sorc/verf_rad${var}.fd
      fi

      make ${mode}
      if [[ $mode = "" ]]; then 
         make install
      fi

      echo

   done

   cd ${top_level}/data_extract/sorc/make_base.fd
   make ${mode}  
   if [[ $mode = "" ]]; then 
      make install
   fi

   cd ${top_level}/data_extract/sorc/validate_time.fd
   make ${mode}  
   if [[ $mode = "" ]]; then 
      make install
   fi


   #------------------------------------------------------------------
   #  make image generation executables
   #------------------------------------------------------------------
   executables="horiz summary time bcoef angle"

   cd ${top_level}/image_gen/src
   echo "Making image_gen/src:"
   for var in ${executables}; do

      make -f makefile.${var} ${mode}
      if [[ $mode = "" ]]; then 
         make -f makefile.${var} install
      fi
      echo
   done

   module unload RadMonBuild
   set +x

else
   echo ${machine} is not supported 
fi

exit

