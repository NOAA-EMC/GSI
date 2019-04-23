#! /bin/bash

#------------------------------------------------------------------
#  cmake_build.sh
#
#  This script builds all of the executables in the 
#  nwprod/radmon_shared/exec, data_extract/exec, and image_gen/exec
#  subdirectories.  Additionally, the NetCDF library is built, since
#  the executables in nwprod/radmon_shared/exec now require it to
#  read NetCDF formatted radstat files.
#
#  The RadMon executables may also be built as part of the whole GSI
#  package.  To do this ensure BUILD_UTIL=ON when running cmake.
#------------------------------------------------------------------
set -ax

mode=${1:-}
top_level=${PWD}
echo "top_level = ${top_level}"

GSI_Pkg=${top_level}/../..
echo "GSI_Pkg = ${GSI_Pkg}"

machine=`./get_hostname.pl`
echo "machine = $machine"


#------------------------------
#  source RadMon_config
#------------------------------
. ${top_level}/parm/RadMon_config
. ${top_level}/parm/radmon.ver


#---------------------------------------------------           
#  Verify this is a supported machine
#---------------------------------------------------           

if [[ ${machine} = "theia" || ${machine} = "wcoss" \
   || ${machine} = "cray"  || ${machine} = "wcoss_d" ]]; then
   echo Building nwprod executables on ${machine}
   echo


   #-------------------------------------
   #  load modules as needed
   #-------------------------------------
   mod_list=`module list`

   if [[ ${machine} = "wcoss_d" ]]; then
      echo "load modules on wcoss_d"

      if grep -q "EnvVars" <<< "$mod_list" ; then
         echo "module EnvVars is loaded";
      else
    	 echo "loading EnvVars";
         module load EnvVars/1.0.2
      fi

      if grep -q "cmake" <<< "$mod_list" ; then
         echo "module cmake is loaded";
      else
    	 echo "loading cmake";
         module load cmake/3.10.0
      fi
      
      if grep -q "ips" <<< "$mod_list" ; then
         echo "module ips is loaded";
      else
    	 echo "loading ips";
         module load ips/18.0.1.163
      fi

      if grep -q "NetCDF" <<< "$mod_list" ; then
         echo "module NetCDF is loaded";
      else
    	 echo "loading NetCDF";
         module load NetCDF/4.5.0
      fi
 
   elif [[ $machine = "wcoss" ]]; then
      echo "load modules on wcoss"

      if grep -q "HDF5" <<< "$mod_list" ; then
         echo "module HDF5 is loaded"
      else
    	 echo "loading HDF5/1.8.9/serial"
         module load HDF5/1.8.9/serial
      fi

      if grep -q "NetCDF" <<< "$mod_list" ; then
         echo "module NetCDF is loaded"
      else
    	 echo "loading NetCDF/4.2/serial"
         module load NetCDF/4.2/serial
      fi

      if grep -q "mpiserial" <<< "$mod_list" ; then
         echo "module mpiserial is loaded"
      else
    	 echo "loading mpiserial/1.0.0"
         module load mpiserial/1.0.0
      fi

      if grep -q "ics" <<< "$mod_list" ; then
         echo "module ics is loaded"
      else
    	 echo "loading ics/17.0.3"
         module load ics/17.0.3
      fi

      if grep -q "ibmpe" <<< "$mod_list" ; then
         echo "module ibmpe is loaded"
      else
    	 echo "loading ibmpe/1.3.0.12"
         module load ibmpe/1.3.0.12
      fi

   elif [[ $machine = "cray" ]]; then
      echo "load modules on cray"
      module use -a /opt/cray/craype/default/modulefiles
      module use -a /opt/cray/ari/modulefiles
      module use -a /gpfs/hps/nco/ops/nwprod/modulefiles
 
      if grep -q "PrgEnv-intel" <<< "$mod_list"; then
         echo "module PrgEnv-intel is loaded";
      else 
    	 echo "loading PrgEnv-intel";
         module load PrgEnv-intel
      fi

      if grep -q "HDF5-serial-intel-haswell" <<< "$mod_list"; then
         echo "module HDF5-serial-intel-haswell is loaded";
      else 
    	 echo "loading HDF5-serial-intel-haswell/1.8.9";
         module load HDF5-serial-intel-haswell/1.8.9
      fi

      if grep -q "NetCDF-intel-haswell" <<< "$mod_list"; then
         echo "module NetCDF-intel-haswell is loaded";
      else 
    	 echo "loading NetCDF-intel-haswell/4.2";
         module load NetCDF-intel-haswell/4.2
      fi

      if grep -q "cray-netcdf" <<< "$mod_list"; then
         echo "module cray-netcdf is loaded";
      else 
    	 echo "loading cray-netcdf/4.3.2";
         module load cray-netcdf/4.3.2
      fi

      if grep -q "cmake" <<< "$mod_list" ; then
         echo "module cmake is loaded";
      else
    	 echo "loading cmake";
         module load cmake
      fi

   elif [[ $machine = "theia" ]]; then
      echo "load modules on theia"

      if grep -q "cmake" <<< "$mod_list" ; then
         echo "module cmake is loaded";
      else
    	 echo "loading cmake";
         module load cmake
      fi

      if grep -q "intel" <<< "$mod_list" ; then
         echo "module intel is loaded"
      else
    	 echo "loading intel/14.0.2"
         module load intel/14.0.2
      fi

      if grep -q "impi" <<< "$mod_list" ; then
         echo "module impi is loaded"
      else
    	 echo "loading impi/5.1.2.150"
         module load impi/5.1.2.150
      fi

      if grep -q "netcdf" <<< "$mod_list" ; then
         echo "module netcdf is loaded"
      else
    	 echo "loading netcdf/4.3.0"
         module load netcdf/4.3.0
      fi

      if grep -q "hdf5" <<< "$mod_list" ; then
         echo "module hdf5 is loaded"
      else
    	 echo "loading hdf5/1.8.14"
         module load udunits/1.8.14
      fi

      if grep -q "cdo" <<< "$mod_list" ; then
         echo "module cdo is loaded"
      else
    	 echo "loading cdo/1.6.7"
         module load cdo/1.6.7
      fi

      if grep -q "contrib" <<< "$mod_list" ; then
         echo "module contrib is loaded"
      else
    	 echo "loading contrib"
         module load contrib
      fi

   fi

   #-------------------------------------
   #  use cmake to build the executables
   #-------------------------------------
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

   file_list_de="rad_make_base.x rad_validate_time.x"
   for file in $file_list_de; do
      cp $file $DE_EXEC/.
   done

   file_list_ig="rad_ig_angle.x rad_ig_bcoef.x rad_ig_horiz.x rad_ig_summary.x rad_ig_time.x"
   for file in $file_list_ig; do
      cp $file $IG_EXEC/.
   done

else
   echo ${machine} is not supported 
fi


set +x

exit
