#! /bin/ksh

#------------------------------------------------------------------
#  makeall.sh
#
#  This scripts makes each of the ConvMon executables in the 
#  ./nwprod/cmon_shared.v1.0.0/sorc and ./image_gen/sorc
#  directories.
#
#  An optional argument to this script is "clean".  Use this if 
#  you wish to remove *.o, *.mod, and *.x files in the sorc
#  directory.  If "clean" is not used, "all" is assumed.
#------------------------------------------------------------------

mode=${1:-all}

top_level=`pwd`
cmon_version_file=${cmon_version_file:-${top_level}/parm/CMon.ver}
if [[ -s ${cmon_version_file} ]]; then
   . ${cmon_version_file}
   echo "able to source ${cmon_version_file}"
else
   echo "Unable to source ${cmon_version_file} file"
   exit 1
fi

machine=`/usr/bin/perl ./get_hostname.pl`
echo machine = $machine

sorc_path="${top_level}/nwprod/cmon_shared.v${cmon_shared_ver}/sorc"
echo "sorc_path = $sorc_path"

ig_sorc_path="${top_level}/image_gen/sorc"

exec_path="${top_level}/nwprod/cmon_shared.v${cmon_shared_ver}/exec"
mod_path="${top_level}/nwprod/cmon_shared.v${cmon_shared_ver}/modulefiles/${machine}"
echo "mod_path = $mod_path"

ig_exec_path="${top_level}/image_gen/exec"

mod_path="${top_level}/nwprod/cmon_shared.v${cmon_shared_ver}/modulefiles/${machine}"
echo "mod_path = ${mod_path}"

executables="conv_time grads_sfc grads_lev grads_sfctime grads_mandlev grads_sig"

ig_executables="read_ps read_pw read_q read_t read_uv"


if [[ ${machine} = "wcoss" ]]; then
   . /usrx/local/Modules/3.2.10/init/ksh
   module unload ics		# ics12.1 is still automatically loaded at login, and this
				# conflicts with ics14.x which is the default ics version.
elif [[ ${machine} = "theia" ]]; then
   echo "loading lmod"
   . /apps/lmod/6.0.1/init/ksh
elif [[ ${machine} = "cray" ]]; then
   . /opt/modules/3.2.6.7/init/ksh
fi


if [[ ${machine} = "wcoss" || ${machine} = "theia" || ${machine} = "cray" ]]; then
#   echo "machine, mod_path = $machine, $mod_path"
   module use -a ${mod_path}
   module load CMonBuild

 
   for var in ${executables}; do
      cd ${sorc_path}/${var}

      echo make ${var} ${mode}
      echo ""
      echo ""

      make -f makefile.${var} ${mode}
      echo ""
      echo ""

      if [[ ${mode} = "all" ]]; then
         make -f makefile.${var} install
         echo ""
         echo ""
      fi

   done

   for var in ${ig_executables}; do
      cd ${ig_sorc_path}/${var}

      echo make ${var} ${mode}
      echo ""
      echo ""

      make -f makefile.${var} ${mode}
      echo ""
      echo ""

      if [[ ${mode} = "all" ]]; then
         make -f makefile.${var} install
         echo ""
         echo ""
      fi

   done


   module unload CMonBuild 

else
   echo ${machine} is not supported 
fi


exit

