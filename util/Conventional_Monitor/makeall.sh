#! /bin/ksh

#------------------------------------------------------------------
#  makeall.sh
#
#  This scripts makes each of the ConvMon executables in the 
#  ./nwprod/cmon_shared.v1.0.0/sorc directory.
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
exec_path="${top_level}/nwprod/cmon_shared.v${cmon_shared_ver}/exec"

executables="conv_time grads_sfc read_t read_q grads_lev grads_sfctime read_ps read_uv grads_mandlev grads_sig read_pw"

if [[ ${machine} = "wcoss" || ${machine} = "theia" ]]; then

   for var in ${executables}; do
      cd ${sorc_path}/${var}
      rm -f Makefile.conf
      cp -f ${top_level}/parm/Makefile.conf.${machine} Makefile.conf

      echo make ${var} ${mode}
      echo ""
      echo ""

      make -f makefile.${var} ${mode}
      echo ""
      echo ""

      if [[ ${mode} = "all" ]]; then
         cp -f ${var}.x ${exec_path}/.
         ln -s ${exec_path}/${var}.x ${top_level}/image_gen/exec/.
      fi
   done

else
   echo ${machine} is not supported 
fi


exit

