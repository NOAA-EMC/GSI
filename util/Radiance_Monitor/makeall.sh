#! /bin/ksh

#------------------------------------------------------------------
#  makeall
#
#  This scripts makes all of the executables in the nwprod, 
#  data_extract and image_gen subdirectories and places the 
#  executables into the proper exec directories.
#
#  An optional argument to this script is "clean".  Use this if 
#  you wish to remove *.o, *.mod, and *.x files in the various src 
#  directories.  If "clean" is not used, "all" is assumed.
#------------------------------------------------------------------

mode=${1:-all}

top_level=`pwd`

machine=`./get_hostname.pl`
echo "machine = $machine"

#------------------------------
#  set correct version numbers
#------------------------------
. ${top_level}/parm/radmon.ver


if [[ ${machine} = "zeus" || ${machine} = "theia" || ${machine} = "wcoss" ]]; then
   echo Building executables on ${machine}
   echo

   #------------------------------------------------------------------
   #  make data extract executables
   #------------------------------------------------------------------

   if [[ ${machine} = "wcoss" ]]; then
      echo loading module command for wcoss
      . /usrx/local/Modules/default/init/ksh
      module load ics
   fi

   executables="angle bcoef bcor time"
   echo "Making executables in nwprod/radmon_shared.v${radmon_shared_ver}/sorc:"
   for var in ${executables}; do
      if [[ $var = "angle" ]]; then
         cd ${top_level}/nwprod/radmon_shared.v${radmon_shared_ver}/sorc/verf_radang.fd
      else
         cd ${top_level}/nwprod/radmon_shared.v${radmon_shared_ver}/sorc/verf_rad${var}.fd
      fi

      if [[ ${machine} != "wcoss" ]]; then
         rm -f Makefile.conf
         cp -f ${top_level}/parm/Makefile.conf.${machine} Makefile.conf
      fi

      echo make ${var} ${mode}
      make ${mode}
      echo

      if [[ $mode = all ]]; then
         cp -f radmon_${var} ${top_level}/nwprod/radmon_shared.v${radmon_shared_ver}/exec/.
      fi
   done

   cd ${top_level}/data_extract/sorc/make_base.fd
   rm -f Makefile.conf
   cp -f ${top_level}/parm/Makefile.conf.${machine} Makefile.conf
   make ${mode}  

   cd ${top_level}/data_extract/sorc/validate_time.fd
   rm -f Makefile.conf
   cp -f ${top_level}/parm/Makefile.conf.${machine} Makefile.conf
   make ${mode}  


   #------------------------------------------------------------------
   #  make image generation executables
   #------------------------------------------------------------------
   executables="horiz summary time bcoef angle"

   cd ${top_level}/image_gen/src
   echo "Making image_gen/src:"
   for var in ${executables}; do
      rm -f Makefile.conf
      cp -f ${top_level}/parm/Makefile.conf.${machine} Makefile.conf

      echo make ${var} ${mode}
      make -f makefile.${var} ${mode}
      echo
   done

else
   echo ${machine} is not supported 
fi

exit

