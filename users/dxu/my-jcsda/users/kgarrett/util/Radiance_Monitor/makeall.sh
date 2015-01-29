#! /bin/ksh

#------------------------------------------------------------------
#  makeall
#
#  This scripts makes each of the executables in the data_extract
#  and image_gen subdirectories for both rgn (regional) and glb
#  (global) sources.  The executables will be put into the correct
#  exec directories. 
#
#  An optional argument to this script is "clean".  Use this if 
#  you wish to remove *.o, *.mod, and *.x files in the various src 
#  directories.  If "clean" is not used, "all" is assumed.
#------------------------------------------------------------------

mode=${1:-all}

top_level=`pwd`

machine=`./get_hostname.pl`
echo "machine = $machine"

if [[ ${machine} = "ccs" || ${machine} = "zeus" || ${machine} = "wcoss" ]]; then
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
   echo "Making data extraction executables in data_extract/nwprod/sorc:"
   for var in ${executables}; do
      if [[ $var = "angle" ]]; then
         cd ${top_level}/nwprod/sorc/verf_radang.fd
      else
         cd ${top_level}/nwprod/sorc/verf_rad${var}.fd
      fi

      rm -f Makefile.conf
      ln -s ${top_level}/parm/Makefile.conf.${machine} Makefile.conf

      echo make ${var} ${mode}
      make ${mode}
      echo

      if [[ $mode = all ]]; then
         cp -f radmon_${var} ${top_level}/nwprod/exec/.
      fi
   done

   cd ${top_level}/nwprod/sorc/make_base.fd
   rm -f Makefile.conf
   ln -s ${top_level}/parm/Makefile.conf.${machine} Makefile.conf
   make ${mode}  
   if [[ $mode = all ]]; then
      cp -f make_base ${top_level}/nwprod/exec/.
   fi


   #------------------------------------------------------------------
   #  make image generation executables
   #------------------------------------------------------------------
   executables="horiz"

   cd ${top_level}/image_gen/src
   echo "Making image_gen/src:"
   for var in ${executables}; do
      rm -f Makefile.conf
      ln -s ${top_level}/parm/Makefile.conf.${machine} Makefile.conf

      echo make ${var} ${mode}
      make -f makefile.${var} ${mode}
      echo
   done

else
   echo ${machine} is not supported 
fi

exit

