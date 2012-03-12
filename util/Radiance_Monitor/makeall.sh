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

#------------------------------------------------------------------
#  make data extract executables
#------------------------------------------------------------------
executables="angle_bias bcoef bcor time make_base"

cd ${top_level}/data_extract/src/glb
echo "Making data_extract/src/glb:"
for var in ${executables}; do
   echo make ${var} ${mode}
   make -f makefile.${var} ${mode}
   echo
done

executables="angle_bias bcoef bcor time"
cd ${top_level}/data_extract/src/rgn
echo "Making data_extract/src/rgn:"
for var in ${executables}; do
   echo make ${var} ${mode}
   make -f makefile.${var} ${mode}
   echo
done

executables="angle bcoef bcor time"
echo "Making data_extract/nwprod/sorc:"
for var in ${executables}; do
   if [[ $var = "angle" ]]; then
      cd ${top_level}/nwprod/sorc/verf_radang.fd
   else
      cd ${top_level}/nwprod/sorc/verf_rad${var}.fd
   fi

   echo make ${var} ${mode}
   make ${mode}
   echo

   if [[ $mode = all ]]; then
      cp -f radmon_${var}.glb ${top_level}/nwprod/exec/.
   fi
done

#------------------------------------------------------------------
#  make image generation executables
#------------------------------------------------------------------
executables="horiz"

cd ${top_level}/image_gen/src/glb
echo "Making image_gen/src/glb:"
for var in ${executables}; do
   echo make ${var} ${mode}
   make -f makefile.${var} ${mode}
   echo
done

cd ${top_level}/image_gen/src/rgn
echo "Making image_gen/src/rgn:"
for var in ${executables}; do
   echo make ${var} ${mode}
   make -f makefile.${var} ${mode}
   echo
done


exit

