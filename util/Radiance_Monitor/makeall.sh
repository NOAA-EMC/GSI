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

executables="angle bcoef bcor time"
echo "Making global executables in data_extract/nwprod/sorc:"
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

echo "Making regional executables in data_extract/nwprod/sorc:"
for var in ${executables}; do
   if [[ $var = "angle" ]]; then
      cd ${top_level}/nwprod/sorc/verf_radang_rgn.fd
   else
      cd ${top_level}/nwprod/sorc/verf_rad${var}_rgn.fd
   fi

   echo make ${var} ${mode}
   make ${mode}
   echo

   if [[ $mode = all ]]; then
      cp -f radmon_${var}.rgn ${top_level}/nwprod/exec/.
   fi
done

cd ${top_level}/nwprod/sorc/make_base.fd
make ${mode}  
if [[ $mode = all ]]; then
   cp -f make_base ${top_level}/nwprod/exec/.
fi


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

