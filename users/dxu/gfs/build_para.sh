#!/bin/ksh
#-------------------------------------------------------
# Builds GFS codes found under the /para directory
#  - compiles codes under /para/sorc and para/util/sorc  
#-------------------------------------------------------

set -x -e
pwd=`pwd`

mac=`hostname |cut -c1`
if [ $mac = g -o $mac = t ] ; then
  machine=wcoss
elif [ $mac = z -o $mac = f -o $mac = r ] ; then
  machine=zeus
fi

# Compile codes under /para/sorc
compile1='global_fcst global_chgres global_cycle gfs_bufr gfs_flux'

for comp in $compile1
do
  echo "Compiling ${comp}"
  cd $pwd/para/sorc/${comp}.fd
  sh makefile.sh
done

# Compile codes under /para/util/sorc
#compile2='anomgb copygb2 copygb gettrk grb2index grbindex grib2grib ndate nhour reduced_gaussian_grid tocsbufr upaprep'
compile2='gettrk tocsbufr'

for comp in $compile2
do
  echo "Compiling ${comp}"
  cd $pwd/para/util/sorc/${comp}.fd
  sh makefile.sh
done

