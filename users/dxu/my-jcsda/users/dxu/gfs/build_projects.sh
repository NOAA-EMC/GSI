#!/bin/ksh
#---------------------------------------------------------------
# Builds GFS components found under the /projects directory
#  - compiles all, unless arguments are provided on command line
#  - components: enkf, gsi, obsproc, post, verif
#---------------------------------------------------------------
set -x -e
pwd=`pwd`
path=$pwd/projects

mac=`hostname |cut -c1`
if [ $mac = g -o $mac = t ] ; then
  machine=wcoss
elif [ $mac = z -o $mac = f -o $mac = r ] ; then
  machine=zeus
fi

compile=$1
compile=${compile:-"enkf gsi obsproc post storm_relocation verif"}

echo "Compiling the following components: $compile"

for comp in $compile
do
 case $comp in

  enkf)
    # Compile global_enkf
    cd $path/enkf/src
    ./configure $machine
    make -f Makefile
    # Compile adderrspec_nmcmeth_spec.x
    cd $path/enkf/util/src/adderrspec_nmcmeth_spec.fd
    ./configure $machine
    make -f Makefile
    # Compile getsfcensmean.x
    cd $path/enkf/util/src/getsfcensmeanp.fd
    ./configure $machine
    make -f Makefile
    # Compile getsigensmean_smooth.x
    cd $path/enkf/util/src/getsigensmeanp_smooth_ncep.fd
    ./configure $machine
    make -f Makefile
    # Compile recentersigp.x
    cd $path/enkf/util/src/recentersigp.fd
    ./configure $machine
    make -f Makefile
  ;;

  gsi)
    # Compile global_gsi
    cd $path/gsi/src
    ./configure $machine
    make -f Makefile
  ;;

  obsproc)
    echo "Compiling obsproc code"
    # Compile global_postevents
    cd $path/obsproc_prep_post/sorc
    sh build.sh
    # Compile prepbufr codes
    cd $path/obsproc_prep/sorc
    sh build.sh
  ;;

  post)
    cd $path/post/sorc/ncep_post.fd
    make -f makefile_$machine
  ;;

  storm_relocation)
    cd $path/storm_relocation/sorc
    sh build.sh
    sh install.sh
  ;;    

  verif)
    echo "Verif compilation not yet supported. Please see build.sh within tag to build verif."
#   machine=`echo $machine | tr 'a-z' 'A-Z'`
#   cd $path/verif
#   sh build.sh $machine
  ;;

 esac
done # comp
