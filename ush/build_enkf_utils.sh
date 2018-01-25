#!/bin/sh

set -ex

cd ..
pwd=$(pwd)

target=${1:-cray}
dir_root=${2:-$pwd}
clean=${3:-"YES"}

if [ $target = wcoss ]; then
    . /usrx/local/Modules/3.2.10/init/sh
    conf_target=nco
elif [ $target = cray -o $target = wcoss_c ]; then
    . $MODULESHOME/init/sh
    conf_target=nco
elif [ $target = theia ]; then
    . /apps/lmod/lmod/init/sh
    conf_target=theia
else
    echo "unknown target = $target"
    exit 9
fi

dir_modules=$dir_root/modulefiles
if [ ! -d $dir_modules ]; then
    echo "modulefiles does not exist in $dir_modules"
    exit 10
fi
[ -d $dir_root/exec ] || mkdir -p $dir_root/exec

module purge
if [ $target = wcoss -o $target = cray ]; then
    module load $dir_modules/modulefile.gdas_enkf.$target
else
    source $dir_modules/modulefile.gdas_enkf.$target
fi
module list

dlist="adderrspec_nmcmeth_spec.fd getsfcensmeanp.fd getsigensstatp.fd getnstensmeanp.fd getsfcnstensupdp.fd getsigensmeanp_smooth_ncep.fd recentersigp.fd calc_increment_ens.fd gribmean.fd"

for dir in $dlist; do

    cd $dir_root/util/EnKF/gfs/src/$dir
    ./configure clean
    ./configure $conf_target
    make -f Makefile clean
    make -f Makefile
    cp -p *.x $dir_root/exec
    if [ $clean = YES ]; then
        rm -f $dir_root/exec/log*.x
        make -f Makefile clean
        ./configure clean
    fi

done

exit 0
