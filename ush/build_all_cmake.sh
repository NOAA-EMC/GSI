#!/bin/sh

set -ex

cd ..
pwd=$(pwd)

dir_root=${1:-$pwd}

if [[ -d /dcom && -d /hwrf ]] ; then
    . /usrx/local/Modules/3.2.10/init/sh
    target=wcoss
    . $MODULESHOME/init/sh
elif [[ -d /cm ]] ; then
    . $MODULESHOME/init/sh
    conf_target=nco
    target=cray
elif [[ -d /ioddev_dell ]]; then
    . $MODULESHOME/init/sh
    conf_target=nco
    target=wcoss_d
elif [[ -d /scratch3 ]] ; then
    . /apps/lmod/lmod/init/sh
    target=theia
elif [[ -d /jetmon ]] ; then
    . $MODULESHOME/init/sh
    target=jet
elif [[ -d /sw/gaea ]] ; then
    . /opt/cray/pe/modules/3.2.10.5/init/sh
    target=gaea
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

rm -rf $dir_root/build
mkdir -p $dir_root/build
cd $dir_root/build

if [ $target = wcoss -o $target = cray -o $target = gaea ]; then
    module purge
    module load $dir_modules/modulefile.ProdGSI.$target
elif [ $target = theia ]; then
    module purge
    source $dir_modules/modulefile.ProdGSI.$target
else 
    module purge
    source $dir_modules/modulefile.ProdGSI.$target
fi

cmake -DBUILD_UTIL=ON -DCMAKE_BUILD_TYPE=PRODUCTION -DBUILD_CORELIBS=OFF ..

make -j 8

exit
