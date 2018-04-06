#!/bin/sh

set -ex

cd ..
pwd=$(pwd)

target=$1
dir_root=${2:-$pwd}

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

rm -rf $dir_root/build
mkdir -p $dir_root/build
cd $dir_root/build

module purge
if [ $target = wcoss -o $target = cray ]; then
    module load $dir_modules/modulefile.ProdGSI.$target
else
    source $dir_modules/modulefile.ProdGSI.$target
fi
module list

cmake -DBUILD_UTIL=ON -DCMAKE_BUILD_TYPE=PRODUCTION ..

make -j 8

exit
