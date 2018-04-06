#!/bin/sh

set -ex

cd ..
pwd=$(pwd)

dir_root=${1:-$pwd}

if [[ -d /dcom && -d /hwrf ]] ; then
    . /usrx/local/Modules/3.2.10/init/sh
    target=wcoss
elif [[ -d /gpfs/hps && -e /etc/SuSE-release ]] ; then
    . $MODULESHOME/init/sh
    target=cray
elif [[ -d /scratch3 ]] ; then
    . /apps/lmod/lmod/init/sh
    target=theia
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
