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

if [ $target = wcoss -o $target = cray ]; then
    module purge
    module load $dir_modules/modulefile.ProdGSI.$target
elif [ $target = theia ]; then
    module purge
    source $dir_modules/modulefile.ProdGSI.$target
elif [ $target = wcoss_d ]; then
    module purge
    source $dir_modules/modulefile.ProdGSI.$target
    export NETCDF_INCLUDE=-I/usrx/local/prod/packages/ips/18.0.1/netcdf/4.5.0/include
    export NETCDF_CFLAGS=-I/usrx/local/prod/packages/ips/18.0.1/netcdf/4.5.0/include
    export NETCDF_LDFLAGS_CXX="-L/usrx/local/prod/packages/ips/18.0.1/netcdf/4.5.0/lib -lnetcdf -lnetcdf_c++"
    export NETCDF_LDFLAGS_CXX4="-L/usrx/local/prod/packages/ips/18.0.1/netcdf/4.5.0/lib -lnetcdf -lnetcdf_c++4"
    export NETCDF_CXXFLAGS=-I/usrx/local/prod/packages/ips/18.0.1/netcdf/4.5.0/include
    export NETCDF_FFLAGS=-I/usrx/local/prod/packages/ips/18.0.1/netcdf/4.5.0/include
    export NETCDF_ROOT=/usrx/local/prod/packages/ips/18.0.1/netcdf/4.5.0
    export NETCDF_LIB=/usrx/local/prod/packages/ips/18.0.1/netcdf/4.5.0/lib
    export NETCDF_LDFLAGS_F="-L/usrx/local/prod/packages/ips/18.0.1/netcdf/4.5.0/lib -lnetcdff"
    export NETCDF_LDFLAGS_C="-L/usrx/local/prod/packages/ips/18.0.1/netcdf/4.5.0/lib -lnetcdf"
    export NETCDF_LDFLAGS="-L/usrx/local/prod/packages/ips/18.0.1/netcdf/4.5.0/lib -lnetcdff"
    export NETCDF=/usrx/local/prod/packages/ips/18.0.1/netcdf/4.5.0
    export NETCDF_INC=/usrx/local/prod/packages/ips/18.0.1/netcdf/4.5.0/include
    export NETCDF_CXX4FLAGS=-I/usrx/local/prod/packages/ips/18.0.1/netcdf/4.5.0/include
fi

cmake -DBUILD_UTIL=ON -DCMAKE_BUILD_TYPE=PRODUCTION ..

make -j 8

exit
