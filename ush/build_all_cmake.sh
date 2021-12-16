#!/bin/sh

set -ex

cd ..
pwd=$(pwd)

build_type=${1:-'PRODUCTION'}
dir_root=${2:-$pwd}
mode=${3:-'EMC'}


# If NCO build, prune directories and files before build
if [ $mode = NCO ]; then
    cd $dir_root/ush
    $dir_root/ush/prune_4nco_global.sh prune
fi


# Initialize and load modules
if [[ -d /dcom && -d /hwrf ]] ; then
    . /usrx/local/Modules/3.2.10/init/sh
    target=wcoss
    . $MODULESHOME/init/sh
elif [[ -d /cm ]] ; then
    . $MODULESHOME/init/sh
    target=wcoss_c
elif [[ -d /ioddev_dell ]]; then
    . $MODULESHOME/init/sh
    target=wcoss_d
elif [[ -d /scratch1 ]] ; then
    . /apps/lmod/lmod/init/sh
    target=hera
elif [[ -d /data/prod ]] ; then
    . /usr/share/lmod/lmod/init/sh
    target=s4
elif [[ -d /jetmon ]] ; then
    . $MODULESHOME/init/sh
    target=jet
elif [[ -d /glade ]] ; then
    . $MODULESHOME/init/sh
    target=cheyenne
elif [[ -d /sw/gaea ]] ; then
    . /opt/cray/pe/modules/3.2.10.5/init/sh
    target=gaea
elif [[ -d /discover ]] ; then
#   . /opt/cray/pe/modules/3.2.10.5/init/sh
    target=discover
    build_type=0
    export SPACK_ROOT=/discover/nobackup/mapotts1/spack
    export PATH=$PATH:$SPACK_ROOT/bin
    . $SPACK_ROOT/share/spack/setup-env.sh    
elif [[ -d /work ]]; then
    . $MODULESHOME/init/sh
    target=orion
elif [[ -d /lfs/h1 ]] ; then
    target=acorn
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

if [ $target = wcoss_d ]; then
    module purge
    module use -a $dir_modules
    module load modulefile.ProdGSI.$target
elif [ $target = wcoss -o $target = gaea ]; then
    module purge
    module load $dir_modules/modulefile.ProdGSI.$target
elif [ $target = hera -o $target = orion -o $target = s4 ]; then
    module purge
    module use $dir_modules
    module load modulefile.ProdGSI.$target
elif [ $target = jet ]; then
    module purge
    module use $dir_modules
    module load modulefile.ProdGSI.$target
elif [ $target = cheyenne ]; then
    module purge
    source $dir_modules/modulefile.ProdGSI.$target
elif [ $target = wcoss_c ]; then
    module purge
    module load $dir_modules/modulefile.ProdGSI.$target
elif [ $target = discover ]; then
    module load $dir_modules/modulefile.ProdGSI.$target
elif [ $target = acorn ]; then
    source /apps/prod/lmodules/startLmod
    module use $dir_modules
    module load modulefile.ProdGSI.$target
else 
    module purge
    source $dir_modules/modulefile.ProdGSI.$target
fi

if [ $build_type = PRODUCTION -o $build_type = DEBUG ] ; then
  cmake -DBUILD_UTIL=ON -DBUILD_NCDIAG_SERIAL=ON -DCMAKE_BUILD_TYPE=$build_type -DBUILD_CORELIBS=OFF ..
else 
  cmake ..
fi

# Build apps.  Echo extra printout for NCO build
if [ $mode = NCO ]; then
    make VERBOSE=1 -j 8
else
    make -j 8
fi
rc=$?

# If NCO build is successful, remove build directory
if [ $mode = NCO -a $rc -eq 0 ]; then
    rm -rf $dir_root/build
fi

exit
