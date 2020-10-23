#!/bin/bash
# load_modules_gsi.sh
#
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
elif [[ -d /carddata ]] ; then
    . /opt/apps/lmod/3.1.9/init/sh
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
else
    echo "unknown target = $target"
    exit 9
fi

dir_modules=$GSIDIR/modulefiles
if [ ! -d $dir_modules ]; then
    echo "modulefiles does not exist in $dir_modules"
    exit 10
fi

if [ $target = wcoss_d ]; then
    module purge
    module use -a $dir_modules
    module load modulefile.ProdGSI.$target
elif [ $target = wcoss -o $target = gaea ]; then
    module purge
    module load $dir_modules/modulefile.ProdGSI.$target
elif [ $target = hera -o $target = cheyenne -o $target = orion ]; then
    module purge
    source $dir_modules/modulefile.ProdGSI.$target
elif [ $target = wcoss_c ]; then
    module purge
    module load $dir_modules/modulefile.ProdGSI.$target
elif [ $target = discover ]; then
    module load $dir_modules/modulefile.ProdGSI.$target
else
    module purge
    source $dir_modules/modulefile.ProdGSI.$target
fi
