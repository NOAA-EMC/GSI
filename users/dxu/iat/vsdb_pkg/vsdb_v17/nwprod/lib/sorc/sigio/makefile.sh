#!/bin/sh
set -x
export LIBDIR=../..
export INCMOD=$LIBDIR/incmod/sigio_4
#export FCMP95=xlf95_r
export FCMP=${1:-ftn}

if [ $FCMP = xlf_r ] ; then
 export FFLAGS="-qnosave -O3 -q free=f90 -qsmp=noauto:omp -I$INCMOD"
 export FFLAGB="-qnosave -O3 -q fixed"    
 export ARFLAGS="-ruv -X64"
else
 export FFLAGS="-O2 -xHOST -convert big_endian -traceback -g -FR -I$INCMOD"
 export FFLAGB="-O2 -xHOST -convert big_endian -traceback -g"
 export ARFLAGS="-ruv"
 export AR=ar
fi
 mkdir -p $INCMOD
 make -f makefile_4

