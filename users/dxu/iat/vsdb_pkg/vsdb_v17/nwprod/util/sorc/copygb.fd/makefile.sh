#!/bin/sh
set -x
export LIBDIR=../../../lib
export W3LIB=w3lib-2.0_d

# IBM: xlf_r; Gaea: ftn; Zeus: ifort
export FCMP=${1:-ftn}

if [ $FCMP = xlf_r ] ; then
 export LIBDIR=/nwprod/lib
 export W3LIB=w3_d
 export FFLAGS="-O3 -qrealsize=8 -qnosave"
 export LFLAGS="-qsmp=noauto  -lessl"
else
#export FFLAGS="-O0 -r8 -g -check all -traceback"
 export FFLAGS="-O0 -r8 -g -traceback"
#export FFLAGS="-O2 -r8 -g -traceback"
 export LFLAGS=
fi
make -f Makefile
