#!/bin/sh
set -x

# IBM: xlf_r; Gaea: ftn; Zeus: ifort
export FCMP=${1:-ftn}

export LIBDIR=../../nwprod/lib
export W3LIB=w3lib-2.0_4

if [ $FCMP = xlf_r ] ; then
 export LIBDIR=/nwprod/lib
 export W3LIB=w3_4
 export FFLAGS="-qsmp=noauto -l essl"
else
 export FFLAGS="-O2 -convert big_endian -g -traceback"
fi

make -f Makefile
make -f Makefile_b
