#!/bin/sh
set -x

# IBM: xlf_r; Gaea: ftn; Zeus: ifort
export FCMP=${1:-ftn}

if [ $FCMP = xlf_r ] ; then
 export FFLAGS="-qfree=f90"
 export LIBS=
else
 export FFLAGS="-convert big_endian -g -traceback -FR"
 export LIBS=
fi

make -f Makefile
make -f Makefile_vsdb
