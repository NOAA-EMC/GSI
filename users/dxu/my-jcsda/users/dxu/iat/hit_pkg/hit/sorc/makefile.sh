#!/bin/sh
set -x

# IBM: xlf_r; Gaea: ftn; Zeus and WCOSS: ifort
export FCMP=${1:-ifort}

if [ $FCMP = xlf_r ]; then
 export FFLAGS=" -O2 "
else
 export FFLAGS="-O2 -convert big_endian -g -traceback "
fi

make -f Makefile_wrtdat
make -f Makefile
