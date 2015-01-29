#!/bin/sh
set -x
export LIBDIR=../../../lib
#IBM: xlf_r; Zeus: ifort; Gaea: ftn
export FCMP=${1:-ftn}

if [ $FCMP = xlf_r ]; then
 export LIBDIR=/nwprod/lib
fi

make -f Makefile
