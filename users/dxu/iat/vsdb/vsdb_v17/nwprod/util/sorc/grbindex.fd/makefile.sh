#!/bin/sh
set -x
export LIBDIR=../../../lib
export W3LIB=w3lib-2.0_4
         
#IBM: xlf_r; Zeus: ifort; Gaea: ftn
export FCMP=${1:-ftn}

if [ $FCMP = xlf_r ]; then
 export LIBDIR=/nwprod/lib
 export W3LIB=w3_4          
fi

make -f Makefile
