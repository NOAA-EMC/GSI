#!/bin/sh
set -x

# IBM: xlf_r; Gaea: ftn; Zeus: ifort
export FCMP=${1:-ifort}

export LIBDIR="../../lib"
export W3LIB="w3lib-2.0_4"

if [ $FCMP = xlf_r ] ; then
 export LIBDIR="/nwprod/lib"
 export W3LIB="w3_4"
 export LIBS="-L$LIBDIR -lbufr_s_64 -lbacio_4 -l$W3LIB "
 export FFLAGS="-qcheck -qextchk -qfixed -bnoquiet "
else
 export LIBS="-L$LIBDIR -lbufr_4_64 -l$W3LIB "
 #export LIBS="-L$LIBDIR -lbufr_v10.2.3_4_64 -l$W3LIB "
 export FFLAGS=" -convert big_endian -heap-arrays -i4"
#export FFLAG="-O0 -g -traceback -check all -fpe0 ftrapuv -convet big_endian"
fi

make -f Makefile
