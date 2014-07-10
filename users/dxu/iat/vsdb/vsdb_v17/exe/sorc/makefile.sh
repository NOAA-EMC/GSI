#!/bin/sh
set -x

# IBM: xlf_r; Gaea: ftn; Zeus: ifort
export FCMP=${1:-ftn}

export LIBDIR="../../nwprod/lib"
export W3LIB="w3lib-2.0_4"

if [ $FCMP = xlf_r ] ; then
 export LIBDIR="/nwprod/lib"
 export W3LIB="w3_4"
 export LIBS="-L$LIBDIR -lip_4 -lsp_4 -lbacio_4 -l$W3LIB -l essl"
 export FFLAGS="-qsmp=noauto"
 make -f Makefile_ibm
else
 export LIBS="-L$LIBDIR -lip_4 -lsp_4 -lbacio_4 -l$W3LIB "
 export FFLAGS=" -convert big_endian -traceback "
 make -f Makefile
fi

