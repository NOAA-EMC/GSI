#!/bin/sh
set -x
export LIBDIR=/nwprod/lib
export FCMP=ifort
if [ $FCMP = xlf_r ] ; then
 export FFLAGSM="-O2 -qcheck -qextchk -qarch=auto -qflttrap=ov:zero:inv:enable -qintsize=4 -qrealsize=8"
 export LDFLAGSM="-bnoquiet -bloadmap:loadmap.txt"
else
 export FFLAGSM=" -O2 -g -traceback -i4 -r8"
 export LDFLAGSM="-Wl,-Map,loadmap.txt"
fi
make -f Makefile
