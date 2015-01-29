#!/bin/sh
set -x
mac=$(hostname | cut -c1-1)
mac2=$(hostname | cut -c1-2)

if [ $mac2 = ga ] ; then                         # For GAEA
 machine=gaea
elif [ $mac = z -o $mac = h -o $mac = f ] ; then # For ZEUS
 machine=zeus
 export LIBDIR=/contrib/nceplibs/nwprod/lib
 export NCEPLIB=/contrib/nceplibs/nwprod/lib
elif [ $mac = t -o $mac = e -o $mac = g ] ; then # For WCOSS
 machine=wcoss
 export LIBDIR=/nwprod/lib
 export NCEPLIB=/usrx/local/nceplibs
fi

export LIBDIR=${LIBDIR:-/nwprod/lib}
export FCMP=ifort
if [ $FCMP = xlf_r ] ; then
 export FFLAGSM="-O2 -qcheck -qextchk -qarch=auto -qflttrap=ov:zero:inv:enable -qintsize=4 -qrealsize=8"
 export LDFLAGSM="-bnoquiet -bloadmap:loadmap.txt"
else
 export FFLAGSM=" -O2 -g -traceback -i4 -r8"
 export LDFLAGSM="-Wl,-Map,loadmap.txt"
fi
make -f Makefile
