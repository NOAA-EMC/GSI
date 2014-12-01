#!/bin/ksh
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

export FCMP=${1:-ifort}
export FCMP95=${2:-${FCMP}}
export LIBDIR=${3:-${LIBDIR:-/nwprod/lib}}

if [ $FCMP = xlf_r ] ; then    # For IBMP6
 export LIBS="-L$LIBDIR -lsigio_4 -lsfcio_4 -llandsfcutil_d -lsp_d -lip_d -lbacio_4 -lw3_d -lnemsio -L$LIBDIR2 -lgfsio_4"

 export FFLAGSM=" -qnodbg -qrealsize=8 -qnosave -qsmp=noauto:omp -O3 -qmaxmem=-1"
 export FFLAGS2M=" -qnodbg -qrealsize=8 -qnosave -qsmp=noauto:omp -O3 -qmaxmem=-1 -qfree"
 export LDFLAGSM="-lesslsmp -qsmp=noauto:omp"
 export OMPFLAGM=
 export RECURS=
else
 export LIBS="-L$LIBDIR -lgfsio_v1.1.0_4 -lsigio_4  -lsfcio_4 -lbacio_4 -llandsfcutil_d -lip_d -lw3emc_d -lw3nco_d -lnemsio -lsp_v2.0.1_d "

 export FFLAGSM="-i4 -O3 -r8  -convert big_endian -fp-model precise"
 export FFLAGS2M="-i4 -O3 -r8 -convert big_endian -fp-model precise -FR"

#export RECURS="-recursive"
 export RECURS=

 export LIBFLAGSM=" "
 export LDFLAGSM="-openmp -auto"
 export OMPFLAGM="-openmp -auto"
fi
make -f Makefile
