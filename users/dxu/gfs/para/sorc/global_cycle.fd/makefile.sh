#!/bin/ksh
set -x
export FCMP=${1:-ifort}
export FCMP95=${2:-$FCMP}
export LIBDIR=${3:-${LIBDIR:-/nwprod/lib}}
mac=$(hostname | cut -c1-1)

if [ $mac = f ] ; then  #For Zeus
  export LIBDIR=/contrib/nceplibs/nwprod/lib
  export INCS="-I $LIBDIR/incmod/sfcio_v1.1.0"
  export LIBS="-L$LIBDIR -lsfcio -lw3emc_d -lw3nco_d -lbacio_4 -lsp_d"
else
  export INCS="-I $LIBDIR/incmod/sfcio_4"
  export LIBS="-L$LIBDIR -lsfcio_4 -lw3emc_d -lw3nco_d -lbacio_4 -lsp_d"
fi
  export FFLAGSM="$INCS -O3 -r8 -convert big_endian -traceback -g"
  export OMPFLAG=-openmp
  export LDFLG=-openmp
make -f Makefile
mkdir ../../exec
cp -p global_cycle ../../exec/.
