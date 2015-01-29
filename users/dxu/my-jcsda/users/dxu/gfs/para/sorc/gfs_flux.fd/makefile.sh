#!/bin/ksh
set -x
export FCMP=${1:-ifort}
export FCMP95=${2:-$FCMP}
export LIBDIR=${3:-${LIBDIR:-/nwprod/lib}}
mac=$(hostname | cut -c1-1)

if [ $mac = f ] ; then  #For Zeus
  export LIBDIR=/contrib/nceplibs/nwprod/lib
#  export INCMOD="-I $LIBDIR/incmod/sigio_4 -I $LIBDIR/incmod/w3emc_v2.0.5_4"
  export LIBSMOD="-L$LIBDIR -lw3nco_4 -lbacio_4 -lsp_4"
else
  export INCMOD="-I $LIBDIR/incmod/sigio_4 -I /nwprod/lib/incmod/w3emc_v2.0.3_4"
  export LIBSMOD="-L$LIBDIR -lw3emc_v2.0.3_4 -lw3nco_4 -lbufr_4_64 -lbacio_4 -lsp_4 -lsigio_4"
fi
export FFLAGSM="-O2 -g -convert big_endian"
make -f Makefile
