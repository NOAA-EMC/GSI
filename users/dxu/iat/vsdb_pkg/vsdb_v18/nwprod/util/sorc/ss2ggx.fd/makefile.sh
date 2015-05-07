#!/bin/ksh
set -x
mac=$(hostname | cut -c1-1)
mac2=$(hostname | cut -c1-2)
if [ $mac2 = ga ] ; then
 export machine=gaea
 export FC=ftn
 export FFLAGSM="-O3 -free -convert big_endian -traceback"
 export LDFLAGSM=
 export LIBDIR=../../../lib
 export LIBSM="-L${LIBDIR} -lbacio_4 -lw3_4 -lsp_4 -lsigio_4"

elif [ $mac = g -o $mac = t ] ; then
 export machine=wcoss
 export FC=ifort
 export FFLAGSM="-O3 -free -convert big_endian -traceback"
 export LDFLAGSM=-openmp
 export LIBDIR=/nwprod/lib
 export INCMOD=/nwprod/lib/incmod/sigio_4
 export LIBSM="-L${LIBDIR} -lw3emc_4 -lw3nco_4 -lbacio_4 -lsp_4 -lsigio_4"

elif [ $mac = z -o $mac = f -o $mac = h -o ]; then
 export machine=zeus
 export FC=ifort
 export FFLAGSM="-O3 -free -convert big_endian -traceback"
 export LDFLAGSM=-openmp
 export LIBDIR=../../../lib
 export INCMOD=$LIBDIR/incmod/sigio_4
 export LIBSM="-L${LIBDIR} -lbacio_4 -lw3lib-2.0_4 -lw3nco_4 -lsp_4 -lsigio_4"

elif [ $mac2 = s4 ]; then
 export machine=s4-cardinal
 export FC=ifort
 export FFLAGSM="-O3 -free -convert big_endian -traceback"
 export LDFLAGSM=-openmp
 export LIBDIR=../../../lib
 export INCMOD=$LIBDIR/incmod/sigio_4
 export LIBSM="-L${LIBDIR} -lbacio_4 -lw3lib-2.0_4 -lw3nco_4 -lsp_4 -lsigio_4"

else
 machine=IBMP6
 export FC=xlf90_r
 export FFLAGSM=" -O -qmaxmem=-1 -qnosave -qsmp=noauto "
 export LIBFLAGSM=
 export LIBDIR=/nwprod/lib
 export INCMOD=/nwprod/lib/incmod
 export LIBSM="-L${LIBDIR} -lw3_4 -lbacio_4 -lsp_4 -lsigio_4"
fi
make -f Makefile_ss2gg
make -f Makefile_ss2ggx
make -f Makefile_ss2lv
make -f Makefile_sigdif

cp -p ss2gg  ../../exec/.
cp -p ss2ggx ../../exec/.
cp -p sigdif ../../exec/.
