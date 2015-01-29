#!/bin/ksh
set -x

machine=${machine:-WCOSS}
LDIR=/nwprod/lib            
CF=ifort
export MP_CORE_FILE_FORMAT=lite
FFOPTS="-g -O0 -i4 -r8 -check all -ftrapuv -convert big_endian -fp-stack-check -fstack-protector -heap-arrays -recursiv -traceback -openmp"
LIBS="-L/$LDIR -lw3emc_d -lw3nco_d -lbacio_4 -lsp_v2.0.1_d"
LDOPTS="-openmp -mkl"

f=mtnlm7_slm30g.f 
x=../../exec/terrain.x
$CF $FFOPTS $f $LIBS $LDOPTS -o $x


