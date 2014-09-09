#!/bin/sh
###############################################################
#
#   AUTHOR:    Vuong - W/NP11
#
#   DATE:      28/11/2000
#
#   PURPOSE:   This script uses the make utility to update the libip 
#              archive libraries.
#              It first reads a list of source files in the library and
#              then generates a makefile used to update the archive
#              libraries.  The make command is then executed for each
#              archive library, where the archive library name and 
#              compilation flags are passed to the makefile through 
#              environment variables.
#
#   REMARKS:   Only source files that have been modified since the last
#              library update are recompiled and replaced in the object
#              archive libraries.  The make utility determines this
#              from the file modification times.
#
#              New source files are also compiled and added to the object 
#              archive libraries.
#
###############################################################

#
#     Generate a list of object files that corresponds to the
#     list of Fortran ( .f ) files in the current directory
#
for i in `ls *.f` ; do
  obj=`basename $i .f`
  OBJS="$OBJS ${obj}.o"
done
#
#     Remove make file, if it exists.  May need a new make file
#     with an updated object file list.
#
if [ -f make.libip ] ; then
  rm -f make.libip
fi
#
#     Generate a new make file ( make.libip), with the updated object list,
#     from this HERE file.
#
cat > make.libip << EOF
SHELL=/bin/sh

\$(LIB):	\$(LIB)( ${OBJS} )

.f.a:
	\$(FCMP) -c \$(FFLAGS) \$<
	ar -ruv \$(AFLAGS) \$@ \$*.o
	rm -f \$*.o

EOF
export FCMP=${1:-${FCMP:-xlf_r}}
#
#     Update 4-byte version of libip_4.a
#
#BSM export LIB="./libip_4.a"
export LIB="../../libip_4.a"
if [ $FCMP = xlf_r ] ; then
 export FFLAGS=" -O3 -qsmp=noauto -qnosave"
 export AFLAGS=" -X64"
 export CFLAGS=" -O3 -q64"
else
 export FFLAGS=" -O3 -xHOST -convert big_endian -traceback -g"
#export FFLAGS=" -O0 -xHOST -convert big_endian -traceback -g"
 export AFLAGS=" "
fi
make -f make.libip
#
#     Update 8-byte version of libip_8.a
#
#BSM export LIB="./libip_8.a"
export LIB="../../libip_8.a"
if [ $FCMP = xlf_r ] ; then
 export FFLAGS=" -O3 -qsmp=noauto -qnosave -qintsize=8 -qrealsize=8"
 export AFLAGS=" -X64"
 export CFLAGS=" -O3 -q64"
else
 export FFLAGS=" -O3 -i8 -r8 -xHOST -convert big_endian -traceback -g"
#export FFLAGS=" -O0 -i8 -r8 -xHOST -convert big_endian -traceback -g"
 export AFLAGS=" "
fi
make -f make.libip
#
#     Update Double Precision (Size of Real 8-byte and default Integer) version
#     of libip_d.a
#
#BSM export LIB="./libip_d.a"
export LIB="../../libip_d.a"
if [ $FCMP = xlf_r ] ; then
 export FFLAGS=" -O3 -qsmp=noauto -qnosave -qintsize=4 -qrealsize=8"
 export AFLAGS=" -X64"
 export CFLAGS=" -O3 -q64"
else
 export FFLAGS=" -O3 -r8 -xHOST -convert big_endian -traceback -g"
#export FFLAGS=" -O0 -r8 -xHOST -convert big_endian -traceback -g"
 export AFLAGS=" "
fi
make -f make.libip
rm -f make.libip
