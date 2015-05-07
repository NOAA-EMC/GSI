#!/bin/sh
###############################################################
#
#   AUTHOR:    Vuong - SIB
#
#   DATE:      10/12/2012
#
#   PURPOSE:   This script uses the make utility to update the libw3 
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
export FC=${1:-ifort}
export CC=${2:-icc}
#
for i in `ls *.f` ; do
  obj=`basename $i .f`
  OBJS="$OBJS ${obj}.o"
done
#
#     Generate a list of object files that corresponds to the
#     list of C ( .c ) files in the current directory
#
for i in `ls *.c` ; do
  obj=`basename $i .c`
  OBJS="$OBJS ${obj}.o"
done
#
#     Remove make file, if it exists.  May need a new make file
#     with an updated object file list.
#
if [ -f make.libw3nco ] ; then
  rm -f make.libw3nco
fi
#
#     Generate a new make file ( make.libw3), with the updated object list,
#     from this HERE file.
#
cat > make.libw3nco << EOF
SHELL=/bin/sh

\$(LIB):	\$(LIB)( ${OBJS} )

.f.a:
	$FC -c \$(FFLAGS) \$<
	ar -ruv \$(AFLAGS) \$@ \$*.o
	rm -f \$*.o

.c.a:
	$CC -c \$(CFLAGS) \$<
	ar -ruv  \$(AFLAGS) \$@ \$*.o
	rm -f \$*.o
EOF
#
#     Update 4-byte version of libw3nco_4.a
#
export LIB="../../libw3nco_4.a"
export FFLAGS=" -O3 -g"
export AFLAGS=" "
export CFLAGS=" -O3 -DLINUX"
make -f make.libw3nco

#
#     Update 8-byte version of libw3nco_8.a
#
export LIB="../../libw3nco_8.a"
export FFLAGS=" -O3 -g -r8 -i8"
export AFLAGS=" "
export CFLAGS=" -O3 -DLINUX"
make -f make.libw3nco

#
#     Update Double Precision (Size of Real 8-byte and default Integer) version 
#     of libw3nco_d.a
#
export LIB="../../libw3nco_d.a"
export FFLAGS=" -O3 -g -r8"
export AFLAGS=" "
export CFLAGS=" -O3 -DLINUX"
make -f make.libw3nco

rm -f make.libw3nco
