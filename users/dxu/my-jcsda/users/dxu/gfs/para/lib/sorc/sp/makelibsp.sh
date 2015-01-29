#!/bin/sh
###############################################################
#
#   AUTHOR:    Vuong - W/NP11
#
#   DATE:      12/04/2000
#
#   PURPOSE:   This script uses the make utility to update the libsp 
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
for i in `ls *.f`
do
  obj=`basename $i .f`
  OBJS="$OBJS ${obj}.o"
done
#
#     Remove make file, if it exists.  May need a new make file
#     with an updated object file list.
#
if [ -f make.libsp ] 
then
  rm -f make.libsp
fi
#
#     Generate a new make file ( make.libsp), with the updated object list,
#     from this HERE file.
#
cat > make.libsp << EOF
SHELL=/bin/sh

\$(LIB):	\$(LIB)( ${OBJS} )

ncepxlf=xlf_r
.f.a:
	\$(ncepxlf) -c \$(FFLAGS) \$<
	ar -ruv \$(AFLAGS)  \$@ \$*.o
	rm -f \$*.o

EOF
#
#     Update 4-byte version of libspi_4.a 
#
export LIB="../../libsp_4.a"
export FFLAGS="-qnosave -qsmp=noauto -qtune=auto -O3 -qintsize=4 -qrealsize=4"
export AFLAGS=" -X64"
make -f make.libsp
#
#     Update 8-byte version of libsp_8.a
#
export LIB="../../libsp_8.a"
export FFLAGS=" -qnosave -qsmp=noauto -qtune=auto -O3 -qintsize=8 -qrealsize=8"
export AFLAGS=" -X64"
make -f make.libsp
#
#     Update Double Precision (Size of Real 8-byte and default Integer) version
#     of libsp_d.a
#
export LIB="../../libsp_d.a"
export FFLAGS=" -qnosave -qsmp=noauto -qtune=auto -O3 -qintsize=4 -qrealsize=8"
export AFLAGS=" -X64"
make -f make.libsp

rm -f make.libsp
