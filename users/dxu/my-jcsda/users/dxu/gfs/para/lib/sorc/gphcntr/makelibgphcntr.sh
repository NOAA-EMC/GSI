#!/bin/sh
###############################################################
#
#   AUTHORS:    Gilbert/Kumar - W/NP11 -  W/NP12
#
#   DATE:      01/08/99
#
#   PURPOSE:   This script uses the make utility to update the gphcntr 
#              archive library.
#              It first reads a list of the source files in the library and
#              then generates a makefile used to update the archive
#              library.  The make command is then executed,
#              where the archive library name and 
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
#              Krishna Kumar modified source files to run on IBM/SP 6000  
#              and recompiled this library 
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
if [ -f make.gphcntr ] 
then
  rm -f make.gphcntr
fi
#
#     Generate a new make file ( make.gphcntr), with the updated object list,
#     from this HERE file.
#
cat > make.gphcntr << EOF
SHELL=/bin/sh

\$(LIB):	\$(LIB)( ${OBJS} )

.f.a:
	\$(FC) -c \$(FFLAGS) \$<
	ar -ruv -X64 \$@ \$*.o
	rm -f \$*.o

EOF
#
#     Update 8-byte version of gphcntr for Winterhawk
#
export FC=xlf_r
export LIB="../../libgphcntr_8.a"
export FFLAGS=" -O -qnosave -qintsize=8 -qrealsize=8 -qnoescape -qctyplss -qmaxmem=-1"
make -f make.gphcntr

rm -f make.gphcntr
