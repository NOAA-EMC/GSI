#!/bin/sh
###############################################################
#
#   PURPOSE:   This script uses the make utility to update the BUFR
#              archive libraries (libbufr*.a).
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

#-------------------------------------------------------------------------------
#     Determine the byte-ordering scheme used by the local machine.

cat > endiantest.c << ENDIANTEST

#define Order(x)\
	fill((char *)&x, sizeof(x)); \
	for (i=1; i<=sizeof(x); i++) { \
	    c=((x>>(byte_size*(sizeof(x)-i)))&mask); \
	    putchar(c==0 ? '?' : (char)c); \
	} \
	printf("\n");

void fill(p, size) char *p; int size; {
	char *ab= "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
	int i;

	for (i=0; i<size; i++) p[i]= ab[i];
}

void endian(byte_size) int byte_size; {
	int j=0;
	unsigned int mask, i, c;

	mask=0;
	for (i=1; i<=(unsigned)byte_size; i++) mask= (mask<<1)|1;
	Order(j);
}

int cprop() {
	/* Properties of type char */
	char c;
	int byte_size;

	c=1; byte_size=0;
	do { c<<=1; byte_size++; } while(c!=0);

	return byte_size;
}

main()
{
	int byte_size;

	byte_size= cprop();
	endian(byte_size);
}
ENDIANTEST

ncepxlc=xlc_r

$ncepxlc -o endiantest endiantest.c

if [ `./endiantest | cut -c1` = "A" ]
then
    byte_order=BIG_ENDIAN
else
    byte_order=LITTLE_ENDIAN
fi

rm -f endiantest.c endiantest

    
#-------------------------------------------------------------------------------
#     Preprocess any Fortran *.F files into corresponding *.f files.

BNFS=""

for i in `ls *.F`
do
  bn=`basename $i .F`
  bnf=${bn}.f
  BNFS="$BNFS $bnf"
  /usr/lib/cpp -P -D$byte_order $i $bnf
done

#-------------------------------------------------------------------------------
#     Generate a list of object files that correspond to the
#     list of Fortran ( *.f ) files in the current directory.

OBJS=""

for i in `ls *.f`
do
  obj=`basename $i .f`
  OBJS="$OBJS ${obj}.o"
done
 
#-------------------------------------------------------------------------------
#     Generate a list of object files that corresponds to the
#     list of C ( .c ) files in the current directory.
 
for i in `ls *.c`
do
  obj=`basename $i .c`
  OBJS="$OBJS ${obj}.o"
done
 
#-------------------------------------------------------------------------------
#     Remove make file, if it exists.  May need a new make file
#     with an updated object file list.
 
if [ -f make.libbufr ] 
then
  rm -f make.libbufr
fi

#-------------------------------------------------------------------------------
#     Generate a new make file ( make.libbufr), with the updated object list,
#     from this HERE file.

cat > make.libbufr << EOF
SHELL=/bin/sh

\$(LIB):	\$(LIB)( ${OBJS} )

ncepxlf=xlf_r
ncepxlc=xlc_r

.f.a:
	\$(ncepxlf) -c \$(FFLAGS) \$<
	ar -ruv \$(AFLAGS) \$@ \$*.o
	rm -f \$*.o

.c.a:
	\$(ncepxlc) -c \$(CFLAGS) \$<
	ar -ruv \$(AFLAGS) \$@ \$*.o
	rm -f \$*.o
EOF

#-------------------------------------------------------------------------------
#     Generate the bufrlib.prm header file.

/usr/lib/cpp -P -DBUILD=NORMAL bufrlib.PRM bufrlib.prm

#-------------------------------------------------------------------------------
#   Update libbufr_4_64.a (4-byte REAL, 4-byte INT, 64-bit compilation,
#                          Fortran optimization level 4, C optimization level 3)
 
export LIB="../../libbufr_4_64.a"
export FFLAGS=" -O4 -q64 -qsource -qstrict -qnosave -qintsize=4 -qrealsize=4 -qxlf77=leadzero"
export CFLAGS=" -O3 -q64"
export AFLAGS=" -X64"
make -f make.libbufr
err_make=$?
[ $err_make -ne 0 ]  && exit 99
 
#-------------------------------------------------------------------------------
#   Update libbufr_8_64.a (8-byte REAL, 8-byte INT, 64-bit compilation,
#                          Fortran optimization level 4, C optimization level 3)
 
export LIB="../../libbufr_8_64.a"
export FFLAGS=" -O4 -q64 -qsource -qstrict -qnosave -qintsize=8 -qrealsize=8 -qxlf77=leadzero"
export CFLAGS=" -O3 -q64 -DF77_INTSIZE_8"
export AFLAGS=" -X64"
make -f make.libbufr
err_make=$?
[ $err_make -ne 0 ]  && exit 99

#-------------------------------------------------------------------------------
#   Update libbufr_d_64.a (8-byte REAL, 4-byte INT, 64-bit compilation,
#                          Fortran optimization level 4, C optimization level 3)

export LIB="../../libbufr_d_64.a"
export FFLAGS=" -O4 -q64 -qsource -qstrict -qnosave -qintsize=4 -qrealsize=8 -qxlf77=leadzero"
export CFLAGS=" -O3 -q64"
export AFLAGS=" -X64"
make -f make.libbufr
err_make=$?
[ $err_make -ne 0 ]  && exit 99

#-------------------------------------------------------------------------------
#     Generate a new bufrlib.prm header file.

/usr/lib/cpp -P -DBUILD=C32BITS bufrlib.PRM bufrlib.prm

#-------------------------------------------------------------------------------
#   Update libbufr_4_32.a (4-byte REAL, 4-byte INT, 32-bit compilation,
#                          Fortran optimization level 3, C optimization level 3)

export LIB="../../libbufr_4_32.a"
export FFLAGS=" -O3 -q32 -qsource -qnosave -qintsize=4 -qrealsize=4 -qxlf77=leadzero"
export CFLAGS=" -O3 -q32"
export AFLAGS=" -X32"
make -f make.libbufr
err_make=$?
[ $err_make -ne 0 ]  && exit 99

#-------------------------------------------------------------------------------
#     Generate a new bufrlib.prm header file.

/usr/lib/cpp -P -DBUILD=MAXMTBL bufrlib.PRM bufrlib.prm

#-------------------------------------------------------------------------------
#   Update libbufr_m_64.a (8-byte REAL, 8-byte INT, 64-bit compilation,
#                          maximum number of DX tables for use with internal memory messages,
#                          Fortran optimization level 4, C optimization level 3)

export LIB="../../libbufr_m_64.a"
export FFLAGS=" -O4 -q64 -qsource -qstrict -qnosave -qintsize=8 -qrealsize=8 -qxlf77=leadzero"
export CFLAGS=" -O3 -q64 -DF77_INTSIZE_8"
export AFLAGS=" -X64"
make -f make.libbufr
err_make=$?
[ $err_make -ne 0 ]  && exit 99

#-------------------------------------------------------------------------------
#     Generate a new bufrlib.prm header file.

/usr/lib/cpp -P -DBUILD=SUPERSIZE bufrlib.PRM bufrlib.prm

#-------------------------------------------------------------------------------
#   Update libbufr_s_64.a (4-byte REAL, 4-byte INT, 64-bit compilation, extra-large array sizes,
#                          Fortran optimization level 4, C optimization level 3)
 
export LIB="../../libbufr_s_64.a"
export FFLAGS=" -O4 -q64 -qsource -qstrict -qnosave -qintsize=4 -qrealsize=4 -qxlf77=leadzero"
export CFLAGS=" -O3 -q64"
export AFLAGS=" -X64"
make -f make.libbufr
err_make=$?
[ $err_make -ne 0 ]  && exit 99
 
#-------------------------------------------------------------------------------

#     Clean up and check how we did.

rm -f make.libbufr bufrlib.prm $BNFS

if [ -s ../../libbufr_s_64.a ] ; then
   echo
   echo "SUCCESS: The script updated all BUFR archive libraries"
   echo
   rm *.lst
else
   echo
   echo "FAILURE: The script did NOT update all BUFR archive libraries"
   echo
fi
