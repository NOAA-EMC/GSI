#!/bin/sh
###############################################################
#
#   AUTHOR:    Gilbert - W/NP11
#
#   DATE:      01/11/1999
#
#   PURPOSE:   This script uses the make utility to update the bacio 
#              archive libraries.
#
###############################################################

#
#     Remove make file, if it exists.  May need a new make file
#
if [ -f make.bacio ] 
then
  rm -f make.bacio
fi
#
#     Generate a make file ( make.bacio) from this HERE file.
#
cat > make.bacio << EOF
SHELL=/bin/sh
ncepxlc=xlc_r
ncepxlf=xlf_r

\$(LIB):	\$(LIB)( bacio.v1.4.o baciof.o bafrio.o )

\$(LIB)(bacio.v1.4.o):       bacio.v1.4.c \$(INC)
	ln -fs \$(INC) clib.h
	\$(ncepxlc) -c \$(CFLAGS) bacio.v1.4.c
	ar -rv \$(AFLAGS) \$(LIB) bacio.v1.4.o
#	rm clib.h

\$(LIB)(baciof.o):   baciof.f
	\$(ncepxlf) -c \$(FFLAGS) baciof.f
	ar -rv \$(AFLAGS) \$(LIB) baciof.o 

\$(LIB)(bafrio.o):   bafrio.f
	\$(ncepxlf) -c \$(FFLAGS) bafrio.f
	ar -rv \$(AFLAGS) \$(LIB) bafrio.o 
	rm -f baciof.o bafrio.o clib.h
EOF
#
#     Update 4-byte version of libbacio_4.a
#
#BSM export LIB="./libbacio_4.a"
export LIB="../../libbacio_4.a"
export INC="clib4.h"
export FFLAGS=" -O3 -qnosave"
export AFLAGS=" -X64"
export CFLAGS=" -q64 -O3"
make -f make.bacio
#
#     Update 8-byte version of libbacio_8.a
#
 #BSM export LIB="./libbacio_8.a"
 export LIB="../../libbacio_8.a"
 export INC="clib8.h"
 export FFLAGS=" -O3 -qnosave -qintsize=8 -qrealsize=8"
 export AFLAGS=" -X64"
 export CFLAGS=" -q64 -O3 -qlonglong"
 make -f make.bacio

 rm -f make.bacio
