# makefile for nssl_porcess
#!/bin/sh -v

# Define the fortran compiler and options
#

include ../../../configure.gsi

FC   =  ${DM_FC}
FLAGS= -fp-model precise -assume byterecl -fpe0 -g -traceback

INCLD=  -I../../../include -I${NETCDF}/include
LIBS =  -L../../../lib -lbufr_i4r8 -L${NETCDF}/lib -lnetcdf

OBJS = write_bufr_ref.o process_NSSL_mosaic.o \
       read_nssl_binary.o nc_subs.o

all: process_NSSL_mosaic

process_NSSL_mosaic: ${OBJS}
	${FC} -o process_NSSL_mosaic.exe ${FLAGS} ${OBJS} ${LIBS} 

.SUFFIXES : .f90 .o

.f90.o :
	${FC} ${FLAGS} ${INCLD} -c $<

clean:
	/bin/rm -f *.o *.exe
