#! /bin/sh -v

# Define the fortran compiler and options

FC   =  ifort # pgf90
FLAGS=  -O2  
INCLD=  -I../../include
LIBS =  -L../../lib -lbufr_i4r8

all: bufr_encode_sample bufr_decode_sample bufr_append_sample \
     prepbufr_decode_all bufr_decode_radiance \
     prepbufr_encode_surface prepbufr_append_surface \
     prepbufr_encode_upperair prepbufr_append_upperair \
     prepbufr_append_retrieve prepbufr_inventory

bufr: bufr_encode_sample bufr_decode_sample bufr_append_sample

sfc: prepbufr_encode_surface prepbufr_append_surface prepbufr_decode_all

up: prepbufr_encode_upperair prepbufr_append_upperair prepbufr_decode_all

others: bufr_decode_radiance prepbufr_append_retrieve

append_prepbufr_multilevel: append_prepbufr_multilevel.o
	${FC} -o append_prepbufr_multilevel.exe ${FLAGS} append_prepbufr_multilevel.o ${LIBS} 

bufr_encode_sample: bufr_encode_sample.o
	${FC} -o bufr_encode_sample.exe ${FLAGS} bufr_encode_sample.o ${LIBS} 
	rm bufr_encode_sample.o

bufr_decode_sample: bufr_decode_sample.o
	${FC} -o bufr_decode_sample.exe ${FLAGS} bufr_decode_sample.o ${LIBS} 
	rm bufr_decode_sample.o

bufr_append_sample: bufr_append_sample.o
	${FC} -o bufr_append_sample.exe ${FLAGS} bufr_append_sample.o ${LIBS} 
	rm bufr_append_sample.o

prepbufr_encode_surface: prepbufr_encode_surface.o
	${FC} -o prepbufr_encode_surface.exe ${FLAGS} prepbufr_encode_surface.o ${LIBS} 
	rm prepbufr_encode_surface.o

prepbufr_decode_all: prepbufr_decode_all.o
	${FC} -o prepbufr_decode_all.exe ${FLAGS} prepbufr_decode_all.o ${LIBS} 
	rm prepbufr_decode_all.o

prepbufr_append_surface: prepbufr_append_surface.o
	${FC} -o prepbufr_append_surface.exe ${FLAGS} prepbufr_append_surface.o ${LIBS} 
	rm prepbufr_append_surface.o

prepbufr_encode_upperair: prepbufr_encode_upperair.o
	${FC} -o prepbufr_encode_upperair.exe ${FLAGS} prepbufr_encode_upperair.o ${LIBS} 
	rm prepbufr_encode_upperair.o

prepbufr_append_upperair: prepbufr_append_upperair.o
	${FC} -o prepbufr_append_upperair.exe ${FLAGS} prepbufr_append_upperair.o ${LIBS} 
	rm prepbufr_append_upperair.o

prepbufr_append_retrieve: prepbufr_append_retrieve.o
	${FC} -o prepbufr_append_retrieve.exe ${FLAGS} prepbufr_append_retrieve.o ${LIBS} 
	rm prepbufr_append_retrieve.o

bufr_decode_radiance: bufr_decode_radiance.o
	${FC} -o bufr_decode_radiance.exe ${FLAGS} bufr_decode_radiance.o ${LIBS} 
	rm bufr_decode_radiance.o

prepbufr_inventory: prepbufr_inventory.o
	${FC} -o prepbufr_inventory.exe ${FLAGS} prepbufr_inventory.o ${LIBS} 
	rm prepbufr_inventory.o

.SUFFIXES : .f90  .o

.f90.o :
	${FC} ${FLAGS} ${INCLD} -c $<

clean:
	/bin/rm -f *.o *.exe
