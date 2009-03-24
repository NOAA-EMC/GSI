#!/bin/make

help:
	@echo "	make all"
	@echo "	make test"

all: mksi.x

#FC	= lf95
#FFLAGS	= -Wa,--32

# ../Makefile.conf is resulted from "(cd ..; ./configure)"
include ../Makefile.conf

.SUFFIXES:
.SUFFIXES: .F90 .o

.F90.o:
	$(FC) -c $(FFLAGS) $<

SRCS =  main.F90		\
	m_actvchan.F90		\
	m_satinst.F90		\
	m_satops.F90		\
	m_sitmpl.F90		\
	satinfo_util.F90

OBJS =	$(SRCS:.F90=.o)

mksi.x: $(OBJS)
	$(FC) -o $@ $(OBJS)

list:
	@ echo $(SRCS)

main.o: m_actvchan.o m_satinst.o m_satops.o m_sitmpl.o satinfo_util.o
m_actvchan.o: satinfo_util.o
m_satinst.o: satinfo_util.o
m_satops.o: satinfo_util.o
m_sitmpl.o: satinfo_util.o
satinfo_util.o: assert.H

clean:
	rm -f mksi.x *.o *.mod

test: mksi.x
	./mksi.x <setup.nml
#.
