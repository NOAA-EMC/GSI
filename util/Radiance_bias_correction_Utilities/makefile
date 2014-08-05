SHELL=/bin/sh
#
#
.SUFFIXES: .f90

OBJS= write_biascr_option.o

CMD=write_biascr_option.x

FC=ifort

LIBS = -L/nwprod/lib -lw3nco_4 -lw3emc_4 -lbufr_4_64

FFLAGS = -O2 -convert big_endian -assume byterecl -debug

LDFLAGS = 


$(CMD):		$(OBJS) 
	$(FC) $(LDFLAGS) ${FFLAGS} -o $(@) $(OBJS) ${LIBS}


clean:
	-rm -f *.o ${CMD}

clobber:	clean
	-rm -f $(CMD) $(CMD).prof

void:	clobber
	-rm -f $(SRCS)

.f.o:
	${FC} -c ${FFLAGS} $*.f

.f90.o:
	${FC} -c ${FFLAGS} $*.f90


