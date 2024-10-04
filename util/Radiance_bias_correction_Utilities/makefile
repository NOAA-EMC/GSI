SHELL=/bin/sh
#
#
.SUFFIXES: .f90

OBJS= write_biascr_option.o

CMD=write_biascr_option.x

FC=ifort

$(CMD):		$(OBJS) 
	$(FC) -o $(@) $(OBJS) ${LIBS}


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


