SHELL=	/bin/ksh
FFLAGS	= $(FFLAGSM)
#DEBUG='-ftrapuv -check all -check nooutput_conversion -fp-stack-check -fstack-protector -traceback -g'
FC=$(FCMP)
OBJS	=num_parthds.o machine.o cycle.o sfcsub.o
global_cycle:	$(OBJS)
	${FC} ${DEBUG} $(FFLAGS) $(LDFLG) $(OBJS) $(LIBS) -o global_cycle

num_parthds.o:	num_parthds.f
	$(FC) $(FFLAGS) $(OMPFLAG) -c num_parthds.f

machine.o:	machine.f
	${FC} $(FFLAGS) -c machine.f

sfcsub.o:	sfcsub.f
	$(FC) $(FFLAGS) $(OMPFLAG) -c sfcsub.f

cycle.o:	cycle.f
	$(FC) $(FFLAGS) -c cycle.f

clean:
	-rm -f $(OBJS) *.mod
