SHELL	=/bin/sh
INCMOD	=$(LIBDIR)/incmod/sigio_4
SRCM	=sighdr.f
INCM	=
SRCS	=
OBJS	=
MODS	=
FC	=$(FCMP)
FFLAGS	=-qnosave -O2   -qmaxmem=-1
LDFLAGS	=-qsmp=noauto:omp
LIBS    =  -L$(LIBDIR) -lsigio_4 -lbacio_4 -lw3_4
CMD	=global_sighdr
$(CMD):	$(SRCM) $(OBJS)
	$(FC) -I $(INCMOD) $(FFLAGS) $(LDFLAGS) $(SRCM) $(OBJS) $(LIBS) -o $(CMD)
clean:
	-rm -f $(OBJS) $(MODS)
clobber:        clean
	-rm -f $(CMD)
void:   clobber
	-rm -f $(SRCS) $(SRCM) $(INCM) makefile
