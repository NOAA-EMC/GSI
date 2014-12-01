SHELL	=/bin/sh
INCMOD	=$(LIBDIR)/incmod/sfcio_4
SRCM	=sfchdr.f
INCM	=
SRCS	=
OBJS	=
MODS	=
FC	=$(FCMP)
FFLAGS	=-qnosave -O2   -qmaxmem=-1
LDFLAGS	=   
LIBS    = -L$(LIBDIR) -lsfcio_4 -lbacio_4 -lw3_4
CMD	=global_sfchdr
$(CMD):	$(SRCM) $(OBJS)
	$(FC) -I $(INCMOD) $(FFLAGS) $(LDFLAGS) $(SRCM) $(OBJS) $(LIBS) -o $(CMD)
clean:
	-rm -f $(OBJS) $(MODS)
clobber:        clean
	-rm -f $(CMD)
void:   clobber
	-rm -f $(SRCS) $(SRCM) $(INCM) makefile
