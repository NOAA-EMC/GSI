################################################################################
# This makefile was produced by fmjif at 17:21:11 on 97/02/24.
SHELL=	/bin/sh
CMD=	../../exec/copygb
FOPTS=	$(FFLAGS)
LOPTS=	$(LFLAGS)
INCS=	
OBJS=	copygb.o
LIBS= -L$(LIBDIR) -lbacio_4 -lip_d -lsp_d -l$(W3LIB)	
################################################################################
$(CMD):	$(OBJS)
	$(FCMP) $(LOPTS) $(OBJS) $(LIBS) -o $(CMD)
$(OBJS):	$(INCS)
copygb.o:	copygb.f
	$(FCMP) -c $(FOPTS) $*.f
clean:	
	-rm -f $(OBJS)
