################################################################################
# This makefile was produced by fmjif at 17:21:11 on 97/02/24.
SHELL=	/bin/sh
CMD=	copygb2
FOPTS=	-O -C -qrealsize=8 
LOPTS=	-qsmp=noauto    -lessl
INCS=	-I$(LIBDIR)/incmod/g2_d
OBJS=	copygb2.o
LIBS= -L$(LIBDIR) -lg2_d -lw3_d -lbacio_4 -lip_d -lsp_d\
      -L/usrx/local/64bit.cirrus/lib -ljasper -lpng -lz
################################################################################
$(CMD):	$(OBJS)
	$(FCMP) $(LOPTS) $(OBJS) $(LIBS) -o $(CMD)
#$(OBJS):	$(INCS)
copygb2.o:	copygb2.f
	$(FCMP) -c $(INCS) $(FOPTS) $*.f
clean:	
	-rm -f $(OBJS)
