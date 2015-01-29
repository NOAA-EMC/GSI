################################################################################
# This makefile was produced by fmjif at 17:21:11 on 97/02/24.
SHELL=	/bin/sh
CMD=	../../exec/grbindex
FOPTS=	
#FOPTS=	-O3   -qintsize=8 -qrealsize=8 
LOPTS=	
INCS=	
OBJS=	grbindex.o
LIBS =    -L$(LIBDIR) -l$(W3LIB) -lbacio_4
################################################################################
$(CMD):	$(OBJS)
	$(FCMP) $(LOPTS) $(OBJS) $(LIBS) -o $(CMD)
$(OBJS):	makefile $(INCS)
grbindex.o:	grbindex.f
	$(FCMP) -c $(FOPTS) $*.f
clean:	
	-rm -f $(OBJS)
