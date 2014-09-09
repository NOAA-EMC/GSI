SHELL=  /bin/sh
CMD=	mean_anl.exe      

FOPTS=  $(FFLAGS)
SRCS=	mean_anl.f
LIBS=   -L$(LIBDIR) -lbacio_4 -l$(W3LIB) 
$(CMD): $(SRCS)
	$(FCMP) $(FOPTS) $(SRCS) $(LIBS) -o $(CMD)
