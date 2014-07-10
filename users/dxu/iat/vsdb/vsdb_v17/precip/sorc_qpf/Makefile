SHELL=  /bin/sh
CMD=	../exec/PVRFY     

FOPTS=  $(FFLAGS)
SRCS=	PVRFY.f VRFY.f ANNATV.f GRANGE.f                                        
LIBS=   -L$(LIBDIR) -lbacio_4 -lip_4 -lsp_4 -l$(W3LIB) 
$(CMD): $(SRCS)
	$(FCMP) $(FOPTS) $(SRCS) $(LIBS) -o $(CMD)
