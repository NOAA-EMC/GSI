INCMOD	=$(LIBDIR)/incmod/sigio_4
LIBS	= -L$(LIBDIR) -lbacio_4 -lw3_4 -lsp_4 -lsigio_4 -lessl
FFLAGS = -qmaxmem=-1 -qnosave -qsmp=noauto -O -I $(INCMOD) 
FC     = $(FCMP)
global_sigavg:	sigavg.o getopts.o
	$(FC) $(FFLAGS) sigavg.o getopts.o $(LIBS) -o global_sigavg
sigavg.o:	sigavg.f
	$(FC) $(FFLAGS) -c sigavg.f
getopts.o:	getopts.f
	$(FC) $(FFLAGS) -c getopts.f
