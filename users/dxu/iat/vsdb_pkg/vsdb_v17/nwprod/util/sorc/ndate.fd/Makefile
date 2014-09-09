#LIBS	=    -L$(LIBDIR) -l$(W3LIB) -L/opt/cray/netcdf/3.6.2/netcdf-gnu -lnetcdf
LIBS	=    -L$(LIBDIR) -l$(W3LIB)
ndate:	ndate.f
	$(FCMP) -o ../../exec/ndate ndate.f $(LIBS)
