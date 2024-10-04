.SUFFIXES: .f90 .o

OBJ  =  da_blas.o

$(OBJ):
	rm -f  $@
	$(CPP) $(CPPFLAGS) $(OMPCPP) $*.f90  > $*.f
	$(FC) -o $@ -c $(FCFLAGS) $*.f

clean:
	rm -f $(OBJ) *.f *.mod

superclean: clean
