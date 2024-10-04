.SUFFIXES: .f90 .o

OBJ  =  da_lapack.o

$(OBJ):
	rm -f $@
	$(CPP) $(CPPFLAGS) $(OMPCPP) $(FPPFLAGS) $*.f90 > $*.f
	$(FC) -o $@ -c -I../blas $(FCFLAGS) $*.f

clean:
	rm -f $(OBJ) *.f *.mod

superclean: clean
