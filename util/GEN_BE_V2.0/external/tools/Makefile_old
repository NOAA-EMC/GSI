.SUFFIXES: .f90 .o

OBJ =  da_reporting.o da_tools_serial.o da_etkf.o da_gen_be.o da_control.o module_ffts.o da_be_spectral.o

AR = ar
ARFLAGS = cr
TARGET = libtools.a

library: $(OBJ)
	$(AR) $(ARFLAGS) $(TARGET) $(OBJ)
	$(RANLIB) $(TARGET)

da_control.o : da_control.f90
module_ffts.o : module_ffts.f90
da_reporting.o : da_reporting.f90 da_control.o
da_tools_serial.o : da_tools_serial.f90 da_control.o da_reporting.o
da_etkf.o : da_etkf.f90 da_control.o
da_gen_be.o : da_gen_be.f90 da_control.o da_reporting.o da_tools_serial.o 
da_be_spectral.o : da_be_spectral.f90 da_control.o da_reporting.o da_tools_serial.o

.f90.o :
	rm -f  $@
	$(CPP) $(CPPFLAGS) $(OMPCPP) $*.f90  > $*.f
	$(FC) -o $@ -c -I../blas -I../lapack $(FCFLAGS) $*.f

clean:
	rm -f $(OBJ) $(TARGET) *.f *.mod

superclean: clean
