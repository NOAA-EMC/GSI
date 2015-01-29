LIB     = libnemsio.a
INCMOD  = ../../incmod/nemsio
FC      = xlf95_r
FFLAGS  = -O3
AR      = ar
ARFLAGS = -rv -X64

MODULES  = nemsio_openclose.o nemsio_read.o nemsio_write.o nemsio_module.o

$(LIB): $(MODULES)
	$(AR) $(ARFLAGS) $@ $(MODULES)
	mv *.mod $(INCMOD)

clean:
	rm -f *.o *.mod $(LIB)
	
.SUFFIXES:
.SUFFIXES: .f90 .o

.f90.o:
	$(FC) $(FFLAGS) -c $*.f90

nemsio_read.o: nemsio_openclose.o
nemsio_write.o: nemsio_openclose.o
nemio_module.o: nemsio_openclose.o nemsio_read.o nemsio_write.o
