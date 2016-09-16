# Makefile for variants of wrf_io_flags.h and wrf_status_codes.h

all: foo_io_flags.h

# The if statement below modifies WRF data type codes for builds made with 
# compiler autopromotion of REAL -> DOUBLE.  
foo_io_flags.h : wrf_io_flags.h ../../configure.gen_be
	( /bin/rm -f foo_io_flags.h; \
	/bin/cp wrf_io_flags.h foo_io_flags.h; \
	if [ $(RWORDSIZE) -ne $(NATIVE_RWORDSIZE) ] ; then \
	  /bin/rm -f foo_io_flags.h; \
	  sed -e 's/104/105/' wrf_io_flags.h > foo_io_flags.h ;\
	fi )

superclean: 
	/bin/rm -f foo_io_flags.h

# DEPENDENCIES : only dependencies after this line 

