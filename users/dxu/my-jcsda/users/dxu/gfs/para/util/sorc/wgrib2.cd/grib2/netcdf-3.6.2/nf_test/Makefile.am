## This is a automake file, part of Unidata's netCDF package.
# Copyright 2005-2006, see the COPYRIGHT file for more information.

# This file builds and runs the following: 

# - The nf_test program, which tests the netCDF fortran 77 interface.

# - The ftest program, which tests the netCDF fortran 77 V2
# interface. (Unless V2 is disabled).

# - The ftst_vars program, if netCDF-4 is enabled.

# - The tst_f90 program, which tests the netCDF fortran 90
# interface. (Unless F90 API is disabled).

# - The f90tst_vars program, if netCDF-4 is enabled and F90 is not
# disabled.

# $Id: Makefile.am,v 1.32 2007/03/01 19:46:05 ed Exp $

LDADD = 

# nf_test is the main test program. tst_f77_v2 is a small extra test
# program.
TESTPROGRAMS = nf_test
nf_test_SOURCES = test_get.m4 test_put.m4 nf_error.F nf_test.F	\
test_read.F test_write.F util.F fortlib.c tests.inc

# Cleaning up files created during the process.
CLEANFILES = scratch.nc test.nc copy.nc fills.nc tst_f77_v2.nc
MAINTAINERCLEANFILES = Makefile.in test_get.F test_put.F

# Did the user build the V2 F77 API? If so, run this test.
if BUILD_V2
TESTPROGRAMS += tst_f77_v2
tst_f77_v2_SOURCES = tst_f77_v2.F
CLEANFILES += tst_f77_v2.nc
endif # BUILD_V2

# If building a separate fortran library, include it so the tests can
# find it.
if BUILD_SEPARATE_FORTRAN
LDADD += ../fortran/libnetcdff.la -lm
endif

# Some fortran compilers think your files should not have an .f90
# extension! The value of FCFLAGS_f90 is set in the configure script,
# based on the fortran compiler.
AM_FCFLAGS = $(FCFLAGS_f90)
AM_F90FLAGS = $(FCFLAGS_f90)
AM_FFLAGS =

# Is the user building netCDF-4?
if USE_NETCDF4

# Add this netCDF-4 f77 test program.
TESTPROGRAMS += ftst_vars
ftst_vars_SOURCES = ftst_vars.F fortlib.c handle_err.f
CLEANFILES += ftst_vars.nc

# Add this netCDF-4 f90 test program.
if BUILD_F90
TESTPROGRAMS += f90tst_vars
f90tst_vars_SOURCES = f90tst_vars.f90
CLEANFILES += f90tst_vars.nc
endif #BUILD_F90

# If the user specified a root for HDF5, use it.
if USE_HDF5_DIR
AM_FFLAGS += -I@HDF5DIR@/include
AM_FCFLAGS += -I@HDF5DIR@/include
AM_LDFLAGS = -L@HDF5DIR@/lib
endif

# Find header file in libsrc4 directory.
AM_FFLAGS += -I$(top_srcdir)/libsrc4
AM_FCFLAGS += -I$(top_srcdir)/libsrc4

# Since netCDF-4 is being build, link to the netcdf library in
# ../libsrc4, also link to HDF5 and HDF5 high-level libraries.
LDADD += ../libsrc4/libnetcdf.la -lhdf5_hl -lhdf5

else # not USE_NETCDF4

# Find header file in libsrc directory.
AM_FFLAGS += -I$(top_srcdir)/libsrc
AM_FCFLAGS += -I$(top_srcdir)/libsrc

# Include the C library in the link.
LDADD += ../libsrc/libnetcdf.la 

endif # USE_NETCDF4

# This is the fortran v2 test. It depends on the utilities being built
# to generate it's input file.
if BUILD_V2
if BUILD_UTILITIES
TESTPROGRAMS += ftest
ftest_SOURCES = ftest.F fortlib.c 
CLEANFILES += test.nc
endif
endif

# This is the netCDF-3 F90 test.
if BUILD_F90
AM_FFLAGS += @MOD_FLAG@../f90
AM_FCFLAGS += @MOD_FLAG@../f90
TESTPROGRAMS += tst_f90
tst_f90_SOURCES = tst_f90.f90
CLEANFILES += tst_f90.nc
endif

check_PROGRAMS = $(TESTPROGRAMS)

if BUILD_UTILITIES
# The create_fills shell creates the file fills.nc, needed by later
# tests.
TESTS = create_fills.sh $(TESTPROGRAMS)
else
TESTS = $(TESTPROGRAMS)
endif

# Tell make how to turn .m4 files into .F files.
.m4.F:
	m4 $(M4FLAGS) $< >$@

# test_get.F and test_put.f need to be distributed, so that the user
# need not have m4. fills.nc is used by test program ftest.
EXTRA_DIST = test_get.F test_put.F fills.cdl create_fills.sh

test: check
